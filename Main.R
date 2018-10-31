# Paulo Nunes
# Data: 2017-09-19 (1st version)
# MWI Project - Main module
# Considering the signals of each antenna as a case: total of 960 cases.
# Considering the signals of all 4 antennas concatenated as a case: total of 240 cases.

projectBaseDir = getwd()
source("Libraries.R")
source("ImportDataset.R")
source("PreprocessData.R")
source("FeatureExtraction.R")
source("Classify.R")
source("PostprocessData.R")

#********************************************************************************************
# SET general PARAMETERS --------------------------------------------------------------------
#********************************************************************************************
parameters.globals.minVariance = 0.00001
parameters.globals.prepareMode = "ByAntenna" # "AllAntennasConcat"# "AllAntennasMean"# "AllAntennas"#"AllAntennasBalanced"# "AllAntennasTestWithTrain"
parameters.globals.separationMode = 'Bootstrap' # 'Holdout' #'10-Fold'# #
parameters.globals.usedsignals = 'Simplified' #'Original' # 

datasetDir = "./Input Dataset/"
mainworkDir = "./Work Dataset/"
plotDir = "./Plots/"
switch(parameters.globals.prepareMode,
       "AllAntennas" = {
         workDir = "./Work Dataset/AllAntennas/"
         plotDir = "./Plots/AllAntennas/"
         resultsDir = "./Results/AllAntennas/"
       },
       "ByAntenna" = {
         workDir = "./Work Dataset/ByAntenna/"
         plotDir = "./Plots/ByAntenna/"
         resultsDir = "./Results/ByAntenna/"
       },
       "AllAntennasConcat" = {
         workDir = "./Work Dataset/AllAntennasConcat/"
         plotDir = "./Plots/AllAntennasConcat/"
         resultsDir = "./Results/AllAntennasConcat/"
       },
       "AllAntennasMean" = {
         workDir = "./Work Dataset/AllAntennasMean/"
         plotDir = "./Plots/AllAntennasMean/"
         resultsDir = "./Results/AllAntennasMean/"
       },
       "AllAntennasBalanced" = {
         mainworkDir = "./Dataset Balanced/Work Dataset/"
         workDir = "./Dataset Balanced/Work Dataset/AllAntennas/"
         plotDir = "./Dataset Balanced/Plots/AllAntennas/"
         resultsDir = "./Dataset Balanced/Results/AllAntennas/"
       }#,
#       "AllAntennasTestWithTrain" = {}
)
#********************************************************************************************
# IMPORT DATASET ----------------------------------------------------------------------------
#********************************************************************************************
tStt = Sys.time()
dataset_DF <- importDataset(datasetDir)
tStp = Sys.time()
td = tStp - tStt#Time difference of 2.975037 mins
#View(dataset_DF)

## ---------------------- SAVE DATASET ------------------------------------------------- ###
save(dataset_DF, file = paste(mainworkDir, "dataset_DF", ".RData", sep=""))
#write.csv(dataset_DF, file = paste(mainworkDir, "dataset_DT", ".csv", sep=""), na = "NA", row.names = TRUE)
#------#
#load(paste(mainworkDir, "dataset_DF", ".RData", sep=""))
if (parameters.globals.prepareMode == "AllAntennasBalanced") {
  ###################
  #Malignant=480(TumType==1;120(TumRad25)+120(TumRad50)+120(TumRad75)+120(TumRad100))+160(TumType==2)=640
  #NOT Malignant=160(TumType==3;40(TumRad25)+40(TumRad50)+40(TumRad75)+40(TumRad100))+160(TumType==4)=320
  #Duplicate rows with TumType==3 and TumType==4
  nrow(dataset_DF) #960
  dataset_DF_TTYPE3 = dataset_DF[dataset_DF$TumType==3,]
  dataset_DF_TTYPE4 = dataset_DF[dataset_DF$TumType==4,]
  dataset_DF = rbind(dataset_DF, dataset_DF_TTYPE3)
  dataset_DF = rbind(dataset_DF, dataset_DF_TTYPE4)
  rm(dataset_DF_TTYPE3)
  rm(dataset_DF_TTYPE4)
  nrow(dataset_DF) #1280
  #Malignant=480(TumType==1;120(TumRad25)+120(TumRad50)+120(TumRad75)+120(TumRad100))+160(TumType==2)=640
  #NOT Malignant=320(TumType==3;80(TumRad25)+80(TumRad50)+80(TumRad75)+80(TumRad100))+320(TumType==4)=640
  #Large=320(TumRad==75)+320(TumRad==100)=640
  #NOT Large=320(TumRad==25)+320(TumRad==50)=640
  #Microlobulated=160
  #NOT Microlobulated=1120
  #Macrolobulated=320
  #NOT Macrolobulated=960
  #Smooth=320
  #NOT Smooth=960
  #Spiculated=480
  #NOT Spiculated=800
  #Duplicate again all dataset (just to increase number of cases)
  dataset_DF = rbind(dataset_DF, dataset_DF)
  nrow(dataset_DF) #2560
  save(dataset_DF, file = paste(mainworkDir, "dataset_DF", ".RData", sep=""))
  #write.csv(dataset_DF, file = paste(mainworkDir, "dataset_DT", ".csv", sep=""), na = "NA", row.names = TRUE)
  #------#
  #load(paste(mainworkDir, "dataset_DF", ".RData", sep=""))
  ###################
}
#********************************************************************************************
# GET DIMENSIONS AND SIGNAL DATASETS --------------------------------------------------------
#********************************************************************************************
rowNumCol <- max(which(colnames(dataset_DF)=="caseNum"))
dimCol.first <- which(colnames(dataset_DF)=="TumModel")
dimCol.last <- which(colnames(dataset_DF)=="AntAng270")
sigCol.first <- which(colnames(dataset_DF)=="V1")
sigCol.last <- length(dataset_DF[1,])

tStt = Sys.time()
dimensions_DF<-getDimensionsFromDataset(dataset_DF, c(rowNumCol,dimCol.first:dimCol.last))
signal_DF<-getSignalFromDataset(dataset_DF, c(rowNumCol,sigCol.first:sigCol.last))
tStp = Sys.time()
td = tStp - tStt#ime difference of 2.635484 mins

## ---------------------- SAVE DIMENSIONS AND SIGNAL DATASETS -------------------------- ###
save(dimensions_DF, file = paste(mainworkDir, "dimensions_DF", ".RData", sep=""))
#write.csv(dimensions_DF, file = paste(mainworkDir, "dimensions_DF",".csv", sep=""), na = "NA", row.names = TRUE)
save(signal_DF, file = paste(mainworkDir, "signal_DF", ".RData", sep=""))
#write.csv(signal_DF, file = paste(mainworkDir, "signal_DF",".csv", sep=""), na = "NA", row.names = TRUE)
#------#
#load(paste(mainworkDir, "dimensions_DF", ".RData", sep=""))
#load(paste(mainworkDir, "signal_DF", ".RData", sep=""))

plotSignal_Cairo(name='Orig_', dimensions_DF, signal_DF, 15, plotDir) #plot case number 15, as example


#********************************************************************************************
# GET SIMPLIFIED SIGNAL DATASET -------------------------------------------------------------
#********************************************************************************************
if (parameters.globals.usedsignals != 'Original') {
  simpSig_DF<-removeLowVariancePoints(signal_DF, parameters.globals.minVariance)
  
  #plotSignal_Cairo(name='Simpl_', dimensions_DF, simpSig_DF, 15, plotDir) #plot case number 15, as example
  # examp <- simpSig_DF[15,-1]
  # Cairo(file='./Plots/Simpl_Signal.pdf', type="pdf", width=11, height=8.5, units="in", bg="transparent")
  # plot(1:length(examp), examp, col = "Black", main = paste("Sinal simplificado", sep=""), xlab="tempo", ylab="amplitude", pch='.')
  # dev.off()
  
  save(simpSig_DF, file = paste(mainworkDir, "signal_DF", sprintf("%f", parameters.globals.minVariance), ".RData", sep=""))
  #write.csv(simpSig_DT, file = paste(mainworkDir, "signal_DF", sprintf("%f", parameters.globals.minVariance), ".csv", sep=""), na = "NA", row.names = TRUE)
  #------#
  #load(paste(mainworkDir, "signal_DF", sprintf("%f", parameters.globals.minVariance), ".RData", sep=""))
  #signal_DF <- simpSig_DF
}

#********************************************************************************************
# PREPARE THE SIGNALS OF THE 4 ANTENNAS -----------------------------------------------------
#********************************************************************************************
#nrow(dimensions_DF)#960 rows
#nrow(simpSig_DF)#960 rows
switch(parameters.globals.prepareMode,
         "AllAntennas" = {
              result <- list(dimset=dimensions_DF, sigset=signal_DF)
         },
         "ByAntenna" = {
           result <- list(dimset=dimensions_DF, sigset=signal_DF)
         },
         "AllAntennasConcat" = {
              result <- concat4AntennasSignals(list(dimset=dimensions_DF, sigset=signal_DF))
          },
         "AllAntennasMean" = {
           result <- mean4AntennasSignals(list(dimset=dimensions_DF, sigset=signal_DF))
         },
         "AllAntennasBalanced" = {
           result <- list(dimset=dimensions_DF, sigset=signal_DF)
         },
         "AllAntennasTestWithTrain" = {}
)
dimSet <- result$dimset
sigSet <- result$sigset
rm(result)
#nrow(dimSet)#240 rows
#nrow(sigSet)#240 rows
save(dimSet, file = paste(workDir, "dimSet", ".RData", sep=""))
save(sigSet, file = paste(workDir, "sigSet", ".RData", sep=""))
#------#
#load(paste(workDir, "dimSet", ".RData", sep=""))
#load(paste(workDir, "sigSet", ".RData", sep=""))

#********************************************************************************************
# GET TRAINING AND TEST SETS ----------------------------------------------------------------
#********************************************************************************************
tStt = Sys.time()
# Train and test sets obtained with Holdout by model: 7 models for train and 3 for test
# Train and test sets obtained with Bootstrap by model: 10 models for train (some are repeated) and 3 (not used in train) for test
result<-getTrainAndTestSets(list(dimset=dimSet, sigset=sigSet), parameters.globals.separationMode)

dimTrainset<-result$trainset$dimset
sigTrainset<-result$trainset$sigset
dimTestset<-result$testset$dimset
sigTestset<-result$testset$sigset
rm(result)
gc()
tStp = Sys.time()
td = tStp - tStt#Time difference of 0.114007 (Holdout)/ 1.208069 (Bootstrap) secs

#Classifiers need that classes columns to be a factor to assume binary classification, otherwise will assume regression
#RF and SVM needs characteristics to be a factor to assume binary classification, otherwise will assume regression
dimTrainset$Malignant = as.factor(dimTrainset$Malignant)
dimTestset$Malignant = as.factor(dimTestset$Malignant)
dimTrainset$Large = as.factor(dimTrainset$Large)
dimTestset$Large = as.factor(dimTestset$Large)
dimTrainset$TumMicrolob = as.factor(dimTrainset$TumMicrolob)
dimTestset$TumMicrolob = as.factor(dimTestset$TumMicrolob)
dimTrainset$TumSmooth = as.factor(dimTrainset$TumSmooth)
dimTestset$TumSmooth = as.factor(dimTestset$TumSmooth)
dimTrainset$TumRad75 = as.factor(dimTrainset$TumRad75)
dimTestset$TumRad75 = as.factor(dimTestset$TumRad75)
dimTrainset$TumRad25 = as.factor(dimTrainset$TumRad25)
dimTestset$TumRad25 = as.factor(dimTestset$TumRad25)

save(dimTrainset, file = paste(workDir, "dimTrainset", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".RData", sep=""))
#write.csv(dimTrainset, file = paste(workDir, "dimTrainset", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".csv", sep=""), na = "NA", row.names = TRUE)
save(dimTestset, file = paste(workDir, "dimTestset", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".RData", sep=""))
#write.csv(dimTestset, file = paste(workDir, "dimTestset", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".csv", sep=""), na = "NA", row.names = TRUE)
save(sigTrainset, file = paste(workDir, "sigTrainset", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".RData", sep=""))
#write.csv(sigTrainset, file = paste(workDir, "sigTrainset", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".csv", sep=""), na = "NA", row.names = FALSE)
#write.table(sigTrainset[,-1], paste(workDir, "sigTrainset", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".txt", sep=""), sep=" ", row.names = FALSE)
save(sigTestset, file = paste(workDir, "sigTestset", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".RData", sep=""))
#write.csv(sigTestset, file = paste(workDir, "sigTestset", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".csv", sep=""), na = "NA", row.names = TRUE)
#------#
# load(paste(workDir, "dimTrainset", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".RData", sep=""))
# load(paste(workDir, "dimTestset", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".RData", sep=""))
# load(paste(workDir, "sigTrainset", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".RData", sep=""))
# load(paste(workDir, "sigTestset", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".RData", sep=""))

if (parameters.globals.prepareMode == 'ByAntenna') {
  #********************************************************************************************
  # SPLIT DATASETS BY ANTENNA -----------------------------------------------------------------
  #********************************************************************************************
  processDataset <- splitByAntenna(sigTrainset, dimTrainset, sigTestset, dimTestset)
} else {
  processDataset <- list()
  processDataset[[1]] <- list(sigtrain=sigTrainset, dimtrain=dimTrainset, sigtest=sigTestset, dimtest=dimTestset)
}
rm(dimTrainset)
rm(dimTestset)
rm(sigTrainset)
rm(sigTestset)
save(processDataset, file = paste(workDir, "processDataset", "_", parameters.globals.separationMode, ".RData", sep=""))
#------#
# load(paste(workDir, "processDataset", "_", parameters.globals.separationMode, ".RData", sep=""))

parameters.globals.trainCases = nrow(processDataset[[1]]$sigtrain)
parameters.globals.testCases = nrow(processDataset[[1]]$sigtest)
#Number of column of classes to be classified
malignantClassCol=7#Malignant
largeClassCol=12#Large
microlobClassCol=10#TumMicrolob
smoothClassCol=8#TumSmooth
rad75ClassCol=15#TumRad75
rad25ClassCol=13#TumRad25

#********************************************************************************************
# SET EXTRACT & CLASSIFICATION PARAMETERS ---------------------------------------------------
#********************************************************************************************
#parameters.dwt.filter:
# = Daubechies:"d2,4,6,8,10,12,14,16,18,20" 
# = Least Asymetric:"la8,10,12,14,16,18,20"
# = Best Localized:"bl14,18,20"
# = Coiflet:"c6,12,18,24,30"
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4927172/:
# - wavelet length impacts the sensitivity of the method to detect differences between health and disease and tunes classification accuracy
# - filters (Daubechies Extremal Phase, Daubechies Least Asymmetric, and Coiflet families), and lengths (2–24)
# - wavelet length of 8 or greater
# 
#parameters.dwt.level:
# = 5
#parameters.dwt.bound
#= "periodic", "reflection"
parameters.dwt <- data.frame(filter=as.character(c('c6','c12','d6','d8')),
                             level=as.numeric(c(5,5,5,5)),
                             bound=as.character(c('periodic','periodic','periodic','periodic')))

#parameters.interp.levels
#= 5, 
#parameters.interp.points
#= 2, 4
#parameters.interp.epsilon
#= 0.0001,
#parameters.interp.nansubs
#= 0, 0.2 (max=0.170557)
parameters.interp <- data.frame(levels=as.numeric(c(5,4,5)),
                                interp_points=as.numeric(c(4,4,2)),
                                epsilon=as.double(c(0.0001,0.0002,0.0001)))

#parameters.pca.scale
#= T, F
#parameters.pca.tol
#= 0.00001
parameters.pca <- data.frame(scale=as.logical(c(T,T,T,T,T,T)),
                             tol=as.double(c(0.1,0.01,0.001,0.0001,0.00001,0.000001)),
                             comp=as.numeric(c(5,10,20, 30, 40, 50)))

#parameters.svm.kern
#= 'linear', linear, polynomial, radial basis, sigmoid
#parameters.svm.cost
#= 0.001, 0.01, 0.1, 1, 5, 10, 100
#parameters.svm.gam
#= 0.5, 1, 2, 3, 4
#####parameters.svm <- data.frame(kern=as.character(c('linear','linear','linear')), cost=as.numeric(c(0.01,10,0.01)), gam=as.numeric(c(0.5,1,1)))#gam=1/ncol
parameters.svm <- data.frame()
svm.tuned <- svmTune(trainset = processDataset[[1]]$sigtrain, dimset = processDataset[[1]]$dimtrain, classCol = malignantClassCol, kern=as.character('radial'), rang = list(gamma = 10^(-3:0), cost = 10^(1:5)))
parameters.svm <- rbind(parameters.svm, data.frame(kern=as.character('radial'), cost=svm.tuned$best.parameters$cost, gam=svm.tuned$best.parameters$gamma))
svm.tuned <- svmTune(trainset = processDataset[[1]]$sigtrain, dimset = processDataset[[1]]$dimtrain, classCol = largeClassCol, kern=as.character('radial'), rang = list(gamma = 10^(-5:-1), cost = 10^(1:5)))
parameters.svm <- rbind(parameters.svm, data.frame(kern=as.character('radial'), cost=svm.tuned$best.parameters$cost, gam=svm.tuned$best.parameters$gamma))
# svm.tuned <- svmTune(trainset = sigTrainset, dimset = dimTrainset, classCol = largeClassCol, kern=as.character('linear'), rang = list(gamma = 10^(-5:-1), cost = 10^(-1:3)))
# parameters.svm <- rbind(parameters.svm, data.frame(kern=as.character('linear'), cost=svm.tuned$best.parameters$cost, gam=svm.tuned$best.parameters$gamma))
# svm.tuned <- svmTune(trainset = sigTrainset, dimset = dimTrainset, classCol = malignantClassCol, kern=as.character('linear'), rang = list(gamma = 10^(-4:-1), cost = 10^(1:4)))
# parameters.svm <- rbind(parameters.svm, data.frame(kern=as.character('linear'), cost=svm.tuned$best.parameters$cost, gam=svm.tuned$best.parameters$gamma))
rm(svm.tuned)
save(parameters.svm, file = paste(workDir, "parameters.svm", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".RData", sep=""))
#load(paste(workDir, "parameters.svm", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".RData", sep=""))
#parameters.svm.kern=radial
#parameters.svm.cost=10
#parameters.svm.gam=0.01 (Malignant);0.001 (Large)
#parameters.svm <- data.frame(kern=as.character(c('radial','radial','radial','radial','radial','radial','radial','radial','radial','radial')),
#                             cost=as.numeric(c(5,10,15,20,25,30,35,40,45,50)),
#                             gam=as.numeric(c(0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01)))


# List to accumulate all run parameters and results 
runResults <- list()

#********************************************************************************************
# DWT+SVM  (Stage classification: Malignant, Large, Microlobulated, Smooth, Rad75, Rad25)
#          Can´t obtain results: only few cases, on second stage there's only one class
#********************************************************************************************
# stt=Sys.time()
# #List of each run parameters and results
# runResults <- list()
# run <- list()
# res.dwt <- apply(parameters.dwt, 1, function(fd) applyDWT(fd['filter'], fd['level'], fd['bound'], trainset = sigTrainset, testset = sigTestset))
# for (d in 1:nrow(parameters.dwt)){
#   run$parameters.pca <- list(scale = NULL, tol = NULL, components = NULL)
#   run$parameters.rf <- NULL
#   run$parameters.dwt <- list(filter = parameters.dwt[d,'filter'], level = parameters.dwt[d,'level'], bound = parameters.dwt[d,'bound'], coeff = ncol(res.dwt[[d]]$test))
#  #Malignant
#  res.svmMal <- apply(parameters.svm, 1, function(fs) svmClassify_Malignant(fs['kern'], fs['cost'], fs['gam'], classCol = malignantClassCol, trainset = res.dwt[[d]]$train, dimTrainset = dimTrainset, testset = res.dwt[[d]]$test, dimTestset = dimTestset))
#  for (s in 1:nrow(parameters.svm)){
#    run$name <- "DWT_SVM-Malignant"
#    run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
#                                   extract1 = "DWT", extract2 = "", classif = "SVM", class = "Malignant",
#                                   classCol = malignantClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
#    run$parameters.svm <- list(kern = parameters.svm[s,'kern'], cost = parameters.svm[s,'cost'], gam = parameters.svm[s,'gam'])
#    run$results <- list(model=res.svmMal[[s]]$model, pred=res.svmMal[[s]]$pred, prob=res.svmMal[[s]]$prob, tbl=res.svmMal[[s]]$tbl, testcases=nrow(res.dwt[[d]]$test))
#    runResults[[length(runResults)+1]] <- run
#    #Split trainsets using the real Malignant value (in the original dataset)
#    trainset_Malignant <- res.dwt[[d]]$train[res.dwt[[d]]$train$caseNum %in% dimTrainset[dimTrainset$Malignant == 1,]$caseNum,]
#    dimTrainset_Malignant <- dimTrainset[dimTrainset$caseNum %in% trainset_Malignant$caseNum,]
#    trainset_notMalignant <- res.dwt[[d]]$train[res.dwt[[d]]$train$caseNum %in% dimTrainset[dimTrainset$Malignant == 0,]$caseNum,]
#    dimTrainset_notMalignant <- dimTrainset[dimTrainset$caseNum %in% trainset_notMalignant$caseNum,]
#    #Split testsets using Malignant value from previous classification - Missclassification will occur ???????
#    testset_Malignant <- res.dwt[[d]]$test[res.dwt[[d]]$test$caseNum %in% res.svmMal[[s]]$pred[res.svmMal[[s]]$pred$pred == 1,]$caseNum,]
#    dimTestset_Malignant <- dimTestset[dimTestset$caseNum %in% testset_Malignant$caseNum,]
#    testset_notMalignant <- res.dwt[[d]]$test[res.dwt[[d]]$test$caseNum %in% res.svmMal[[s]]$pred[res.svmMal[[s]]$pred$pred == 0,]$caseNum,]
#    dimTestset_notMalignant <- dimTestset[dimTestset$caseNum %in% testset_notMalignant$caseNum,]
#    res.svmMic <- apply(parameters.svm, 1, function(fs) svmClassify_Microlob(fs['kern'], fs['cost'], fs['gam'], classCol = microlobClassCol, trainset = trainset_Malignant, dimTrainset = dimTrainset_Malignant, testset = testset_Malignant, dimTestset = dimTestset_Malignant))
#    for (ss in 1:nrow(parameters.svm)){
#      run$name <- "DWT_SVM-Malignant_Microlobulated"
#      run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
#                                     extract1 = "DWT", extract2 = "", classif = "SVM", class = "Malignant_Microlobulated",
#                                     classCol = microlobClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
#      run$parameters.svm <- list(kern = parameters.svm[ss,'kern'], cost = parameters.svm[ss,'cost'], gam = parameters.svm[ss,'gam'])
#      run$results <- list(model=res.svmMic[[ss]]$model, pred=res.svmMic[[ss]]$pred, prob=res.svmMic[[ss]]$prob, tbl=res.svmMic[[ss]]$tbl, testcases=nrow(testset_Malignant))
#      runResults[[length(runResults)+1]] <- run
#    }
#    res.svmSmo <- apply(parameters.svm, 1, function(fs) svmClassify_Smooth(fs['kern'], fs['cost'], fs['gam'], classCol = smoothClassCol, trainset = trainset_notMalignant, dimTrainset = dimTrainset_notMalignant, testset = testset_notMalignant, dimTestset = dimTestset_notMalignant))
#    for (ss in 1:nrow(parameters.svm)){
#      run$name <- "DWT_SVM-notMalignant_Smooth"
#      run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
#                                     extract1 = "DWT", extract2 = "", classif = "SVM", class = "notMalignant_Smooth",
#                                     classCol = smoothClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
#      run$parameters.svm <- list(kern = parameters.svm[ss,'kern'], cost = parameters.svm[ss,'cost'], gam = parameters.svm[ss,'gam'])
#      run$results <- list(model=res.svmSmo[[ss]]$model, pred=res.svmSmo[[ss]]$pred, prob=res.svmSmo[[ss]]$prob, tbl=res.svmSmo[[ss]]$tbl, testcases=nrow(testset_notMalignant))
#      runResults[[length(runResults)+1]] <- run
#    }
#  }
#  #Large
#  res.svmLar <- apply(parameters.svm, 1, function(fs) svmClassify_Large(fs['kern'], fs['cost'], fs['gam'], classCol = largeClassCol, trainset = res.dwt[[d]]$train, dimTrainset = dimTrainset, testset = res.dwt[[d]]$test, dimTestset = dimTestset))
#  for (s in 1:nrow(parameters.svm)){
#    run$name <- "DWT_SVM-Large"
#    run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
#                                   extract1 = "DWT", extract2 = "", classif = "SVM", class = "Large",
#                                   classCol = largeClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
#    run$parameters.svm <- list(kern = parameters.svm[s,'kern'], cost = parameters.svm[s,'cost'], gam = parameters.svm[s,'gam'])
#    run$results <- list(model=res.svmLar[[s]]$model, pred=res.svmLar[[s]]$pred, prob=res.svmLar[[s]]$prob, tbl=res.svmLar[[s]]$tbl, testcases=nrow(res.dwt[[d]]$test))
#    runResults[[length(runResults)+1]] <- run
#    #Split trainsets using the real Large value (in the original dataset)
#    trainset_Large <- res.dwt[[d]]$train[res.dwt[[d]]$train$caseNum %in% dimTrainset[dimTrainset$Large == 1,]$caseNum,]
#    dimTrainset_Large <- dimTrainset[dimTrainset$caseNum %in% trainset_Large$caseNum,]
#    trainset_notLarge <- res.dwt[[d]]$train[res.dwt[[d]]$train$caseNum %in% dimTrainset[dimTrainset$Large == 0,]$caseNum,]
#    dimTrainset_notLarge <- dimTrainset[dimTrainset$caseNum %in% trainset_notLarge$caseNum,]
#    #Split testsets using Large value from previous classification - Missclassification will occur ???????
#    testset_Large <- res.dwt[[d]]$test[res.dwt[[d]]$test$caseNum %in% res.svmLar[[s]]$pred[res.svmLar[[s]]$pred$pred == 1,]$caseNum,]
#    dimTestset_Large <- dimTestset[dimTestset$caseNum %in% testset_Large$caseNum,]
#    testset_notLarge <- res.dwt[[d]]$test[res.dwt[[d]]$test$caseNum %in% res.svmLar[[s]]$pred[res.svmLar[[s]]$pred$pred == 0,]$caseNum,]
#    dimTestset_notLarge <- dimTestset[dimTestset$caseNum %in% testset_notLarge$caseNum,]
#    res.svmR75 <- apply(parameters.svm, 1, function(fs) svmClassify_Rad75(fs['kern'], fs['cost'], fs['gam'], classCol = rad75ClassCol, trainset = trainset_Large, dimTrainset = dimTrainset_Large, testset = testset_Large, dimTestset = dimTestset_Large))
#    for (ss in 1:nrow(parameters.svm)){
#      run$name <- "DWT_SVM-Large_Rad75"
#      run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
#                                     extract1 = "DWT", extract2 = "", classif = "SVM", class = "Large_Rad75",
#                                     classCol = rad75ClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
#      run$parameters.svm <- list(kern = parameters.svm[ss,'kern'], cost = parameters.svm[ss,'cost'], gam = parameters.svm[ss,'gam'])
#      run$results <- list(model=res.svmR75[[ss]]$model, pred=res.svmR75[[ss]]$pred, prob=res.svmR75[[ss]]$prob, tbl=res.svmR75[[ss]]$tbl, testcases=nrow(testset_Large))
#      runResults[[length(runResults)+1]] <- run
#    }
#    res.svmR25 <- apply(parameters.svm, 1, function(fs) svmClassify_Rad25(fs['kern'], fs['cost'], fs['gam'], classCol = rad25ClassCol, trainset = trainset_notLarge, dimTrainset = dimTrainset_notLarge, testset = testset_notLarge, dimTestset = dimTestset_notLarge))
#    for (ss in 1:nrow(parameters.svm)){
#      run$name <- "DWT_SVM-notLarge_Rad25"
#      run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
#                                     extract1 = "DWT", extract2 = "", classif = "SVM", class = "notLarge_Rad25",
#                                     classCol = rad25ClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
#      run$parameters.svm <- list(kern = parameters.svm[ss,'kern'], cost = parameters.svm[ss,'cost'], gam = parameters.svm[ss,'gam'])
#      run$results <- list(model=res.svmR25[[ss]]$model, pred=res.svmR25[[ss]]$pred, prob=res.svmR25[[ss]]$prob, tbl=res.svmR25[[ss]]$tbl, testcases=nrow(testset_notLarge))
#      runResults[[length(runResults)+1]] <- run
#    }
#  }
# }
# stp=Sys.time()
# ttDSst <- stp - stt#Time difference of 2.21476 mins, 80 runs
# save(runResults, file = paste(resultsDir, "runResults_DWT-SVM_stages", "_", parameters.globals.separationMode, ".RData", sep=""))
# saveResults(runResults, paste(resultsDir, "Results_DWT-SVM_stages", "_", parameters.globals.separationMode, sep=""))
# 
# rm(trainset_Malignant)
# rm(dimTrainset_Malignant)
# rm(trainset_notMalignant)
# rm(dimTrainset_notMalignant)
# rm(testset_Malignant)
# rm(dimTestset_Malignant)
# rm(testset_notMalignant)
# rm(dimTestset_notMalignant)
# rm(trainset_Large)
# rm(dimTrainset_Large)
# rm(trainset_notLarge)
# rm(dimTrainset_notLarge)
# rm(testset_Large)
# rm(dimTestset_Large)
# rm(testset_notLarge)
# rm(dimTestset_notLarge)
# rm(res.dwt)
# rm(res.svmMal)
# rm(res.svmMic)
# rm(res.svmSmo)
# rm(res.svmLar)
# rm(res.svmR75)
# rm(res.svmR25)
# rm(run)

#********************************************************************************************
# DWT+SVM  (Malignant, Large) ---------------------------------------------------------------
#********************************************************************************************
stt=Sys.time()
runResultsList <- list()
for(ant in 1:length(processDataset)){
  # List of each run parameters and results 
  runResults <- list()
  run <- list()
  run$parameters.pca <- list(scale = NULL, tol = NULL, components = NULL)
  run$parameters.interp <- list(levels = NULL, interp_points = NULL, epsilon=NULL, points=NULL)
  #run$parameters.dwt <- list(filter = NULL, level = NULL, bound = NULL, coeff = NULL)
  #run$parameters.svm <- list(kern = NULL, cost = NULL, gam = NULL)
  run$parameters.rf <- NULL
  run$parameters.lda <- NULL
  sigTrainset <- processDataset[[ant]]$sigtrain
  dimTrainset <- processDataset[[ant]]$dimtrain
  sigTestset <- processDataset[[ant]]$sigtest
  dimTestset <- processDataset[[ant]]$dimtest
  res.dwt <- apply(parameters.dwt, 1, function(fd) applyDWT(fd['filter'], fd['level'], fd['bound'], trainset = sigTrainset, testset = sigTestset))
  for (d in 1:nrow(parameters.dwt)){
    run$parameters.dwt <- list(filter = parameters.dwt[d,'filter'], level = parameters.dwt[d,'level'], bound = parameters.dwt[d,'bound'], coeff = ncol(res.dwt[[d]]$test))
    #Malignant
    #res.svm <- apply(parameters.svm, 1, function(fs) svmClassify_Malignant(fs['kern'], fs['cost'], fs['gam'], classCol = malignantClassCol, trainset = res.dwt[[d]]$train, dimTrainset = dimTrainset, testset = res.dwt[[d]]$test, dimTestset = dimTestset))
    #Optimal SVM parameters for Malignant are in row 1
    res.svm <- list()
    res.svm[[1]] <- svmClassify_Malignant(parameters.svm[1,'kern'], parameters.svm[1,'cost'], parameters.svm[1,'gam'], classCol = malignantClassCol, trainset = res.dwt[[d]]$train, dimTrainset = dimTrainset, testset = res.dwt[[d]]$test, dimTestset = dimTestset)
    for (s in 1:length(res.svm)){
      run$name <- "DWT_SVM-Malignant"
      run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
                                     extract1 = "DWT", extract2 = "", classif = "SVM", class = "Malignant",
                                     classCol = malignantClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
      run$parameters.svm <- list(kern = parameters.svm[s,'kern'], cost = parameters.svm[s,'cost'], gam = parameters.svm[s,'gam'])
      #res.eval <- getPerformance(res.svm[[s]]$model, res.dwt[[d]]$test, dimTestset, malignantClassCol)
      #run$results <- list(model=res.svm[[s]]$model, pred=res.svm[[s]]$pred, prob=res.svm[[s]]$prob, tbl=res.svm[[s]]$tbl, roc=res.eval$roc, auc=res.eval$auc, confmatrix=res.eval$confmatrix, perf=res.eval$perf)
      run$results <- list(model=res.svm[[s]]$model, pred=res.svm[[s]]$pred, prob=res.svm[[s]]$prob, tbl=res.svm[[s]]$tbl, testcases=nrow(res.dwt[[d]]$test))
      runResults[[length(runResults)+1]] <- run
    }
    #Large
    # res.svm <- apply(parameters.svm, 1, function(fs) svmClassify_Large(fs['kern'], fs['cost'], fs['gam'], classCol = largeClassCol, trainset = res.dwt[[d]]$train, dimTrainset = dimTrainset, testset = res.dwt[[d]]$test, dimTestset = dimTestset))
    #Optimal SVM parameters for Large are in row 2
    res.svm <- list()
    res.svm[[1]] <- svmClassify_Large(parameters.svm[2,'kern'], parameters.svm[2,'cost'], parameters.svm[2,'gam'], classCol = largeClassCol, trainset = res.dwt[[d]]$train, dimTrainset = dimTrainset, testset = res.dwt[[d]]$test, dimTestset = dimTestset)
    for (s in 1:length(res.svm)){
      run$name <- "DWT_SVM-Large"
      run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
                                     extract1 = "DWT", extract2 = "", classif = "SVM", class = "Large",
                                     classCol = largeClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
      run$parameters.svm <- list(kern = parameters.svm[s,'kern'], cost = parameters.svm[s,'cost'], gam = parameters.svm[s,'gam'])
      #res.eval <- getPerformance(res.svm[[s]]$model, res.dwt[[d]]$test, dimTestset, largeClassCol)
      #run$results <- list(model=res.svm[[s]]$model, pred=res.svm[[s]]$pred, prob=res.svm[[s]]$prob, tbl=res.svm[[s]]$tbl, roc=res.eval$roc, auc=res.eval$auc, confmatrix=res.eval$confmatrix, perf=res.eval$perf)
      run$results <- list(model=res.svm[[s]]$model, pred=res.svm[[s]]$pred, prob=res.svm[[s]]$prob, tbl=res.svm[[s]]$tbl, testcases=nrow(res.dwt[[d]]$test))
      runResults[[length(runResults)+1]] <- run
    }
  }
  runResultsList[[ant]] <- runResults
  rm(run)
  rm(runResults)
  rm(dimTrainset)
  rm(dimTestset)
  rm(sigTrainset)
  rm(sigTestset)
}
if (parameters.globals.prepareMode == 'ByAntenna') {
  save(runResultsList, file = paste(resultsDir, "runResultsList_DWT-SVM", "_", parameters.globals.separationMode, ".RData", sep=""))
  # load(paste(resultsDir, "runResultsList_DWT-SVM", "_", parameters.globals.separationMode, ".RData", sep=""))
  computeAntennasVoting(dimtest=processDataset[[ant]]$dimtest, results4Ant=runResultsList, classificationType="svm")
}
runResults <- runResultsList[[1]]
stp=Sys.time()
ttDS <- stp - stt#Time difference of 1.784235(holdout)/2.541912(bootstrap)/5.000336(original) mins, runs=8
save(runResults, file = paste(resultsDir, "runResults_DWT-SVM", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".RData", sep=""))
saveResults(runResults, paste(resultsDir, "Results_DWT-SVM", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, sep=""))

rm(res.dwt)
rm(res.svm)


#********************************************************************************************
# Interp+SVM (Malignant, Large) -------------------------------------------------------------
#********************************************************************************************
stt=Sys.time()
# List of each run parameters and results
#Interpolation was disregarded because of points become NaN: classifiers does not handle NaN values
#NEW APPROACH: Replace all NaNs for the original value in columns that have a non NaN value at least in one row of all the dataset (train+test) all NaNs
runResultsList <- list()
for(ant in 1:length(processDataset)){
  runResults <- list()
  run <- list()
  run$parameters.pca <- list(scale = NULL, tol = NULL, components = NULL)
  run$parameters.dwt <- list(filter = NULL, level = NULL, bound = NULL, coeff = NULL)
  #run$parameters.interp <- list(levels = NULL, interp_points = NULL, epsilon=NULL, points=NULL)
  #run$parameters.svm <- list(kern = NULL, cost = NULL, gam = NULL)
  run$parameters.rf <- NULL
  run$parameters.lda <- NULL
  sigTrainset <- processDataset[[ant]]$sigtrain
  dimTrainset <- processDataset[[ant]]$dimtrain
  sigTestset <- processDataset[[ant]]$sigtest
  dimTestset <- processDataset[[ant]]$dimtest

  dyn.load("./interpV2_0.dll")
  res.interp <- list()
  #Next lines are only for tests. Comment them otherwise
  # res.interp[[1]] <- applyInterp(parameters.interp$levels[2], parameters.interp$interp_points[2], parameters.interp$epsilon[2], trainset=sigTrainset, testset=sigTestset)
  # plotSignal_Cairo(name='Interp_woNans_notSimplified_', dimTrainset, res.interp[[1]]$train, 13, plotDir)
  # res.svm[[1]] <- svmClassify_Malignant(parameters.svm[1,'kern'], parameters.svm[1,'cost'], parameters.svm[1,'gam'], classCol = malignantClassCol, trainset = res.interp[[1]]$train, dimTrainset = dimTrainset, testset = res.interp[[1]]$test, dimTestset = dimTestset)
  #
  # run$name <- "Interp_SVM-Malignant"
  # run$parameters.interp <- list(levels = parameters.interp$levels[2], interp_points = parameters.interp$interp_points[2], epsilon = parameters.interp$epsilon[2], points = ncol(res.interp[[1]]$test))
  # run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
  #                                extract1 = "Interp", extract2 = "", classif = "SVM", class = "Malignant",
  #                                classCol = malignantClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
  # run$parameters.svm <- list(kern = parameters.svm[1,'kern'], cost = parameters.svm[1,'cost'], gam = parameters.svm[1,'gam'])
  # run$results <- list(model=res.svm[[1]]$model, pred=res.svm[[1]]$pred, prob=res.svm[[1]]$prob, tbl=res.svm[[1]]$tbl, testcases=nrow(res.interp[[1]]$test))
  # runResults[[length(runResults)+1]] <- run
  # saveResults(runResults, paste(resultsDir, "Results_Interp-SVM_only1cycle", "_", parameters.globals.separationMode, sep=""))
  # #
  res.interp[[1]] <- applyInterp(parameters.interp$levels[1], parameters.interp$interp_points[1], parameters.interp$epsilon[1], trainset=sigTrainset, testset=sigTestset)
  #
  for (a in 1:nrow(parameters.interp)){
    res.interp[[a]] <- applyInterp(parameters.interp$levels[a], parameters.interp$interp_points[a], parameters.interp$epsilon[a], trainset=sigTrainset, testset=sigTestset)
  }
  dyn.unload("./interpV2_0.dll")
  save(res.interp, file = paste(workDir, "res.interp_", parameters.globals.usedsignals, "-",  parameters.globals.separationMode, "-", parameters.globals.prepareMode ,".RData", sep=""))
  #load(paste(workDir, "res.interp_", parameters.globals.usedsignals, "-",  parameters.globals.separationMode, "-", parameters.globals.prepareMode ,".RData", sep=""))
  
  #stt=Sys.time()
  for (i in 1:nrow(parameters.interp)){
    run$parameters.interp <- list(levels = parameters.interp[i,'levels'], interp_points = parameters.interp[i,'interp_points'], epsilon = parameters.interp[i,'epsilon'], points = ncol(res.interp[[i]]$test))
    #Malignant
    #Optimal SVM parameters for Malignant are in row 1
    res.svm <- list()
    res.svm[[1]] <- svmClassify_Malignant(parameters.svm[1,'kern'], parameters.svm[1,'cost'], parameters.svm[1,'gam'], classCol = malignantClassCol, trainset = res.interp[[i]]$train, dimTrainset = dimTrainset, testset = res.interp[[i]]$test, dimTestset = dimTestset)
    for (s in 1:length(res.svm)){
      run$name <- "Interp_SVM-Malignant"
      run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
                                     extract1 = "Interp", extract2 = "", classif = "SVM", class = "Malignant",
                                     classCol = largeClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
      run$parameters.svm <- list(kern = parameters.svm[s,'kern'], cost = parameters.svm[s,'cost'], gam = parameters.svm[s,'gam'])
      #res.eval <- getPerformance(res.svm[[s]]$model, res.interp[[i]]$test, dimTestset, malignantClassCol)
      #run$results <- list(model=res.svm[[s]]$model, pred=res.svm[[s]]$pred, prob=res.svm[[s]]$prob, tbl=res.svm[[s]]$tbl, roc=res.eval$roc, auc=res.eval$auc, confmatrix=res.eval$confmatrix, perf=res.eval$perf)
      run$results <- list(model=res.svm[[s]]$model, pred=res.svm[[s]]$pred, prob=res.svm[[s]]$prob, tbl=res.svm[[s]]$tbl, testcases=nrow(dimTestset))
      runResults[[length(runResults)+1]] <- run
    }
    #Large
    #Optimal SVM parameters for Large are in row 2
    #res.svm <- apply(parameters.svm, 1, function(s) svmClassify_Large(s['kern'], s['cost'], s['gam'], classCol = largeClassCol, trainset = res.interp[[i]]$train, dimTrainset = dimTrainset, testset = res.interp[[i]]$test, dimTestset = dimTestset))
    res.svm <- list()
    res.svm[[1]] <- svmClassify_Large(parameters.svm[2,'kern'], parameters.svm[2,'cost'], parameters.svm[2,'gam'], classCol = largeClassCol, trainset = res.interp[[i]]$train, dimTrainset = dimTrainset, testset = res.interp[[i]]$test, dimTestset = dimTestset)
    for (s in 1:length(res.svm)){
      run$name <- "Interp_SVM-Large"
      run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
                                     extract1 = "Interp", extract2 = "", classif = "SVM", class = "Large",
                                     classCol = largeClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
      run$parameters.svm <- list(kern = parameters.svm[s,'kern'], cost = parameters.svm[s,'cost'], gam = parameters.svm[s,'gam'])
      #res.eval <- getPerformance(res.svm[[s]]$model, res.interp[[i]]$test, dimTestset, largeClassCol)
      #run$results <- list(model=res.svm[[s]]$model, pred=res.svm[[s]]$pred, prob=res.svm[[s]]$prob, tbl=res.svm[[s]]$tbl, roc=res.eval$roc, auc=res.eval$auc, confmatrix=res.eval$confmatrix, perf=res.eval$perf)
      run$results <- list(model=res.svm[[s]]$model, pred=res.svm[[s]]$pred, prob=res.svm[[s]]$prob, tbl=res.svm[[s]]$tbl, testcases=nrow(dimTestset))
      runResults[[length(runResults)+1]] <- run
    }
  }
  runResultsList[[ant]] <- runResults
  rm(run)
  rm(runResults)
  rm(dimTrainset)
  rm(dimTestset)
  rm(sigTrainset)
  rm(sigTestset)
}
if (parameters.globals.prepareMode == 'ByAntenna') {
  save(runResultsList, file = paste(resultsDir, "runResultsList_Interp-SVM", "_", parameters.globals.separationMode, ".RData", sep=""))
  # load(paste(resultsDir, "runResultsList_Interp-SVM", "_", parameters.globals.separationMode, ".RData", sep=""))
  computeAntennasVoting(dimtest=processDataset[[ant]]$dimtest, results4Ant=runResultsList, classificationType="svm")
}
runResults <- runResultsList[[1]]
stp=Sys.time()
ttIS <- stp - stt #Time difference of 3.231485(simplified)/ 13.31514(Original) mins, runs=6
save(runResults, file = paste(resultsDir, "runResults_Interp-SVM", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals,".RData", sep=""))
#load(paste(workDir, "runResults_Interp-SVM", "_", parameters.globals.separationMode, ".RData", sep=""))
saveResults(runResults, paste(resultsDir, "Results_Interp-SVM", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, sep=""))
#
rm(res.interp)
rm(res.svm)

#********************************************************************************************
# PCA+SVM (Malignant, Large) ----------------------------------------------------------------
#********************************************************************************************
stt=Sys.time()
# List of each run parameters and results 
runResultsList <- list()
for(ant in 1:length(processDataset)){
  runResults <- list()
  run <- list()
  #run$parameters.pca <- list(scale = NULL, tol = NULL, components = NULL)
  run$parameters.dwt <- list(filter = NULL, level = NULL, bound = NULL, coeff = NULL)
  run$parameters.interp <- list(levels = NULL, interp_points = NULL, epsilon=NULL, points=NULL)
  #run$parameters.svm <- list(kern = NULL, cost = NULL, gam = NULL)
  run$parameters.rf <- NULL
  run$parameters.lda <- NULL
  sigTrainset <- processDataset[[ant]]$sigtrain
  dimTrainset <- processDataset[[ant]]$dimtrain
  sigTestset <- processDataset[[ant]]$sigtest
  dimTestset <- processDataset[[ant]]$dimtest
  res.pca <- apply(parameters.pca, 1, function(p) applyPCA(trainset=sigTrainset, testset = sigTestset,
                                                           scale = p['scale'], tol = p['tol'], compNbr = p['comp']))
  for (p in 1:nrow(parameters.pca)){
    run$parameters.pca <- list(scale = parameters.pca[p,'scale'], tol = parameters.pca[p,'tol'], components = ncol(res.pca[[p]]$test))
    #Malignant
    #Optimal SVM parameters for Malignant are in row 1
    #res.svm <- apply(parameters.svm, 1, function(s) svmClassify_Malignant(s['kern'], s['cost'], s['gam'], classCol = malignantClassCol, trainset = res.pca[[p]]$train, dimTrainset = dimTrainset, testset = res.pca[[p]]$test, dimTestset = dimTestset))
    res.svm <- list()
    res.svm[[1]] <- svmClassify_Malignant(parameters.svm[1,'kern'], parameters.svm[1,'cost'], parameters.svm[1,'gam'], classCol = malignantClassCol, trainset = res.pca[[p]]$train, dimTrainset = dimTrainset, testset = res.pca[[p]]$test, dimTestset = dimTestset)
    for (s in 1:length(res.svm)){
      run$name <- "PCA_SVM-Malignant"
      run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
                                     extract1 = "PCA", extract2 = "", classif = "SVM", class = "Malignant",
                                     classCol = malignantClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
      run$parameters.svm <- list(kern = parameters.svm[s,'kern'], cost = parameters.svm[s,'cost'], gam = parameters.svm[s,'gam'])
      #res.eval <- getPerformance(res.svm[[s]]$model, res.pca[[p]]$test, dimTestset, malignantClassCol)
      #run$results <- list(model=res.svm[[s]]$model, pred=res.svm[[s]]$pred, prob=res.svm[[s]]$prob, tbl=res.svm[[s]]$tbl, roc=res.eval$roc, auc=res.eval$auc, confmatrix=res.eval$confmatrix, perf=res.eval$perf)
      run$results <- list(model=res.svm[[s]]$model, pred=res.svm[[s]]$pred, prob=res.svm[[s]]$prob, tbl=res.svm[[s]]$tbl, testcases=nrow(res.pca[[p]]$test))
      runResults[[length(runResults)+1]] <- run
    }
    #Large
    #Optimal SVM parameters fo Large are in row 2
    #res.svm <- apply(parameters.svm, 1, function(s) svmClassify_Large(s['kern'], s['cost'], s['gam'], classCol = largeClassCol, trainset = res.pca[[p]]$train, dimTrainset = dimTrainset, testset = res.pca[[p]]$test, dimTestset = dimTestset))
    res.svm <- list()
    res.svm[[1]] <- svmClassify_Large(parameters.svm[2,'kern'], parameters.svm[2,'cost'], parameters.svm[2,'gam'], classCol = largeClassCol, trainset = res.pca[[p]]$train, dimTrainset = dimTrainset, testset = res.pca[[p]]$test, dimTestset = dimTestset)
    for (s in 1:length(res.svm)){
      run$name <- "PCA_SVM-Large"
      run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
                                     extract1 = "PCA", extract2 = "", classif = "SVM", class = "Large",
                                     classCol = largeClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
      run$parameters.svm <- list(kern = parameters.svm[s,'kern'], cost = parameters.svm[s,'cost'], gam = parameters.svm[s,'gam'])
      #res.eval <- getPerformance(res.svm[[s]]$model, res.pca[[p]]$test, dimTestset, largeClassCol)
      #run$results <- list(model=res.svm[[s]]$model, pred=res.svm[[s]]$pred, prob=res.svm[[s]]$prob, tbl=res.svm[[s]]$tbl, roc=res.eval$roc, auc=res.eval$auc, confmatrix=res.eval$confmatrix, perf=res.eval$perf)
      run$results <- list(model=res.svm[[s]]$model, pred=res.svm[[s]]$pred, prob=res.svm[[s]]$prob, tbl=res.svm[[s]]$tbl, testcases=nrow(res.pca[[p]]$test))
      runResults[[length(runResults)+1]] <- run
    }
  }
  runResultsList[[ant]] <- runResults
  rm(run)
  rm(runResults)
  rm(dimTrainset)
  rm(dimTestset)
  rm(sigTrainset)
  rm(sigTestset)
}
if (parameters.globals.prepareMode == 'ByAntenna') {
  save(runResultsList, file = paste(resultsDir, "runResultsList_PCA-SVM", "_", parameters.globals.separationMode, ".RData", sep=""))
  # load(paste(resultsDir, "runResultsList_PCA-SVM", "_", parameters.globals.separationMode, ".RData", sep=""))
  computeAntennasVoting(dimtest=processDataset[[ant]]$dimtest, results4Ant=runResultsList, classificationType="svm")
}
runResults <- runResultsList[[1]]
stp=Sys.time()
ttPS <- stp - stt#Time difference of 25.91648(simplified)/50.98092(Original) secs, runs 12
save(runResults, file = paste(resultsDir, "runResults_PCA-SVM", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".RData", sep=""))
saveResults(runResults, paste(resultsDir, "Results_PCA-SVM", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, sep=""))

rm(res.pca)
rm(res.svm)


#********************************************************************************************
# DWT+LDA (Malignant, Large) ----------------------------------------------------------------
#********************************************************************************************
stt=Sys.time()
# List of each run parameters and results
runResultsList <- list()
for(ant in 1:length(processDataset)){
  runResults <- list()
  run <- list()
  run$parameters.pca <- list(scale = NULL, tol = NULL, components = NULL)
  #run$parameters.dwt <- list(filter = NULL, level = NULL, bound = NULL, coeff = NULL)
  run$parameters.svm <- list(kern = NULL, cost = NULL, gam = NULL)
  run$parameters.interp <- list(levels = NULL, interp_points = NULL, epsilon=NULL, points=NULL)
  run$parameters.rf <- NULL
  run$parameters.lda <- NULL
  sigTrainset <- processDataset[[ant]]$sigtrain
  dimTrainset <- processDataset[[ant]]$dimtrain
  sigTestset <- processDataset[[ant]]$sigtest
  dimTestset <- processDataset[[ant]]$dimtest
  res.dwt <- apply(parameters.dwt, 1, function(fd) applyDWT(fd['filter'], fd['level'], fd['bound'], trainset = sigTrainset, testset = sigTestset))
  for (d in 1:nrow(parameters.dwt)){
    run$parameters.dwt <- list(filter = parameters.dwt[d,'filter'], level = parameters.dwt[d,'level'], bound = parameters.dwt[d,'bound'], coeff = ncol(res.dwt[[d]]$test))
    res.lda <- ldaClassify_Malignant(classCol = malignantClassCol, trainset = res.dwt[[d]]$train, dimTrainset = dimTrainset, testset = res.dwt[[d]]$test, dimTestset = dimTestset)
    run$name <- "DWT_LDA-Malignant"
    run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
                                   extract1 = "DWT", extract2 = "", classif = "LDA", class = "Malignant",
                                   classCol = malignantClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
    #res.eval <- getPerformance(res.lda$model, res.dwt[[d]]$test, dimTestset, malignantClassCol)
    #run$results <- list(model=res.lda$model, pred=res.lda$pred, prob=res.lda$prob, tbl=res.lda$tbl, roc=res.eval$roc, auc=res.eval$auc, confmatrix=res.eval$confmatrix, perf=res.eval$perf)
    run$results <- list(model=res.lda$model, pred=res.lda$pred, prob=res.lda$prob, tbl=res.lda$tbl, testcases=nrow(res.dwt[[d]]$test))
    runResults[[length(runResults)+1]] <- run
    
    res.lda <- ldaClassify_Large(classCol = largeClassCol, trainset = res.dwt[[d]]$train, dimTrainset = dimTrainset, testset = res.dwt[[d]]$test, dimTestset = dimTestset)
    run$name <- "DWT_LDA-Large"
    run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
                                   extract1 = "DWT", extract2 = "", classif = "LDA", class = "Large",
                                   classCol = largeClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
    #res.eval <- getPerformance(res.lda$model, res.dwt[[d]]$test, dimTestset, largeClassCol)
    #run$results <- list(model=res.lda$model, pred=res.lda$pred, prob=res.lda$prob, tbl=res.lda$tbl, roc=res.eval$roc, auc=res.eval$auc, confmatrix=res.eval$confmatrix, perf=res.eval$perf)
    run$results <- list(model=res.lda$model, pred=res.lda$pred, prob=res.lda$prob, tbl=res.lda$tbl, testcases=nrow(res.dwt[[d]]$test))
    runResults[[length(runResults)+1]] <- run
  }
  runResultsList[[ant]] <- runResults
  rm(run)
  rm(runResults)
  rm(dimTrainset)
  rm(dimTestset)
  rm(sigTrainset)
  rm(sigTestset)
}
if (parameters.globals.prepareMode == 'ByAntenna') {
  save(runResultsList, file = paste(resultsDir, "runResultsList_DWT-LDA", "_", parameters.globals.separationMode, ".RData", sep=""))
  # load(paste(resultsDir, "runResultsList_DWT-LDA, "_", parameters.globals.separationMode, ".RData", sep=""))
  computeAntennasVoting(dimtest=processDataset[[ant]]$dimtest, results4Ant=runResultsList, classificationType="lda")
}
runResults <- runResultsList[[1]]
stp=Sys.time()
ttDL <- stp - stt#Time difference of  1.887491 mins, runs 8
save(runResults, file = paste(resultsDir, "runResults_DWT-LDA", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".RData", sep=""))
saveResults(runResults, paste(resultsDir, "Results_DWT-LDA", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, sep=""))

rm(res.dwt)
rm(res.lda)


#********************************************************************************************
# Interp+LDA (Malignant, Large) -------------------------------------------------------------
#********************************************************************************************
stt=Sys.time()
# List of each run parameters and results
#Interpolation was disregarded because of points become NaN: classifiers does not handle NaN values
#NEW APPROACH: Replace all NaNs for the original value in columns that have a non NaN value at least in one row of all the dataset (train+test) all NaNs
runResultsList <- list()
for(ant in 1:length(processDataset)){
  runResults <- list()
  run <- list()
  run$parameters.pca <- list(scale = NULL, tol = NULL, components = NULL)
  run$parameters.dwt <- list(filter = NULL, level = NULL, bound = NULL, coeff = NULL)
  run$parameters.svm <- list(kern = NULL, cost = NULL, gam = NULL)
  #run$parameters.interp <- list(levels = NULL, interp_points = NULL, epsilon=NULL, points=NULL)
  run$parameters.rf <- NULL
  run$parameters.lda <- NULL
  sigTrainset <- processDataset[[ant]]$sigtrain
  dimTrainset <- processDataset[[ant]]$dimtrain
  sigTestset <- processDataset[[ant]]$sigtest
  dimTestset <- processDataset[[ant]]$dimtest
  
  # dyn.load("./interpV2_0.dll")
  # res.interp <- list()
  # for (a in 1:nrow(parameters.interp)){
  #   res.interp[[a]] <- applyInterp(parameters.interp$levels[a], parameters.interp$interp_points[a], parameters.interp$epsilon[a], trainset=sigTrainset, testset=sigTestset)
  # }
  # dyn.unload("./interpV2_0.dll")
  # save(res.interp, file = paste(workDir, "res.interp_", parameters.globals.usedsignals, "-",  parameters.globals.separationMode, "-", parameters.globals.prepareMode ,".RData", sep=""))
  load(paste(workDir, "res.interp_", parameters.globals.usedsignals, "-",  parameters.globals.separationMode, "-", parameters.globals.prepareMode ,".RData", sep=""))
  
  for (i in 1:nrow(parameters.interp)){
    run$parameters.interp <- list(levels = parameters.interp[i,'levels'], interp_points = parameters.interp[i,'interp_points'], epsilon = parameters.interp[i,'epsilon'], points = ncol(res.interp[[i]]$test))
    res.lda <- ldaClassify_Malignant(classCol = malignantClassCol, trainset = res.interp[[i]]$train, dimTrainset = dimTrainset, testset = res.interp[[i]]$test, dimTestset = dimTestset)
    run$name <- "Interp_LDA-Malignant"
    run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
                                   extract1 = "Interp", extract2 = "", classif = "LDA", class = "Malignant",
                                   classCol = malignantClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
    run$results <- list(model=res.lda$model, pred=res.lda$pred, prob=res.lda$prob, tbl=res.lda$tbl, testcases=nrow(res.interp[[i]]$test))
    runResults[[length(runResults)+1]] <- run
    
    res.lda <- ldaClassify_Large(classCol = largeClassCol, trainset = res.interp[[i]]$train, dimTrainset = dimTrainset, testset = res.interp[[i]]$test, dimTestset = dimTestset)
    run$name <- "Interp_LDA-Large"
    run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
                                   extract1 = "Interp", extract2 = "", classif = "LDA", class = "Large",
                                   classCol = largeClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
    run$results <- list(model=res.lda$model, pred=res.lda$pred, prob=res.lda$prob, tbl=res.lda$tbl, testcases=nrow(res.interp[[i]]$test))
    runResults[[length(runResults)+1]] <- run
  }
  runResultsList[[ant]] <- runResults
  rm(run)
  rm(runResults)
  rm(dimTrainset)
  rm(dimTestset)
  rm(sigTrainset)
  rm(sigTestset)
}
if (parameters.globals.prepareMode == 'ByAntenna') {
  save(runResultsList, file = paste(resultsDir, "runResultsList_Interp-LDA", "_", parameters.globals.separationMode, ".RData", sep=""))
  # load(paste(resultsDir, "runResultsList_Interp-LDA", "_", parameters.globals.separationMode, ".RData", sep=""))
  computeAntennasVoting(dimtest=processDataset[[ant]]$dimtest, results4Ant=runResultsList, classificationType="lda")
}
runResults <- runResultsList[[1]]
stp=Sys.time()
ttIL <- stp - stt#Time difference of 2.178125 secs, numRuns 6
save(runResults, file = paste(resultsDir, "runResults_Interp-LDA", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".RData", sep=""))
saveResults(runResults, paste(resultsDir, "Results_Interp-LDA", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, sep=""))

rm(res.interp)
rm(res.lda)


#********************************************************************************************
# PCA+LDA (Malignant, Large) ----------------------------------------------------------------
#********************************************************************************************
stt=Sys.time()
# List of each run parameters and results
runResultsList <- list()
for(ant in 1:length(processDataset)){
  runResults <- list()
  run <- list()
  #run$parameters.pca <- list(scale = NULL, tol = NULL, components = NULL)
  run$parameters.dwt <- list(filter = NULL, level = NULL, bound = NULL, coeff = NULL)
  run$parameters.svm <- list(kern = NULL, cost = NULL, gam = NULL)
  run$parameters.interp <- list(levels = NULL, interp_points = NULL, epsilon=NULL, points=NULL)
  run$parameters.rf <- NULL
  run$parameters.lda <- NULL
  sigTrainset <- processDataset[[ant]]$sigtrain
  dimTrainset <- processDataset[[ant]]$dimtrain
  sigTestset <- processDataset[[ant]]$sigtest
  dimTestset <- processDataset[[ant]]$dimtest
  res.pca <- apply(parameters.pca, 1, function(p) applyPCA(trainset=sigTrainset, testset = sigTestset,
                                                           scale = p['scale'], tol = p['tol'], compNbr = p['comp']))
  for (p in 1:nrow(parameters.pca)){
    run$parameters.pca <- list(scale = parameters.pca[p,'scale'], tol = parameters.pca[p,'tol'], components = ncol(res.pca[[p]]$test))
    res.lda <- ldaClassify_Malignant(classCol = malignantClassCol, trainset = res.pca[[p]]$train, dimTrainset = dimTrainset, testset = res.pca[[p]]$test, dimTestset = dimTestset)
    run$name <- "PCA_LDA-Malignant"
    run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
                                   extract1 = "PCA", extract2 = "", classif = "LDA", class = "Malignant",
                                   classCol = malignantClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
    #res.eval <- getPerformance(res.lda$model, res.pca[[p]]$test, dimTestset, malignantClassCol)
    #run$results <- list(model=res.lda$model, pred=res.lda$pred, prob=res.lda$prob, tbl=res.lda$tbl, roc=res.eval$roc, auc=res.eval$auc, confmatrix=res.eval$confmatrix, perf=res.eval$perf)
    run$results <- list(model=res.lda$model, pred=res.lda$pred, prob=res.lda$prob, tbl=res.lda$tbl, testcases=nrow(res.pca[[p]]$test))
    runResults[[length(runResults)+1]] <- run
    
    res.lda <- ldaClassify_Large(classCol = largeClassCol, trainset = res.pca[[p]]$train, dimTrainset = dimTrainset, testset = res.pca[[p]]$test, dimTestset = dimTestset)
    run$name <- "PCA_LDA-Large"
    run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
                                   extract1 = "PCA", extract2 = "", classif = "LDA", class = "Large",
                                   classCol = largeClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
    #res.eval <- getPerformance(res.lda$model, res.pca[[p]]$test, dimTestset, largeClassCol)
    #run$results <- list(model=res.lda$model, pred=res.lda$pred, prob=res.lda$prob, tbl=res.lda$tbl, roc=res.eval$roc, auc=res.eval$auc, confmatrix=res.eval$confmatrix, perf=res.eval$perf)
    run$results <- list(model=res.lda$model, pred=res.lda$pred, prob=res.lda$prob, tbl=res.lda$tbl, testcases=nrow(res.pca[[p]]$test))
    runResults[[length(runResults)+1]] <- run
  }
  runResultsList[[ant]] <- runResults
  rm(run)
  rm(runResults)
  rm(dimTrainset)
  rm(dimTestset)
  rm(sigTrainset)
  rm(sigTestset)
}
if (parameters.globals.prepareMode == 'ByAntenna') {
  save(runResultsList, file = paste(resultsDir, "runResultsList_PCA-LDA", "_", parameters.globals.separationMode, ".RData", sep=""))
  # load(paste(resultsDir, "runResultsList_PCA-LDA", "_", parameters.globals.separationMode, ".RData", sep=""))
  computeAntennasVoting(dimtest=processDataset[[ant]]$dimtest, results4Ant=runResultsList, classificationType="lda")
}
runResults <- runResultsList[[1]]
stp=Sys.time()
ttPL <- stp - stt#Time difference of 23.78836 secs, runs 12
save(runResults, file = paste(resultsDir, "runResults_PCA-LDA", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".RData", sep=""))
saveResults(runResults, paste(resultsDir, "Results_PCA-LDA", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, sep=""))

rm(res.pca)
rm(res.lda)


#********************************************************************************************
# LDA (Malignant, Large) ----------------------------------------------------------------
#********************************************************************************************
stt=Sys.time()
# List of each run parameters and results
runResultsList <- list()
for(ant in 1:length(processDataset)){
  runResults <- list()
  run <- list()
  run$parameters.pca <- list(scale = NULL, tol = NULL, components = NULL)
  run$parameters.dwt <- list(filter = NULL, level = NULL, bound = NULL, coeff = NULL)
  run$parameters.svm <- list(kern = NULL, cost = NULL, gam = NULL)
  run$parameters.interp <- list(levels = NULL, interp_points = NULL, epsilon=NULL, points=NULL)
  run$parameters.rf <- NULL
  run$parameters.lda <- NULL
  sigTrainset <- processDataset[[ant]]$sigtrain
  dimTrainset <- processDataset[[ant]]$dimtrain
  sigTestset <- processDataset[[ant]]$sigtest
  dimTestset <- processDataset[[ant]]$dimtest
  res.lda <- ldaClassify_Malignant(classCol = malignantClassCol, trainset = sigTrainset, dimTrainset = dimTrainset, testset = sigTestset, dimTestset = dimTestset)
  run$name <- "LDA-Malignant"
  run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
                                 extract1 = "", extract2 = "", classif = "LDA", class = "Malignant",
                                 classCol = malignantClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
  run$results <- list(model=res.lda$model, pred=res.lda$pred, prob=res.lda$prob, tbl=res.lda$tbl, testcases=nrow(sigTestset))
  runResults[[length(runResults)+1]] <- run
  
  res.lda <- ldaClassify_Large(classCol = largeClassCol, trainset = sigTrainset, dimTrainset = dimTrainset, testset = sigTestset, dimTestset = dimTestset)
  run$name <- "LDA-Large"
  run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
                                 extract1 = "", extract2 = "", classif = "LDA", class = "Large",
                                 classCol = largeClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
  run$results <- list(model=res.lda$model, pred=res.lda$pred, prob=res.lda$prob, tbl=res.lda$tbl, testcases=nrow(sigTestset))
  runResults[[length(runResults)+1]] <- run
  
  runResultsList[[ant]] <- runResults
  rm(run)
  rm(runResults)
  rm(dimTrainset)
  rm(dimTestset)
  rm(sigTrainset)
  rm(sigTestset)
}
if (parameters.globals.prepareMode == 'ByAntenna') {
  save(runResultsList, file = paste(resultsDir, "runResultsList_LDA", "_", parameters.globals.separationMode, ".RData", sep=""))
  # load(paste(resultsDir, "runResultsList_LDA", "_", parameters.globals.separationMode, ".RData", sep=""))
  computeAntennasVoting(dimtest=processDataset[[ant]]$dimtest, results4Ant=runResultsList, classificationType="lda")
}
runResults <- runResultsList[[1]]
stp=Sys.time()
ttPL <- stp - stt#Time difference of 23.78836 secs, runs 12
save(runResults, file = paste(resultsDir, "runResults_LDA", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".RData", sep=""))
saveResults(runResults, paste(resultsDir, "Results_LDA", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, sep=""))

rm(res.lda)


#********************************************************************************************
# DWT+RF  (Malignant, Large) ---------------------------------------------------------------
#********************************************************************************************
stt=Sys.time()
# List of each run parameters and results 
runResultsList <- list()
for(ant in 1:length(processDataset)){
  runResults <- list()
  run <- list()
  run$parameters.pca <- list(scale = NULL, tol = NULL, components = NULL)
  #run$parameters.dwt <- list(filter = NULL, level = NULL, bound = NULL, coeff = NULL)
  run$parameters.svm <- list(kern = NULL, cost = NULL, gam = NULL)
  run$parameters.interp <- list(levels = NULL, interp_points = NULL, epsilon=NULL, points=NULL)
  #run$parameters.rf <- NULL
  #run$parameters.lda <- NULL
  sigTrainset <- processDataset[[ant]]$sigtrain
  dimTrainset <- processDataset[[ant]]$dimtrain
  sigTestset <- processDataset[[ant]]$sigtest
  dimTestset <- processDataset[[ant]]$dimtest
  res.dwt <- apply(parameters.dwt, 1, function(fd) applyDWT(fd['filter'], fd['level'], fd['bound'], trainset = sigTrainset, testset = sigTestset))
  for (d in 1:nrow(parameters.dwt)){
    run$parameters.dwt <- list(filter = parameters.dwt[d,'filter'], level = parameters.dwt[d,'level'], bound = parameters.dwt[d,'bound'], coeff = ncol(res.dwt[[d]]$test))
    #Malignant
    res.rf <- rfClassify_Malignant(classCol = malignantClassCol, trainset = res.dwt[[d]]$train, dimTrainset = dimTrainset, testset = res.dwt[[d]]$test, dimTestset = dimTestset)
    run$name <- "DWT_RF-Malignant"
    run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
                                   extract1 = "DWT", extract2 = "", classif = "RF", class = "Malignant",
                                   classCol = malignantClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
    #res.eval <- getPerformance(res.svm[[s]]$model, res.dwt[[d]]$test, dimTestset, malignantClassCol)
    #run$results <- list(model=res.svm[[s]]$model, pred=res.svm[[s]]$pred, prob=res.svm[[s]]$prob, tbl=res.svm[[s]]$tbl, roc=res.eval$roc, auc=res.eval$auc, confmatrix=res.eval$confmatrix, perf=res.eval$perf)
    run$results <- list(model=res.rf$model, pred=res.rf$pred, prob=res.rf$prob, tbl=res.rf$tbl, testcases=nrow(res.dwt[[d]]$test))
    runResults[[length(runResults)+1]] <- run
    #Large
    res.rf <- rfClassify_Large(classCol = largeClassCol, trainset = res.dwt[[d]]$train, dimTrainset = dimTrainset, testset = res.dwt[[d]]$test, dimTestset = dimTestset)
    run$name <- "DWT_RF-Large"
    run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
                                   extract1 = "DWT", extract2 = "", classif = "RF", class = "Large",
                                   classCol = largeClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
    #res.eval <- getPerformance(res.svm[[s]]$model, res.dwt[[d]]$test, dimTestset, largeClassCol)
    #run$results <- list(model=res.svm[[s]]$model, pred=res.svm[[s]]$pred, prob=res.svm[[s]]$prob, tbl=res.svm[[s]]$tbl, roc=res.eval$roc, auc=res.eval$auc, confmatrix=res.eval$confmatrix, perf=res.eval$perf)
    run$results <- list(model=res.rf$model, pred=res.rf$pred, prob=res.rf$prob, tbl=res.rf$tbl, testcases=nrow(res.dwt[[d]]$test))
    runResults[[length(runResults)+1]] <- run
  }
  runResultsList[[ant]] <- runResults
  rm(run)
  rm(runResults)
  rm(dimTrainset)
  rm(dimTestset)
  rm(sigTrainset)
  rm(sigTestset)
}
if (parameters.globals.prepareMode == 'ByAntenna') {
  save(runResultsList, file = paste(resultsDir, "runResultsList_DWT-RF", "_", parameters.globals.separationMode, ".RData", sep=""))
  # load(paste(resultsDir, "runResultsList_DWT-RF", "_", parameters.globals.separationMode, ".RData", sep=""))
  computeAntennasVoting(dimtest=processDataset[[ant]]$dimtest, results4Ant=runResultsList, classificationType="rf")
}
runResults <- runResultsList[[1]]
stp=Sys.time()
ttDR <- stp - stt#Time difference of 2.062535 (Simplified)/5.196781(Original) mins, runs=8
save(runResults, file = paste(resultsDir, "runResults_DWT-RF", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".RData", sep=""))
saveResults(runResults, paste(resultsDir, "Results_DWT-RF", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, sep=""))

rm(res.dwt)
rm(res.rf)


#********************************************************************************************
# Interp+RF  (Malignant, Large) ---------------------------------------------------------------
#********************************************************************************************
stt=Sys.time()
# List of each run parameters and results
#Interpolation was disregarded because of points become NaN: classifiers does not handle NaN values
#NEW APPROACH: Replace all NaNs for the original value in columns that have a non NaN value at least in one row of all the dataset (train+test) all NaNs
# List of each run parameters and results 
runResultsList <- list()
for(ant in 1:length(processDataset)){
  runResults <- list()
  run <- list()
  run$parameters.pca <- list(scale = NULL, tol = NULL, components = NULL)
  run$parameters.dwt <- list(filter = NULL, level = NULL, bound = NULL, coeff = NULL)
  run$parameters.svm <- list(kern = NULL, cost = NULL, gam = NULL)
  #run$parameters.interp <- list(levels = NULL, interp_points = NULL, epsilon=NULL, points=NULL)
  #run$parameters.rf <- NULL
  #run$parameters.lda <- NULL
  sigTrainset <- processDataset[[ant]]$sigtrain
  dimTrainset <- processDataset[[ant]]$dimtrain
  sigTestset <- processDataset[[ant]]$sigtest
  dimTestset <- processDataset[[ant]]$dimtest
  
  # dyn.load("./interpV2_0.dll")
  # res.interp <- list()
  # for (a in 1:nrow(parameters.interp)){
  #   res.interp[[a]] <- applyInterp(parameters.interp$levels[a], parameters.interp$interp_points[a], parameters.interp$epsilon[a], trainset=sigTrainset, testset=sigTestset)
  # }
  # dyn.unload("./interpV2_0.dll")
  # save(res.interp, file = paste(workDir, "res.interp_", parameters.globals.usedsignals, "-",  parameters.globals.separationMode, "-", parameters.globals.prepareMode ,".RData", sep=""))
  load(paste(workDir, "res.interp_", parameters.globals.usedsignals, "-",  parameters.globals.separationMode, "-", parameters.globals.prepareMode ,".RData", sep=""))
  
  for (i in 1:nrow(parameters.interp)){
    run$parameters.interp <- list(levels = parameters.interp[i,'levels'], interp_points = parameters.interp[i,'interp_points'], epsilon = parameters.interp[i,'epsilon'], points = ncol(res.interp[[i]]$test))
    #Malignant
    res.rf <- rfClassify_Malignant(classCol = malignantClassCol, trainset = res.interp[[i]]$train, dimTrainset = dimTrainset, testset = res.interp[[i]]$test, dimTestset = dimTestset)
    run$name <- "Interp_RF-Malignant"
    run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
                                   extract1 = "Interp", extract2 = "", classif = "RF", class = "Malignant",
                                   classCol = malignantClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
    run$results <- list(model=res.rf$model, pred=res.rf$pred, prob=res.rf$prob, tbl=res.rf$tbl, testcases=nrow(res.interp[[i]]$test))
    runResults[[length(runResults)+1]] <- run
    #Large
    res.rf <- rfClassify_Large(classCol = largeClassCol, trainset = res.interp[[i]]$train, dimTrainset = dimTrainset, testset = res.interp[[i]]$test, dimTestset = dimTestset)
    run$name <- "Interp_RF-Large"
    run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
                                   extract1 = "Interp", extract2 = "", classif = "RF", class = "Large",
                                   classCol = largeClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
    run$results <- list(model=res.rf$model, pred=res.rf$pred, prob=res.rf$prob, tbl=res.rf$tbl, testcases=nrow(res.interp[[i]]$test))
    runResults[[length(runResults)+1]] <- run
  }
  runResultsList[[ant]] <- runResults
  rm(run)
  rm(runResults)
  rm(dimTrainset)
  rm(dimTestset)
  rm(sigTrainset)
  rm(sigTestset)
}
if (parameters.globals.prepareMode == 'ByAntenna') {
  save(runResultsList, file = paste(resultsDir, "runResultsList_Interp-RF", "_", parameters.globals.separationMode, ".RData", sep=""))
  # load(paste(resultsDir, "runResultsList_Interp-RF", "_", parameters.globals.separationMode, ".RData", sep=""))
  computeAntennasVoting(dimtest=processDataset[[ant]]$dimtest, results4Ant=runResultsList, classificationType="rf")
}
runResults <- runResultsList[[1]]
stp=Sys.time()
ttIR <- stp - stt#Time difference of 20.37717 secs, numRuns 6
save(runResults, file = paste(resultsDir, "runResults_Interp-RF", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".RData", sep=""))
saveResults(runResults, paste(resultsDir, "Results_Interp-RF", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, sep=""))

rm(res.interp)
rm(res.rf)


#********************************************************************************************
# PCA+RF  (Malignant, Large) ---------------------------------------------------------------
#********************************************************************************************
stt=Sys.time()
# List of each run parameters and results 
runResultsList <- list()
for(ant in 1:length(processDataset)){
  runResults <- list()
  run <- list()
  #run$parameters.pca <- list(scale = NULL, tol = NULL, components = NULL)
  run$parameters.dwt <- list(filter = NULL, level = NULL, bound = NULL, coeff = NULL)
  run$parameters.svm <- list(kern = NULL, cost = NULL, gam = NULL)
  run$parameters.interp <- list(levels = NULL, interp_points = NULL, epsilon=NULL, points=NULL)
  #run$parameters.rf <- NULL
  #run$parameters.lda <- NULL
  sigTrainset <- processDataset[[ant]]$sigtrain
  dimTrainset <- processDataset[[ant]]$dimtrain
  sigTestset <- processDataset[[ant]]$sigtest
  dimTestset <- processDataset[[ant]]$dimtest
  
  res.pca <- apply(parameters.pca, 1, function(p) applyPCA(trainset=sigTrainset, testset = sigTestset,
                                                           scale = p['scale'], tol = p['tol'], compNbr = p['comp']))
  for (p in 1:nrow(parameters.pca)){
    run$parameters.pca <- list(scale = parameters.pca[p,'scale'], tol = parameters.pca[p,'tol'], components = ncol(res.pca[[p]]$test))
    #Malignant
    res.rf <- rfClassify_Malignant(classCol = malignantClassCol, trainset = res.pca[[p]]$train, dimTrainset = dimTrainset, testset = res.pca[[p]]$test, dimTestset = dimTestset)
    run$name <- "PCA_RF-Malignant"
    run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
                                   extract1 = "PCA", extract2 = "", classif = "RF", class = "Malignant",
                                   classCol = malignantClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
    run$results <- list(model=res.rf$model, pred=res.rf$pred, prob=res.rf$prob, tbl=res.rf$tbl, testcases=nrow(res.pca[[p]]$test))
    runResults[[length(runResults)+1]] <- run
    #Large
    res.rf <- rfClassify_Large(classCol = largeClassCol, trainset = res.pca[[p]]$train, dimTrainset = dimTrainset, testset = res.pca[[p]]$test, dimTestset = dimTestset)
    run$name <- "PCA_RF-Large"
    run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
                                   extract1 = "PCA", extract2 = "", classif = "RF", class = "Large",
                                   classCol = largeClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
    run$results <- list(model=res.rf$model, pred=res.rf$pred, prob=res.rf$prob, tbl=res.rf$tbl, testcases=nrow(res.pca[[p]]$test))
    runResults[[length(runResults)+1]] <- run
  }
  runResultsList[[ant]] <- runResults
  rm(run)
  rm(runResults)
  rm(dimTrainset)
  rm(dimTestset)
  rm(sigTrainset)
  rm(sigTestset)
}
if (parameters.globals.prepareMode == 'ByAntenna') {
  save(runResultsList, file = paste(resultsDir, "runResultsList_PCA-RF", "_", parameters.globals.separationMode, ".RData", sep=""))
  # load(paste(resultsDir, "runResultsList_PCA-RF", "_", parameters.globals.separationMode, ".RData", sep=""))
  computeAntennasVoting(dimtest=processDataset[[ant]]$dimtest, results4Ant=runResultsList, classificationType="rf")
}
runResults <- runResultsList[[1]]
stp=Sys.time()
ttPR <- stp - stt#Time difference of 52.72501(Original) secs
save(runResults, file = paste(resultsDir, "runResults_PCA-RF", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".RData", sep=""))
saveResults(runResults, paste(resultsDir, "Results_PCA-RF", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, sep=""))

rm(res.pca)
rm(res.rf)


#********************************************************************************************
# RF  (Malignant, Large) --------------------------------------------------------------------
#********************************************************************************************
stt=Sys.time()
# List of each run parameters and results 
runResultsList <- list()
for(ant in 1:length(processDataset)){
  runResults <- list()
  run <- list()
  run$parameters.pca <- list(scale = NULL, tol = NULL, components = NULL)
  run$parameters.dwt <- list(filter = NULL, level = NULL, bound = NULL, coeff = NULL)
  run$parameters.svm <- list(kern = NULL, cost = NULL, gam = NULL)
  run$parameters.interp <- list(levels = NULL, interp_points = NULL, epsilon=NULL, points=NULL)
  #run$parameters.rf <- NULL
  #run$parameters.lda <- NULL
  sigTrainset <- processDataset[[ant]]$sigtrain
  dimTrainset <- processDataset[[ant]]$dimtrain
  sigTestset <- processDataset[[ant]]$sigtest
  dimTestset <- processDataset[[ant]]$dimtest
  
  #TODO: Pred tem valores <> de 1 e 0. Pesquisar informação.
  
  #Malignant
  res.rf <- rfClassify_Malignant(classCol = malignantClassCol, trainset = sigTrainset, dimTrainset = dimTrainset, testset = sigTestset, dimTestset = dimTestset)
  run$name <- "RF-Malignant"
  run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
                                 extract1 = "", extract2 = "", classif = "RF", class = "Malignant",
                                 classCol = malignantClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
  #res.eval <- getPerformance(res.svm[[s]]$model, res.dwt[[d]]$test, dimTestset, malignantClassCol)
  #run$results <- list(model=res.svm[[s]]$model, pred=res.svm[[s]]$pred, prob=res.svm[[s]]$prob, tbl=res.svm[[s]]$tbl, roc=res.eval$roc, auc=res.eval$auc, confmatrix=res.eval$confmatrix, perf=res.eval$perf)
  run$results <- list(model=res.rf$model, pred=res.rf$pred, prob=res.rf$prob, tbl=res.rf$tbl, testcases=nrow(dimTestset))
  runResults[[length(runResults)+1]] <- run
  #Large
  res.rf <- rfClassify_Large(classCol = largeClassCol, trainset = sigTrainset, dimTrainset = dimTrainset, testset = sigTestset, dimTestset = dimTestset)
  run$name <- "RF-Large"
  run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
                                 extract1 = "", extract2 = "", classif = "RF", class = "Large",
                                 classCol = largeClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
  #res.eval <- getPerformance(res.svm[[s]]$model, res.dwt[[d]]$test, dimTestset, largeClassCol)
  #run$results <- list(model=res.svm[[s]]$model, pred=res.svm[[s]]$pred, prob=res.svm[[s]]$prob, tbl=res.svm[[s]]$tbl, roc=res.eval$roc, auc=res.eval$auc, confmatrix=res.eval$confmatrix, perf=res.eval$perf)
  run$results <- list(model=res.rf$model, pred=res.rf$pred, prob=res.rf$prob, tbl=res.rf$tbl, testcases=nrow(sigTestset))
  runResults[[length(runResults)+1]] <- run
  runResultsList[[ant]] <- runResults
  rm(run)
  rm(runResults)
  rm(dimTrainset)
  rm(dimTestset)
  rm(sigTrainset)
  rm(sigTestset)
}
if (parameters.globals.prepareMode == 'ByAntenna') {
  save(runResultsList, file = paste(resultsDir, "runResultsList_RF", "_", parameters.globals.separationMode, ".RData", sep=""))
  # load(paste(resultsDir, "runResultsList_RF", "_", parameters.globals.separationMode, ".RData", sep=""))
  computeAntennasVoting(dimtest=processDataset[[ant]]$dimtest, results4Ant=runResultsList, classificationType="rf")
}
runResults <- runResultsList[[1]]
stp=Sys.time()
ttR <- stp - stt#Time difference of 1.316575 mins, runs=2
save(runResults, file = paste(resultsDir, "runResults_RF", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".RData", sep=""))
saveResults(runResults, paste(resultsDir, "Results_RF", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, sep=""))

rm(res.rf)


#********************************************************************************************
# Interp+PCA+SVM (Malignant, Large) ---------------------------------------------------------
#********************************************************************************************
#Interpolation was disregarded because of points become NaN: classifiers does not handle NaN values
#NEW APPROACH: Replace all NaNs for the original value in columns that have a non NaN value at least in one row of all the dataset (train+test) all NaNs
stt=Sys.time()
runResults <- list()
run <- list()
run$parameters.pca <- list(scale = NULL, tol = NULL, components = NULL)
run$parameters.dwt <- list(filter = NULL, level = NULL, bound = NULL, coeff = NULL)
#run$parameters.interp <- list(levels = NULL, interp_points = NULL, epsilon=NULL, points=NULL)
#run$parameters.svm <- list(kern = NULL, cost = NULL, gam = NULL)
run$parameters.rf <- NULL
run$parameters.lda <- NULL

# dyn.load("./interpV2_0.dll")
# #Next lines are only for tests. Comment them otherwise
# #res.interp[[3]] <- applyInterp(parameters.interp$levels[3], parameters.interp$interp_points[3], parameters.interp$epsilon[3], trainset=sigTrainset, testset=sigTestset)
# 
# #res.interp <- apply(parameters.interp, 1, function(i) applyInterp(i['levels'], i['interp_points'], i['epsilon'], trainset=sigTrainset, testset=sigTestset))
# res.interp <- list()
# for (a in 1:nrow(parameters.interp)){
#   res.interp[[a]] <- applyInterp(parameters.interp$levels[a], parameters.interp$interp_points[a], parameters.interp$epsilon[a], trainset=sigTrainset, testset=sigTestset)
# }
# dyn.unload("./interpV2_0.dll")
# save(res.interp, file = paste(workDir, "res.interp_", parameters.globals.usedsignals, "-",  parameters.globals.separationMode, "-", parameters.globals.prepareMode ,".RData", sep=""))
load(paste(workDir, "res.interp_", parameters.globals.usedsignals, "-",  parameters.globals.separationMode, "-", parameters.globals.prepareMode ,".RData", sep=""))

for (i in 1:nrow(parameters.interp)){
  run$parameters.interp <- list(levels = parameters.interp[i,'levels'], interp_points = parameters.interp[i,'interp_points'], epsilon = parameters.interp[i,'epsilon'], points = ncol(res.interp[[i]]$test))
  res.pca <- apply(parameters.pca, 1, function(p) applyPCA(trainset=res.interp[[i]]$train, testset = res.interp[[i]]$test,
                                                       scale = p['scale'], tol = p['tol'], compNbr = p['comp']))
  for (p in 1:nrow(parameters.pca)){
    run$parameters.pca <- list(scale = parameters.pca[p,'scale'], tol = parameters.pca[p,'tol'], components = ncol(res.pca[[p]]$test))
    #Malignant
    #Optimal SVM parameters for Malignant are in row 1
    #res.svm <- apply(parameters.svm, 1, function(s) svmClassify_Malignant(s['kern'], s['cost'], s['gam'], classCol = malignantClassCol, trainset = res.pca[[p]]$train, dimTrainset = dimTrainset, testset = res.pca[[p]]$test, dimTestset = dimTestset))
    res.svm <- list()
    res.svm[[1]] <- svmClassify_Malignant(parameters.svm[1,'kern'], parameters.svm[1,'cost'], parameters.svm[1,'gam'], classCol = malignantClassCol, trainset = res.pca[[p]]$train, dimTrainset = dimTrainset, testset = res.pca[[p]]$test, dimTestset = dimTestset)
    for (s in 1:length(res.svm)){
      run$name <- "Interp_PCA_SVM-Malignant"
      run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
                                     extract1 = "Interp", extract2 = "PCA", classif = "SVM", class = "Malignant",
                                     classCol = largeClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
      run$parameters.svm <- list(kern = parameters.svm[s,'kern'], cost = parameters.svm[s,'cost'], gam = parameters.svm[s,'gam'])
      #res.eval <- getPerformance(res.svm[[s]]$model, res.pca[[p]]$test, dimTestset, malignantClassCol)
      #run$results <- list(model=res.svm[[s]]$model, pred=res.svm[[s]]$pred, prob=res.svm[[s]]$prob, tbl=res.svm[[s]]$tbl, roc=res.eval$roc, auc=res.eval$auc, confmatrix=res.eval$confmatrix, perf=res.eval$perf)
      run$results <- list(model=res.svm[[s]]$model, pred=res.svm[[s]]$pred, prob=res.svm[[s]]$prob, tbl=res.svm[[s]]$tbl, testcases=nrow(res.pca[[p]]$test))
      runResults[[length(runResults)+1]] <- run
    }
    #Large
    #Optimal SVM parameters for Large are in row 2
    #res.svm <- apply(parameters.svm, 1, function(s) svmClassify_Large(s['kern'], s['cost'], s['gam'], classCol = largeClassCol, trainset = res.pca[[p]]$train, dimTrainset = dimTrainset, testset = res.pca[[p]]$test, dimTestset = dimTestset))
    res.svm <- list()
    res.svm[[1]] <- svmClassify_Large(parameters.svm[2,'kern'], parameters.svm[2,'cost'], parameters.svm[2,'gam'], classCol = largeClassCol, trainset = res.pca[[p]]$train, dimTrainset = dimTrainset, testset = res.pca[[p]]$test, dimTestset = dimTestset)
    for (s in 1:length(res.svm)){
      run$name <- "Interp_PCA_SVM-Large"
      run$parameters.globals <- list(dataset = parameters.globals.prepareMode, separat = parameters.globals.separationMode,
                                     extract1 = "Interp", extract2 = "PCA", classif = "SVM", class = "Large",
                                     classCol = largeClassCol, trainset = parameters.globals.trainCases, testset = parameters.globals.testCases)
      run$parameters.svm <- list(kern = parameters.svm[s,'kern'], cost = parameters.svm[s,'cost'], gam = parameters.svm[s,'gam'])
      #res.eval <- getPerformance(res.svm[[s]]$model, res.pca[[p]]$test, dimTestset, largeClassCol)
      #run$results <- list(model=res.svm[[s]]$model, pred=res.svm[[s]]$pred, prob=res.svm[[s]]$prob, tbl=res.svm[[s]]$tbl, roc=res.eval$roc, auc=res.eval$auc, confmatrix=res.eval$confmatrix, perf=res.eval$perf)
      run$results <- list(model=res.svm[[s]]$model, pred=res.svm[[s]]$pred, prob=res.svm[[s]]$prob, tbl=res.svm[[s]]$tbl, testcases=nrow(res.pca[[p]]$test))
      runResults[[length(runResults)+1]] <- run
    }
  }
}
stp=Sys.time()
ttIPS <- stp - stt#Time difference of 3.203633 mins, runs 36
save(runResults, file = paste(resultsDir, "runResults_Interp-PCA-SVM", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals, ".RData", sep=""))
#load(paste(workDir, "runResults_Interp-PCA-SVM", "_", parameters.globals.separationMode,".RData", sep=""))
saveResults(runResults, paste(resultsDir, "Results_Interp-PCA-SVM", "_", parameters.globals.separationMode, "-", parameters.globals.usedsignals,sep=""))

rm(res.interp)
rm(res.pca)
rm(res.svm)
rm(run)



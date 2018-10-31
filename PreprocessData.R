# Paulo Nunes
# Data: 2017-08-07 (1st version)

#********************************************************************************************
#********************************************************************************************
# PREPROCESS DATASET ------------------------------------------------------------------------
#********************************************************************************************
#********************************************************************************************

removeLowVariancePoints<-function(signal.DF, minVariance){
  startTime <- Sys.time()
  
  minPoint <- min(which(apply(signal.DF[,2:length(signal.DF)], 2, var) > minVariance)) #Excluir a coluna nrow
  maxPoint <- max(which(apply(signal.DF[,2:length(signal.DF)], 2, var) > minVariance))
  #com minVariance=0.00001: min=796 e max=2322
  #com minVariance=0.0001: min=796 e max=1875

  #simpSig.DF <- signal.DF[,c(1,(minPoint+1):(maxPoint-1))] ???Com variaveis nao retorna um dataframe
  
  simpSig.DF<-subset(signal.DF, select = c(1,(minPoint+1):(maxPoint-1))) # necessita library(dplyr)
  
  stopTime <- Sys.time()
  totalTime <- stopTime - startTime
  return(simpSig.DF)
}

sum4antennaSignals<-function(dimensions_DF, signal.DF){
  
  return ("notImplemented")
}

avg4antennaSignals<-function(dimensions_DF, signal.DF){
  return ("notImplemented")
}

getTrainAndTestSets<-function(dataset, divisionMode){
  #divisionMode('Holdout', '10-Fold', 'Bootstrap', ...)
  #DONE: hold out: split dataset by model: 7 random modules for training and the remaining 3 for test (preserve module's classes proporcionality)
  #TODO: Bootstrap: random selection of a model, repeated 10 times (same module can be selected more than once). Test with remaining not selected modules
  #TODO: 10-Fold: run 10 training-test epochs, each epoch with 9 models for trainig and the remaining for test (using a diferent module for test in each epoch)
  dim <- dataset$dimset
  sig <- dataset$sigset
  switch(divisionMode,
         'Holdout'={
           set.seed(0.1)
           testModels <- sample(1:10, 3) #choose randomly 3 models
           dimTestset <- dim[dim$TumModel %in% testModels,]
           sigTestset <- sig[sig$caseNum %in% dimTestset$caseNum,]
           dimTrainset <- dim[!(dim$TumModel %in% testModels),]
           sigTrainset <- sig[!(sig$caseNum %in% dimTestset$caseNum),]
        },
        'Bootstrap'={
          dimTrainset <- NULL
          sigTrainset <- NULL
          trainModelNums <- list()
          set.seed(0.1)
          for (r in 1:10) {
            mNum <- sample(1:10, 1) #choose randomly 1 model
            dimTrainset <- rbind(dimTrainset, dim[dim$TumModel %in% mNum,])
            sigTrainset <- rbind(sigTrainset, sig[sig$caseNum %in% dim[dim$TumModel %in% mNum,]$caseNum,])
            trainModelNums[[r]] <- mNum
          }
          testModelNums <- list(1,2,3,4,5,6,7,8,9,10)
          testModelNums <- testModelNums[!(testModelNums %in% trainModelNums)]
          dimTestset <- dim[dim$TumModel %in% testModelNums[1:3],]
          sigTestset <- sig[sig$caseNum %in% dimTestset$caseNum,]
          rm(mNum)
          rm(trainModelNums)
          rm(testModelNums)
        },
        '10-Fold'={
           load(paste(filename,collapse=NULL))
         },
         stop("Enter something that switches me!")
  )
  #TODO: Verificar neste ponto a proporcionalidade dos dataset para treino e teste
  #      , relativamente a Malignant e Large
  return(list(trainset=list(dimset=dimTrainset, sigset=sigTrainset), testset=list(dimset=dimTestset,sigset=sigTestset)))
}

splitByAntenna<-function(sigtrainset, dimtrainset, sigtestset, dimtestset){
  res <- matrix(list(), nrow=3, ncol=1)
  i = 1
  for (a in c('0','90','180','270')){
    dimtrain <- dimtrainset[dimtrainset$angle == a,]
    sigtrain <- sigtrainset[sigtrainset$caseNum %in% dimtrain$caseNum,]
    dimtest <- dimtestset[dimtestset$angle == a,]
    sigtest <- sigtestset[sigtestset$caseNum  %in% dimtest$caseNum,]
    #Renumber caseNum to identify the same amoung all 4 antennas
    dimtrain$caseNum = c(1:nrow(dimtrain))
    sigtrain$caseNum = c(1:nrow(sigtrain))
    dimtest$caseNum = c((nrow(dimtrain)+1):(nrow(dimtrain)+nrow(dimtest)))
    sigtest$caseNum = c((nrow(sigtrain)+1):(nrow(sigtrain)+nrow(sigtest)))
    ant <- list(sigtrain=sigtrain, dimtrain=dimtrain, sigtest=sigtest, dimtest=dimtest)
    res[[i]] <- ant
    i = i + 1
  }
  return(res)
}

concat4AntennasSignals<-function(dataset){
  dim <- dataset$dimset
  sig <- dataset$sigset
  newCaseNum = 1
  newdim <- data.frame()
  newsig <- data.frame()
  for (i in seq(1, nrow(dim), 4)){#The 4 antennas are in consecutive rows
    #browser()
    newdim <- rbind(newdim, cbind(newCaseNum, dim[i,-1]))
    #browser()
    newsig <- rbind(newsig, cbind(newCaseNum, sig[i,-1], sig[i+1,-1], sig[i+2,-1], sig[i+3,-1]))
    newCaseNum = newCaseNum + 1
  }
  colnames(newdim) = c('caseNum', colnames(dim)[2:ncol(dim)])
  #Do not drop columns because it will change classes columns numbers
  #newdim <- newdim[, !(names(newdim) %in% c('angle', 'AntAng0', 'AntAng90', 'AntAng180', 'AntAng270'))]
  colnames(newsig) = c('caseNum', paste('V', seq(1, (ncol(sig)-1)*4, 1), sep=""))
  return(list(dimset=newdim, sigset=newsig))
}
  
mean4AntennasSignals<-function(dataset){
  dim <- dataset$dimset
  sig <- dataset$sigset
  newCaseNum = 1
  newdim <- data.frame()
  newsig <- data.frame()
  for (i in seq(1, nrow(dim), 4)){#The 4 antennas are in consecutive rows
    newdim <- rbind(newdim, cbind(newCaseNum, dim[i,-1]))
    #browser()
    tempSig <- data.frame()
    tempSig <- rbind(sig[i,-1], sig[i+1,-1], sig[i+2,-1], sig[i+3,-1])
    tempSig <- as.data.frame(t(colMeans(tempSig)))#calculates the mean of the 4 antennas
    newsig <- rbind(newsig, cbind(caseNum=newCaseNum, tempSig))
    #browser()
    newCaseNum = newCaseNum + 1
  }
  colnames(newdim) = c('caseNum', colnames(dim)[2:ncol(dim)])
  #Do not drop columns because it will change classes columns numbers
  #newdim <- newdim[, !(names(newdim) %in% c('angle', 'AntAng0', 'AntAng90', 'AntAng180', 'AntAng270'))]
  #colnames(newsig) = c('caseNum', paste('V', seq(1, (ncol(sig)-1)*4, 1), sep=""))
  return(list(dimset=newdim, sigset=newsig))
}


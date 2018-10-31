# Paulo Nunes
# Data: 2017-09-19 (1st version)

#********************************************************************************************
#********************************************************************************************
# FEATURE EXTRACTION ------------------------------------------------------------------------
#********************************************************************************************
#********************************************************************************************

#********************************************************************************************
# DWT - Discrete Wavelet Transform ----------------------------------------------------------
#********************************************************************************************
gc()
#filter = Daubechies:"d2,4,6,8,10,12,14,16,18,20"; Least Asymetric:"la8,10,12,14,16,18,20"; Best Localized:"bl14,18,20"; Coiflet:"c6,12,18,24,30"
applyDWT<-function(filter, level, bound, trainset, testset){
  wtData <- as.data.frame(NULL)
  l=length(trainset)
  #print(sprintf("Num rows=%d, filter=%s, level=%d, bound=%s.", nrow(trainset), filter, as.numeric(level), bound))
  for (i in 1:nrow(trainset)) {
   iData <- t(trainset[i,-1])#excluir a coluna caseNum
    iwtData <- dwt(iData, filter=filter, n.levels=as.numeric(level), boundary=bound, fast=TRUE)
    wtData <- rbind(wtData, cbind(t(iwtData@W[[iwtData@level]]),t(iwtData@V[[iwtData@level]])))
  }
  train <- cbind(caseNum=trainset[,1], wtData) #Voltar a adicionar caseNum
  wtData <- as.data.frame(NULL)
  l=length(testset)
  #print(sprintf("Num rows=%d, filter=%s, level=%d, bound=%s.", nrow(testset), filter, as.numeric(level), bound))
  for (i in 1:nrow(testset)) {
    iData <- t(testset[i,-1])#excluir a coluna caseNum
    iwtData <- dwt(iData, filter=filter, n.levels=as.numeric(level), boundary=bound, fast=TRUE)
    wtData <- rbind(wtData, cbind(t(iwtData@W[[iwtData@level]]),t(iwtData@V[[iwtData@level]])))
  }
  test <- cbind(caseNum=testset[,1], wtData) #Voltar a adicionar caseNum
  return(list(train=train,test=test))
}
#dwt<-data.frame(filter=as.character(c('c6','c12','c18')), level=as.numeric(c(5,4,3)), bound=as.character(c('periodic','periodic','reflection')))
#apply(dwt, 1, function(d) applyDWT(d['filter'], d['level'], d['bound'], trainset=sigTrainset, testset=sigTestset))
#dataset=matrix(data.frame(), nrow=3, ncol=1)
#dataset[[1,1]]<-sigTrainset[1:3,]
#dataset[[2,1]]<-sigTrainset[4:5,]
#dataset[[3,1]]<-sigTrainset[6:9,]

applyDWT_o<-function(dataset, filt, lvl, bound){
  wtData <- as.data.frame(NULL)
  l=length(dataset)
  for (i in 1:nrow(dataset)) {
    iData <- t(dataset[i,2:l])#excluir a coluna caseNum
    iwtData <- dwt(iData, filter=filt, n.levels=lvl, boundary=bound, fast=TRUE)
    wtData <- rbind(wtData, cbind(t(iwtData@W[[iwtData@level]]),t(iwtData@V[[iwtData@level]])))
  }
  wtData <- cbind(caseNum=dataset[,1], wtData) #Voltar a adicionar caseNum
  return(wtData)
}

applyDWT_old<-function(dataset, filt, lvl, bound){
  wtData <- as.data.frame(NULL)
  l=length(dataset)
  for (i in 1:nrow(dataset)) {
    iwtData <- dwt(t(dataset[i,2:l]), filter=filt, n.levels=lvl, boundary=bound, fast=TRUE)#retirar o "rn"
    wtData <- rbind(wtData, cbind(t(iwtData@W[[iwtData@level]]),t(iwtData@V[[iwtData@level]])))
  }
  return(wtData)
}
#********************************************************************************************
# PCA - Principal Component Analysis --------------------------------------------------------
#********************************************************************************************
applyPCA<-function(trainset, testset, scale = T, tol = 0, compNbr = 30){
  #Kernel PCA
  trainset = trainset[ ,apply(trainset, 2, var) != 0]#remove zero variance columns from the trainset
  pc <- prcomp(trainset[,-1], scale. = as.logical(scale), tol = as.numeric(tol), rank. = as.numeric(compNbr))#use tol to remove components with SD < tol
  train <- cbind(caseNum=trainset[,1], as.data.frame(pc$x)) #Add caseNum column again

  pred <- predict(pc, newdata = testset[,-1])
  test <- cbind(caseNum=testset[,1], as.data.frame(pred))
  return(list(train=train,test=test))
}

#********************************************************************************************
# Interp - Interpolating Wavelets -----------------------------------------------------------
#********************************************************************************************
# COMPILAR CODIGO C PARA O R
#--R CMD SHLIB DOUBLER.C
#cd C:\Users\Paulo.Nunes\Desktop\ISEL TESE\Codigo\ProjetoMWI>
#PATH=C:\ProgramData\Oracle\Java\javapath;C:\Windows\system32;C:\Windows;C:\Windows\System32\Wbem;C:\Windows\System32\WindowsPowerShell\v1.0\;C:\Program Files\Microsoft ASP.NET\ASP.NET Web Pages\v1.0\;C:\Program Files\Windows Kits\8.0\Windows Performance Toolkit\;C:\Program Files\Microsoft SQL Server\110\Tools\Binn\;C:\Program Files\Microsoft\Web Platform Installer\;C:\ProgramData\chocolatey\bin;C:\Program Files\Git\cmd;C:\Java\apache-maven-3.3.1\bin;C:\Java\apache-karaf-3.0.3\bin;C:\Program Files\Microsoft SQL Server\120\DTS\Binn\;C:\Program Files\Microsoft SQL Server\Client SDK\ODBC\110\Tools\Binn\;C:\Program Files\Microsoft SQL Server\120\Tools\Binn\;C:\Program Files\Microsoft SQL Server\120\Tools\Binn\ManagementStudio\;c:\Program Files\Microsoft SQL Server\110\DTS\Binn\;C:\Program Files\MiKTeX 2.9\miktex\bin\;C:\Rtools\bin\;C:\Rtools\mingw_32\bin
#"C:\Program Files\R\R-3.5.0\bin\i386\r.exe" CMD SHLIB interpV2_0.c
applyInterp<-function(levels, interp_points, epsilon, trainset, testset){
#  dyn.load("./interpV2_0.dll")
  # #dataset length must be power of 2
  # #length(trainset[,-1]) = 1525 /4000
  # lenTrain = length(trainset[,-1])
  # #length(trainset) must be 1536 (=48*32) 2048 (2^11)/ 4096(=2^12)
  # newLenTrain = 2^nextpow2(lenTrain)
  # #naColNames = paste('NA',c(1:(2048-1525)),sep="")#TODO: do the maths
  # #naColNames = paste('NA',c(1:(4096-4000)),sep="")#TODO: do the maths
  # naColNames = paste('NA',c(1:(newLenTrain-lenTrain)),sep="")#TODO: do the maths
  # newtrainset <- trainset
  # newtrainset[,naColNames] <- 0
  # #save(newsigTrainset, file = paste(workDir, "newsigTrainset",".RData", sep=""))
  # newtestset <- testset
  # newtestset[,naColNames] <- 0
  
  #samples value computation: (samples-1) * 2^levels = length(trainset); length must be a power of 2
  #samples = (length(trainset)/2^levels) + 1
  samples = (ncol(trainset)-1)/(2^as.numeric(levels)) + 1
  ncols = as.integer(length(trainset[,-1]))
  res <- as.data.frame(NULL)
  
  examp <- trainset[15,-1]
  Cairo(file='./Plots/Interp_OriginalSignal', type="pdf", width=11, height=8.5, units="in", bg="transparent")
  plot(1:length(examp), examp, col = "Black", main = paste("Sinal a interpolar (", sprintf("%s",length(examp)), " pontos)", sep=""), xlab="tempo", ylab="amplitude", pch='.')
  dev.off()

  for (i in 1:nrow(trainset)) {
    iData <- t(trainset[i,-1]) #remove caseNum column
    ires <- .C("spr_data_to_sparse", iData, as.integer(length(iData)), as.integer(levels), as.integer(samples), as.integer(interp_points), as.double(epsilon))
    ires <- ires[[1]]
    res <- rbind(res, t(ires))
  }
  train <- cbind(caseNum=trainset[,1], res) #Add caseNum column again
  #train <- train[,1:1526]#Remove added columns#TODO: do the maths
  #train <- train[,1:4001]#Remove added columns#TODO: do the maths
  # train <- train[,-c((lenTrain+2):(newLenTrain+1))]#Remove added columns#TODO: do the maths
  
  #ncolNAInterp <- ncol(train[15,is.na(train[15,])])
  #ncolInterp <- ncol(train[15,!is.na(train[15,])])
    
  # examp <- train[15,-1]
  # Cairo(file='./Plots/Interp_IterpolatedSignal', type="pdf", width=11, height=8.5, units="in", bg="transparent", pointsize = 14)
  # plot(1:length(examp), examp, col = "Black", main = paste("Sinal interpolado (", sprintf("%s", length(examp[!is.na(examp)])), " pontos não NaN de ", length(examp), " pontos)", sep="") , xlab="tempo", ylab="amplitude", pch='*')
  # dev.off()

  samples = (ncol(testset)-1)/(2^as.numeric(levels)) + 1
  ncols = as.integer(length(testset[,-1]))
  res <- as.data.frame(NULL)
  for (i in 1:nrow(testset)) {
    iData <- t(testset[i,-1])
    ires <- .C("spr_data_to_sparse", iData, as.integer(length(iData)), as.integer(levels), as.integer(samples), as.integer(interp_points), as.double(epsilon))
    ires <- ires[[1]]
    res <- rbind(res, t(ires))
  }
  test <- cbind(caseNum=testset[,1], res) #Voltar a adicionar caseNum
  #test <- test[,1:1526]#Remove added columns#TODO: do the maths
  #test <- test[,1:4001]#Remove added columns#TODO: do the maths
  # test <- test[,-c((lenTrain+2):(newLenTrain+1))]#Remove added columns#TODO: do the maths
  #  dyn.unload("./interpV2_0.dll")
  
  #Check (in all dataset) which columns has at least one row with non NaN value
  # - Those columns should be present in all rows with significant values
  fullInterpDataSet <- rbind(train,test)
  noNaNCols <- vapply(fullInterpDataSet, function(x) length(unique(x)) > 1, logical(1L))
  #Replace those column, in the rows with NaN value, with the original value
  # - first keep only those columns on train and test sets
  train <- train[,noNaNCols]
  test <- test[,noNaNCols]
  trainset1 <- trainset[,noNaNCols]
  testset1 <- testset[,noNaNCols]
  # - Now replace the NaN
  train[is.na(train)] <- trainset1[is.na(train)]
  test[is.na(test)] <- testset1[is.na(test)]
  #Interp reduced form 1526 to 102 columns (with parameter set 2) ou 296

  #ncolUnif <- ncol(train[15,])
  # examp <- train[15,-1]
  # Cairo(file='./Plots/Interp_UniformizedInterpolatedSignal', type="pdf", width=11, height=8.5, units="in", bg="transparent")
  # plot(1:length(examp), examp, col = "Black", main = paste("Sinal interpolado padronizado (", sprintf("%s", length(examp)), " pontos)", sep=""), xlab="tempo", ylab="amplitude", pch='*')
  # dev.off()

  return(list(train=train, test=test))
  #return(c(ncolInterp, ncolNAInterp, ncolUnif))
}

#interp<-data.frame(levels=as.numeric(c(5,4)), interp_points=as.numeric(c(2,4)), epsilon=as.double(c(0.0001,0.0002)))
#apply(interp, 1, function(i) applyInterp(i['levels'], i['interp_points'], i['epsilon'], dataset=newsigTrainset))
#dataset=matrix(data.frame(), nrow=3, ncol=1)
#dataset[[1,1]]<-sigTrainset[1:3,]
#dataset[[2,1]]<-sigTrainset[4:5,]
#dataset[[3,1]]<-sigTrainset[6:9,]
#apply(dwt, 1, function(d) applyDWT(d['filter'], d['level'], d['bound'], dataset=dataset))

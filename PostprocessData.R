# Paulo Nunes
# Data: 2017-08-07 (1st version)

#********************************************************************************************
#********************************************************************************************
# POSTPROCESS RESULTS -----------------------------------------------------------------------
#********************************************************************************************
#********************************************************************************************

#getPerformance<-function(model, testset, dimTestset, classCol){
#  #TODO: Do this after classification, while we have the current datasets available
#  #TODO: This must be done on several runs, not on each run???
#  data <- cbind(dimTestset[classCol], testset[,-1])#remove caseNum
#  prd <- predict(model, type="class", newdata=data)
#  if (substr(class(model)[1],1,3) == "svm"){
#    prb <- predict(model, type="prob", newdata=data, probability = TRUE) 
#    rocr.predictions <- attr(prb, "probabilities")[,'1']#1=true
#    if (is.null(levels(data[,1]))) { #Test if testset has only one class
#                                    #TODO: add an artificial element from the other class?
#      cm <- NULL
#      rocr <- NULL
#      perf.auc <- NULL
#      auc <- NULL
#      perf <- NULL
#    }
#    else {
#      #cm <- confusionMatrix(prd, data[,1], positive='1')
#      rocr <- prediction(rocr.predictions, data[,1])
#      perf.auc <- performance(rocr,"auc");
#      auc <- as.numeric(perf.auc@y.values)
#      perf <- performance(rocr, "tpr", "fpr")
#    }
#  }
#  else{
#    prb <- predict(model, type="prob", newdata=data) 
#    rocr.predictions<-prb$posterior[,'1']
#    #if (is.null(levels(data[,1]))) {
#    #  cm <- NULL
#    #  rocr <- NULL
#    #  perf.auc <- NULL
#    #  auc <- NULL
#    #  perf <- NULL
#    #}
#    #else {
#      #cm <- confusionMatrix(prd$class, data[,1], positive = '1')
#      rocr <- prediction(rocr.predictions, data[,1])
#      perf.auc <- performance(rocr,"auc");
#      auc <- as.numeric(perf.auc@y.values)
#      perf <- performance(rocr, "tpr", "fpr")
#      #plot(perf)
#    #}
#  }
#  #return(list(roc=rocr, auc=auc, confmatrix=cm, perf=perf))
#  return(list(roc=rocr, auc=auc, perf=perf))
#}

###RESULTS ANALISYS: TODO: NEW MODULE
Accuracy<-function(runResults){
  #TODO: get detailed results, with case number if possible, to plot ROC curve: TPR with diff parameters and algorithms
  res <- matrix(list(), nrow=length(runResults), ncol=1)
  i = 1
  for(s in 1:length(runResults)){
    n_dwt = ifelse(!is.null(runResults[[s]]$parameters.dwt),
                   paste('-DWT',runResults[[s]]$parameters.dwt$filter,
                         runResults[[s]]$parameters.dwt$level,
                         runResults[[s]]$parameters.dwt$bound,
                         runResults[[s]]$parameters.dwt$coeff,sep = "_"),
                   "")
    n_pca = ifelse(!is.null(runResults[[s]]$parameters.pca),
                   paste('-PCA',runResults[[s]]$parameters.pca$scale,
                         runResults[[s]]$parameters.pca$tol,
                         runResults[[s]]$parameters.pca$components,sep='_'),
                   '')
    n_interp = ifelse(!is.null(runResults[[s]]$parameters.interp),
                      paste('-Interp',runResults[[s]]$parameters.interp$levels,
                            runResults[[s]]$parameters.interp$interp_points,
                            runResults[[s]]$parameters.interp$epsilon,
                            runResults[[s]]$parameters.interp$nansubs,
                            runResults[[s]]$parameters.interp$points, sep='_'),
                      '')
    n_svm = ifelse(!is.null(runResults[[s]]$parameters.svm),
                   paste('-SVM',runResults[[s]]$parameters.svm$kern,
                         runResults[[s]]$parameters.svm$cost,
                         runResults[[s]]$parameters.svm$gam, sep='_'),
                   '')
    n_lda = ifelse(!is.null(runResults[[s]]$parameters.lda), '','')
    name = paste(runResults[[s]]$name,n_dwt, n_pca, n_interp, n_svm, n_lda, sep='')
    
    t = runResults[[s]]$results$tbl
    #Sometimes there aren't both classes either in pred or true
    t11<-ifelse((('1' %in% rownames(t)) && ('1' %in% colnames(t))), t['1','1'], 0)
    t00<-ifelse((('0' %in% rownames(t)) && ('0' %in% colnames(t))), t['0','0'], 0)
    t01<-ifelse((('0' %in% rownames(t)) && ('1' %in% colnames(t))), t['0','1'], 0)
    t10<-ifelse((('1' %in% rownames(t)) && ('0' %in% colnames(t))), t['1','0'], 0)
    accur = (t00 + t11)/sum(t)*100
    tpr=t11/(t11+t01)*100
#    accur = (t['0','0'] + t['1','1'])/sum(t)*100
#    tpr=t['1','1']/(t['1','1']+t['0','1'])*100
    rr <- list(name=name, accur=accur, tpr=tpr
               #,roc=runResults[[s]]$results$roc, perf=runResults[[s]]$results$perf, confM=runResults[[s]]$results$confmatrix)
    )
    res[[i]] <- rr
    i = i + 1
  }
  return(res)
}

saveResults<-function(res, filename){
  rrr<-NULL
  for (i in 1:length(res)){
    t = res[[i]]$results$tbl
    #Rows are prediction and columns are true values. So, t01 are values classified as '0' but where really '1'
    #Sometimes there aren't both classes either in pred or true
    t11<-ifelse((('1' %in% rownames(t)) && ('1' %in% colnames(t))), t['1','1'], 0)
    t00<-ifelse((('0' %in% rownames(t)) && ('0' %in% colnames(t))), t['0','0'], 0)
    t01<-ifelse((('0' %in% rownames(t)) && ('1' %in% colnames(t))), t['0','1'], 0)
    t10<-ifelse((('1' %in% rownames(t)) && ('0' %in% colnames(t))), t['1','0'], 0)
    accur = (t00 + t11)/sum(t)*100
    tpr=t11/(t11+t01)*100
    tnr=t00/(t00+t10)*100
    rrr = cbind(name=res[[i]]$name, globals=t(res[[i]]$parameters.globals),
                dwt=t(res[[i]]$parameters.dwt), pca=t(res[[i]]$parameters.pca),
                svm=t(res[[i]]$parameters.svm), interp=t(res[[i]]$parameters.interp),
                #rf=t(res[[i]]$parameters.rf),
                accur=accur, tpr=tpr, tnr=tnr, tp=t11, tn=t00, fp=t10,fn=t01, testcases=res[[i]]$results$testcases) #paste(res[[i]]$results$testcases, sum(t), sep="_"))
    write.table(rrr, file = paste(filename, "_", substr(Sys.time(), 1, 10), ".txt", sep=""),
                append = T, sep = "|", row.names = F,
                col.names=!file.exists(paste(filename, "_", substr(Sys.time(), 1, 10), ".txt", sep="")))
  }
}

saveDWTSVMResults<-function(res, filename){
  rrr<-NULL
  for (i in 1:length(res)){
    t = res[[i]]$results$tbl
    #Sometimes there aren't both classes either in pred or true
    t11<-ifelse((('1' %in% rownames(t)) && ('1' %in% colnames(t))), t['1','1'], 0)
    t00<-ifelse((('0' %in% rownames(t)) && ('0' %in% colnames(t))), t['0','0'], 0)
    t01<-ifelse((('0' %in% rownames(t)) && ('1' %in% colnames(t))), t['0','1'], 0)
    t10<-ifelse((('1' %in% rownames(t)) && ('0' %in% colnames(t))), t['1','0'], 0)
    accur = (t00 + t11)/sum(t)*100
    tpr=t11/(t11+t01)*100
    rrr = cbind(name=res[[i]]$name, globals=t(res[[i]]$parameters.globals),
                dwt=t(res[[i]]$parameters.dwt), pca=res[[i]]$parameters.pca,
                svm=t(res[[i]]$parameters.svm), rf=t(res[[i]]$parameters.rf),
                accur=accur, tpr=tpr, testcases=res[[i]]$results$testcases) #paste(res[[i]]$results$testcases, sum(t), sep="_"))
    write.table(rrr, file = paste(filename, "_", substr(Sys.time(), 1, 10), ".txt", sep=""),
                append = T, sep = "|", row.names = F,
                col.names=!file.exists(paste(filename, "_", substr(Sys.time(), 1, 10), ".txt", sep="")))
  }
}

saveInterpSVMResults<-function(res, filename){
  rrr<-NULL
  for (i in 1:length(res)){
    t = res[[i]]$results$tbl
    #Sometimes there aren't both classes either in pred or true
    t11<-ifelse((('1' %in% rownames(t)) && ('1' %in% colnames(t))), t['1','1'], 0)
    t00<-ifelse((('0' %in% rownames(t)) && ('0' %in% colnames(t))), t['0','0'], 0)
    t01<-ifelse((('0' %in% rownames(t)) && ('1' %in% colnames(t))), t['0','1'], 0)
    t10<-ifelse((('1' %in% rownames(t)) && ('0' %in% colnames(t))), t['1','0'], 0)
    accur = (t00 + t11)/sum(t)*100
    tpr=t11/(t11+t01)*100
#    accur = (t['0','0'] + t['1','1'])/sum(t)*100
#    tpr=t['1','1']/(t['1','1']+t['0','1'])*100
    rrr = cbind(nname=res[[i]]$name, globals=t(res[[i]]$parameters.globals),
                interp=t(res[[i]]$parameters.interp), svm=t(res[[i]]$parameters.svm), accur=accur, tpr=tpr, testcases=paste(res[[i]]$results$testcases, sum(t), sep="_"))
    
    write.table(rrr, file = paste(filename, "_", substr(Sys.time(), 1, 10), ".txt", sep=""),
                append = T, sep = "|", row.names = F,
                col.names=!file.exists(paste(filename, "_", substr(Sys.time(), 1, 10), ".txt", sep="")))
  }
}

savePCASVMResults<-function(res, filename){
  rrr<-NULL
  for (i in 1:length(res)){
    t = res[[i]]$results$tbl
    #Sometimes there aren't both classes either in pred or true
    t11<-ifelse((('1' %in% rownames(t)) && ('1' %in% colnames(t))), t['1','1'], 0)
    t00<-ifelse((('0' %in% rownames(t)) && ('0' %in% colnames(t))), t['0','0'], 0)
    t01<-ifelse((('0' %in% rownames(t)) && ('1' %in% colnames(t))), t['0','1'], 0)
    t10<-ifelse((('1' %in% rownames(t)) && ('0' %in% colnames(t))), t['1','0'], 0)
    accur = (t00 + t11)/sum(t)*100
    tpr=t11/(t11+t01)*100
    rrr = cbind(name=res[[i]]$name, globals=t(res[[i]]$parameters.globals),
                interp=t(res[[i]]$parameters.pca), svm=t(res[[i]]$parameters.svm), accur=accur, tpr=tpr, testcases=paste(res[[i]]$results$testcases, sum(t), sep="_"))
    
    write.table(rrr, file = paste(filename, "_", substr(Sys.time(), 1, 10), ".txt", sep=""),
                append = T, sep = "|", row.names = F,
                col.names=!file.exists(paste(filename, "_", substr(Sys.time(), 1, 10), ".txt", sep="")))
  }
}

saveInterpPCASVMResults<-function(res, filename){
  rrr<-NULL
  for (i in 1:length(res)){
    t = res[[i]]$results$tbl
    #Sometimes there aren't both classes either in pred or true
    t11<-ifelse((('1' %in% rownames(t)) && ('1' %in% colnames(t))), t['1','1'], 0)
    t00<-ifelse((('0' %in% rownames(t)) && ('0' %in% colnames(t))), t['0','0'], 0)
    t01<-ifelse((('0' %in% rownames(t)) && ('1' %in% colnames(t))), t['0','1'], 0)
    t10<-ifelse((('1' %in% rownames(t)) && ('0' %in% colnames(t))), t['1','0'], 0)
    accur = (t00 + t11)/sum(t)*100
    tpr=t11/(t11+t01)*100
#    accur = (t['0','0'] + t['1','1'])/sum(t)*100
#    tpr=t['1','1']/(t['1','1']+t['0','1'])*100
    rrr = cbind(name=res[[i]]$name, globals=t(res[[i]]$parameters.globals),
                interp=t(res[[i]]$parameters.interp), svm=t(res[[i]]$parameters.pca), svm=t(res[[i]]$parameters.svm), accur=accur, tpr=tpr, testcases=paste(res[[i]]$results$testcases, sum(t), sep="_"))
    
    write.table(rrr, file = paste(filename, "_", substr(Sys.time(), 1, 10), ".txt", sep=""),
                append = T, sep = "|", row.names = F,
                col.names=!file.exists(paste(filename, "_", substr(Sys.time(), 1, 10), ".txt", sep="")))
  }
}

saveDWTRFResults<-function(res, filename){
  rrr<-NULL
  for (i in 1:length(res)){
    t = res[[i]]$results$tbl
    #Sometimes there aren't both classes either in pred or true
    t11<-ifelse((('1' %in% rownames(t)) && ('1' %in% colnames(t))), t['1','1'], 0)
    t00<-ifelse((('0' %in% rownames(t)) && ('0' %in% colnames(t))), t['0','0'], 0)
    t01<-ifelse((('0' %in% rownames(t)) && ('1' %in% colnames(t))), t['0','1'], 0)
    t10<-ifelse((('1' %in% rownames(t)) && ('0' %in% colnames(t))), t['1','0'], 0)
    accur = (t00 + t11)/sum(t)*100
    tpr=t11/(t11+t01)*100
    rrr = cbind(name=res[[i]]$name, globals=t(res[[i]]$parameters.globals),
                dwt=t(res[[i]]$parameters.dwt), accur=accur, tpr=tpr, testcases=paste(res[[i]]$results$testcases, sum(t), sep="_"))
    
    write.table(rrr, file = paste(filename, "_", substr(Sys.time(), 1, 10), ".txt", sep=""),
                append = T, sep = "|", row.names = F,
                col.names=!file.exists(paste(filename, "_", substr(Sys.time(), 1, 10), ".txt", sep="")))
  }
}

saveRFResults<-function(res, filename){
  rrr<-NULL
  for (i in 1:length(res)){
    t = res[[i]]$results$tbl
    #Sometimes there aren't both classes either in pred or true
    t11<-ifelse((('1' %in% rownames(t)) && ('1' %in% colnames(t))), t['1','1'], 0)
    t00<-ifelse((('0' %in% rownames(t)) && ('0' %in% colnames(t))), t['0','0'], 0)
    t01<-ifelse((('0' %in% rownames(t)) && ('1' %in% colnames(t))), t['0','1'], 0)
    t10<-ifelse((('1' %in% rownames(t)) && ('0' %in% colnames(t))), t['1','0'], 0)
    accur = (t00 + t11)/sum(t)*100
    tpr=t11/(t11+t01)*100
    rrr = cbind(name=res[[i]]$name, globals=t(res[[i]]$parameters.globals),
                accur=accur, tpr=tpr, testcases=paste(res[[i]]$results$testcases, sum(t), sep="_"))
    
    write.table(rrr, file = paste(filename, "_", substr(Sys.time(), 1, 10), ".txt", sep=""),
                append = T, sep = "|", row.names = F,
                col.names=!file.exists(paste(filename, "_", substr(Sys.time(), 1, 10), ".txt", sep="")))
  }
}


#********************************************************************************************
# COMPUTE FINAL CLASSIFICATION BY VOTING OF EACH ANTENNA ------------------------------------
#********************************************************************************************
computeAntennasVoting<-function(dimtest, results4Ant, classificationType){
  dim <- length(results4Ant[[1]])
  for(tim in 1:dim){
    #*******for each classification mode and parameters*********
    # List to accumulate all votes, one row for each classification mode, one vote for each antenna
    ballotBox <- data.frame(dimtest[,'caseNum'], votesY=0, votesN=0, pred=0, conf=0)#case numbers are the same for all antennas
    for (ant in 1:4){
      #dimTest1a <- splitByAnt[[ant]]$dimtest
      #if (substr(class(results4Ant[[ant]][[tim]]$results$model)[1],1,3) == "svm"){
      if (classificationType == "svm"){
        #p <- as.data.frame(cbind(dimTest1a[,'caseNum'],as.data.frame(runResults4Ant[[ant]][[1]]$results$pred)))
        pp <- as.data.frame(results4Ant[[ant]][[tim]]$results$pred)
        #colnames(pp) <- c("caseNum","pred")
      } else {
        pp <- as.data.frame(cbind(dimtest[,'caseNum'],as.data.frame(results4Ant[[ant]][[tim]]$results$pred$pred)))
        colnames(pp) <- c("caseNum","pred")
      }
      #match with ballotBox and add a vote (yes/no) to each case
      ballotBox[ballotBox$caseNum %in% pp[pp$pred == 1,]$caseNum,]$votesY =
        ballotBox[ballotBox$caseNum %in% pp[pp$pred == 1,]$caseNum,]$votesY +
        as.integer(as.character(pp[pp$pred == 1,]$pred))
      ballotBox[ballotBox$caseNum %in% pp[pp$pred == 0,]$caseNum,]$votesN =
        ballotBox[ballotBox$caseNum %in% pp[pp$pred == 0,]$caseNum,]$votesN +
        abs(as.integer(as.character(pp[pp$pred == 0,]$pred)))
    }
    #Count votes and set resulting prediction
    ballotBox$pred = ifelse(ballotBox$votesY >= ballotBox$votesN, 1, 0)
    ballotBox$conf = ifelse(ballotBox$votesY >= ballotBox$votesN, ballotBox$votesY, ballotBox$votesN)
    #Replace results in antenna 1 position, that will be the golbal result
    results4Ant[[1]][[tim]]$results$pred <- ballotBox$pred
    results4Ant[[1]][[tim]]$results$tbl <- table(pred=ballotBox$pred, true=dimtest[,results4Ant[[1]][[tim]]$parameters.globals$classCol])
  }
}
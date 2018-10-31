# Paulo Nunes
# Data: 2017-09-19 (1st version)

#********************************************************************************************
#********************************************************************************************
# CLASSIFICADORES ---------------------------------------------------------------------------
#********************************************************************************************
#********************************************************************************************

#********************************************************************************************
# SVM - Support Vector Machines -------------------------------------------------------------
#********************************************************************************************
gc()
svmTune<-function(trainset, dimset, classCol, kern, rang){
    tuned <- tune(svm, train.x = trainset[,-1], train.y = dimset[,classCol], type = 'C-classification', kernel = kern,
                  ranges = rang, tunecontrol = tune.control(sampling = "fix"))
  return(tuned)
#All antennas balanced:
  # Call:
  #   best.tune(method = svm, train.x = trainset[, -1], train.y = dimset[, largeClassCol], ranges = list(gamma = 10^(-3:1), 
  #                                                                           cost = 10^(0:4)), tunecontrol = tune.control(sampling = "fix"), type = "C-classification")
  # Parameters:
  #   SVM-Type:  C-classification 
  #   SVM-Kernel:  radial 
  #   cost:  100 
  #   gamma:  0.01
  # Number of Support Vectors:  490
  #best performance=0.003344482
  # Call:
  #   best.tune(method = svm, train.x = Large ~ ., data = data, ranges = list(gamma = 10^(-4:-1), 
  #                                                                           cost = 10^(0:4)), tunecontrol = tune.control(sampling = "fix"))
  # Parameters:
  #   SVM-Type:  eps-regression 
  #   SVM-Kernel:  radial 
  #   cost:  1000 
  #   gamma:  0.01 
  #   epsilon:  0.1 
  # Number of Support Vectors:  730
  #tuned$best.parameters$gamma=0.01
  #tuned$best.parameters$cost=1000
  #tuned$best.model$kernel=2
  #best performance=0.05689847
  #All antennas balanced - after PCA(parameters=1):
  # Call:
  #   best.tune(method = svm, train.x = Large ~ ., data = data, ranges = list(gamma = 10^(-2:-3), 
  #                                                                           cost = 10^(0:4)), tunecontrol = tune.control(sampling = "fix"))
  # Parameters:
  #   SVM-Type:  eps-regression 
  #   SVM-Kernel:  radial 
  #   cost:  100 
  #   gamma:  0.01
  #   epsilon:  0.1 
  # Number of Support Vectors:  730
  #tuned$best.parameters$gamma=0.01
  #tuned$best.parameters$cost=1000
  #tuned$best.model$kernel=2
  #best performance=0.05535773
}

svmModel_Malignant<-function(trainset, dimset, classCol, kern, cost, gam){
  #data <- cbind(dimset[classCol], trainset)
  #model <- svm(Malignant ~ ., data = data, type = 'C-classification', kernel = kern, cost = cost, gamma = gam, probability = TRUE)
  model <- svm(x = trainset, y = dimset[,classCol], type = 'C-classification', kernel = kern, cost = cost, gamma = gam, probability = TRUE)
  return(model)
}

svmModel_Large<-function(trainset, dimset, classCol, kern, cost, gam){
  #data <- cbind(dimset[classCol], trainset)
  #model <- svm(Large ~ ., data = data, type = 'C-classification', kernel = kern, cost = cost, gamma = gam, probability = TRUE)
  model <- svm(x = trainset, y = dimset[,classCol], type = 'C-classification', kernel = kern, cost = cost, gamma = gam, probability = TRUE)
  return(model)
}

svmModel_Microlob<-function(trainset, dimset, classCol, kern, cost, gam){
  #data <- cbind(dimset[classCol], trainset)
  #model <- svm(TumMicrolob ~ ., data = data, type = 'C-classification', kernel = kern, cost = cost, gamma = gam, probability = TRUE)
  model <- svm(x = trainset, y = dimset[,classCol], type = 'C-classification', kernel = kern, cost = cost, gamma = gam, probability = TRUE)
  return(model)
}

svmModel_Smooth<-function(trainset, dimset, classCol, kern, cost, gam){
  #data <- cbind(dimset[classCol], trainset)
  #model <- svm(TumSmooth ~ ., data = data, type = 'C-classification', kernel = kern, cost = cost, gamma = gam, probability = TRUE)
  model <- svm(x = trainset, y = dimset[,classCol], type = 'C-classification', kernel = kern, cost = cost, gamma = gam, probability = TRUE)
  return(model)
}

svmModel_Rad75<-function(trainset, dimset, classCol, kern, cost, gam){
  #data <- cbind(dimset[classCol], trainset)
  #model <- svm(TumRad75 ~ ., data = data, type = 'C-classification', kernel = kern, cost = cost, gamma = gam, probability = TRUE)
  model <- svm(x = trainset, y = dimset[,classCol], type = 'C-classification', kernel = kern, cost = cost, gamma = gam, probability = TRUE)
  return(model)
}

svmModel_Rad25<-function(trainset, dimset, classCol, kern, cost, gam){
  #data <- cbind(dimset[classCol], trainset)
  #model <- svm(TumRad25 ~ ., data = data, type = 'C-classification', kernel = kern, cost = cost, gamma = gam, probability = TRUE)
  model <- svm(x = trainset, y = dimset[,classCol], type = 'C-classification', kernel = kern, cost = cost, gamma = gam, probability = TRUE)
  return(model)
}

svmPred<-function(model, testset){
  pred <- predict(model, testset)
  return(pred)
}

svmProb<-function(model, testset){
  prob <- predict(model, type="prob", testset, probability = TRUE)
  return(prob)
}

svmClassify_Malignant<-function(kern, cost, gam, classCol, trainset, dimTrainset, testset, dimTestset){
  model = svmModel_Malignant(trainset[,-1], dimTrainset, classCol, kern, cost, gam)
  pred = svmPred(model, testset[,-1])
  #prob = svmProb(model, testset[,-1])
  tbl <- table(pred=pred, true=dimTestset[,classCol])
  pred = cbind(caseNum=testset$caseNum, as.data.frame(pred))
  #prob = cbind(caseNum=testset$caseNum, as.data.frame(prob))
  #return(list(model = model, pred = pred, prob = prob, tbl = tbl))
  return(list(model = model, pred = pred, tbl = tbl))
}
#parameters.svm <- data.frame(kern=as.character(c('linear','linear')), cost=as.numeric(c(16,16)), gam=as.numeric(c(2,2)))
#res.svm <- apply(parameters.svm, 1, function(s) svmClassify_Malignant(s['kern'], s['cost'], s['gam'], classCol = malignantClassCol, trainset = res.dwt[[1]]$train, dimTrainset = dimTrainset, testset = res.dwt[[1]]$test, dimTestset = dimTestset))
svmClassify_Large<-function(kern, cost, gam, classCol, trainset, dimTrainset, testset, dimTestset){
  model = svmModel_Large(trainset[,-1], dimTrainset, classCol, kern, cost, gam)
  pred = svmPred(model, testset[,-1])
  #prob = svmProb(model, testset[,-1])
  tbl <- table(pred=pred, true=dimTestset[,classCol])
  pred = cbind(caseNum=testset$caseNum, as.data.frame(pred))
  #prob = cbind(caseNum=testset$caseNum, as.data.frame(prob))
  #return(list(model = model, pred = pred, prob = prob, tbl = tbl))
  return(list(model = model, pred = pred, tbl = tbl))
}

svmClassify_Microlob<-function(kern, cost, gam, classCol, trainset, dimTrainset, testset, dimTestset){
  model = svmModel_Microlob(trainset[,-1], dimTrainset, classCol, kern, cost, gam)
  pred = svmPred(model, testset[,-1])
  #prob = svmProb(model, testset[,-1])
  tbl <- table(pred=pred, true=dimTestset[,classCol])
  pred = cbind(caseNum=testset$caseNum, as.data.frame(pred))
  #prob = cbind(caseNum=testset$caseNum, as.data.frame(prob))
  #return(list(model = model, pred = pred, prob = prob, tbl = tbl))
  return(list(model = model, pred = pred, tbl = tbl))
}

svmClassify_Smooth<-function(kern, cost, gam, classCol, trainset, dimTrainset, testset, dimTestset){
  model = svmModel_Smooth(trainset[,-1], dimTrainset, classCol, kern, cost, gam)
  pred = svmPred(model, testset[,-1])
  #prob = svmProb(model, testset[,-1])
  tbl <- table(pred=pred, true=dimTestset[,classCol])
  pred = cbind(caseNum=testset$caseNum, as.data.frame(pred))
  #prob = cbind(caseNum=testset$caseNum, as.data.frame(prob))
  #return(list(model = model, pred = pred, prob = prob, tbl = tbl))
  return(list(model = model, pred = pred, tbl = tbl))
}

svmClassify_Rad75<-function(kern, cost, gam, classCol, trainset, dimTrainset, testset, dimTestset){
  model = svmModel_Rad75(trainset[,-1], dimTrainset, classCol, kern, cost, gam)
  pred = svmPred(model, testset[,-1])
  #prob = svmProb(model, testset[,-1])
  tbl <- table(pred=pred, true=dimTestset[,classCol])
  pred = cbind(caseNum=testset$caseNum, as.data.frame(pred))
  #prob = cbind(caseNum=testset$caseNum, as.data.frame(prob))
  #return(list(model = model, pred = pred, prob = prob, tbl = tbl))
  return(list(model = model, pred = pred, tbl = tbl))
}

svmClassify_Rad25<-function(kern, cost, gam, classCol, trainset, dimTrainset, testset, dimTestset){
  model = svmModel_Rad25(trainset[,-1], dimTrainset, classCol, kern, cost, gam)
  pred = svmPred(model, testset[,-1])
  #prob = svmProb(model, testset[,-1])
  tbl <- table(pred=pred, true=dimTestset[,classCol])
  pred = cbind(caseNum=testset$caseNum, as.data.frame(pred))
  #prob = cbind(caseNum=testset$caseNum, as.data.frame(prob))
  #return(list(model = model, pred = pred, prob = prob, tbl = tbl))
  return(list(model = model, pred = pred, tbl = tbl))
}


#********************************************************************************************
# LDA - Linear Discriminant Analysis --------------------------------------------------------
#********************************************************************************************
gc()
ldaModel_Malignant<-function(trainset, dimset, classCol){
  tryCatch({
    startTime <- Sys.time()
    data <- cbind(dimset[classCol], trainset)
    model <- lda(Malignant ~ ., data, tol=-1.066792e-26)#tol set to (min trainset var)^2 to solve the "Error in lda.default(x, grouping, ...): variables ... appear to be constant within groups"
    stopTime <- Sys.time()
    totalTime <- stopTime - startTime
    return(model)
  }, interrupt = function(ex) {
    cat("[LDAModel_Malignant]An interrupt was detected.\n");
    print(ex);
  }, error = function(ex) {
    cat("[LDAModel_Malignant]An error was detected.\n");
    print(ex);
  }, finally = {
    cat("[LDAModel_Malignant]Releasing resources...");
    rm(data, startTime, stopTime, totalTime);
  })
}

ldaModel_Large<-function(trainset, dimset, classCol){
  tryCatch({
    startTime <- Sys.time()
    data <- cbind(dimset[classCol], trainset)
    model <- lda(Large ~ ., data, tol=-1.066792e-26)#tol set to (min trainser var)^2 to solve the "Error in lda.default(x, grouping, ...): variables ... appear to be constant within groups"
    stopTime <- Sys.time()
    totalTime <- stopTime - startTime
    return(model)
  }, interrupt = function(ex) {
    cat("[LDAModel_Malignant]An interrupt was detected.\n");
    print(ex);
  }, error = function(ex) {
    cat("[LDAModel_Malignant]An error was detected.\n");
    print(ex);
  }, finally = {
    cat("[LDAModel_Malignant]Releasing resources...");
    rm(data, startTime, stopTime, totalTime);
  })
}

ldaPred <- function(model, testset){
  tryCatch({
    startTime <- Sys.time()
    pred <- predict(model, newdata=testset)
    stopTime <- Sys.time()
    totalTime <- stopTime - startTime
    return(pred)
  }, interrupt = function(ex) {
    cat("[LDAPred]An interrupt was detected.\n");
    print(ex);
  }, error = function(ex) {
    cat("[LDAPred]An error was detected.\n");
    print(ex);
  }, finally = {
    cat("[LDAPred]Releasing resources...");
    rm(startTime, stopTime, totalTime);
  })
}
ldaProb <- function(model, testset){
  tryCatch({
    startTime <- Sys.time()
    prob <- predict(model, newdata=testset, type='prob', probability = TRUE)
    stopTime <- Sys.time()
    totalTime <- stopTime - startTime
    return(prob)
  }, interrupt = function(ex) {
    cat("[LDAProb]An interrupt was detected.\n");
    print(ex);
  }, error = function(ex) {
    cat("[LDAProb]An error was detected.\n");
    print(ex);
  }, finally = {
    cat("[LDAProb]Releasing resources...");
    rm(startTime, stopTime, totalTime);
  })
}

ldaClassify_Malignant<-function(classCol, trainset, dimTrainset, testset, dimTestset) {
  model <- ldaModel_Malignant(trainset[,-1], dimTrainset, classCol)
  pred <- ldaPred(model, testset[,-1])
  #prob <- ldaProb(model, testset[,-1])
  tbl <- table(pred=pred$class, true=dimTestset[,classCol])
  pred = cbind(caseNum=testset$caseNum, as.data.frame(pred))
  #prob = cbind(caseNum=testset$caseNum, as.data.frame(prob))
  #return(list(model = model, pred = pred, prob = prob, tbl = tbl))
  return(list(model = model, pred = pred, tbl = tbl))
}

ldaClassify_Large<-function(classCol, trainset, dimTrainset, testset, dimTestset) {
  model <- ldaModel_Large(trainset[,-1], dimTrainset, classCol)
  pred <- ldaPred(model, testset[,-1])
  #prob <- ldaProb(model, testset[,-1])
  tbl <- table(pred=pred$class, true=dimTestset[,classCol])
  pred = cbind(caseNum=testset$caseNum, as.data.frame(pred))
  #prob = cbind(caseNum=testset$caseNum, as.data.frame(prob))
  #return(list(model = model, pred = pred, prob = prob, tbl = tbl))
  return(list(model = model, pred = pred, tbl = tbl))
}

rfModel_Malignant<-function(trainset, dimset, classCol){
  data <- cbind(dimset[classCol], trainset)
  #if(nrow(data)>3000){
  #  model <- randomForest(Malignant ~ ., data = data, ntree=10)
  #}else
  #{
    model <- randomForest(Malignant ~ ., data = data) #default ntree=500
  #}
  return(model)
}

rfModel_Large<-function(trainset, dimset, classCol){
  data <- cbind(dimset[classCol], trainset)
  #if(nrow(data)>3000){
  #  model <- randomForest(Large ~ ., data = data, ntree=10)
  #}else
  #{
    model <- randomForest(Large ~ ., data = data) #default ntree=500
  #}
  return(model)
}

rfPred<-function(model, testset){
  pred <- predict(model, type="class", newdata = testset)
  return(pred)
}

rfProb<-function(model, testset){
  prob <- predict(model, type="prob", newdata = testset, probability = TRUE)
  return(prob)
}

rfClassify_Malignant<-function(classCol, trainset, dimTrainset, testset, dimTestset){
  model = rfModel_Malignant(trainset[,-1], dimTrainset, classCol)
  pred = rfPred(model, testset[,-1])
  #prob = svmProb(model, testset[,-1])
  tbl <- table(pred=pred, true=dimTestset[,classCol])
  pred = cbind(caseNum=testset$caseNum, as.data.frame(pred))
  #prob = cbind(caseNum=testset$caseNum, as.data.frame(prob))
  #return(list(model = model, pred = pred, prob = prob, tbl = tbl))
  return(list(model = model, pred = pred, tbl = tbl))
}

rfClassify_Large<-function(classCol, trainset, dimTrainset, testset, dimTestset){
  model = rfModel_Large(trainset[,-1], dimTrainset, classCol)
  pred = rfPred(model, testset[,-1])
  #prob = svmProb(model, testset[,-1])
  tbl <- table(pred=pred, true=dimTestset[,classCol])
  pred = cbind(caseNum=testset$caseNum, as.data.frame(pred))
  #prob = cbind(caseNum=testset$caseNum, as.data.frame(prob))
  #return(list(model = model, pred = pred, prob = prob, tbl = tbl))
  return(list(model = model, pred = pred, tbl = tbl))
}

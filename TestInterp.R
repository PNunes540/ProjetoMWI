projectBaseDir = getwd()
source("Libraries.R")


parameters.globals.separationMode = 'Bootstrap'
workDir = "./Work Dataset/ByAntenna/"
load(paste(workDir, "processDataset", "_", parameters.globals.separationMode, ".RData", sep=""))
sigTrainset <- processDataset[[1]]$sigtrain
trainset = sigTrainset

levels = as.numeric(5)
interp_points = as.numeric(4)
epsilon = as.double(0.0001)
samples = (ncol(trainset)-1)/(2^as.numeric(levels)) + 1
ncols = as.integer(length(trainset[,-1]))
res <- as.data.frame(NULL)

dyn.load("./interpV2_0.dll")
#iData <- t(trainset[1,-1]) #remove caseNum column
#save(iData, file = paste(workDir, "iData", "_", parameters.globals.separationMode, ".RData", sep=""))
load(paste(workDir, "iData", "_", parameters.globals.separationMode, ".RData", sep=""))
ires <- .C("spr_data_to_sparse", iData, as.integer(length(iData)), as.integer(levels), as.integer(samples), as.integer(interp_points), as.double(epsilon))
ires <- ires[[1]]
res <- rbind(res, t(ires))
dyn.unload("./interpV2_0.dll")

train <- cbind(caseNum=trainset[1,1], res) #Add caseNum column again


# Paulo Nunes
# Data: 2017-08-07 (1st version)
# Inputs: Dataset from RConceicao
# Outputs: Dataset for automatic classification (added dimensions, different signal compositions) and plots


#Example: importDataset("C:/Users/Paulo.Nunes/Desktop/ISEL TESE/Codigo/Projeto/Input Dataset",
#                       "C:/Users/Paulo.Nunes/Desktop/ISEL TESE/Codigo/Projeto/Work Dataset",
#                       "dataset_DT")
importDataset<-function(inputDir){
  ## ---------------------- DATAFRAME ------------------------------------------------------- ###
  # create empty data.frame
  dataset.DF <- data.frame(caseNum = numeric(),
                       TumModel = numeric(),
                       TumType = numeric(),
                       TumRad = numeric(),
                       Spics = numeric(),
                       angle = numeric(),
                       #Dimensoes calculadas: 0 <=> FALSE
            					 Malignant = factor(), #TumType==1 (spiculated) or TumType==2 (microlobulated)
            					 TumSmooth = factor(), #TumType==4
            					 TumMacrolob = factor(), #TumType==3
            					 TumMicrolob = factor(), #TumType==2
            					 TumSpicul = factor(), #TumType==1
            					 Large = factor(), #TumRad==75 or TumRad==100
            					 TumRad25 = factor(), #TumRad==25
            					 TumRad50 = factor(), #TumRad==50
            					 TumRad75 = factor(), #TumRad==75
            					 TumRad100 = factor(), #TumRad==100
            					 TumSpics3 = factor(), #TumType==1 & Spics==03
            					 TumSpics5 = factor(), #TumType==1 & Spics==05
            					 TumSpics10 = factor(), #TumType==1 & Spics==10
            					 AntAng0 = factor(), #angle==0
            					 AntAng90 = factor(), #angle==90
            					 AntAng180 = factor(), #angle==180
            					 AntAng270 = factor() #angle==270
            					 )
  
  ## ---------------------- READ DATASET AND FILL NEW DATAFRAME ------------------------------------------------------- ###
  # Set the new current directory
  #setwd(inputDir)
  dataset.DF <- NULL
  iCase = 1
  for(mod in c(1:10)){ 				#Model number: from 1 to 10
    for(tipo in c(1:4)){ 				#Tumor type: 1-spiculated, 2-microlobulated, 3-macrolobulated and 4-smooth
      for(rad in c(25, 50, 75, 100)){ #Tumor radius: 25-2.5 mm,  50-5 mm, 75-7.5 mm and 100-10mm
        if(tipo != 1){
          spic <- 00
          f <- c(paste(inputDir, "monostatic_SignalOutMinusNoTumor_Model_01_TumModel_", sprintf("%02d",mod) ,"_TumType_",
                         tipo,"_TumRad_",sprintf("%03d",rad),"_Spics_",sprintf("%02d",spic), ".mat",sep=""))
          mat.File <- readMat(f)
          i <- 0
          for (ang in c(0, 90, 180, 270)){
            i <- i+1
            dataset.DF <- rbind(dataset.DF, data.frame(caseNum = iCase,
                                                       TumModel = mod,
                                                       TumType = tipo,
                                                       TumRad = rad,
                                                       Spics = spic,
                                                       angle = ang,
                                                       Malignant = (if((tipo==1) | (tipo==2)) 1 else 0),
                                                       TumSmooth = (if(tipo==4) 1 else 0),
                                                       TumMacrolob = (if(tipo==3) 1 else 0),
                                                       TumMicrolob = (if(tipo==2) 1 else 0),
                                                       TumSpicul = (if(tipo==1) 1 else 0),
                                                       Large = (if((rad==75) | (rad==100)) 1 else 0),
                                                       TumRad25 = (if(rad==25) 1 else 0),
                                                       TumRad50 = (if(rad==50) 1 else 0),
                                                       TumRad75 = (if(rad==75) 1 else 0),
                                                       TumRad100 = (if(rad==100) 1 else 0),
                                                       TumSpics3 = (if(spic==3) 1 else 0),
                                                       TumSpics5 = (if(spic==5) 1 else 0),
                                                       TumSpics10 = (if(spic==10) 1 else 0),
                                                       AntAng0 = (if(ang==0) 1 else 0),
                                                       AntAng90 = (if(ang==90) 1 else 0),
                                                       AntAng180 = (if(ang==180) 1 else 0),
                                                       AntAng270 = (if(ang==270) 1 else 0),
                                                       as.data.frame(t(mat.File$signalOutMinusNoTumorMonostatic[i,]))
                                                       ))
            iCase = iCase + 1
          }
        }
        else{
          for(spic in c(3, 5, 10)){	#Number of spics: 3, 5 and 10
            f <- c(paste(inputDir, "monostatic_SignalOutMinusNoTumor_Model_01_TumModel_", sprintf("%02d",mod) ,"_TumType_",
                         tipo,"_TumRad_",sprintf("%03d",rad),"_Spics_",sprintf("%02d",spic), ".mat",sep=""))
            mat.File <- readMat(f)
            i <- 0
            for (ang in c(0, 90, 180, 270)){
              i <- i+1
              dataset.DF <- rbind(dataset.DF, data.frame(caseNum = iCase,
                                                         TumModel = mod,
                                                         TumType = tipo,
                                                         TumRad = rad,
                                                         Spics = spic,
                                                         angle = ang,
                                                         Malignant = (if((tipo==1) | (tipo==2)) 1 else 0),
                                                         TumSmooth = (if(tipo==4) 1 else 0),
                                                         TumMacrolob = (if(tipo==3) 1 else 0),
                                                         TumMicrolob = (if(tipo==2) 1 else 0),
                                                         TumSpicul = (if(tipo==1) 1 else 0),
                                                         Large = (if((rad==75) | (rad==100)) 1 else 0),
                                                         TumRad25 = (if(rad==25) 1 else 0),
                                                         TumRad50 = (if(rad==50) 1 else 0),
                                                         TumRad75 = (if(rad==75) 1 else 0),
                                                         TumRad100 = (if(rad==100) 1 else 0),
                                                         TumSpics3 = (if(spic==3) 1 else 0),
                                                         TumSpics5 = (if(spic==5) 1 else 0),
                                                         TumSpics10 = (if(spic==10) 1 else 0),
                                                         AntAng0 = (if(ang==0) 1 else 0),
                                                         AntAng90 = (if(ang==90) 1 else 0),
                                                         AntAng180 = (if(ang==180) 1 else 0),
                                                         AntAng270 = (if(ang==270) 1 else 0),
                                                         as.data.frame(t(mat.File$signalOutMinusNoTumorMonostatic[i,]))
                                                         ))
              iCase = iCase + 1
            }
          }
        }
      }
    }
  }
  return(dataset.DF) 
}

## ---------------------- GET DIMENSIONS DATAFRAME ------------------------------------------------------- ###
#Example: getDimensionsFromDataset(dataset.DT, c(which(colnames(dataset.DF)=="rn"),which(colnames(dataset.DF)=="TumModel"):which(colnames(dataset.DF)=="AntAng270"))
#                       "C:/Users/Paulo.Nunes/Desktop/ISEL TESE/Codigo/Projeto/Work Dataset")
getDimensionsFromDataset<-function(dataset, dimensionsCols) {
    
  #rowNumCol <- min(which(colnames(dataset.DT)=="rn")) #DT com 2 colunas 'rn'
  #tumModCol <- which(colnames(dataset.DT)=="TumModel")
  #antenna270Col <- which(colnames(dataset.DT)=="AntAng270")
  #sigLastCol <- length(dataset.DT[1,])
  
  dimensions <- as.data.frame(NULL)
  for (i in 1:nrow(dataset)) {
    iDim <- dataset[i,dimensionsCols]
    dimensions <- rbind(dimensions, iDim)
  }
  return(dimensions)
}

## ---------------------- GET SIGNAL DATAFRAME ------------------------------------------------------- ###
#Example: getSignalFromDataset(dataset.DT, c(which(colnames(dataset.DF)=="rn"),(which(colnames(dataset.DF)=="AntAng270")+1):length(dataset.DF[1,]))
#                           "C:/Users/Paulo.Nunes/Desktop/ISEL TESE/Codigo/Projeto/Work Dataset")
getSignalFromDataset<-function(dataset, dataCols) {
  
  signal <- as.data.frame(NULL)
  for (i in 1:nrow(dataset)) {
    iSig <- dataset[i,dataCols]
    signal <- rbind(signal, iSig)
  }
  return(signal)
}

## ---------------------- PLOT SIGNAL DATAFRAME ------------------------------------------------- ###
plotSignal_Cairo<-function(name='', dimensions.DF,signal.DF,rowNum, plotDir){
  # Set the new current directory
  #setwd(plotDir)
  
  examp_dataset.DF <- signal.DF[rowNum,-1]
  pdfname <- paste(plotDir, name, "Sinal_modelo", sprintf("%02s",dimensions.DF[rowNum,'TumModel']) ,"_tipo",
                   dimensions.DF[rowNum,'TumType'],"_raio",sprintf("%03s",dimensions.DF[rowNum,'TumRad']),
                   "_espiculos",sprintf("%02s",dimensions.DF[rowNum,'Spics']), ".pdf",sep="")
  leg <- c("rn","model","type","rad","spics")
  Cairo(file=pdfname, type="pdf", width=11, height=8.5, units="in", bg="transparent")
  plot(1:length(examp_dataset.DF), examp_dataset.DF, col = "Black", main = paste(leg, dimensions.DF[rowNum, 1:5], sep="=", collapse=", "), xlab="tempo", ylab="amplitude", pch='.')
  dev.off()
  
  return()
}

plotSignal_CairoSingle<-function(name='', dimensions.DF,signal,rowNum, plotDir){

  pdfname <- paste(plotDir, name, "Sinal_modelo", sprintf("%02s",dimensions.DF[rowNum,'TumModel']) ,"_tipo",
                   dimensions.DF[rowNum,'TumType'],"_raio",sprintf("%03s",dimensions.DF[rowNum,'TumRad']),
                   "_espiculos",sprintf("%02s",dimensions.DF[rowNum,'Spics']), ".pdf",sep="")
  leg <- c("rn","model","type","rad","spics")
  Cairo(file=pdfname, type="pdf", width=11, height=8.5, units="in", bg="transparent")
  plot(1:length(signal), signal, col = "Black", main = paste(leg, dimensions.DF[rowNum, 1:5], sep="=", collapse=", "), xlab="tempo", ylab="amplitude", pch='.')
  dev.off()
  
  return()
}

plotSignal_BarCairo<-function(name='', dimensions.DF, signal.DF, rowNum, plotDir){
  # Set the new current directory
  #setwd(plotDir)
  
  examp_dataset.DF <- signal.DF[rowNum,-1]
  pdfname <- paste(plotDir, name, "Sinal_modelo", sprintf("%02s",dimensions.DF[rowNum,'TumModel']) ,"_tipo",
                   dimensions.DF[rowNum,'TumType'],"_raio",sprintf("%03s",dimensions.DF[rowNum,'TumRad']),
                   "_espiculos",sprintf("%02s",dimensions.DF[rowNum,'Spics']),sep="")
  xx <- barplot(matrix(dimensions_DF[rowNum,], 1, ncol(dimensions_DF[rowNum,])), xaxt= "n", main = paste("Dimensões - amostra ", rowNum, sep=""), xlab = "", ylab = "Valor", names.arg = colnames(dimensions_DF[rowNum,]), col = "lightblue")
  text(x = xx, y = dimensions_DF[rowNum,], label = matrix(dimensions_DF[rowNum,], 1, ncol(dimensions_DF[rowNum,])), pos = 3, cex = 0.8, col = "green")
  axis(1, at=xx, labels=colnames(dimensions_DF[15,]), tick=FALSE, las=2, line=-0.5, cex.axis=0.5)
  #Export PDF. Name=pdfname+"_dimensions.pdf
  Cairo(file=paste(pdfname,"_signal.pdf", sep=""), type="pdf", width=11, height=8.5, units="in", bg="transparent")
  plot(1:length(examp_dataset.DF), examp_dataset.DF, col = "Black", main = paste("Sinal - amostra ", rowNum, sep=""), xlab="tempo", ylab="amplitude", pch='.')
  dev.off()
  
  return()
}

#GGPlot2 - TODO Testar
plotSignal_ggplot<-function(workdir, plotdir){
  #library(ggplot2)
  #library(reshape)
  # Set the new current directory
  setwd(workdir)
  load("signal_DT.RData")
  signal.DF <- as.data.frame(signal.DT)
  load("dimensions_DT.RData")
  dimensions.DF <- as.data.frame(dimensions.DT)
  
  # Set the new current directory
  setwd(plotDir)
  
  exampRow <- which(signal.DF$rn==15)
  examp_dataset.DF <- signal.DF[exampRow,which(colnames(signal.DF)=="V1"):length(signal.DF)]
  pdfname <- paste("Sinal_modelo", sprintf("%02d",dimensions.DF[exampRow,'TumModel']) ,"_tipo",
                   dimensions.DF[exampRow,'TumType'],"_raio",sprintf("%03d",dimensions.DF[exampRow,'TumRad']),
                   "_espiculos",sprintf("%02d",dimensions.DF[exampRow,'Spics']), ".pdf",sep="")

  pdf(file = pdfname, pointsize=10)
  #data <-  rbind(examp_dataset.DF,
  #               as.data.frame(cbind(angle = 0, signalWeightAverage.DT[1,])))
  #data <- data.frame(time = seq(0, ncol(signalWeightAverage.DT)-5), t(data[,-c(1:5)]))
  #colnames(data) <- c("time","angle0","angle90","angle180","angle270","weightaverage")
  #Molten <- melt(data, id.vars = "time")
  #ggplot(Molten, aes(x = time, y = value, colour = variable)) +
  #  labs(size = "angle", x = "time", y = "signal") +
  #  geom_line()
  #dev.off()
  #rm(data,Molten)
  return()
}


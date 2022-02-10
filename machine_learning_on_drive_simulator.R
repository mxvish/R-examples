library(rpart)

calc <- function(judgeResultList, cat11_total, cat12_total, cat21_total, cat22_total) {
    colnames(judgeResultList) <- c("accuracy", "precision", "recall", "fMeasure")
    
    if ((cat11_total + cat12_total + cat21_total + cat22_total) != 0) {
      accuracyTotal <- (cat11_total + cat22_total) / (cat11_total + cat12_total + cat21_total + cat22_total)
    }
    if ((cat22_total + cat12_total) != 0) {
      precisionTotal <- cat22_total / (cat22_total + cat12_total)
    }
    if ((cat22_total + cat21_total) != 0) {
      recallTotal <- cat22_total / (cat22_total + cat21_total)
    }
    if ((recallTotal + precisionTotal) != 0) {
      fMeasureTotal <- (2 * recallTotal * precisionTotal) / (recallTotal + precisionTotal)
    }
    
    return(c(accuracyTotal, precisionTotal, recallTotal, fMeasureTotal))
}

summary <- function(version, course) {
    if (version == "normal") {
        filePath <- "C:\\Users\\oshik\\Documents\\LastReportData\\normal"
    } else {
        filePath <- "C:\\Users\\oshik\\Documents\\LastReportData\\irregular"
    }
    
    playLog_play <- list()
    playLog_lap <- list()
    curSubDirList <- list.files(filePath, pattern = "[^\\.]")
    
    for (tmpDir in curSubDirList) {
      tmpCourseStr <- tolower(tmpDir)
      if (grepl(course, tmpCourseStr) == TRUE) {
        playLog_loaded <- data.frame()
        playLog_loaded_lap <- list()
        curDirSDFiles <- list.files(paste(filePath, tmpDir, sep="\\"), pattern = ".csv", full.names = T)
        
        for (tmpfile in curDirSDFiles) {
          tmpdata <- read.csv(tmpfile, header=F)
          colnames(tmpdata) <- c("WPosX", "WPosY", "WPosZ", "WRotX", "WRotY", "WRotZ", "RoadCategory", "KWallCrush", "KSpeedPad", "KCrushObj", "KSpeed", "KeyRight", "KeyLeft", "KeyForward", "KeyBackward", "Time")
          
          playLog_loaded <- rbind(playLog_loaded, tmpdata)
          playLog_loaded_lap <- append(playLog_loaded_lap, list(tmpdata))
        }
        
        playLog_play <- append(playLog_play, list(playLog_loaded))
        playLog_lap <- append(playLog_lap, playLog_loaded_lap)
      }
    }
   
    curData <- playLog_play
    summary_nooverlap <- list()
    summary_overlap <- list()
    dataSummarySpan <- 16
    
    for (processingData in curData) {
      data_noOverlap <- data.frame(nrow = dim(processingData)[1] %/% dataSummarySpan, ncol = 5)
      data_overlap <- data.frame(nrow = dim(processingData)[1] %/% (dataSummarySpan / 2) - 1, ncol = 5)
      
      loopBreak1 <- FALSE 
      loopCounter1 <- 0 
      
      result.local_KCrush <- "f" 
      result.local_sum <- 0 
      result.local_minSpeed <- 10000 
      result.local_maxSpeed <- 0 
      result.local_minAcc <- 10000 
      result.local_maxAcc <- 0 
      while (loopBreak1 == FALSE) {
        loopCounter1 <- loopCounter1 + 1 
        
        if (processingData[loopCounter1,8] == "t") {
          result.local_KCrush <- "t"
        }
        
        result.local_sum <- result.local_sum + processingData[loopCounter1,11]
        
        if (result.local_minSpeed > processingData[loopCounter1,11]) {
          result.local_minSpeed <- processingData[loopCounter1,11]
        }
        
        if (result.local_maxSpeed < processingData[loopCounter1,11]) {
          result.local_maxSpeed <- processingData[loopCounter1,11]
        }
        
        if (loopCounter1 > 1) { 
          localAcc <- processingData[loopCounter1,11] - processingData[loopCounter1 - 1,11]
          
          if (result.local_minAcc > localAcc) {
            result.local_minAcc <- localAcc
          }
          
          if (result.local_maxAcc < localAcc) {
            result.local_maxAcc <- localAcc
          }
        }
        
        if (loopCounter1 %% dataSummarySpan == 0) { 
          data_noOverlap[loopCounter1 %/% dataSummarySpan,1] <- result.local_KCrush
          data_noOverlap[loopCounter1 %/% dataSummarySpan,2] <- result.local_sum / dataSummarySpan
          data_noOverlap[loopCounter1 %/% dataSummarySpan,3] <- result.local_maxSpeed - result.local_minSpeed
          data_noOverlap[loopCounter1 %/% dataSummarySpan,4] <- result.local_minAcc
          data_noOverlap[loopCounter1 %/% dataSummarySpan,5] <- result.local_maxAcc - result.local_minAcc
          
          result.local_KCrush <- "f" 
          result.local_sum <- 0 
          result.local_minSpeed <- 10000 
          result.local_maxSpeed <- 0 
          result.local_minAcc <- 10000 
        }
        
        if (loopCounter1 >= dim(processingData)[1]){
          loopBreak1 <- TRUE
        }
      }
      
      data_noOverlap[,1] <- as.factor(data_noOverlap[,1])
      colnames(data_noOverlap) <- c("wallCrush", "aveSpeed", "speedRange", "minAcc", "accRange")
      
      loopBreak1 <- FALSE 
      loopCounter1 <- 0 
      
      result.local_KCrush <- "f" 
      result.local_sum <- 0 
      result.local_minSpeed <- 10000 
      result.local_maxSpeed <- 0 
      result.local_minAcc <- 10000 
      sumCounter <- 0 
      saveCounter <- 0 
      while (loopBreak1 == FALSE) {
        loopCounter1 <- loopCounter1 + 1 
        sumCounter <- sumCounter + 1 
        
        if (processingData[loopCounter1,8] == "t") {
          result.local_KCrush <- "t"
        }
        
        result.local_sum <- result.local_sum + processingData[loopCounter1,11]
        
        if (result.local_minSpeed > processingData[loopCounter1,11]) {
          result.local_minSpeed <- processingData[loopCounter1,11]
        }
        
        if (result.local_maxSpeed < processingData[loopCounter1,11]) {
          result.local_maxSpeed <- processingData[loopCounter1,11]
        }
        
        if (loopCounter1 > 1) { 
          localAcc <- processingData[loopCounter1,11] - processingData[loopCounter1 - 1,11]
          
          if (result.local_minAcc > localAcc) {
            result.local_minAcc <- localAcc
          }
          
          if (result.local_maxAcc < localAcc) {
            result.local_maxAcc <- localAcc
          }
        }
        
        if (sumCounter == dataSummarySpan) {
          saveCounter <- saveCounter + 1
          data_overlap[saveCounter,1] <- result.local_KCrush
          data_overlap[saveCounter,2] <- result.local_sum / dataSummarySpan
          data_overlap[saveCounter,3] <- result.local_maxSpeed - result.local_minSpeed
          data_overlap[saveCounter,4] <- result.local_minAcc
          data_overlap[saveCounter,5] <- result.local_maxAcc - result.local_minAcc
          
          result.local_KCrush <- "f" 
          result.local_sum <- 0 
          result.local_minSpeed <- 10000 
          result.local_maxSpeed <- 0 
          result.local_minAcc <- 10000 
          sumCounter <- 0 
          loopCounter1 <- loopCounter1 - (dataSummarySpan / 2) 
        }
        
        if (loopCounter1 >= dim(processingData)[1]){
          loopBreak1 <- TRUE
        }
      }
      
      data_overlap[,1] <- as.factor(data_overlap[,1])
      colnames(data_overlap) <- c("wallCrush", "aveSpeed", "speedRange", "minAcc", "accRange")
      summary_nooverlap <- append(summary_nooverlap, list(data_noOverlap))
      summary_overlap <- append(summary_overlap, list(data_overlap))
    }
    return(summary_overlap)
}

step1analyze <- function(cvBaseData, model) {
    judgeResultList <- list()
    cat11_total <- 0 
    cat12_total <- 0 
    cat21_total <- 0 
    cat22_total <- 0 
    
    message(paste("Total", length(cvBaseData), "processes."))
    
    dataIndex <- 0 
    while (dataIndex < length(cvBaseData)){
      dataIndex <- dataIndex + 1;
      
      tmpTrainingData <- list() 
      for (i in 1:length(cvBaseData)) {
        
        if (i != dataIndex) {
          tmpTrainingData <- rbind(tmpTrainingData, cvBaseData[[i]])
        }
      }
      
      if (model == "decisiontree") {
        tmpResult <- rpart(wallCrush ~. , data = tmpTrainingData)
      } else {
        tmpTrainingData$KWallCrush <- as.data.frame(tmpTrainingData$KWallCrush)
        tmpResult <- ksvm(wallCrush ~ ., data = tmpTrainingData)
      }
      
      tmpPredict <- predict(tmpResult, cvBaseData[[dataIndex]], type = "class")
      
      if (model == "decisiontree") {
        predictTable <- table(cvBaseData[[dataIndex]]$wallCrush, tmpPredict)
      } else {
        predictTable <- table(as.factor(cvBaseData[[dataIndex]]$wallCrush), tmpPredict)
      }
      
      if (dim(predictTable)[1] > 1) {
        cat11 <- predictTable[1,1] 
        cat12 <- predictTable[1,2] 
        cat21 <- predictTable[2,1] 
        cat22 <- predictTable[2,2] 
      }
      else {
        cat11 <- predictTable[1,1] 
        cat12 <- predictTable[1,2] 
        cat21 <- 0 
        cat22 <- 0 
      }
      
      accuracyLocal <- 0
      if ((cat11 + cat12 + cat21 + cat22) != 0) {
        accuracyLocal <- (cat11 + cat22) / (cat11 + cat12 + cat21 + cat22)
      }
      precisionLocal <- 0
      if ((cat22 + cat12) != 0) {
        precisionLocal <- cat22 / (cat22 + cat12)
      }
      recallLocal <- 0
      if ((cat22 + cat12) != 0) {
        recallLocal <- cat22 / (cat22 + cat21)
      }
      fMeasureLocal <- 0
      if ((recallLocal + precisionLocal) != 0) {
        fMeasureLocal <- (2 * recallLocal * precisionLocal) / (recallLocal + precisionLocal)
      }
      
      judgeResultList <- rbind(judgeResultList, cbind(accuracyLocal, precisionLocal, recallLocal, fMeasureLocal))
      cat11_total <- cat11_total + cat11
      cat12_total <- cat12_total + cat12
      cat21_total <- cat21_total + cat21
      cat22_total <- cat22_total + cat22
    }
    return(calc(judgeResultList, cat11_total, cat12_total, cat21_total, cat22_total)) 
}

fusion <- function(curFusionData) {
    fusionedData <- list()
    
    for (tmpFusionData in curFusionData) {
      fusionedData <- rbind(fusionedData, tmpFusionData)
    }
    
    return(fusionedData)
}

step2analyze <- function(predictData, trainData1, trainData2, model) {
    accuracyTotal <- 0    
    precisionTotal <- 0   
    recallTotal <- 0      
    fMeasureTotal <- 0    
    
    judgeResultList <- list()
    
    cat11_total <- 0 
    cat12_total <- 0 
    cat21_total <- 0 
    cat22_total <- 0 
    
    message(paste("Total 1 processes."))
    
    tmpTrainingData <- list() 
    allTrainData <- rbind(trainData1, trainData2)
    return(step3analyze(predictData, allTrainData, model))
}

step3analyze <- function(predictData, trainData, model) {
    judgeResultList <- list()
    
    cat11_total <- 0 
    cat12_total <- 0 
    cat21_total <- 0 
    cat22_total <- 0 
    
    message(paste("Total 1 processes."))
    
    tmpTrainingData <- list() 
    trainData <- as.data.frame(trainData)
    tmpTrainingData <- rbind(tmpTrainingData, trainData)
    
    if (model == "decisiontree") {
      tmpResult <- rpart(wallCrush ~. , data = tmpTrainingData)
    } else {
      tmpTrainingData$KWallCrush <- as.data.frame(tmpTrainingData$KWallCrush)
      tmpResult <- ksvm(wallCrush ~ ., data = tmpTrainingData)
    }
    
    tmpPredict <- predict(tmpResult, predictData, type = "class")
    
    if (model == "decisiontree") {
      predictTable <- table(predictData$wallCrush, tmpPredict)
    } else {
      predictTable <- table(as.factor(predictData$wallCrush), tmpPredict)
    }
    
    if (dim(predictTable)[1] > 1) {
      cat11 <- predictTable[1,1] 
      cat12 <- predictTable[1,2] 
      cat21 <- predictTable[2,1] 
      cat22 <- predictTable[2,2] 
    }
    else {
      cat11 <- predictTable[1,1] 
      cat12 <- predictTable[1,2] 
      cat21 <- 0 
      cat22 <- 0 
    }
    
    accuracyLocal <- 0
    if ((cat11 + cat12 + cat21 + cat22) != 0) {
      accuracyLocal <- (cat11 + cat22) / (cat11 + cat12 + cat21 + cat22)
    }
    precisionLocal <- 0
    if ((cat22 + cat12) != 0) {
      precisionLocal <- cat22 / (cat22 + cat12)
    }
    recallLocal <- 0
    if ((cat22 + cat12) != 0) {
      recallLocal <- cat22 / (cat22 + cat21)
    }
    fMeasureLocal <- 0
    if ((recallLocal + precisionLocal) != 0) {
      fMeasureLocal <- (2 * recallLocal * precisionLocal) / (recallLocal + precisionLocal)
    }
    
    judgeResultList <- rbind(judgeResultList, cbind(accuracyLocal, precisionLocal, recallLocal, fMeasureLocal))
    cat11_total <- cat11_total + cat11
    cat12_total <- cat12_total + cat12
    cat21_total <- cat21_total + cat21
    cat22_total <- cat22_total + cat22
   
    return(calc(judgeResultList, cat11_total, cat12_total, cat21_total, cat22_total)) 
}

countryNormalSummary <- summary("normal", "country")
windingNormalSummary <- summary("normal","winding")
mountainNormalSummary <- summary("normal","mountain")
countryIrregularSummary <- summary("irregular", "country")
windingIrregularSummary <- summary("irregular", "winding")
mountainIrregularSummary <- summary("irregular","mountain")

countryNormalRpart <- step1analyze(countryNormalSummary, "decisiontree")
windingNormalRpart <- step1analyze(windingNormalSummary, "decisiontree")
mountainNormalRpart <- step1analyze(mountainNormalSummary, "decisiontree")
countryIrregularRpart <- step1analyze(countryIrregularSummary, "decisiontree")
windingIrregularRpart <- step1analyze(windingIrregularSummary, "decisiontree")
mountainIrregularRpart <- step1analyze(mountainIrregularSummary, "decisiontree")

countryNormalFusioned <- fusion(countryNormalSummary)
windingNormalFusioned <- fusion(windingNormalSummary)
mountainNormalFusioned <- fusion(mountainNormalSummary)
countryIrregularFusioned <- fusion(countryIrregularSummary)
windingIrregularFusioned <- fusion(windingIrregularSummary) 
mountainIrregularFusioned <- fusion(mountainIrregularSummary)

countryNormalPredict <- step2analyze(countryNormalFusioned, windingNormalFusioned, mountainNormalFusioned, "decisiontree")
windingNormalPredict <- step2analyze(windingNormalFusioned, countryNormalFusioned, mountainNormalFusioned, "decisiontree")
mountainNormalPredict <- step2analyze(mountainNormalFusioned, countryNormalFusioned, windingNormalFusioned, "decisiontree")
countryIrregularPredict <- step2analyze(countryIrregularFusioned, windingIrregularFusioned, mountainIrregularFusioned, "decisiontree")
windingIrregularPredict <- step2analyze(windingIrregularFusioned, countryIrregularFusioned, mountainIrregularFusioned, "decisiontree")
mountainIrregularPredict <- step2analyze(mountainIrregularFusioned, countryIrregularFusioned, windingIrregularFusioned, "decisiontree")

allNormalFusioned <- rbind(countryNormalFusioned, windingNormalFusioned, mountainNormalFusioned)
allIrregularFusioned <- rbind(countryIrregularFusioned, windingIrregularFusioned, mountainIrregularFusioned)

allNormalPredict <- step3analyze(allNormalFusioned, allIrregularFusioned, "decisiontree")
allIrregularPredict <- step3analyze(allIrregularFusioned, allNormalFusioned, "decisiontree")

print("------------------analyze1------------------")
countryNormalRpart
windingNormalRpart
mountainNormalRpart
countryIrregularRpart
windingIrregularRpart
mountainIrregularRpart

print("------------------analyze2------------------")
countryNormalPredict
windingNormalPredict
mountainNormalPredict
countryIrregularPredict
windingIrregularPredict
mountainIrregularPredict

print("------------------analyze3------------------")
allNormalPredict
allIrregularPredict

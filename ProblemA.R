library(rectools)
library(reshape2)
library(plyr)
library(polyreg)
library(ggplot2)

getMeltedData <- function() {
  data <- read.csv("house-votes-84.data", header = FALSE)
  colnames(data) <- c("party", 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
  ids <- 1:nrow(data)
  data <- cbind(ids, data)
  meltedData <- melt(data, id.vars = c("ids", "party"))
  
  meltedData <- meltedData[c(1,3,4,2)]
  colnames(meltedData) <- c("s", "d", "y","party")
  
  meltedData$party <- revalue(meltedData$party, c('democrat' = 1))
  meltedData$party <- revalue(meltedData$party, c('republican' = 0))
  meltedData$y <- revalue(meltedData$y, c('y' = 1))
  meltedData$y <- revalue(meltedData$y, c('n' = 0))
  meltedData
}

getKnownVotes <- function(meltedData) {
  knownVotes <- meltedData[meltedData$y != '?',]
  knownVotes
}

getUnknownVotes <- function(meltedData) {
  unknownVotes <- meltedData[meltedData$y == '?',]
  unknownVotes
}

getMissingData <- function(meltedData) {
  topredictVotes <- meltedData[meltedData$y == '?',]
  topredictVotes
}

getUserCovs <- function(meltedData) {
  usercov <- NULL
  for (u in unique(meltedData$s)){
    id <- which(meltedData$s == u)[1]
    usercov <- append(usercov,meltedData[id,4])
  }
  usercov
}

knnBestKAnalysis <- function(meltedData, knownVotes) {
  #KNN
  # First do an MAPE and then PGEC analysis using KNN by splitting the knownVotes data set into training and test sets
  uniqueuser <- unique(meltedData$s)
  usercov <- getUserCovs(meltedData)
  set.seed(10,kind = "Mersenne-Twister",normal.kind = "Inversion")
  Ids <- sample(1:nrow(knownVotes), 1000) #picked a sample of 1000 amongst all of the data points in knownVotes
  train <- knownVotes[-Ids,]
  train$y <- as.numeric(train$y)
  test <- knownVotes[Ids,]
  #test$y <- as.numeric(test$y)
  test$y <- as.numeric(test$y)
  test2 <- test
  #test2$y <- NA
  trainSetC <- formUserData(train[,1:3],as.matrix(usercov))
  testSetC <- formUserData(test2[,1:3],as.matrix(usercov))
  trainSet <- formUserData(train[,1:3])
  testSet <- formUserData(test2[,1:3])
  
  p1 <- 0
  p2 <- 0
  #K <- c(2,5,10,50,100,150,200)
  K <- c(1,10,30,50,100,200)
  MAPEC <- NULL
  MAPENC <- NULL
  PGECC <- NULL
  PGENC <- NULL
  covCorrect <- 0
  noncovCorrect <- 0
  totalPredictions <- 0
  for (k in K){
    pred1 <- NULL
    pred2 <- NULL
    i <- 1
    for (user in testSetC){
      data_mat <- test[which(test$s == user$userID),1:3]
      userNC <- testSet[[i]]
      i <- i + 1
      items <- user$itms
      for (item in items){
        p1 <- predict(trainSetC,user,item,k,wtcovs = 1)
        p2 <- predict(trainSet, userNC,item,k,wtcovs = NULL)
        expectedVal <- data_mat[data_mat$d == item,]$y
        pred1 <- c(pred1,abs(p1 - expectedVal))
        pred2 <- c(pred2,abs(p2 - expectedVal))
        totalPredictions <- totalPredictions + 1
        if (round(p1) == expectedVal) {
          covCorrect <- covCorrect + 1 
        }
        if (round(p2) == expectedVal) {
          noncovCorrect <- noncovCorrect + 1
        }
      }
    }
    MAPEC <- c(MAPEC,mean(pred1,na.rm = TRUE))
    MAPENC <- c(MAPENC,mean(pred2,na.rm = TRUE))
    
    PGECC <- c(PGECC, covCorrect / totalPredictions)
    PGENC <- c(PGENC, noncovCorrect / totalPredictions)
    
  }
  
  knn1 <- data.frame(K,MAPEC)
  knn2 <- data.frame(K,MAPENC)
  knn3 <- data.frame(K, PGECC)
  knn4 <- data.frame(K, PGENC)
  
  ggplot() +
    ggtitle("MAPE Values for KNN prediction with and without covariates") +
    geom_point(data = knn1, aes(x = K, y = MAPEC),color = "red") +
    geom_point(data = knn2, aes(x = K, y = MAPENC),color = "blue") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
    xlab("K - number of neighbors") +
    ylab("MAPE error prediction values")
  
  ggplot() +
    ggtitle("PGEC Values for KNN prediction with and without covariates") +
    geom_point(data = knn3, aes(x = K, y = PGECC),color = "red") +
    geom_point(data = knn4, aes(x = K, y = PGENC),color = "blue") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
    xlab("K - number of neighbors") +
    ylab("PGEC - probability of guessing correctly")
  
}

knnPredictUnknownVotes <- function(meltedData) {
  usercov <- getUserCovs(meltedData)
  set.seed(10,kind = "Mersenne-Twister",normal.kind = "Inversion")
  Ids <- sample(1:nrow(knownVotes), 1000) #picked a sample of 1000 amongst all of the data points in knownVotes
  train <- knownVotes
  train$y <- as.numeric(train$y)
  test <- getUnknownVotes(meltedData)
  #test$y <- as.numeric(test$y)
  test$y <- as.numeric(test$y)
  test2 <- test
  #test2$y <- NA
  usercov2 <- getUserCovs(test)
  trainSetC <- formUserData(train[,1:3], usrCovs = as.matrix(usercov))
  testSetC <- formUserData(test2[,1:3], usrCovs = as.matrix(usercov))
  
  k <- 10
  
  predictionsVec <- NULL
  for (user in testSetC){
    data_mat <- test[which(test$s == user$userID),1:3]
    items <- user$itms
    for (item in items){
      p1 <- predict(trainSetC,user,item,k,wtcovs = 1)
      #unknownVotes[unknownVotes$s == testSetC[[user]]$userID && unknownVotes$d == item,1:3]$y <- p1
      #users <- c(users, testSetC[[user]]$userID)
      #items2 <- c(items2, item)
      #ratings <- c(ratings, round(p1))
      #parties <- c(parties, as.numeric(data_mat$party[1]) - 1)
      predictionsVec <- c(predictionsVec, p1)
      
    }
  }
  
  predictionsVec
}

knn_bestMAPE <- function(meltedData, knownVotes) {
  #KNN
  # First do an MAPE and then PGEC analysis using KNN by splitting the knownVotes data set into training and test sets
  uniqueuser <- unique(meltedData$s)
  usercov <- getUserCovs(meltedData)
  set.seed(10,kind = "Mersenne-Twister",normal.kind = "Inversion")
  Ids <- sample(1:nrow(knownVotes), 1000) #picked a sample of 1000 amongst all of the data points in knownVotes
  train <- knownVotes[-Ids,]
  train$y <- as.numeric(train$y)
  test <- knownVotes[Ids,]
  test$y <- as.numeric(test$y)
  test2 <- test
  test2$y <- NA
  trainSetC <- formUserData(train[,1:3],as.matrix(usercov))
  testSetC <- formUserData(test2[,1:3],as.matrix(usercov))
  trainSet <- formUserData(train[,1:3])
  testSet <- formUserData(test2[,1:3])
  
  p1 <- 0
  p2 <- 0
  #K <- c(2,5,10,50,100,150,200)
  K <- c(1,10,30,50,100,200)
  MAPEC <- NULL
  MAPENC <- NULL
  PGECC <- NULL
  PGENC <- NULL
  covCorrect <- 0
  noncovCorrect <- 0
  totalPredictions <- 0
  for (k in K){
    pred1 <- NULL
    pred2 <- NULL
    for (user in 1:length(unique(test$s))){
      data_mat <- test[which(test$s == testSetC[[user]]$userID),1:3]
      items <- data_mat$d
      for (item in items){
        p1 <- predict(trainSetC,testSetC[[user]],item,k,wtcovs = 1)
        p2 <- predict(trainSet, testSet[[user]],item,k)
        expectedVal <- data_mat$y[data_mat$d == item]
        pred1 <- c(pred1,abs(p1 - expectedVal))
        pred2 <- c(pred2,abs(p2 - expectedVal))
        totalPredictions = totalPredictions + 1
        if (round(p1) == expectedVal) {
          covCorrect = covCorrect + 1 
        }
        if (round(p2) == expectedVal) {
          noncovCorrect = noncovCorrect + 1
        }
      }
    }
    MAPEC <- c(MAPEC,mean(pred1,na.rm = TRUE))
    MAPENC <- c(MAPENC,mean(pred2,na.rm = TRUE))
    
    PGECC <- c(PGECC, covCorrect / totalPredictions)
    PGENC <- c(PGENC, noncovCorrect / totalPredictions)
    
  }
  
  knn1 <- data.frame(K,MAPEC)
  knn2 <- data.frame(K,MAPENC)
  knn3 <- data.frame(K, PGECC)
  knn4 <- data.frame(K, PGENC)
  
  bestMAPE <- min(knn1[,2])
  bestMAPEK <- knn1[which(knn1[,2] == bestMAPE),]
  bestMAPEK 
}

knn_bestPGEC <- function(meltedData, knownVotes) {
  #KNN
  # First do an MAPE and then PGEC analysis using KNN by splitting the knownVotes data set into training and test sets
  uniqueuser <- unique(meltedData$s)
  usercov <- getUserCovs(meltedData)
  set.seed(10,kind = "Mersenne-Twister",normal.kind = "Inversion")
  Ids <- sample(1:nrow(knownVotes), 1000) #picked a sample of 1000 amongst all of the data points in knownVotes
  train <- knownVotes[-Ids,]
  train$y <- as.numeric(train$y)
  test <- knownVotes[Ids,]
  test$y <- as.numeric(test$y)
  test2 <- test
  test2$y <- NA
  trainSetC <- formUserData(train[,1:3],as.matrix(usercov))
  testSetC <- formUserData(test2[,1:3],as.matrix(usercov))
  trainSet <- formUserData(train[,1:3])
  testSet <- formUserData(test2[,1:3])
  
  p1 <- 0
  p2 <- 0
  #K <- c(2,5,10,50,100,150,200)
  K <- c(1,10,30,50,100,200)
  MAPEC <- NULL
  MAPENC <- NULL
  PGECC <- NULL
  PGENC <- NULL
  covCorrect <- 0
  noncovCorrect <- 0
  totalPredictions <- 0
  for (k in K){
    pred1 <- NULL
    pred2 <- NULL
    for (user in 1:length(unique(test$s))){
      data_mat <- test[which(test$s == testSetC[[user]]$userID),1:3]
      items <- data_mat$d
      for (item in items){
        p1 <- predict(trainSetC,testSetC[[user]],item,k,wtcovs = 1)
        p2 <- predict(trainSet, testSet[[user]],item,k)
        expectedVal <- data_mat$y[data_mat$d == item]
        pred1 <- c(pred1,abs(p1 - expectedVal))
        pred2 <- c(pred2,abs(p2 - expectedVal))
        totalPredictions = totalPredictions + 1
        if (round(p1) == expectedVal) {
          covCorrect = covCorrect + 1 
        }
        if (round(p2) == expectedVal) {
          noncovCorrect = noncovCorrect + 1
        }
      }
    }
    MAPEC <- c(MAPEC,mean(pred1,na.rm = TRUE))
    MAPENC <- c(MAPENC,mean(pred2,na.rm = TRUE))
    
    PGECC <- c(PGECC, covCorrect / totalPredictions)
    PGENC <- c(PGENC, noncovCorrect / totalPredictions)
    
  }
  
  knn1 <- data.frame(K,MAPEC)
  knn2 <- data.frame(K,MAPENC)
  knn3 <- data.frame(K, PGECC)
  knn4 <- data.frame(K, PGENC)
  
  bestPGEC <- max(knn3[,2])
  bestPGECK <- knn3[which(knn3[,2] == bestPGEC),]
  bestPGECK 
}

nmf_analysis <- function(meltedData,knownVotes){
  set.seed(9999)
  uniqueuser <- unique(meltedData$s)
  usercov <- getUserCovs(meltedData)
  Ids <- sample(1:nrow(knownVotes), 1000) #picked a sample of 1000 amongst all of the data points in knownVotes
  train <- knownVotes[-Ids,]
  test <- knownVotes[Ids,]
  train$y <- as.numeric(train$y)
  test$y <- as.numeric(test$y)
  train$party <- as.numeric(train$party)
  test$party <- as.numeric(test$party)
  ranks <- c(1,10,30,50,100,200)
  MAPECB <- NULL
  MAPENCB <- NULL
  MAPECNB <- NULL
  MAPENCNB <- NULL
  PGECCB <- NULL
  PGECCNB <- NULL
  PGECNCB <- NULL
  PGECNCNB <- NULL
  pred1 <- NULL
  pred2 <- NULL
  pred3 <- NULL
  pred4 <- NULL
  total_prediction <- 0
  p1 <- NULL
  p2 <- NULL
  p3 <- NULL
  p4 <- NULL
  for (rank in ranks){
    trCB <- trainReco(train,rank,nmf = TRUE,biasAdjust = TRUE)
    trCNB <- trainReco(train,rank,nmf = TRUE,biasAdjust = FALSE)
    trNCB <- trainReco(train[,-4],rank,nmf = TRUE, biasAdjust = TRUE)
    trNCNB <- trainReco(train[,-4],rank,nmf = TRUE,biasAdjust = FALSE)
    p1 <- predict(trCB,test[,-3])
    p2 <- predict(trCNB,test[,-3])
    p3 <- predict(trNCB,test[,1:2])
    p4 <- predict(trNCNB,test[,1:2])
    pred1 <- abs(p1 - test[,3])
    pred2 <- abs(p2 - test[,3])
    pred3 <- abs(p3 - test[,3])
    pred4 <- abs(p4 - test[,3])
    MAPECB <- c(MAPECB,mean(pred1,na.rm = TRUE))
    MAPECNB <- c(MAPECNB,mean(pred2,na.rm = TRUE))
    MAPENCB <- c(MAPENCB,mean(pred3,na.rm = TRUE))
    MAPENCNB <- c(MAPENCNB,mean(pred4,na.rm = TRUE))
    total_prediction <- length(p1)
    covB <- 0
    covNB <- 0
    ncovB <- 0
    ncovNB <- 0
    for (i in 1:length(p1)){
      expVal <- test[i,3]
      if (round(p1[i]) == expVal){
        covB <- covB + 1
      }
      if (round(p2[i]) == expVal){
        covNB <- covNB + 1
      }
      if (round(p3[i]) == expVal){
        ncovB <- ncovB + 1
      }
      if (round(p4[i]) == expVal){
        ncovNB <- ncovNB + 1
      }
    }
    
    PGECCB <- c(PGECCB,covB/total_prediction)
    PGECCNB <- c(PGECCNB,covNB / total_prediction)
    PGECNCB <- c(PGECNCB,ncovB / total_prediction)
    PGECNCNB <- c(PGECNCNB,ncovNB / total_prediction)
  }
  
  print("MAPE VALUES")
  print(MAPECB)
  print(MAPECNB)
  print(MAPENCB)
  print(MAPENCNB)
  print("PGEC VALUES")
  print(PGECCB)
  print(PGECCNB)
  print(PGECNCB)
  print(PGECNCNB)
  matfact1 <- data.frame(ranks,MAPECB)
  matfact2 <- data.frame(ranks,MAPECNB)
  matfact3 <- data.frame(ranks,MAPENCB)
  matfact4 <- data.frame(ranks,MAPENCNB)
  matfact5 <- data.frame(ranks,PGECCB)
  matfact6 <- data.frame(ranks,PGECCNB)
  matfact7 <- data.frame(ranks,PGECNCB)
  matfact8 <- data.frame(ranks,PGECNCNB)
  
  ggplot() +
    ggtitle("MAPE Values for matrix factorization with and without covariates") +
    geom_point(data = matfact2, aes(x = ranks, y = MAPECNB),color = "red") +
    geom_point(data = matfact4, aes(x = ranks, y = MAPENCNB), color = "blue") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
    xlab("r - Rank of the matrix") +
    ylab("MAPE error prediction values")
  
  ggplot() +
    ggtitle("PGEC Values for matrix factorization with and without covariates") +
    geom_point(data = matfact6, aes(x = ranks, y = PGECCNB),color = "red") +
    geom_point(data = matfact8, aes(x = ranks, y = PGECNCNB), color = "blue") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
    xlab("r - Rank of the matrix") +
    ylab("PGEC - probability of guessing correctly")
  
}

nmf_bestPGEC <- function(meltedData,knownVotes){
  set.seed(9999)
  uniqueuser <- unique(meltedData$s)
  usercov <- getUserCovs(meltedData)
  Ids <- sample(1:nrow(knownVotes), 1000) #picked a sample of 1000 amongst all of the data points in knownVotes
  train <- knownVotes[-Ids,]
  test <- knownVotes[Ids,]
  train$y <- as.numeric(train$y)
  test$y <- as.numeric(test$y)
  train$party <- as.numeric(train$party)
  test$party <- as.numeric(test$party)
  ranks <- c(1,10,30,50,100,200)
  MAPECB <- NULL
  MAPENCB <- NULL
  MAPECNB <- NULL
  MAPENCNB <- NULL
  PGECCB <- NULL
  PGECCNB <- NULL
  PGECNCB <- NULL
  PGECNCNB <- NULL
  pred1 <- NULL
  pred2 <- NULL
  pred3 <- NULL
  pred4 <- NULL
  total_prediction <- 0
  p1 <- NULL
  p2 <- NULL
  p3 <- NULL
  p4 <- NULL
  for (rank in ranks){
    trCB <- trainReco(train,rank,nmf = TRUE,biasAdjust = TRUE)
    trCNB <- trainReco(train,rank,nmf = TRUE,biasAdjust = FALSE)
    trNCB <- trainReco(train[,-4],rank,nmf = TRUE, biasAdjust = TRUE)
    trNCNB <- trainReco(train[,-4],rank,nmf = TRUE,biasAdjust = FALSE)
    p1 <- predict(trCB,test[,-3])
    p2 <- predict(trCNB,test[,-3])
    p3 <- predict(trNCB,test[,1:2])
    p4 <- predict(trNCNB,test[,1:2])
    pred1 <- abs(p1 - test[,3])
    pred2 <- abs(p2 - test[,3])
    pred3 <- abs(p3 - test[,3])
    pred4 <- abs(p4 - test[,3])
    MAPECB <- c(MAPECB,mean(pred1,na.rm = TRUE))
    MAPECNB <- c(MAPECNB,mean(pred2,na.rm = TRUE))
    MAPENCB <- c(MAPENCB,mean(pred3,na.rm = TRUE))
    MAPENCNB <- c(MAPENCNB,mean(pred4,na.rm = TRUE))
    total_prediction <- length(p1)
    covB <- 0
    covNB <- 0
    ncovB <- 0
    ncovNB <- 0
    for (i in 1:length(p1)){
      expVal <- test[i,3]
      if (round(p1[i]) == expVal){
        covB <- covB + 1
      }
      if (round(p2[i]) == expVal){
        covNB <- covNB + 1
      }
      if (round(p3[i]) == expVal){
        ncovB <- ncovB + 1
      }
      if (round(p4[i]) == expVal){
        ncovNB <- ncovNB + 1
      }
    }
    
    PGECCB <- c(PGECCB,covB/total_prediction)
    PGECCNB <- c(PGECCNB,covNB / total_prediction)
    PGECNCB <- c(PGECNCB,ncovB / total_prediction)
    PGECNCNB <- c(PGECNCNB,ncovNB / total_prediction)
  }
  
  print("MAPE VALUES")
  print(MAPECB)
  print(MAPECNB)
  print(MAPENCB)
  print(MAPENCNB)
  print("PGEC VALUES")
  print(PGECCB)
  print(PGECCNB)
  print(PGECNCB)
  print(PGECNCNB)
  matfact1 <- data.frame(ranks,MAPECB)
  matfact2 <- data.frame(ranks,MAPECNB)
  matfact3 <- data.frame(ranks,MAPENCB)
  matfact4 <- data.frame(ranks,MAPENCNB)
  matfact5 <- data.frame(ranks,PGECCB)
  matfact6 <- data.frame(ranks,PGECCNB)
  matfact7 <- data.frame(ranks,PGECNCB)
  matfact8 <- data.frame(ranks,PGECNCNB)
  
  bestPGEC <- max(matfact6[,2])
  bestPGECK <- matfact6[which(matfact6[,2] == bestPGEC),]
  bestPGECK
}

nmf_bestMAPE <- function(meltedData,knownVotes){
  set.seed(9999)
  uniqueuser <- unique(meltedData$s)
  usercov <- getUserCovs(meltedData)
  Ids <- sample(1:nrow(knownVotes), 1000) #picked a sample of 1000 amongst all of the data points in knownVotes
  train <- knownVotes[-Ids,]
  test <- knownVotes[Ids,]
  train$y <- as.numeric(train$y)
  test$y <- as.numeric(test$y)
  train$party <- as.numeric(train$party)
  test$party <- as.numeric(test$party)
  ranks <- c(1,10,30,50,100,200)
  MAPECB <- NULL
  MAPENCB <- NULL
  MAPECNB <- NULL
  MAPENCNB <- NULL
  PGECCB <- NULL
  PGECCNB <- NULL
  PGECNCB <- NULL
  PGECNCNB <- NULL
  pred1 <- NULL
  pred2 <- NULL
  pred3 <- NULL
  pred4 <- NULL
  total_prediction <- 0
  p1 <- NULL
  p2 <- NULL
  p3 <- NULL
  p4 <- NULL
  for (rank in ranks){
    trCB <- trainReco(train,rank,nmf = TRUE,biasAdjust = TRUE)
    trCNB <- trainReco(train,rank,nmf = TRUE,biasAdjust = FALSE)
    trNCB <- trainReco(train[,-4],rank,nmf = TRUE, biasAdjust = TRUE)
    trNCNB <- trainReco(train[,-4],rank,nmf = TRUE,biasAdjust = FALSE)
    p1 <- predict(trCB,test[,-3])
    p2 <- predict(trCNB,test[,-3])
    p3 <- predict(trNCB,test[,1:2])
    p4 <- predict(trNCNB,test[,1:2])
    pred1 <- abs(p1 - test[,3])
    pred2 <- abs(p2 - test[,3])
    pred3 <- abs(p3 - test[,3])
    pred4 <- abs(p4 - test[,3])
    MAPECB <- c(MAPECB,mean(pred1,na.rm = TRUE))
    MAPECNB <- c(MAPECNB,mean(pred2,na.rm = TRUE))
    MAPENCB <- c(MAPENCB,mean(pred3,na.rm = TRUE))
    MAPENCNB <- c(MAPENCNB,mean(pred4,na.rm = TRUE))
    total_prediction <- length(p1)
    covB <- 0
    covNB <- 0
    ncovB <- 0
    ncovNB <- 0
    for (i in 1:length(p1)){
      expVal <- test[i,3]
      if (round(p1[i]) == expVal){
        covB <- covB + 1
      }
      if (round(p2[i]) == expVal){
        covNB <- covNB + 1
      }
      if (round(p3[i]) == expVal){
        ncovB <- ncovB + 1
      }
      if (round(p4[i]) == expVal){
        ncovNB <- ncovNB + 1
      }
    }
    
    PGECCB <- c(PGECCB,covB/total_prediction)
    PGECCNB <- c(PGECCNB,covNB / total_prediction)
    PGECNCB <- c(PGECNCB,ncovB / total_prediction)
    PGECNCNB <- c(PGECNCNB,ncovNB / total_prediction)
  }
  
  print("MAPE VALUES")
  print(MAPECB)
  print(MAPECNB)
  print(MAPENCB)
  print(MAPENCNB)
  print("PGEC VALUES")
  print(PGECCB)
  print(PGECCNB)
  print(PGECNCB)
  print(PGECNCNB)
  matfact1 <- data.frame(ranks,MAPECB)
  matfact2 <- data.frame(ranks,MAPECNB)
  matfact3 <- data.frame(ranks,MAPENCB)
  matfact4 <- data.frame(ranks,MAPENCNB)
  matfact5 <- data.frame(ranks,PGECCB)
  matfact6 <- data.frame(ranks,PGECCNB)
  matfact7 <- data.frame(ranks,PGECNCB)
  matfact8 <- data.frame(ranks,PGECNCNB)
  
  bestMAPE <- min(matfact2[,2])
  bestMAPEK <- matfact2[which(matfact2[,2] == bestMAPE),]
  bestMAPEK
}

fullLinearRegressionMAPE <- function(meltedData, knownVotes){
  set.seed(9999)
  Ids <- sample(1:nrow(knownVotes), 1000) #picked a sample for half the size of the data points in knownVotes
  train <- knownVotes[-Ids,]
  train$s <- as.numeric(train$s)
  train$d <- as.numeric(train$d)
  train$y <- as.numeric(train$y)
  train$party <- as.numeric(train$party)
  test <- knownVotes[Ids,]
  test$s <- as.numeric(test$s)
  test$d <- as.numeric(test$d)
  test$y <- as.numeric(test$y)
  test$party <- as.numeric(test$party)
  
  udconv <- RStoReg(train)
  udtest <- RStoReg(test)
  udconvout1 <- lm(y ~., udconv)
  preds.conv1 <- predict(udconvout1, udtest)
  mean ( abs ( preds.conv1 - test [ ,3]) , na.rm = T )
}

fullLinearRegressionPGEC <- function(meltedData, knownVotes){
  set.seed(9999)
  Ids <- sample(1:nrow(knownVotes), 1000) #picked a sample for half the size of the data points in knownVotes
  train <- knownVotes[-Ids,]
  train$s <- as.numeric(train$s)
  train$d <- as.numeric(train$d)
  train$y <- as.numeric(train$y)
  train$party <- as.numeric(train$party)
  test <- knownVotes[Ids,]
  test$s <- as.numeric(test$s)
  test$d <- as.numeric(test$d)
  test$y <- as.numeric(test$y)
  test$party <- as.numeric(test$party)
  
  udconv <- RStoReg(train)
  udtest <- RStoReg(test)
  udconvout1 <- lm(y ~., udconv)
  preds.conv1 <- predict(udconvout1, udtest)
  
  numCorrect <- 0
  for (i in 1:length(preds.conv1)) {
    if (round(preds.conv1[i]) == test[i,]$y) {
      numCorrect <- numCorrect + 1
    }
  }
  
  pgec <- numCorrect / length(preds.conv1)
  pgec
}

fullLinearRegressionPredictUnknowns <- function(meltedData) {
  knownVotes <- getKnownVotes(meltedData)
  unknownVotes <- getUnknownVotes(meltedData)
  test <- knownVotes[knownVotes$s %in% unknownVotes$s,]
  newtest <- unknownVotes
  newtest$y <- as.numeric(newtest$y)
  newtest$s <- as.numeric(newtest$s)
  newtest$d <- as.numeric(newtest$d)
  newtest$party <- as.numeric(newtest$party)
  Ids <- sample(1:nrow(knownVotes), 1000) #picked a sample for half the size of the data points in knownVotes
  train <- knownVotes[-Ids,]
  train$s <- as.numeric(train$s)
  train$d <- as.numeric(train$d)
  train$y <- as.numeric(train$y)
  train$party <- as.numeric(train$party)
  
  udconv <- RStoReg(train)
  udconvout1 <- lm(y ~., train)
  preds.conv1 <- predict(udconvout1, newtest)
  round(preds.conv1)
}

methodOfMomentsMAPE <- function(meltedData, knownVotes) {
  set.seed(9999)
  Ids <- sample(1:nrow(knownVotes), 1000) #picked a sample for half the size of the data points in knownVotes
  train <- knownVotes[-Ids,]
  train$s <- as.numeric(train$s)
  train$d <- as.numeric(train$d)
  train$y <- as.numeric(train$y)
  train$party <- as.numeric(train$party)
  test <- knownVotes[Ids,]
  test$s <- as.numeric(test$s)
  test$d <- as.numeric(test$d)
  test$y <- as.numeric(test$y)
  test$party <- as.numeric(test$party)
  
  mmout <- trainMM(train)
  preds.mm <- predict (mmout , test [ , -3])
  mean ( abs ( preds.mm - test [ ,3]) , na.rm = T )
  
}

methodOfMomentsPGEC <- function(meltedData, knownVotes) {
  set.seed(9999)
  Ids <- sample(1:nrow(knownVotes), 1000) #picked a sample for half the size of the data points in knownVotes
  train <- knownVotes[-Ids,]
  train$s <- as.numeric(train$s)
  train$d <- as.numeric(train$d)
  train$y <- as.numeric(train$y)
  train$party <- as.numeric(train$party)
  test <- knownVotes[Ids,]
  test$s <- as.numeric(test$s)
  test$d <- as.numeric(test$d)
  test$y <- as.numeric(test$y)
  test$party <- as.numeric(test$party)
  
  mmout <- trainMM(train)
  preds.mm <- predict (mmout , test [ , -3])
  
  numCorrect <- 0
  for (i in 1:length(preds.mm)) {
    if (round(preds.mm[i]) == test[i,]$y) {
      numCorrect <- numCorrect + 1
    }
  }
  
  pgec <- numCorrect / length(preds.mm)
  pgec
}

nmfpredictunknowns <- function(meltedData){
  set.seed(9999)
  usercov <- getUserCovs(meltedData)
  Ids <- sample(1:nrow(knownVotes), 1000) #picked a sample of 1000 amongst all of the data points in knownVotes
  train <- knownVotes[-Ids,]
  test <- getUnknownVotes(meltedData)
  #test[test == "?"] <- NA
  train$y <- as.numeric(train$y)
  test$y <- as.numeric(test$y)
  train$party <- as.numeric(train$party)
  test$party <- as.numeric(test$party)
  
  trCNB <- trainReco(train,200,nmf = TRUE,biasAdjust = FALSE)
  p2 <- predict(trCNB,test[,1:2])
  
  round(p2)  
}

MMpredictUnknowns <-function(meltedData) {
  knownVotes <- getKnownVotes(meltedData)
  unknownVotes <- getUnknownVotes(meltedData)
  test <- knownVotes[knownVotes$s %in% unknownVotes$s,]
  newtest <- rbind(test, unknownVotes)
  newtest <- unknownVotes
  newtest$y <- as.numeric(newtest$y)
  newtest$s <- as.numeric(newtest$s)
  newtest$d <- as.numeric(newtest$d)
  newtest$party <- as.numeric(newtest$party)
  Ids <- sample(1:nrow(knownVotes), 1000) #picked a sample for half the size of the data points in knownVotes
  train <- knownVotes[-Ids,]
  train$s <- as.numeric(train$s)
  train$d <- as.numeric(train$d)
  train$y <- as.numeric(train$y)
  train$party <- as.numeric(train$party)
  
  mmout <- trainMM(train)
  preds.mm <- predict (mmout , newtest)
  preds.mm <- round(preds.mm)
}

meltedData <- getMeltedData()
knownVotes <- getKnownVotes(meltedData)
votesToPredict <- getMissingData(meltedData)
knnBestKAnalysis(meltedData, knownVotes)
#nmf_analysis(meltedData,knownVotes)
knnBestMAPE <- knn_bestMAPE(meltedData, knownVotes)
knnBestMAPE
knnBestPGEC <- knn_bestPGEC(meltedData, knownVotes)
knnBestPGEC
nmfBestMAPE <- nmf_bestMAPE(meltedData,knownVotes)
nmfBestPGEC <- nmf_bestPGEC(meltedData,knownVotes)
LRMAPE <- fullLinearRegressionMAPE(meltedData, knownVotes)
LRPGEC <- fullLinearRegressionPGEC(meltedData, knownVotes)
MMMAPE <- methodOfMomentsMAPE(meltedData, knownVotes)
MMPGEC <- methodOfMomentsPGEC(meltedData,knownVotes)

#predict the unknown values with each of the methods
predictionsknn <- knnPredictUnknownVotes(meltedData)

predictionslm <- fullLinearRegressionPredictUnknowns(meltedData)

predictionsnmf <- nmfpredictunknowns(meltedData)

predictionsMM <- MMpredictUnknowns(meltedData)

knnandnmfcompare <- predictionsknn == predictionsnmf
knnandmmcompare <- predictionsknn == predictionsMM
knnandlmcompare <- predictionslm == predictionsknn

nmfandmmcompare <- predictionsnmf == predictionsMM
nmfandlmcompare <- predictionsnmf == predictionslm

mmandlmcompare <- predictionsMM == predictionslm

#compare the results for each of the methods, count how many predicted values are the same
knnandnmf <- sum(knnandnmfcompare, na.rm = TRUE)
knnandmm <- sum(knnandmmcompare, na.rm = TRUE)
nmfandmm <- sum(nmfandmmcompare, na.rm = TRUE)
knnandlm <- sum(knnandlmcompare, na.rm = TRUE)
nmfandlm <- sum(nmfandlmcompare, na.rm = TRUE)
mmandlm <- sum(mmandlmcompare, na.rm = TRUE)

newMeltedData <- meltedData
newMeltedData[newMeltedData$y == "?",]$y <- predictionslm









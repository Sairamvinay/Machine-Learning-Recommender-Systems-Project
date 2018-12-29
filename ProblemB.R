library(rectools)
library(syuzhet)
library(plyr)
library(deepnet)
library(neuralnet)
library(ggplot2)
require(data.table)

linearRegressionMAPE <- function (trainData, testData, onlySentiment) {
  set.seed(9999)
  trainData$usefulCount <- as.numeric(trainData$usefulCount)
  testData$usefulCount <- as.numeric(testData$usefulCount)
  if (onlySentiment) {
    lmout1 <- lm(rating ~ rev_score, data = trainData) 
  } else {
    lmout1 <- lm(rating ~ drugName + condition + rev_score + usefulCount, data = trainData) 
  }
  
  p1 <- predict(lmout1,testData)
  x1 <- abs(p1 - testData[,5])
  mape1 <- mean(x1,na.rm = TRUE)
  mape1
}

deepnetMAPE <- function(trainData, testData, onlySentiment) {
  set.seed(9999)
  trainData$usefulCount <- as.numeric(trainData$usefulCount)
  testData$usefulCount <- as.numeric(testData$usefulCount)
  if (onlySentiment) {
    #udconv <- RStoReg(trainData[,c(5,8)])
    #udconvtest <- RStoReg(testData[,c(5,8)])
    udconv <- trainData[,c(5,8)]
    udconvtest <- testData[,c(5,8)]
  } else {
    udconv <- RStoReg(trainData[,c(-4, -6)])
    udconvtest <- RStoReg(testData[,c(-4,-6)]) 
  }
  udconvm <- as.matrix(udconv)
  udconvtestm <- as.matrix(udconvtest)
  
  if (onlySentiment) {
    dnout <- nn.train(as.matrix(udconvm[,-1]),udconvm[,1],hidden=c(5,2),output='linear')
    preds.dn <- nn.predict(dnout,udconvtestm[,-1])
    MAPE <- mean(abs(preds.dn - udconvtestm[,1]))  
  } else {
    dnout <- nn.train(udconvm[,-3],udconvm[,3],hidden=c(5,2),output='linear')
    preds.dn <- nn.predict(dnout,udconvtestm[,-3])
    MAPE <- mean(abs(preds.dn - udconvtestm[,3])) 
  }
  
  MAPE
}

useFulCountPlay <- function(trainData,testData){
  
  Middle <- mean(testData$usefulCount,na.rm=TRUE)
  limit <- c(5,10,20,40,50,75,100,200,300,400,500,600,Middle)
  dnsMAPES <- NULL
  dncMAPES <- NULL
  lrcMAPES <- NULL
  lrsMAPES <- NULL
  dnsMAPE <- 0
  dncMAPE <- 0
  lrcMAPE <- 0
  lrsMAPE <- 0
  for (l in limit){
    usefulIDs <- which(testData$usefulCount >= l)
    testUseful <- testData[usefulIDs,]
    lrcMAPE <- linearRegressionMAPE(trainData,testUseful,FALSE)
    lrsMAPE <- linearRegressionMAPE(trainData,testUseful,TRUE)
    dnsMAPE <- deepnetMAPE(trainData,testUseful,TRUE)
    dncMAPE <- deepnetMAPE(trainData,testUseful,FALSE)
    dnsMAPES <- c(dnsMAPES,dnsMAPE)
    dncMAPES <- c(dncMAPES,dncMAPE)
    lrcMAPES <- c(lrcMAPES,lrcMAPE)
    lrsMAPES <- c(lrsMAPES,lrsMAPE)
  }
  
  
  return(list(limits = limit, LRC = lrcMAPES, LRS = lrsMAPES, DNS = dnsMAPES, DNC = dncMAPES))
}

set.seed(9999)
trainData <- as.data.frame(fread("drugsComTrain_raw.tsv"))
testData <- as.data.frame(fread("drugsComTest_raw.tsv"))
trainData$drugName <- as.numeric(as.factor(trainData$drugName))
trainData$condition <- as.numeric(as.factor(trainData$condition))
trainData$rev_score <- get_sentiment(trainData$review)
testData$drugName <- as.numeric(as.factor(testData$drugName))
testData$condition <- as.numeric(as.factor(testData$condition))
testData$rev_score <- get_sentiment(testData$review)
lrMAPEOnlySentiment <- linearRegressionMAPE(trainData, testData, TRUE)
dnMAPEOnlySentiment <- deepnetMAPE(trainData, testData, TRUE)
lrMAPEWithCovs <- linearRegressionMAPE(trainData, testData, FALSE)
dnMAPEWithCovs <- deepnetMAPE(trainData, testData, FALSE)

#the manipulation on the usefulCount
UsefulPlay <- useFulCountPlay(trainData,testData)
limits <- UsefulPlay$limits
lrcMAPES <- UsefulPlay$LRC
lrsMAPES <- UsefulPlay$LRS
dncMAPES <- UsefulPlay$DNC
dnsMAPES <- UsefulPlay$DNS
LRC <- data.frame(limits,lrcMAPES)
DNS <- data.frame(limits,dnsMAPES)
LRS <- data.frame(limits,lrsMAPES)
DNC <- data.frame(limits,dncMAPES)
ggplot() +
  ggtitle("MAPE Values for linear model with only rev_score") +
  #geom_point(data = LRC, aes(x = limits, y = lrcMAPES),color = "red") +
  #geom_point(data = DNS, aes(x = limits, y = dnsMAPES), color = "blue") +
  #geom_point(data = DNC, aes(x = limits, y = dncMAPES), color = "red") +
  geom_point(data = LRS, aes(x = limits, y = lrsMAPES), color = "blue") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  xlab("Limits of length of UsefulCounts") +
  ylab("MAPE error prediction values")

M1 <- mean(lrcMAPES,na.rm = TRUE)
M2 <- mean(lrsMAPES,na.rm = TRUE)
M3 <- mean(dncMAPES,na.rm = TRUE)
M4 <- mean(dnsMAPES,na.rm = TRUE)
M1
M2
M3
M4







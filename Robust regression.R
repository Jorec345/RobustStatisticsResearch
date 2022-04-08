library(robustbase)
library(dplyr)

##consuming in the safaricom data
safaricomdata <- read.csv("safaricom_prices.csv", header = TRUE)
str(safaricomdata)
colnames(safaricomdata)
#Data Processing
##selecting data from 2019
trainData <- 
  safaricomdata %>%
  mutate(NewDate = as.Date(MarketDate,format = "%d/%m/%Y"))%>%
  filter(NewDate >= "2015-01-01",NewDate <= "2019-12-31")%>%
  dplyr::select(NewDate,MarketPrice)

###selecting data from 2020
testData <- 
  safaricomdata %>%
  mutate(NewDate = as.Date(MarketDate,format = "%d/%m/%Y"))%>%
  filter(NewDate >= "2020-01-01")%>%
  dplyr::select(NewDate,MarketPrice)

trimmedMean <- function(prices,percentBound=0.2){
  n = length(prices)
  sortedPrices = sort(prices)
  nToBeRemoved = percentBound * n 
  rmfstN = sortedPrices[ -c(1:nToBeRemoved)]
  rmlstN = rmfstN[1:(length(rmfstN) - nToBeRemoved)]
  return(sum(rmlstN)/length(rmlstN))
}

winsoreziedMean <- function(prices,percentBound=0.2){
  n = length(prices)
  sortedPrices = sort(prices)
  nToBeRemoved = percentBound * n 
  rmfstN = sortedPrices[ -c(1:nToBeRemoved)]
  rmlstN = rmfstN[1:(length(rmfstN) - nToBeRemoved)]
  maxVal = max(rmlstN)
  minValue = min(rmlstN)
  newFstSeq = rep(maxVal,nToBeRemoved)
  newLStVals = rep(minValue,nToBeRemoved)
  newSeq = c(newFstSeq,rmlstN)
  newSeqWthTail = c(newSeq,newLStVals)
  return(sum(newSeqWthTail)/length(newSeqWthTail))
}

percentremove <- 0.2
##applying the robust estimators
trimmedMean(trainData$MarketPrice,percentremove)
winsoreziedMean(trainData$MarketPrice,percentremove)
mean(trainData$MarketPrice)

#robust estimators
meansFunc <- function(input,percentremove = 0.2, type = 3){
  if (type==1){
trimmedMean(input,percentremove)
    }else if(type==2){
winsoreziedMean(input,percentremove)
    } else{
mean(input)
    }
}


##write a function that calculates returns
##moving avaerage

rollingmeans <- function(arr,n=20,percentremove = 0.2,type =5){

  res = arr
  for(i in n:length(arr)){
    if (type==1){
      res[i] = trimmedMean(arr[(i-n):i])
    }else if(type==2){
      res[i] = winsoreziedMean(arr[(i-n):i],percentremove)
    } else{
      res[i] = mean(arr[(i-n):i])
    }
  }
 return(res)
}

rollingmeans(trainData$MarketPrice,1)

###getting returns for safaricom

returnFunction <- function(inputArr){
  returns = (inputArr[2:length(inputArr)]/inputArr[1:length(inputArr)-1]) - 1.0
  return(c(0.0,returns))
}

trainData$returns <- returnFunction(trainData$MarketPrice)
normalizedData <-
  trainData %>%
  mutate(trimmedRets = rollingmeans(returns,1),
        WinsorizedRets = rollingmeans(returns,2),
         meanRets = rollingmeans(returns,3))

#####Chgecking for nomrality assumption 
library(ggplot2)


ggplot(trainData,aes(x = returns)) + geom_histogram()

meanDfs <- 
  trainData %>% 
  mutate(trimmedReturns = rollingmeans(returns,1),
         winsorisedReturns = rollingmeans(returns,2),
         meanReturns = rollingmeans(returns,3))


ggplot(meanDfs,aes(x = trimmedReturns)) + geom_histogram()
ggplot(meanDfs,aes(x = winsorisedReturns)) + geom_histogram()
ggplot(meanDfs,aes(x = meanReturns)) + geom_histogram()


# ggplot(meanDfs, aes(x=NewDate)) +
#   geom_line(aes(y = trimmedReturns), color = "darkred") +
#   geom_line(aes(y = winsorisedReturns), color = "darkblue") +
#   geom_line(aes(y = meanReturns), color="steelblue", linetype="twodash")
# 

###read on regression
##read on test for normality

##normality tests
##1. Histogram test
par(mfrow = c(1,3))
for (i in c(3,4,5)) {
 hist(normalizedData[,i]) 
}

##qq plot test
par(mfrow = c(1,3))
for (i in c(3,4,5)) {
  qqnorm(normalizedData[,i]) 
}


##kolomogorov test
for (i in c(3,4,5)) {
  print(ks.test(normalizedData[,i], 'pnorm'))
  
}

##shapiro test
shapiro.test(normalizedData[,i])

for (i in c(3,4,5)) {
  print(shapiro.test(normalizedData[,i]))
  
}




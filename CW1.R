library(readxl)

whiteWineData <- read_excel("~/Desktop/Courses/Data Mining and Machine Learning/CW1/Whitewine.xlsx")

head(whiteWineData)


scaleWineData <- function(data){
  data2 <- data
  data2$quality <- NULL
  scaledData <- scale(data2)
  return(scaledData)
} #Returns: Scaled Data

idealNumberOfClusters <- function(scaledData){
  
  clusterSuggestion <- NbClust(scaledData, min.nc = 2, max.nc = 10, method = "kmeans")
  return(clusterSuggestion)
} #Returns: clusterSuggestion

## Bar plot if in case
barplot(table(clusterSuggestion$Best.nc[1,]), ylab = "Number of Criteria", xlab = "Number of Clusters",
        main = "Number of Clusters chosen by 26 Criteria")

howManyClustersYouNeed <- function(scaledData,numberOfCluster){
  library(fpc)
  library(MASS)

  clustersWanted <- kmeans(scaledData,numberOfCluster)
  plotcluster(scaledData, clustersWanted$cluster)
  #parcoord(scaledData, clustersWanted$cluster)
  return(clustersWanted)
} #Returns: Output of kmeans

parcoord(scaledData, clustersWanted$cluster)

validation <- function(data,kmeansOutput){
  
  library(flexclust)
  confuseTable <- table(data$quality, kmeansOutput$cluster)
  confuseTable
  
  randIndex(confuseTable)
  
}

## Messy things

useTwoClusters <- kmeans(scaledWWData,2)
library(fpc)
plotcluster(scaledWWData, useTwoClusters$cluster)
library(MASS)
parcoord(scaledWWData, useTwoClusters$cluster)



################ Objective #2 #####################

library(dendextend)
library(corrplot)

testWhiteWineData <- whiteWineData
testWhiteWineData$quality <- NULL

distWhiteWine <- dist(testWhiteWineData)

singleLinkage <- hclust(distWhiteWine, method = "single")
singleGroup <- cutree(singleLinkage, k=5)
beep("coin")
  
completeLinkage <- hclust(distWhiteWine, method = "complete")
completeGroup <- cutree(completeLinkage, k=5)
beep("coin")

temp <- cutree(completeLinkage, k=5)[order.dendrogram(as.dendrogram(completeLinkage))]
table(temp, whiteWineData$quality)

averageLinkage <- hclust(distWhiteWine, method = "average")
averageGroup <- cutree(averageLinkage, k=5)
beep("coin")


plot(singleLinkage)
plot(completeLinkage)
rect.hclust(completeLinkage, k=2, border = "red")
plot(averageLinkage)

wineDendList <- dendlist(as.dendrogram(singleLinkage), as.dendrogram(completeLinkage),
                         as.dendrogram(averageLinkage))

names(wineDendList) <- c("Single Linkage", "Complete Linkage", "Average Linkage")

startTime <- Sys.time()
correlationWine <- cor.dendlist(wineDendList)
beep("mario")
endTime <- Sys.time()
codeExecuTime <- endTime - startTime

corrplot(correlationWine, "pie", "lower")


############# Objective #3 ####################

library(neuralnet)
library(NeuralNetTools)

library(readxl)
Exchange <- read_excel("~/Desktop/Courses/Data Mining and Machine Learning/CW1/Exchange.xlsx")

exchangeRate <- Exchange$`USD/EUR`

exchangeRateTrain <- exchangeRate[1:320]
exchangeRateTest <- exchangeRate[321:390]

firstDay <- 1:(length(exchangeRateTrain)-3)
secondDay <- 1:(length(exchangeRateTrain)-3)
thirdDay <- 1:(length(exchangeRateTrain)-3)
predictionDay <- 1:(length(exchangeRateTrain)-3)

counter=1
for(i in 1:(length(exchangeRateTrain)-3)){
  
  firstDay[counter]<- exchangeRateTrain[i]
  secondDay[counter] <- exchangeRateTrain[i+1]
  thirdDay[counter] <- exchangeRateTrain[i+2]
  predictionDay[counter] <- exchangeRateTrain[i+3]
  
  counter = counter +1
}

exchangeTrainingData <- as.data.frame(cbind(firstDay,secondDay,thirdDay,predictionDay))
colnames(exchangeTrainingData) <- c("FirstInput", "SecondInput", "ThirdInput", "Output")

exchangeNeuralNet <- neuralnet(Output~FirstInput + SecondInput + ThirdInput, exchangeTrainingData, hidden = 2)

trainOutput <- cbind(as.data.frame(predictionDay), as.data.frame(exchangeNeuralNet$net.result), id= 1:length(predictionDay))
colnames(trainOutput) <- c("Expected Output", "Predicted Output", "ID")
print(trainOutput)

meltedOutput <- melt(cleanOutput, id = 'ID')
plot(meltedOutput)

ggplot(data=meltedOutput, aes(x=ID, y=value, color=variable)) +
  geom_line()

plot(cleanOutput)

firstDayPred <- 1:(length(exchangeRateTest)-4)
secondDayPred <- 1:(length(exchangeRateTest)-4)
thirdDayPred <- 1:(length(exchangeRateTest)-4)

counter= 1
for(i in 1:(length(exchangeRateTest)-4)){
  
  firstDayPred[counter]<- exchangeRateTest[i]
  secondDayPred[counter] <- exchangeRateTest[i+1]
  thirdDayPred[counter] <- exchangeRateTest[i+2]
  
  counter = counter +1
}

exchangeTestData <- as.data.frame(cbind(firstDayPred,secondDayPred,thirdDayPred))
colnames(exchangeTestData) <- c("FirstInput", "SecondInput", "ThirdInput")

exchangeResults <- compute(exchangeNeuralNet, as.data.frame(exchangeTestData))

cleanOutput <- cbind(as.data.frame(exchangeRateTest[3:68]), as.data.frame(exchangeResults$net.result), id= 1:length(firstDayPred))
colnames(cleanOutput) <- c("Expected Output", "Predicted Output", "ID")
print(cleanOutput)

cor(exchangeRateTest[3:68], exchangeResults$net.result)

plot(cleanOutput)
meltedOutput <- melt(cleanOutput, id = 'ID')


ggplot(data=meltedOutput, aes(x=ID, y=value, color=variable)) +
  geom_line()

trainOutput$`Predicted Output` <- NA
cumulativeOutput = rbind(trainOutput,cleanOutput)
cumulativeOutput = cbind(cumulativeOutput, cumID = 1:383)

cumulativeOutput$ID <- NULL
meltedCumulativeOutput <- melt(cumulativeOutput, id = 'cumID')

ggplot(data=meltedCumulativeOutput, aes(x=cumID, y=value, color=variable)) +
  geom_line()


### For 6 days

sixDsFirstDay <- 1:(length(exchangeRateTrain)-6)
sixDsSecondDay <- 1:(length(exchangeRateTrain)-6)
sixDsThirdDay <- 1:(length(exchangeRateTrain)-6)
sixDsFourthDay <- 1:(length(exchangeRateTrain)-6)
sixDsFifthDay <- 1:(length(exchangeRateTrain)-6)
sixDsSixthDay <- 1:(length(exchangeRateTrain)-6)
sixDsPredictionDay <- 1:(length(exchangeRateTrain)-6)

counter=1
for(i in 1:(length(exchangeRateTrain)-6)){
  
  sixDsFirstDay[counter]<- exchangeRateTrain[i]
  sixDsSecondDay[counter] <- exchangeRateTrain[i+1]
  sixDsThirdDay[counter] <- exchangeRateTrain[i+2]
  sixDsFourthDay[counter]<- exchangeRateTrain[i+3]
  sixDsFifthDay[counter] <- exchangeRateTrain[i+4]
  sixDsSixthDay[counter] <- exchangeRateTrain[i+5]
  sixDsPredictionDay[counter] <- exchangeRateTrain[i+6]
  
  counter = counter +1
}

sixDsExchangeTrainingData <- as.data.frame(cbind(sixDsFirstDay,sixDsSecondDay,sixDsThirdDay,sixDsFourthDay,sixDsFifthDay,sixDsSixthDay,sixDsPredictionDay))
colnames(sixDsExchangeTrainingData) <- c("FirstInput", "SecondInput", "ThirdInput","FourthInput", "FifthInput", "SixthInput", "Output")

sixDsExchangeNeuralNet <- neuralnet(Output~FirstInput + SecondInput + ThirdInput + FourthInput + FifthInput + SixthInput, sixDsExchangeTrainingData, hidden = 2)

sixDsTrainOutput <- cbind(as.data.frame(sixDsPredictionDay), as.data.frame(sixDsExchangeNeuralNet$net.result), id= 1:length(sixDsPredictionDay))
colnames(sixDsTrainOutput) <- c("Expected Output", "Predicted Output", "ID")
print(sixDsTrainOutput)


sixDsMeltedOutput <- melt(sixDsTrainOutput, id = 'ID')
plot(sixDsMeltedOutput)

ggplot(data=sixDsMeltedOutput, aes(x=ID, y=value, color=variable)) +
  geom_line()


sixDsFirstDayPred <- 1:(length(exchangeRateTest)-6)  ## get the names right
sixDsSecondDayPred <- 1:(length(exchangeRateTest)-6)
sixDsThirdDayPred <- 1:(length(exchangeRateTest)-6)
sixDsFourthDayPred <- 1:(length(exchangeRateTest)-6)
sixDsFifthDayPred <- 1:(length(exchangeRateTest)-6)
sixDsSixthDayPred <- 1:(length(exchangeRateTest)-6)
sixDsPredictionDayPred <- 1:(length(exchangeRateTest)-6)

counter= 1
for(i in 1:(length(exchangeRateTest)-6)){
  
  sixDsFirstDayPred[counter]<- exchangeRateTest[i]
  sixDsSecondDayPred[counter] <- exchangeRateTest[i+1]
  sixDsThirdDayPred[counter] <- exchangeRateTest[i+2]
  sixDsFourthDayPred[counter]<- exchangeRateTest[i+3]
  sixDsFifthDayPred[counter] <- exchangeRateTest[i+4]
  sixDsSixthDayPred[counter] <- exchangeRateTest[i+5]
  sixDsPredictionDayPred[counter] <- exchangeRateTest[i+6]
  
  counter = counter +1
}

sixDsExchangeTestData <- as.data.frame(cbind(sixDsFirstDayPred,sixDsSecondDayPred,sixDsThirdDayPred,sixDsFourthDayPred, sixDsFifthDayPred, sixDsSixthDayPred))
colnames(sixDsExchangeTestData) <- c("FirstInput", "SecondInput", "ThirdInput", "FourthInput","FifthInput","SixthInput")

sixDsExchangeResults <- compute(sixDsExchangeNeuralNet, as.data.frame(sixDsExchangeTestData))
beep("coin")

sixDsCleanOutput <- cbind(as.data.frame(exchangeRateTest[2:65]), as.data.frame(sixDsExchangeResults$net.result), id= 1:length(sixDsFirstDayPred))
colnames(sixDsCleanOutput) <- c("Expected Output", "Predicted Output", "ID")
print(sixDsCleanOutput)

cor(exchangeRateTest[2:65], sixDsExchangeResults$net.result)

plot(sixDsCleanOutput)
sixDsMeltedOutput <- melt(sixDsCleanOutput, id = 'ID')


ggplot(data=sixDsMeltedOutput, aes(x=ID, y=value, color=variable)) +
  geom_line()

sixDsTrainOutput$`Predicted Output` <- NA
sixDsCumulativeOutput = rbind(sixDsTrainOutput,sixDsCleanOutput)
sixDsCumulativeOutput = cbind(sixDsCumulativeOutput, cumID = 1:383)

sixDsCumulativeOutput$ID <- NULL
sixDsMeltedCumulativeOutput <- melt(sixDsCumulativeOutput, id = 'cumID')

ggplot(data=sixDsMeltedCumulativeOutput, aes(x=cumID, y=value, color=variable)) +
  geom_line()


################# Objective #4  #############################
library(e1071)

exchangeTrainingData

svmExTrain <- subset(exchangeTrainingData, select = -Output)
#nrow(svmExTrain) = 317

expectedOutput <- exchangeTrainingData$Output
#length(expectedOutput) = 317

exchangeSvmModel <- svm(svmExTrain,expectedOutput)
beep("coin")

#nrow(exchangeTestData) = 66 
predictedOutput <- predict(exchangeSvmModel, exchangeTestData)
beep("coin")

cleanOutputSvm <- cbind(
  as.data.frame(exchangeRateTest[3:68]), 
  as.data.frame(predictedOutput), 
  id= 1:length(firstDayPred))
colnames(cleanOutputSvm) <- c("Expected Output", "Predicted Output", "ID")
print(cleanOutputSvm)

errorOnSvm <- sqrt(mean((signif(predictedOutput, digits = 5) - exchangeRateTest[3:68])^2))
### Error calculation

plot(cleanOutputSvm)

meltedOutputSvm <- melt(cleanOutputSvm, id = 'ID')

ggplot(data=meltedOutputSvm, aes(x=ID, y=value, color=variable)) +
  geom_line()

# Tune the svm

svmTune <- tune(svm, train.x = svmExTrain, train.y = expectedOutput, kernel= "radial",
                ranges = list(cost= 10^(-1:2), gamma= seq(0,1, by= 0.01)))
beep("coin")
print(svmTune)

exchangeSvmModelTuned <- svm(svmExTrain,expectedOutput, gamma = 0.01, cost = 10)
beep("coin")

predictedOutputTuned <- predict(exchangeSvmModelTuned, exchangeTestData)
beep("coin")

cleanOutputSvmTuned <- cbind(as.data.frame(exchangeRateTest[3:68]), as.data.frame(predictedOutputTuned), id= 1:length(firstDayPred))
colnames(cleanOutputSvmTuned) <- c("Expected Output", "Predicted Output", "ID")
print(cleanOutputSvmTuned)

## Error for tuned data
errorOnSvmTuned <- sqrt(mean((signif(predictedOutputTuned, digits = 5) - exchangeRateTest[3:68])^2))

meltedOutputSvmTuned <- melt(cleanOutputSvmTuned, id = 'ID')

ggplot(data=meltedOutputSvmTuned, aes(x=ID, y=value, color=variable)) +
  geom_line()

## For 6 days (inputs)

sixDsSvmExTrain <- subset(sixDsExchangeTrainingData, select = -Output)
sixDsExpectedOutput <- sixDsExchangeTrainingData$Output

sixDsExchangeSvmModel <- svm(x= sixDsSvmExTrain,y= sixDsExpectedOutput)
beep("coin")

sixDsPredictedOutput <- predict(sixDsExchangeSvmModel, sixDsExchangeTestData)
beep("coin")

plot(sixDsPredictedOutput)

sixDsCleanOutputSvm <- cbind(
  as.data.frame(exchangeRateTest[6:69]), 
  as.data.frame(sixDsPredictedOutput), 
  id= 1:length(sixDsFirstDayPred))

colnames(sixDsCleanOutputSvm) <- c("Expected Output", "Predicted Output", "ID")
print(sixDsCleanOutputSvm)

plot(sixDsCleanOutputSvm)

errorOnSvmSixD <- sqrt(mean((signif(sixDsPredictedOutput, digits = 5) - exchangeRateTest[6:69])^2))

sixDsMeltedOutputSvm <- melt(sixDsCleanOutputSvm, id = 'ID')

ggplot(data=sixDsMeltedOutputSvm, aes(x=ID, y=value, color=variable)) +
  geom_line()

# Tune the SVM

sixDsSvmTune <- tune(svm, train.x = sixDsSvmExTrain, train.y = sixDsExpectedOutput, kernel= "radial",
                ranges = list(cost= 10^(-1:2), gamma= seq(0,1, by= 0.01)))
beep("coin")
print(sixDsSvmTune)

sixDsExchangeSvmModelTuned <- svm(sixDsSvmExTrain,sixDsExpectedOutput, gamma = 0.01, cost = 10)
beep("coin")

sixDsPredictedOutputTuned <- predict(sixDsExchangeSvmModelTuned, sixDsExchangeTestData)
beep("coin")

sixDsCleanOutputSvmTuned <- cbind(as.data.frame(exchangeRateTest[6:69]), as.data.frame(sixDsPredictedOutputTuned), id= 1:(length(sixDsFirstDayPred)))
colnames(sixDsCleanOutputSvmTuned) <- c("Expected Output", "Predicted Output", "ID")
print(sixDsCleanOutputSvmTuned)

errorOnSvmSixDTuned <- sqrt(mean((signif(sixDsPredictedOutputTuned, digits = 5) - exchangeRateTest[6:69])^2))

sixDsMeltedOutputSvmTuned <- melt(sixDsCleanOutputSvmTuned, id = 'ID')

ggplot(data=sixDsMeltedOutputSvmTuned, aes(x=ID, y=value, color=variable)) +
  geom_line()

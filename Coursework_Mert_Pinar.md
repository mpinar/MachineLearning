# Coursework #1

This coursework contains four key components, which are;

  - Partitioning clusters with White Wine Data
  - Hierarchical clustering with White Wine Data
  - Forecasting using MLPs (Neural Nets)
  - Forecasting using SVRs

### Clustering Part
For this part, two different techniques on clustering are considered which are partitioning and agglomerative. A set of observations on the number of white wine varieties involving their chemical properties and ranking by tasters are considered. However, the taste is a subjective property which varies from person to person, the program tests whether the machine can define a specific taste cluster for a wine by considering its chemical compounds. All chemical variables are contionuos and the quality factor is desined within 1 (one) to 10 (ten). 

Chemical variables are stated below as:
>Fixed Acidity
>Volatile Acidity
>Citric Acid
>Residual sugar
>Chlorides
>Free Sulfur Dioxide
>Density
>pH
>Sulphates
>Alcohol
>Output Variable

The first eleven attributes are used to form clusters, and output variable used to examine how well the computer made the comparison. The following step will be testing the reliability on the computer's taste buds on the concept of wine.

#####  Objective #1 (Partitioning Clustering)

The k-means clustering analysis have conducted for this part of the study. 

First, the relative libraries and the white wine dataset have imported as shown in the below.
```r
library(readxl)
library(fpc)
library(MASS)
library(flexclust)
library(beepr)
whiteWineData <- read_excel("~/Desktop/Courses/Data Mining and Machine Learning/CW1/Whitewine.xlsx")
```
Then, to get more accurate clustering profiles, the data was scaled. In this case, scaling is normalizing the data according to their representative mean. The function that scales the data and takes the subset of the white wine data according to Quality column have written, then the quality column excluded from the main dataset due to comparison of how the computer generates clustering, and how accurate it is on determining the quality. The function given as the following:
```r
scaleWineData <- function(data){
  data2 <- data
  data2$quality <- NULL
  scaledData <- scale(data2)
  return(scaledData)
} #Returns: Scaled Data
```
Then `NBClust` function have used to determine the ideal number of clusters according to various clustering methods. For this purpose K-means are used, hence the function as stated in the below;
```r
idealNumberOfClusters <- function(scaledData){
  clusterSuggestion <- NbClust(scaledData, min.nc = 2, max.nc = 10, method = "kmeans")
  return(clusterSuggestion)
} #Returns: clusterSuggestion
```
After running the code, the output is given as:

>*** : The Hubert index is a graphical method of determining the number of clusters.
                In the plot of Hubert index, we seek a significant knee that corresponds to a significant increase of the value of the measure i.e the significant peak in Hubert index second differences plot. 
 
>*** : The D index is a graphical method of determining the number of clusters. 
                In the plot of D index, we seek a significant knee (the significant peak in Dindex second differences plot) that corresponds to a significant increase of the value of the measure. 
 
******************************************************************* 
> Among all indices:                                                
 10 proposed 2 as the best number of clusters 
 4 proposed 3 as the best number of clusters 
 1 proposed 4 as the best number of clusters 
 5 proposed 5 as the best number of clusters 
 2 proposed 6 as the best number of clusters 
 2 proposed 10 as the best number of clusters 

> ***** Conclusion *****                            
 
> According to the majority rule, the best number of clusters is  2 
 
*******************************************************************
![suggestion](https://github.com/mpinar/MachineLearning/blob/master/clusterSuggestion.png?raw=true)

The information is used to create a bar plot to get the best numbers of clusters as given in the below.

```r
barplot(table(clusterSuggestion$Best.nc[1,]), ylab = "Number of Criteria",
    xlab = "Number of Clusters", 
    main = "Number of Clusters chosen by 26 Criteria")
```
![bar](https://github.com/mpinar/MachineLearning/blob/master/bar%20and%20cluster.png?raw=true)


After determining the best number of clusters, the k-mean values are determined to observe how well the program did. The function is written that we can test the values and at the same time detect the changes dynamically. In this case the outliers were not eliminated because they should not considered if it is certain that the information is misinterpreted, completely wrong or misleading the other data analysis.
```r
howManyClustersYouNeed <- function(scaledData,numberOfCluster){
  clustersWanted <- kmeans(scaledData,numberOfCluster)
  plotcluster(scaledData, clustersWanted$cluster)
  return(clustersWanted)
} #Returns: Output of kmeans
```
![clusters](https://github.com/mpinar/MachineLearning/blob/master/clusterPlot.png?raw=true)
To see how parallel our output to white wine dataset's quality column, the  `parcoord` function is used from the library `MASS`, code and the output can be seen below;
```r
parcoord(scaledData, clustersWanted$cluster)
```
![parcoord](https://github.com/mpinar/MachineLearning/blob/master/parcoord.png?raw=true)

After the calculations, the reliability of the program is tested on classification. For this end, the confuse table is formed to check the consistency of the results.
```r
validation <- function(data,kmeansOutput){
  confuseTable <- table(data$quality, kmeansOutput$cluster)
  confuseTable
  randIndex(confuseTable)
}
```
Output:
> ARI 
>0.02553198 

Based on the outcome of this experiment, it is not possible to determine the quality of the wine by considering its chemical components. NBClust suggests the using of 2 clusters that's because the function _can not_ give any suggestion less than 2. If it would give a smaller value of a suggestio, it will be use of 0 clusters. As a result of the test, we can observe that there is a limited correlation between the obtained algorithm and the output of the test, those people made on taste the wine. On the other hand, it can be suggested that the accuracy will be improved if we were comparing the main grape that the wine made of, because chemistry will be effective on that concept.
#####  Objective #2 (Hierarchical Clustering)

In this part, even though the considered data set remain the same, white wine dataset, the used clustering technique to get our classification have done is changed to agglomerative clustering.

To begin with, the white wine dataset have formatted by deleting its quality column again, because this experiment contains unsupervised learning. Moreover, the distance of each element is determined by using `dist` function from the `stats` library, and then the relevant libraries are imported for hierarchical clustering.
```r
library(dendextend)
library(corrplot)

testWhiteWineData <- whiteWineData
testWhiteWineData$quality <- NULL

distWhiteWine <- dist(testWhiteWineData)
```

For this purpose, three different hierarchical clustering methods are used which are; Single Linkage, Complete Linkage and Average Linkage. Based on the suggestion of NBClust, there should be 5 clusters which the trees set on 5 clusters. For further information, `beep` function from `beepr` library for notifications is used. Since some of these functions is taking certain time for running, the beep is set for warning when the functions are done working.
```r
singleLinkage <- hclust(distWhiteWine, method = "single")
singleGroup <- cutree(singleLinkage, k=5)
beep("coin")
  
completeLinkage <- hclust(distWhiteWine, method = "complete")
completeGroup <- cutree(completeLinkage, k=5)
beep("coin")

averageLinkage <- hclust(distWhiteWine, method = "average")
averageGroup <- cutree(averageLinkage, k=5)
beep("coin")
```

Here are the plots of those 3 trees (Dendrograms);
```r
plot(singleLinkage)
plot(completeLinkage)
rect.hclust(completeLinkage, k=2, border = "red")
plot(averageLinkage)
```

![single](https://github.com/mpinar/MachineLearning/blob/master/single%20linkage%20plot.png?raw=true)
![complete](https://github.com/mpinar/MachineLearning/blob/master/complete%20linkage%20plot.png?raw=true)
![average](https://github.com/mpinar/MachineLearning/blob/master/average%20linkage%20plot.png?raw=true)


After creating the dendrograms, their cophenetic correlation should be checked which then followed by the formation of a dendrogram list from those. Then this list is used to calculate the cophenetic correlation between the dendrograms. The correlation graph is also plotted. Since as a part of the experiment, the time for the function to run and find the correlation have recorded.
```r
wineDendList <- dendlist(as.dendrogram(singleLinkage), 
            as.dendrogram(completeLinkage), as.dendrogram(averageLinkage))
names(wineDendList) <- c("Single Linkage", "Complete Linkage",
            "Average Linkage")
            
startTime <- Sys.time()
correlationWine <- cor.dendlist(wineDendList)
beep("mario")
endTime <- Sys.time()
codeExecuTime <- endTime - startTime

corrplot(correlationWine, "pie", "lower")
```
Output:
>Time difference of 45.84388 mins


| Item      |    Single Linkage | Complete Linkage  | Average Linkage |
| :-------- | --------:| :---------: | :---------:|
| Single Linkage  | 1.0000000 |  0.1959006  | 0.4296941 |
| Complete Linkage     |   0.1959006 |  1.0000000  | 0.3682917 |
| Average Linkage      |    0.4296941 | 0.3682917  | 1.0000000 |

![corr](https://github.com/mpinar/MachineLearning/blob/master/corrplot.png?raw=true)

It can be seen from these plots that these three dendrograms are not significantly correlated due to the different approaches of these three clustering techniques that applied. Single Linkage finds the smallest distance for each data point and keep them in a next-best-merge array, while the complete linkage computes the n^2 distance in metric and then sort the distances for each data point and lastly, average linkage merges clusters in each iteration with the highest cohesion.

### Forecasting Part

ın forecasting part, two different approaches on regression are used, which are MLP (Neural Net) and SVM. For this end, a dataset contains USD/EUR exchange rates that contains 390 data points are given. It is asked to use of first 320 data point to train our machnines and 70 data points to test how well it was trained. In this time series analysis, it is modified by prepared training dataset using the exact same data as in Exchange Rate dataset. The `neuralnet` library for MLP and `e1071` library for SVM are used to form a new matrix as shown below;

| First Day      |    Second Day | Third Day  | Prediction Day |
| :--------: | :--------:| :---------: | :---------:|
| i | i+1  | i+2  | i+3 |
| i+1  | i+2 | i+3 | i+4 |
| i+2 | i+3 |  i+4 | i+5 |

According to this dataset, machine will take first three columns as input and the fourth column, which is Prediction Day as supervision, since supervised machine learning is used. For experimenting purposes, another matrix that contains for six days as input is formed. R code about those are below;
```r
library(neuralnet)
library(NeuralNetTools)
library(readxl)

Exchange <- read_excel("~/Desktop/Courses/Data Mining and Machine Learning/CW1/Exchange.xlsx")

exchangeRate <- Exchange$`USD/EUR`

exchangeRateTrain <- exchangeRate[1:320] # Training Dataset
exchangeRateTest <- exchangeRate[321:390] # Testing Dataset

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
  counter = counter +1 # Increment
}

exchangeTrainingData <- as.data.frame(cbind(firstDay,secondDay,thirdDay,predictionDay))
colnames(exchangeTrainingData) <- c("FirstInput", "SecondInput", "ThirdInput", "Output")
```

For six input system;
```r
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
  counter = counter +1 # Increment
}

sixDsExchangeTrainingData <- as.data.frame(cbind(sixDsFirstDay,sixDsSecondDay,sixDsThirdDay,sixDsFourthDay,sixDsFifthDay,sixDsSixthDay,sixDsPredictionDay))
colnames(sixDsExchangeTrainingData) <- c("FirstInput", "SecondInput", "ThirdInput","FourthInput", "FifthInput", "SixthInput", "Output")
```

#### Objective #3 (Forecasting using MLP)

After the complete dataset obtained with all the prospects, the machine should be trained. In this part of the experiment, Neural Net (MLP) is used. Since there are two different datasets to train the machine, it can be observed whether more inputs are better or more inputs makes our machine to lean to generate more mistake on regression. For three inputs the formula will be `Output~FirstInput+SecondInput+ThirdInput`. 
```r
exchangeNeuralNet <- neuralnet(Output~FirstInput + SecondInput + ThirdInput, exchangeTrainingData, hidden = 2)

trainOutput <- cbind(as.data.frame(predictionDay), as.data.frame(exchangeNeuralNet$net.result), id= 1:length(predictionDay))
colnames(trainOutput) <- c("Expected Output", "Predicted Output", "ID")
```
![NN3Inputs](https://github.com/mpinar/MachineLearning/blob/master/3%20input%20neural%20net.png?raw=true)


After training another matrix is formed to test the part of the Exchange Dataset. The difference from the training matrix is the prediction column. 

| First Day      |    Second Day | Third Day  |
| :--------: | :--------:| :---------: |
| i | i+1  | i+2  |
| i+1  | i+2 | i+3 |
| i+2 | i+3 |  i+4 |

By using the dataset above the `compute` function is applied to make machine to forecast according to the related data, then form a matrix that has the predicted outputs and expected outputs. An ID column to is added visualize the data more properly. All the data on ID column are melted, so that a better graph is obtained. The `melt` function is used from `reshape2` library for this operation.

```r
exchangeResults <- compute(exchangeNeuralNet, as.data.frame(exchangeTestData))

cleanOutput <- cbind(as.data.frame(exchangeRateTest[1:66]), as.data.frame(exchangeResults$net.result), id= 1:length(firstDayPred))
colnames(cleanOutput) <- c("Expected Output", "Predicted Output", "ID")

meltedOutput <- melt(cleanOutput, id = 'ID')

ggplot(data=meltedOutput, aes(x=ID, y=value, color=variable)) +
  geom_line()
```
![threeInputs](https://github.com/mpinar/MachineLearning/blob/master/NN%203%20input%20graph.png?raw=true)

When the MAPE error test is run on the data of prediction, the calculated error value can be neglected easily, significantly small number. To visualize the trend of the prediction data with the whole graph, both values plotted on the same graph combinationally as shown below;
```r
errorOnNN <- sqrt(mean((exchangeResults$net.result - exchangeRateTest[1:66])^2))

trainOutput$`Predicted Output` <- NA
cumulativeOutput = rbind(trainOutput,cleanOutput)
cumulativeOutput = cbind(cumulativeOutput, cumID = 1:383)

cumulativeOutput$ID <- NULL
meltedCumulativeOutput <- melt(cumulativeOutput, id = 'cumID')

ggplot(data=meltedCumulativeOutput, aes(x=cumID, y=value, color=variable)) +
  geom_line()

```
Output:
>errorOnNN -> 0.003284871809
![3InputCumu](https://github.com/mpinar/MachineLearning/blob/master/NN%203%20input%20cumulative.png?raw=true)

The scaled data was tested with Neuralnet by using `caret` library for this operation. After this, the experiment with the scaled data is achieved by putting the output to MAPE error test, which it's performance with scaled data is observed as poor.
```r
library(caret)

model_nn <- train(exchangeTrainingData[,1:3], exchangeTrainingData[,4], method = 'neuralnet', 
                    preProcess = c("center", "scale"))

predictions <- predict.train(object = model_nn, exchangeTestData[,1:3], type = "raw")

errorOnNNScaled <- sqrt(mean((predictions - exchangeRateTest[1:66])^2))
```
> errorOnNNScaled -> 0.005775149777

Since the MLP's performance is tested with three inputs, it is needed to be examine one more time with different input scheme. For this purpose, six inputs are chosen for application of same test steps as the following;
```r
sixDsExchangeNeuralNet <- neuralnet(Output~FirstInput + SecondInput + ThirdInput + FourthInput + FifthInput + SixthInput, sixDsExchangeTrainingData, hidden = 2)

sixDsTrainOutput <- cbind(as.data.frame(sixDsPredictionDay), as.data.frame(sixDsExchangeNeuralNet$net.result), id= 1:length(sixDsPredictionDay))
colnames(sixDsTrainOutput) <- c("Expected Output", "Predicted Output", "ID")
```
![nn6](https://github.com/mpinar/MachineLearning/blob/master/NN%206%20input.png?raw=true)

When the `compute` function is run, the result will be a prediction which running MAPE on the result give the same outputs as previously obtained from three day input. The code that gives the results and the obtained plots are given in the below.
```r
sixDsCleanOutput <- cbind(as.data.frame(exchangeRateTest[4:67]), as.data.frame(sixDsExchangeResults$net.result), id= 1:length(sixDsFirstDayPred))
colnames(sixDsCleanOutput) <- c("Expected Output", "Predicted Output", "ID")

errorOnNNSixDs <- sqrt(mean((sixDsExchangeResults$net.result - exchangeRateTest[4:67])^2))
errorOnNNSixDs

sixDsMeltedOutput <- melt(sixDsCleanOutput, id = 'ID')

ggplot(data=sixDsMeltedOutput, aes(x=ID, y=value, color=variable)) +
  geom_line()

sixDsTrainOutput$`Predicted Output` <- NA
sixDsCumulativeOutput = rbind(sixDsTrainOutput,sixDsCleanOutput)
sixDsCumulativeOutput = cbind(sixDsCumulativeOutput, cumID = 1:378)

sixDsCumulativeOutput$ID <- NULL
sixDsMeltedCumulativeOutput <- melt(sixDsCumulativeOutput, id = 'cumID')

ggplot(data=sixDsMeltedCumulativeOutput, aes(x=cumID, y=value, color=variable)) +
  geom_line()
```
> errorOnNNSixDs -> 0.003367945112

![nn6InputsG](https://github.com/mpinar/MachineLearning/blob/master/NN%206%20inputs%20graph.png?raw=true)

![nn6InputsCumu](https://github.com/mpinar/MachineLearning/blob/master/NN%206%20Input%20cumulative.png?raw=true)

The six scaled inputs are also tested with Neural Net by applying the same strategy on the previous three input study. The obtained results are worse when compared to previous data.
```r
model_nnSixDs <- train(sixDsExchangeTrainingData[,1:6], sixDsExchangeTrainingData[,7], method = 'neuralnet', 
                  preProcess = c("center", "scale"))

predictionsSixDs <- predict.train(object = model_nnSixDs, sixDsExchangeTestData[,1:6], type = "raw")

errorOnNNScaledSixDs <- sqrt(mean((predictionsSixDs - exchangeRateTest[4:67])^2))
errorOnNNScaledSixDs
```
> errorOnNNScaledSixDs -> 0.006306423344

From this experiment it can be seen that Neural net gives almost the same answer with non-scaled three and six inputs datasets, however the scaled data always give poorer results which is shown in the formed table below.

| Error Table    |    Three Inputs | Six Inputs  |
| :--------: | :--------:| :---------: |
| Scaled | 0.005775149777  | 0.006306423344  |
| Not Scaled  | 0.003284871809 | 0.003367945112 |

#### Objective #4 (Forecasting using SVR)

In this part the SVM Model is used for forecasting problem. When the experiment is completed the results will be compared with the ones obtained from Neural Net. The exact same dataset is used which previously used for Neural Net for the safety and improvement of the experiment validity. The SVM models will be tested for three and six inputs. To begin with, the Exchange Training data is subsetted by extracting the output, which is required for that dataset to form the formula that is going to be used for the SVM Model.
```r
library(e1071)

svmExTrain <- subset(exchangeTrainingData, select = -Output)
expectedOutput <- exchangeTrainingData$Output
```
By following this, tho datasets are obtained based on the application of created formula. With that information SVM model is formed and with given test data the test has been conducted.
```r
exchangeSvmModel <- svm(svmExTrain,expectedOutput)
beep("coin")

predictedOutput <- predict(exchangeSvmModel, exchangeTestData)
beep("coin")

cleanOutputSvm <- cbind(
  as.data.frame(exchangeRateTest[3:68]), 
  as.data.frame(predictedOutput), 
  id= 1:length(firstDayPred))
colnames(cleanOutputSvm) <- c("Expected Output", "Predicted Output", "ID")
```
Output for the SVM model is like below;
> Call:
svm.default(x = svmExTrain, y = expectedOutput)

>Parameters:
   SVM-Type:  eps-regression 
 SVM-Kernel:  radial 
       cost:  1 
      gamma:  0.3333333333 
    epsilon:  0.1 

>Number of Support Vectors:  240

About the testing, the same steps to the graph is achieved, that was the addition of an ID column to melt the data on. The results are checked according to MAPE error calculation and model work in an exceptional way according to Neural Net. Moreover, the code and graphs for both prediction and cumulative are given in the below;
```r
errorOnSvm <- sqrt(mean((signif(predictedOutput, digits = 5) - exchangeRateTest[3:68])^2))
### Error calculation

plot(cleanOutputSvm)

meltedOutputSvm <- melt(cleanOutputSvm, id = 'ID')

ggplot(data=meltedOutputSvm, aes(x=ID, y=value, color=variable)) +
  geom_line()
```
> errorOnSvm -> 0.001498939019

![threeInputs](https://github.com/mpinar/MachineLearning/blob/master/SVM%203%20inputs%20graph.png?raw=true)

Following that part, the changes after tunening the SVM model are observed. The `tune` function had been used for this end. The main purpose of this part is to determine whether output will have less error or not compare to previous models. 
```r
svmTune <- tune(svm, train.x = svmExTrain, train.y = expectedOutput, kernel= "radial",
                ranges = list(cost= 10^(-1:2), gamma= seq(0,1, by= 0.01)))
beep("coin")
print(svmTune)
```
> Parameter tuning of 'svm':

> sampling method: 10-fold cross validation 
> best parameters:
  |cost| gamma|
  | 10 | 0.03 |
> best performance: 0.00006320184555 

SVM model is tuned according to the information above, and test run with the exact same procedure that applied before. MAPE error calculation has applied to the output;
```r
exchangeSvmModelTuned <- svm(svmExTrain,expectedOutput, gamma = 0.03, cost = 10)
beep("coin")

predictedOutputTuned <- predict(exchangeSvmModelTuned, exchangeTestData)
beep("coin")

cleanOutputSvmTuned <- cbind(as.data.frame(exchangeRateTest[3:68]), as.data.frame(predictedOutputTuned), id= 1:length(firstDayPred))
colnames(cleanOutputSvmTuned) <- c("Expected Output", "Predicted Output", "ID")
print(cleanOutputSvmTuned)

## Error for tuned data
errorOnSvmTuned <- sqrt(mean((signif(predictedOutputTuned, digits = 5) - exchangeRateTest[3:68])^2))
errorOnSvmTuned

meltedOutputSvmTuned <- melt(cleanOutputSvmTuned, id = 'ID')

ggplot(data=meltedOutputSvmTuned, aes(x=ID, y=value, color=variable)) +
  geom_line()
```
Error output is;
> errorOnSvmTuned -> 0.0007022690497

![threeTuned](https://github.com/mpinar/MachineLearning/blob/master/SVM%203%20inputs%20tuned%20graph.png?raw=true)

After tests with three inputs are done, tests with six inputs will be applied. Same input matrix as used in Neural net will be used and the same procedure will be followed.
```r
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
```
> svm.default(x = sixDsSvmExTrain, y = sixDsExpectedOutput)


>Parameters:
   SVM-Type:  eps-regression 
 SVM-Kernel:  radial 
       cost:  1 
      gamma:  0.1666666667 
    epsilon:  0.1 
    
>Number of Support Vectors:  240

For ploting the output and calculating the error using MAPE;
```r
errorOnSvmSixD <- sqrt(mean((signif(sixDsPredictedOutput, digits = 5) - exchangeRateTest[6:69])^2))

sixDsMeltedOutputSvm <- melt(sixDsCleanOutputSvm, id = 'ID')

ggplot(data=sixDsMeltedOutputSvm, aes(x=ID, y=value, color=variable)) +
  geom_line()
```
> errorOnSvmSixD -> 0.001467938606

![sixInputs](https://github.com/mpinar/MachineLearning/blob/master/SVM%206%20input%20graph.png?raw=true)

The changes in the  predictions after the tuning of the SVM is observed as the following;
```r
sixDsSvmTune <- tune(svm, train.x = sixDsSvmExTrain, train.y = sixDsExpectedOutput, kernel= "radial",
                ranges = list(cost= 10^(-1:2), gamma= seq(0,1, by= 0.01)))
```

>Parameter tuning of 'svm':
> sampling method: 10-fold cross validation 
> best parameters:
 |cost | gamma|
 |  10  |  0.01|

> best performance: 0.00006409881733 


After tuning the SVM according to the attributes above, the test is run all over again with the same processes as in three input tests.
```r
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
```
> errorOnSvmSixDTuned -> 0.0007430931974

![sixTuned](https://github.com/mpinar/MachineLearning/blob/master/SVM%206%20input%20tuned.png?raw=true)

After all these test, we can compare our results by checking the differences between three and six input systems and tuned systems. As we put a restriction on the errors that our model can do when we tune our SVM, we believed to have better results, lower error margins. Outcome of this experiment we can clearly say that tuning the model makes the regression more accurate. When we take one step behind to take a look at the big picture we can observe that we have got better results on SVMs than MLPs. Since we are passing more parameters to tune the SVM makes it an advanced version of a Neural Net. They both have their respective pros those are; Neural networks are extremely flexible in the types of data they can support and they do a decent job at learning the important features from any data structure. On the other side, SVMs require less grid-searching to get a reasonably accurate model. From the experiment above SVMs did a quite decent job.

| Error Table    |    Three Inputs | Six Inputs  |
| :--------: | :--------:| :---------: |
| Tuned | 0.00057260701  | 0.0007430931974  |
| Not Tuned  | 0.001498939019 | 0.001467938606 |



## References

Rousseeuw, P. J., & Hubert, M. (2011). Robust statistics for outlier detection. Wiley Interdisciplinary Reviews: Data Mining and Knowledge Discovery, 1(1), 73-79.
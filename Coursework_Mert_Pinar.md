# Coursework #1

This coursework contains four key components, those are;

  - Partitioning clusters with White Wine Data
  - Hierarchical clustering with White Wine Data
  - Forecasting using MLPs (Neural Nets)
  - Forecasting using SVRs

### Clustering Part
In clustering part we have observed two different techniques on clustering those are partitioning and agglomerative. We consider a set of observations on a number of white wine varieties involving their chemical properties and ranking by tasters. Although, it varies from person to person but we are experimenting if a machine can taste a wine by checking its chemical compounds. All chemical variables are contionuos and the quality factor is within 1 (one) to 10 (ten). 

Chemical variables are stated below:
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

We will use first eleven attributes to form clusters, and we will use Output variable to test how well our computer did the comparison. After this we will figure out if we can trust our computer's taste buds on the concept of wine.

#####  Objective #1 (Partitioning Clustering)

We have conducted the k-means clustering analysis for this part. 

To begin we have imported our relative libraries and the white wine dataset
```r
library(readxl)
library(fpc)
library(MASS)
library(flexclust)
library(beepr)
whiteWineData <- read_excel("~/Desktop/Courses/Data Mining and Machine Learning/CW1/Whitewine.xlsx")
```
After that we need to scale our data, to get more accurate clustering in our coursework. In this case what scaling do is normalize the data according to their representative mean. I have written a function that scales the data and takes gets the subset of the white wine data according to Quality column. I've got the quality column out of the main dataset because it will be my point to compare how the computer did clustering, and how accurate it is on determining the quality.
```r
scaleWineData <- function(data){
  data2 <- data
  data2$quality <- NULL
  scaledData <- scale(data2)
  return(scaledData)
} #Returns: Scaled Data
```
Then I have used the `NBClust` function to determine the ideal number of clusters according to various clustering methods. In this experiment we have used K-means, so I've written a function as below;
```r
idealNumberOfClusters <- function(scaledData){
  clusterSuggestion <- NbClust(scaledData, min.nc = 2, max.nc = 10, method = "kmeans")
  return(clusterSuggestion)
} #Returns: clusterSuggestion
```
After I ran this, I've got and output as below;

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

I have put the information in a bar plot to see the best numbers of clusters.

```r
barplot(table(clusterSuggestion$Best.nc[1,]), ylab = "Number of Criteria",
    xlab = "Number of Clusters", 
    main = "Number of Clusters chosen by 26 Criteria")
```
![bar](https://github.com/mpinar/MachineLearning/blob/master/bar%20and%20cluster.png?raw=true)


After determining the best number of clusters, it is time to take its k-mean, and see how well it did. I've put it in to a function that we can test it and see the changes dynamically. I didn't delete the outliers because it is normal to delete them if it is obvious that the outlier is due to incorrectly entered or measured data
```r
howManyClustersYouNeed <- function(scaledData,numberOfCluster){
  clustersWanted <- kmeans(scaledData,numberOfCluster)
  plotcluster(scaledData, clustersWanted$cluster)
  return(clustersWanted)
} #Returns: Output of kmeans
```
![clusters](https://github.com/mpinar/MachineLearning/blob/master/clusterPlot.png?raw=true)
To see how parallel our output to white wine dataset's quality column, I have used `parcoord` function from the library `MASS`, code and the output can be seen below;
```r
parcoord(scaledData, clustersWanted$cluster)
```
![parcoord](https://github.com/mpinar/MachineLearning/blob/master/parcoord.png?raw=true)

After the calculations, we need to check how correct the computer did the classification. For that I have formed a confuse table to check the consistency of my results.
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

Outcome of this experiment is, it is not possible to determine the quality of the wine by looking at its chemical components. NBClust suggests that using 2 clusters that's because the function _can not_ give any suggestion less than 2. If it can give us less of a suggestion it would tell us to use 0 clusters. As a result of the test we can see there is really small correlation between what our algorithm and output of the test those people made to taste the wine. On the other hand, I think that would be more accurate if we were comparing the main grape that made that wine, because chemistry will be effective on that concept.
#####  Objective #2 (Hierarchical Clustering)

In this part, we are still working on white wine dataset, but, this time we will be using agglomerative clustering technique to get our classification done.

To begin with, I have formatted the white wine dataset as deleting its quality column again, because this experiment contains unsupervised learning. I also took the distance of each element using `dist` function from the `stats` library, and I need to import the relevant libraries for hierarchical clustering.
```r
library(dendextend)
library(corrplot)

testWhiteWineData <- whiteWineData
testWhiteWineData$quality <- NULL

distWhiteWine <- dist(testWhiteWineData)
```

I have used three different hierarchical clustering methods, those are; Single Linkage, Complete Linkage, Average Linkage. As my NBClust suggest me that there should be 5 clusters I have cut the trees into 5. For further information, I have used `beep` function from `beepr` library for notifications. Since some of these functions taking some time running, I have set the beep to warn me when the functions are done working.
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


After I am done with creating the dendrograms, I need to check their cophenetic correlation for that I need to form a dendrogram list using those three that I have formed before. Then I used this list to calculate the cophenetic correlation between the dendrograms. I have also drawn the correlation graph. Since a part of the experiment, I have wanted to keep the time it take to run the function to find the correlation.
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

From these plots we can observe that these three dendrograms are not quite correlated. That's because of the approaches that three techniques have on the terms of clustering. Single Linkage finds the smallest distance for each data point and keep them in a next-best-merge array, where complete linkage computes the n^2 distance metric and then sort the distances for each data point and average linkage merges in each iteration the pair of clusters with the highest cohesion.

### Forecasting Part

In forecasting part we used two different approaches on regression. Those are MLP (Neural Net) and SVM. In this part we are given a dataset that contains USD/EUR exchange rates that contains 390 data points. We are asked to use first 320 data point to train our machnines and 70 data points to test how well it was trained. In this time series analysis I modified my training dataset using the exact same data as in Exchange Rate dataset. We are using `neuralnet` library for MLP and `e1071` library for SVM I formed a new matrix as shown below;

| First Day      |    Second Day | Third Day  | Prediction Day |
| :--------: | :--------:| :---------: | :---------:|
| i | i+1  | i+2  | i+3 |
| i+1  | i+2 | i+3 | i+4 |
| i+2 | i+3 |  i+4 | i+5 |

According to this dataset machine will take first three columns as input and fourth column which as Prediction Day as supervision, since we are using supervised machine learning. For experimenting purposes I have formed another matrix that is for six days as input. R code about those are below;
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

After we got our dataset completed, we need to train our machine. In this part of experiment we are going to use Neural Net (MLP). Since we have two different datasets to train our machine we will observe that if more inputs are better or more inputs makes our machine to lean to make more mistake on regression. For three inputs our formula will be `Output~FirstInput+SecondInput+ThirdInput`. 
```r
exchangeNeuralNet <- neuralnet(Output~FirstInput + SecondInput + ThirdInput, exchangeTrainingData, hidden = 2)

trainOutput <- cbind(as.data.frame(predictionDay), as.data.frame(exchangeNeuralNet$net.result), id= 1:length(predictionDay))
colnames(trainOutput) <- c("Expected Output", "Predicted Output", "ID")
```
![NN3Inputs](https://github.com/mpinar/MachineLearning/blob/master/3%20input%20neural%20net.png?raw=true)


After training I have formed another matrix from the test part of the Exchange Dataset. What differs from the training matrix is the prediction column. 

| First Day      |    Second Day | Third Day  |
| :--------: | :--------:| :---------: |
| i | i+1  | i+2  |
| i+1  | i+2 | i+3 |
| i+2 | i+3 |  i+4 |

With the dataset above I have used `compute` function to make machine to forecast according to the related data, then form a matrix that has the predicted outputs and expected outputs. I have added an ID column to visualize the data more properly. I have melted all the data on ID column, so that I can get a better graph. I have used `melt` function from `reshape2` library for this operation.

```r
exchangeResults <- compute(exchangeNeuralNet, as.data.frame(exchangeTestData))

cleanOutput <- cbind(as.data.frame(exchangeRateTest[1:66]), as.data.frame(exchangeResults$net.result), id= 1:length(firstDayPred))
colnames(cleanOutput) <- c("Expected Output", "Predicted Output", "ID")

meltedOutput <- melt(cleanOutput, id = 'ID')

ggplot(data=meltedOutput, aes(x=ID, y=value, color=variable)) +
  geom_line()
```
![threeInputs](https://github.com/mpinar/MachineLearning/blob/master/NN%203%20input%20graph.png?raw=true)

When I run MAPE error test on my prediction, I got a small number of error. To visualize the trend I pasted my prediction to the whole graph that is shown below;
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

I have tested Neuralnet with a scaled data. I have used `caret` library for this operation. After I did the experiment with the scaled data I have put the output to MAPE error test, what I observed it's performance with scaled data was poor.
```r
library(caret)

model_nn <- train(exchangeTrainingData[,1:3], exchangeTrainingData[,4], method = 'neuralnet', 
                    preProcess = c("center", "scale"))

predictions <- predict.train(object = model_nn, exchangeTestData[,1:3], type = "raw")

errorOnNNScaled <- sqrt(mean((predictions - exchangeRateTest[1:66])^2))
```
> errorOnNNScaled -> 0.005775149777

Since I have tested my MLP's performance with three inputs, I had to test it again with different input scheme. I chose to have six inputs. I have applied the same steps for six inputs;
```r
sixDsExchangeNeuralNet <- neuralnet(Output~FirstInput + SecondInput + ThirdInput + FourthInput + FifthInput + SixthInput, sixDsExchangeTrainingData, hidden = 2)

sixDsTrainOutput <- cbind(as.data.frame(sixDsPredictionDay), as.data.frame(sixDsExchangeNeuralNet$net.result), id= 1:length(sixDsPredictionDay))
colnames(sixDsTrainOutput) <- c("Expected Output", "Predicted Output", "ID")
```
![nn6](https://github.com/mpinar/MachineLearning/blob/master/NN%206%20input.png?raw=true)

When we run the `compute` function we have got a prediction in our hands. I ran MAPE on the result and I almost have the same result as I get from three day input. You can inspect the code below, I am pasting the results right after the code.
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

I wanted to test my Neural Net with six scaled inputs. I got the results same as I got from three scaled inputs, the results were worse.
```r
model_nnSixDs <- train(sixDsExchangeTrainingData[,1:6], sixDsExchangeTrainingData[,7], method = 'neuralnet', 
                  preProcess = c("center", "scale"))

predictionsSixDs <- predict.train(object = model_nnSixDs, sixDsExchangeTestData[,1:6], type = "raw")

errorOnNNScaledSixDs <- sqrt(mean((predictionsSixDs - exchangeRateTest[4:67])^2))
errorOnNNScaledSixDs
```
> errorOnNNScaledSixDs -> 0.006306423344

From this experiment we can see that Neural net gave almost the same answer with non-scaled three and six inputs. What we can clearly observe that it performs poorly with the scaled data. I have formed the table below, you can see the results below;

| Error Table    |    Three Inputs | Six Inputs  |
| :--------: | :--------:| :---------: |
| Scaled | 0.005775149777  | 0.006306423344  |
| Not Scaled  | 0.003284871809 | 0.003367945112 |

#### Objective #4 (Forecasting using SVR)

In this part we are going to use SVM Model for this forecasting problem. After the experiment is done we will have the chance to compare the results with ones we got from Neural Net. I will use the exact same dataset that I used for Neural Net for the sake of the experiments validity. I am going to test the SVM models for three and six inputs. To begin with, I subsetted the Exchange Training data by extracting the output, I need that dataset to form my formula for the SVM Model.
```r
library(e1071)

svmExTrain <- subset(exchangeTrainingData, select = -Output)
expectedOutput <- exchangeTrainingData$Output
```
By following this I have got two datasets those will form my formula. With that information SVM model formed and with given test data the test had been conducted.
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

About the testing, I have done the same steps to the graph, that was I have added an ID column to melt the data on. The results were checked according to MAPE error calculation and model did a tramendous job according to Neural Net. So the code and  graphs for both prediction and cumulative below;
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

Following that changes after tunening the SVM model observed. `tune` function had been used. Aim of this part was whether output will have less error or not. 
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

SVM model tuned according to the information above, and test ran with the exact same procedure before. MAPE error calculation had been applied to the output;
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

After tests with three inputs are done, tests with six inputs will be applied. Same input matrix as used in Neural net, will be used. Same procedure will be followed.
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

We will observe how the predictions change after we tune the SVM;
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


After tuning the SVM according to the attributes above, I have ran the test all over again with the same processes as in three input tests.
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
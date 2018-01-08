# Coursework #2
### Mert PINAR w1642104

In this project we are expected to test some of the classifier algorithms such as Decision Trees, Naive Bayes and K-nearest neighbour. 

#### Dataset Selection and Visualization

In this experiment guest list of the Titanic had been used. Data contains name of the passenger, how much money he/she paid for the ticket (fare). It also have the information of siblings and children information in addition to embarkment point.

>Data Dictionary

>Variable	Definition	Key
survival:	Survival	0 = No, 1 = Yes
pclass:	Ticket class	1 = 1st, 2 = 2nd, 3 = 3rd
sex:	Sex	
Age:	Age in years	
sibsp:	# of siblings / spouses aboard the Titanic	
parch:	# of parents / children aboard the Titanic	
ticket:	Ticket number	
fare:	Passenger fare	
cabin:	Cabin number	
embarked:	Port of Embarkation	C = Cherbourg, Q = Queenstown, S = Southampton


There is a survival column that states if the passenger passed away or not, and only training set contains that Survival column. 891 variables are used to train the algorithms and 418 are used to test them. Down below summary of the dataset can be seen,
```r
train <- read_csv("~/Desktop/Courses/Data Mining and Machine Learning/CW2/train.csv")
test <- read_csv("~/Desktop/Courses/Data Mining and Machine Learning/CW2/test.csv")
expectedOutput <- read_csv("~/Desktop/Courses/Data Mining and Machine Learning/CW2/expectedOutput.csv")

summary(train)
summary(test)

```
|PassengerId |	Survived |	Pclass |	Name |	Sex|	Age| SibSp|	Parch|	Ticket|	Fare|	Cabin|	Embarked|
|---|---|---|---|---|---|---|---|---|---|---|---|
|1|	0|	3|	Braund, Mr. Owen Harris|	male|	22|	1|	0|	A/5 21171|	7.2500|	|	S|
|2	|1	|1	|Cumings, Mrs. John Bradley (Florence Briggs Thayer)	|female|	38|	1|	0	|PC 17599	|71.2833	|C85|	C
|3|	1|	3|	Heikkinen, Miss. Laina|	female|	26|	0|	0|	STON/O2. 3101282|	7.9250|	|	S
|4|	1|	1|	Futrelle, Mrs. Jacques Heath (Lily May Peel)|	female|	35|	1|	0|	113803|	53.1000|	C123|	S
|5	|0	|3	|Allen, Mr. William Henry|	male|	35|	0|	0|	373450	|8.0500	|	S



Since data was raw, some processes were essential for performance matters and logic issues.

1 - Pclass= needs to be a factor.
2- Cabin= needs to be divided into two (Cabin Deck and Cabin Number)
3 - Name= prefixes should be extracted from the fullname.
4 - family= sum of Sibling/Spouses and Parents/Children aboard the Titanic.
5 - Clean the missing values.

These step are done with the codes below.

```r
# @TODO: Need to find utilize the missing values

train$Survived = factor(train$Survived)
expectedOutput$Survived = factor(expectedOutput$Survived)
train$Embarked = factor(train$Embarked)

test$Embarked = factor(test$Embarked)


# Turning PClass into vector
train$Pclass = factor(train$Pclass, labels = c("1st", "2nd", "3rd"))
test$Pclass = factor(test$Pclass, labels = c("1st", "2nd", "3rd"))

# Creating some Cabin factors
train$CabinDeck = substring(train$Cabin, 1, 1)
train$CabinDeck[is.na(train$CabinDeck) == TRUE] = "MISS"
train$CabinDeck = factor(train$CabinDeck)

test$CabinDeck = substring(test$Cabin, 1 ,1)
test$CabinDeck[is.na(test$CabinDeck) == TRUE] = "MISS"
test$CabinDeck = factor(test$CabinDeck)


train$CabinNum = substring(train$Cabin, regexpr("(\\d)+", train$Cabin),
                           regexpr("(\\d)+", train$Cabin)+2)
train$CabinNum[train$CabinNum == ''] = "-1"
train$CabinNum = as.numeric(train$CabinNum)
train$CabinNum[!is.finite(train$CabinNum)] = -1

test$CabinNum = substring(test$Cabin, regexpr("(\\d)+", test$Cabin),
                          regexpr("(\\d)+", test$Cabin)+2)
test$CabinNum[test$CabinNum == ''] = "-1"
test$CabinNum = as.numeric(test$CabinNum)
test$CabinNum[!is.finite(test$CabinNum)] = -1

# Extract the title out of the names and turn them into factors
train$Name = as.character(train$Name)
pattern = "[A-Z][a-z]*\\."
m = regexpr(pattern, train$Name)
train$title = regmatches(train$Name, m)
train$title = factor(train$title)

test$Name = as.character(test$Name)
m = regexpr(pattern, test$Name)
test$title = regmatches(test$Name, m)
test$title[test$title == "Dona."] = "Don."
test$title = factor(test$title)


# Family size for merging Sibling, Spouse and Parented Child count
train$familySize = (train$SibSp + train$Parch)
test$familySize = (test$SibSp + test$Parch)

```
After all those processes dataset now looks like shown below,

|PassengerId |	Survived |	Pclass |	Name |	Sex|	Age| SibSp|	Parch|	Ticket|	Fare|	Cabin|	Embarked|	Cabin_deck|	Cabin_num|	title|	family_size|
|---|---|-------------|---|---|---|---|---|---|---|---|---|---|---|---|----|
|1|	0|	3|	Braund, Mr. Owen Harris|	male|	22|	1|	0|	A/5 21171|	7.2500|	|	S|	MiSS|	-1|	Mr.|	1
|2	|1	|1	|Cumings, Mrs. John Bradley (Florence Briggs Thayer)	|female|	38|	1|	0	|PC 17599	|71.2833	|C85|	C|	C	|85|	Mrs.|	1
|3|	1|	3|	Heikkinen, Miss. Laina|	female|	26|	0|	0|	STON/O2. 3101282|	7.9250|	|	S|	MiSS|	-1|	Miss.|	0
|4|	1|	1|	Futrelle, Mrs. Jacques Heath (Lily May Peel)|	female|	35|	1|	0|	113803|	53.1000|	C123|	S|	C|	123|	Mrs.|	1|
|5	|0	|3	|Allen, Mr. William Henry|	male|	35|	0|	0|	373450	|8.0500	|	S|	MiSS|	-1|	Mr.|	0|

#### Formation of training and test Datasets

Since stated before, 819 of variables used for training and 418 variables used for testing purposes. Three form had been used, first of them is _Hold-out method_. This method is simply dividing dataset into two.

```r
train <- read_csv("~/Desktop/Courses/Data Mining and Machine Learning/CW2/train.csv")
test <- read_csv("~/Desktop/Courses/Data Mining and Machine Learning/CW2/test.csv")
```

Second method is _Cross-Validation_. It has done by using `caret` library of R.

```r
crossValidate <- trainControl(method = "cv", number = 10)
```

Third method is _Leave-One-Out Cross-Validation_ method. Again `caret` was used.
```r
leaveOneOut <- trainControl(method = "LOOCV")
```

#### Building Train and Test for Decision Tree 

In this part three other libraries other that `caret` was used. Those are,
```r
library(rpart)
library(party)
library(RWeka)
```
`Rpart` was used for Random forest, `RWeka` was used for C4.5 algorithm and `party` for visualization purposes. First a regression formula is formed, that checks passengers cabin information, age, family size and fare that he/she paid, and predicts whether he/she will survive or not.
```r
form=formula(Survived ~ Pclass + Sex + Age + Fare+ Embarked+ CabinDeck + CabinNum+ familySize+ title)
```

First training applied by using Hold-out method. Then tested;
```r
dTreeHO <- rpart(form, method = "class", data = train) #Hold-out method
prp(dTreeHO, box.palette = "Greens", tweak = 1.2 , extra = 7) #Graph

predictTest <- predict(dTreeHO, test, type = "class") #Testing
```
Output ->
> -dTreeHO
>n= 891 
>node), split, n, loss, yval, (yprob)
      * denotes terminal node
      
>  root 891 342 0 (0.61616162 0.38383838)  
>   title=Capt.,Don.,Dr.,Jonkheer.,Mr.,Rev. 533  84 0 (0.84240150 0.15759850) *
>   title=Col.,Countess.,Lady.,Major.,Master.,Miss.,Mlle.,Mme.,Mrs.,Ms.,Sir. 358 100 1  (0.27932961 0.72067039)  
>   Pclass=3rd 172  83 0 (0.51744186 0.48255814)  
>   familySize>=3.5 45   4 0 (0.91111111 0.08888889) *
>   familySize< 3.5 127  48 1 (0.37795276 0.62204724) *
>   Pclass=1st,2nd 186  11 1 (0.05913978 0.94086022) *

![](https://github.com/mpinar/MachineLearning/blob/master/CW2/DTreeHO.png?raw=true)

Second training by using Cross-Validation. This is for Random Forest;

```r
dTreeCV <- train(form, data = train, method = "rf", trControl = crossValidate, tuneLength =10, na.action = na.omit) # Training the model

prp(dTreeCV$finalModel, box.palette = "Reds", extra = 7) # Visualize

testPredCV <- predict(dTreeCV, newdata = test) # Testing the model
```
Output ->
> Random Forest 

> 891 samples
  9 predictor
  2 classes: '0', '1' 

> No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 802, 802, 802, 801, 802, 803, ... 
Resampling results across tuning parameters:

>  mtry  Accuracy   Kappa    
   2    0.8215563  0.6073013
   5    0.8316565  0.6347898
   8    0.8316939  0.6335659
  12    0.8372866  0.6476257
  15    0.8361633  0.6448801
  19    0.8260504  0.6237058
  22    0.8294215  0.6328539
  26    0.8249018  0.6235024
  29    0.8215563  0.6169497
  33    0.8182105  0.6099383

>Accuracy was used to select the optimal model using  the largest value.
The final value used for the model was mtry = 12.

![](https://github.com/mpinar/MachineLearning/blob/master/CW2/dTreeCVHO.png?raw=true)

Model will be re-trained by using C4.5 algorithm with Cross-Validation method. I have tried to graph it but I couldn't do that in more fashionable way. Help from `RWeka` library has been used. This library is different than the other since almost all of the libraries those are used in R, is written in C, but RWeka is written in Java. So I had to give the path of my own Java application to make it work. 

```r
dTreeC45CV <- train(form, data = train, method = "J48", trControl = crossValidate, tuneLength =10, na.action = na.omit) # Training the model (C4.5)

plot(dTreeC45CV$finalModel) #Visualize

testPredC45CV <- predict(dTreeC45CV, newdata = test) # Test the model (C4.5)
```
Output ->
> C4.5-like Trees 

> 891 samples
  9 predictor
  2 classes: '0', '1' 

>No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 802, 801, 802, 802, 802, 803, ... 
Resampling results across tuning parameters:

>  C           M   Accuracy   Kappa    
  0.01000000   1  0.8271618  0.6262178
  0.01000000   2  0.8271618  0.6262178
  0.01000000   3  0.8271618  0.6262178
  0.01000000   4  0.8271618  0.6262178
  0.01000000   5  0.8249146  0.6207010
  0.01000000   6  0.8226674  0.6163548
  0.01000000   7  0.8238038  0.6191096
  0.01000000   8  0.8238038  0.6191096
  0.01000000   9  0.8238038  0.6191096
  0.01000000  10  0.8272129  0.6274938
  0.06444444   1  0.8271870  0.6239180
  0.06444444   2  0.8271870  0.6239180
  0.06444444   3  0.8260759  0.6218161
  0.06444444   4  0.8260759  0.6218161
  0.06444444   5  0.8249523  0.6190740
  0.06444444   6  0.8227176  0.6138446
  0.06444444   7  0.8249648  0.6184829
  0.06444444   8  0.8238412  0.6162418
  0.06444444   9  0.8238412  0.6157082
  0.06444444  10  0.8238412  0.6157082
  0.11888889   1  0.8203691  0.6068694
  0.11888889   2  0.8215438  0.6064813
  0.11888889   3  0.8271743  0.6194909
  0.11888889   4  0.8249015  0.6162159
  0.11888889   5  0.8226927  0.6110136
  0.11888889   6  0.8193468  0.6039760
  0.11888889   7  0.8159636  0.5951360
  0.11888889   8  0.8137289  0.5906727
  0.11888889   9  0.8170996  0.5983622
  0.11888889  10  0.8182232  0.6012689
  0.17333333   1  0.8282343  0.6233983
  0.17333333   2  0.8248635  0.6157369
  0.17333333   3  0.8327922  0.6312565
  0.17333333   4  0.8282848  0.6240299
  0.17333333   5  0.8283231  0.6244114
  0.17333333   6  0.8238412  0.6177591
  0.17333333   7  0.8159636  0.5965393
  0.17333333   8  0.8125803  0.5884001
  0.17333333   9  0.8148400  0.5942814
  0.17333333  10  0.8114181  0.5880070
  0.22777778   1  0.8226166  0.6126095
  0.22777778   2  0.8203567  0.6073666
  0.22777778   3  0.8248763  0.6151125
  0.22777778   4  0.8305320  0.6271231
  0.22777778   5  0.8294467  0.6262721
  0.22777778   6  0.8137538  0.5953924
  0.22777778   7  0.8148400  0.5937382
  0.22777778   8  0.8114567  0.5855991
  0.22777778   9  0.8137164  0.5914804
  0.22777778  10  0.8102945  0.5845433
  0.28222222   1  0.8226166  0.6139459
  0.28222222   2  0.8203317  0.6086102
  0.28222222   3  0.8271110  0.6204823
  0.28222222   4  0.8293959  0.6252174
  0.28222222   5  0.8181725  0.6014584
  0.28222222   6  0.8103447  0.5885206
  0.28222222   7  0.8092087  0.5825431
  0.28222222   8  0.8114567  0.5855991
  0.28222222   9  0.8137164  0.5914804
  0.28222222  10  0.8102945  0.5845433
  0.33666667   1  0.8316437  0.6308729
  0.33666667   2  0.8203317  0.6085741
  0.33666667   3  0.8214925  0.6082580
  0.33666667   4  0.8158994  0.5960886
  0.33666667   5  0.8102940  0.5828004
  0.33666667   6  0.8047012  0.5766108
  0.33666667   7  0.8102942  0.5845228
  0.33666667   8  0.8091709  0.5815196
  0.33666667   9  0.8080598  0.5814060
  0.33666667  10  0.8136908  0.5926250
  0.39111111   1  0.8350020  0.6392529
  0.39111111   2  0.8214927  0.6104043
  0.39111111   3  0.8181339  0.6027018
  0.39111111   4  0.8159119  0.5967519
  0.39111111   5  0.8069232  0.5762137
  0.39111111   6  0.8047012  0.5759209
  0.39111111   7  0.8047387  0.5738818
  0.39111111   8  0.8036154  0.5708911
  0.39111111   9  0.8025043  0.5711248
  0.39111111  10  0.8125672  0.5904493
  0.44555556   1  0.8316437  0.6343258
  0.44555556   2  0.8203691  0.6099027
  0.44555556   3  0.8215554  0.6105182
  0.44555556   4  0.8148008  0.5954874
  0.44555556   5  0.8057996  0.5743154
  0.44555556   6  0.8047012  0.5759209
  0.44555556   7  0.8058623  0.5744220
  0.44555556   8  0.8013682  0.5672381
  0.44555556   9  0.8025043  0.5716225
  0.44555556  10  0.8114686  0.5896018
  0.50000000   1  0.8316437  0.6347864
  0.50000000   2  0.8170108  0.6036430
  0.50000000   3  0.8204318  0.6083358
  0.50000000   4  0.8125536  0.5909675
  0.50000000   5  0.8091704  0.5830887
  0.50000000   6  0.8013554  0.5695845
  0.50000000   7  0.8036151  0.5699447
  0.50000000   8  0.8036154  0.5713948
  0.50000000   9  0.8058750  0.5785950
  0.50000000  10  0.8114686  0.5901332

> Accuracy was used to select the optimal model using  the largest value.
The final values used for the model were C = 0.3911111 and M = 1.

![](https://github.com/mpinar/MachineLearning/blob/master/CW2/dTreeC45CV.png?raw=true)

Third, we are going to use Leave-One-Out Cross-Validation method on Random Forest. When we discuss on time constraints Leave-One-Out method takes really long time. It took me around 2 hours for it to fail to complete the process on C4.5 algorithm.
```r
dTreeLOO <- train(form, data = train, method = "rf", trControl = leaveOneOut, tuneLength =10, na.action = na.omit) # Training the model (Random Forest)

prp(dTreeLOOVisu$finalModel, box.palette = "Blues", tweak = 1.2, extra = 7) #Visualize

testPredLOO <- predict(dTreeLOO, newdata = test) # Testing the model (Random Forest)
```
Output ->
> Random Forest

> 891 samples
  9 predictor
  2 classes: '0', '1' 

> No pre-processing
Resampling: Leave-One-Out Cross-Validation 
Summary of sample sizes: 890, 890, 890, 890, 890, 890, ... 
Resampling results across tuning parameters:

>  cp          Accuracy   Kappa     
  0.00000000  0.8170595   0.6025076
  0.04808317  0.8114478   0.5978334
  0.09616634  0.7822671   0.5476684
  0.14424951  0.7822671   0.5476684
  0.19233268  0.7822671   0.5476684
  0.24041585  0.7822671   0.5476684
  0.28849903  0.7822671   0.5476684
  0.33658220  0.7822671   0.5476684
  0.38466537  0.7822671   0.5476684
  0.43274854  0.4893378  -0.2355652

> Accuracy was used to select the optimal model using  the largest value.
The final value used for the model was cp = 0.

![](https://github.com/mpinar/MachineLearning/blob/master/CW2/dTreeLOO%20graph.png?raw=true)

Final part of this question, training the model by using C4.5 algorithm and Leave-one-out Cross Validation. Due to issues of the algorithm and the validation model, this code could not be completed. Systems crushed after 2 hours for 4 seperate tests with a machine powered by Intel i7 processor. Line of code is below;
```r
dTreeC45LOO <- train(form, data = train, method = "J48", trControl = leaveOneOut, tuneLength =10, na.action = na.omit)
beepr::beep()
```

Performance of the models above will be discussed in Measure Performance Part

#### Build Train and Test for Naive Bayes

Same dataset and same formula are used. For Hold-out method `KlaR` library has used.
```r
modelNbHO <- NaiveBayes(form, data = train) #Building and Training the model
nbPredictHO <- predict(modelNbHO, test) # Testing the model
```
Output ->
> modelNbHO
$apriori
grouping
        0         1 
0.6161616 0.3838384 

> $levels
[1] "0" "1"

> $call
NaiveBayes.default(x = X, grouping = Y)

> $usekernel
[1] FALSE

>$varnames
[1] "Pclass"     "Sex"        "Age"        "Fare"       "Embarked"   "CabinDeck"  "CabinNum"  "familySize" "title"     

>attr(,"class")
[1] "NaiveBayes"

For the other parts I have kept using `caret` library. Second part I have used Cross-Validation method on Naive-Bayes model.
```r
modelNbCV <- train(form, data = train,  trControl = crossValidate, method = "nb", na.action = na.omit) # Building and Training the model

nbPredictCV <- predict(modelNbCV, newdata = test) # Testing the model
```
Output ->
> > modelNbCV
Naive Bayes 

>891 samples
  9 predictor
  2 classes: '0', '1' 

>No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 802, 802, 803, 802, 802, 801, ... 
Resampling results across tuning parameters:

>  usekernel  Accuracy   Kappa    
  FALSE            NaN        NaN
   TRUE      0.7260348  0.3435101

>Tuning parameter 'fL' was held constant at a value of 0
Tuning parameter 'adjust' was held
 constant at a value of 1
Accuracy was used to select the optimal model using  the largest value.
The final values used for the model were fL = 0, usekernel = TRUE and adjust = 1.

Third one is using leave-one-out method. 
```r
modelNbLOO <- train(form, data = train, trControl = leaveOneOut, method = "nb", na.action = na.omit) # Building and Training the model
nbPredictLOO <- predict(modelNbLOO, newdata = test) # Testing the model
```
Output ->
> > modelNbLOO
Naive Bayes 

>891 samples
  9 predictor
  2 classes: '0', '1' 

>No pre-processing
Resampling: Leave-One-Out Cross-Validation 
Summary of sample sizes: 890, 890, 890, 890, 890, 890, ... 
Resampling results across tuning parameters:

>  usekernel  Accuracy   Kappa    
  FALSE             NA         NA
   TRUE      0.7351291  0.3659357

>Tuning parameter 'fL' was held constant at a value of 0
Tuning parameter 'adjust' was held
 constant at a value of 1
Accuracy was used to select the optimal model using  the largest value.
The final values used for the model were fL = 0, usekernel = TRUE and adjust = 1.

Performance of Naive Bayes Model will be discussed in Measurement part

#### Build Train and Test K-Nearest Neighbour

In this part `class` library of R has used for Hold-out method. `class` has the funtion callad `knn` to build a KNN model. First I have formed a special training set for knn function. Since the requirements of the function I took out the labels to another array of characters, and took out the survived variable which is my true answers to a different vector of factor.


```r
library(class)

knnTrainingSet = cbind(train$Pclass, train$Sex, train$Age, train$Fare, train$Embarked, train$CabinDeck, train$CabinNum, train$familySize, train$title)

trainingSetLabels = factor(c("Pclass", "Sex", "Age", "Fare", "Embarked", "CabinDeck", "CabinNum", "familySize", "title"))

knnOutcomeSet = train$Survived #Arranged datasets

modelKnnHO <- knn(t(knnTrainingSet), knnOutcomeSet, cl = trainingSetLabels) #Building and training the model

predictionHO <- predict(modelKnnHO, newdata = test, type = "raw") #testing the model

summary(predictionHO)
```
Output ->
>> modelKnnHO
k-Nearest Neighbors 

>891 samples
  9 predictor
  2 classes: '0', '1' 

>No pre-processing
Resampling:  
Summary of sample sizes: 891, 891, 891, 891, 891, 891, ... 
Resampling results across tuning parameters:

>  k  Accuracy   Kappa    
  5  0.6865027  0.3295961
  7  0.6958392  0.3478211
  9  0.6989008  0.3531459

>Accuracy was used to select the optimal model using  the largest value.
The final value used for the model was k = 9.

Summary of Prediction;
 | 0|   1 |
 |--|--|
|273 |145|

Second part Cross-Validation method was used in Naive Bayes Model. `caret` library was used;

```r
modelKnnCV <- train(form, data = train, method = "knn", trControl = crossValidate, na.action = na.omit) # Building and training the model
predictionCV <- predict(modelKnnCV, newdata = test, type = "raw") # Testing the model

summary(predictionCV)
```
Output ->
> > modelKnnCV
k-Nearest Neighbors 

> 891 samples
  9 predictor
  2 classes: '0', '1' 

> No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 801, 802, 801, 802, 802, 802, ... 
Resampling results across tuning parameters:

>   k  Accuracy   Kappa    
  5  0.7283058  0.4115758
  7  0.7249353  0.3993226
  9  0.7137116  0.3708175

>Accuracy was used to select the optimal model using  the largest value.
The final value used for the model was k = 5.

Summary of Prediction;
 | 0|   1 |
 |--|--|
|269 |149|

Third part, I have used Leave-One-Out method for KNN Model,

```r
modelKnnLOO <- train(form, data = train, method = "knn", trControl = leaveOneOut, na.action = na.omit) # Building and Training the Model
predictionLOO <- predict(modelKnnLOO, newdata = test, type = "raw") # Testing the model

summary(predictionLOO)
```
Output ->
> modelKnnLOO
k-Nearest Neighbors 

>891 samples
  9 predictor
  2 classes: '0', '1' 

>No pre-processing
Resampling: Leave-One-Out Cross-Validation 
Summary of sample sizes: 890, 890, 890, 890, 890, 890, ... 
Resampling results across tuning parameters:

>  k  Accuracy   Kappa    
  5  0.7194164  0.3858256
  7  0.7306397  0.4130725
  9  0.7171717  0.3809122

>Accuracy was used to select the optimal model using  the largest value.
The final value used for the model was k = 7.

Summary of Prediction;
 | 0|   1 |
 |--|--|
|276 |142|

#### Measure Performance

For confusion matrix estimation I have put all the models into a for loop and made estimation for each of them. Code and the matrices are below;

```r
predictions = as.data.frame(cbind(predictTest,testPredCV,testPredC45CV ,testPredLOO, nbPredictHO$class, nbPredictCV, nbPredictLOO,
                predictionHO, predictionCV, predictionLOO)) # Each predict outputs
                
predictionNames = c("Decision Tree - Holdout", "Decision Tree - Cross Validation", "C45 - Cross Validation", "Decision Tree - Leave-One-Out",
                    "Naive Bayes - Holdout", "Naive Bayes - Cross Validation", "Naiva Bayes - Leave-One-Out",
                    "K-NN - Holdout", "K-NN - Cross Validation", "K-NN - Leave-One-Out") #Name of the models

for(i in 1:length(predictions)){
  print(predictionNames[i])
  confusionMatrix(predictions[i], expectedOutput$Survived)
}
```

Decision Tree - Holdout
 |     |Reference| |
 |---|---|---|
|Prediction|   0|   1|
        | 0| 248|   6|
        | 1|  18| 146|

Decision Tree - Cross-Validation
 |     |Reference| |
 |---|---|---|
|Prediction|   0|   1|
        | 0| 236|   29|
        | 1|  30| 123|

Decision Tree - Cross-Validation C4.5
 |     |Reference| |
 |---|---|---|
|Prediction|   0|   1|
        | 0| 251|   5|
        | 1|  15| 147|
        
Decision Tree - Leave-One-out Cross-Validation
 |     |Reference| |
 |---|---|---|
|Prediction|   0|   1|
        | 0| 229|   25|
        | 1|  37| 127|
        
Naive Bayes - Hold-out 
 |     |Reference| |
 |---|---|---|
|Prediction|   0|   1|
        | 0| 220|   100|
        | 1|  46| 52|

Naive Bayes - Cross-Validation 
 |     |Reference| |
 |---|---|---|
|Prediction|   0|   1|
        | 0| 264|   98|
        | 1|  2| 54|

Naive Bayes - Leave-One-Out Cross-Validation 
 |     |Reference| |
 |---|---|---|
|Prediction|   0|   1|
        | 0| 264|   98|
        | 1|  2| 54|

K-Nearest Neighbour - Hold-out 
 |     |Reference| |
 |---|---|---|
|Prediction|   0|   1|
        | 0| 207|   66|
        | 1|  59| 86|

K-Nearest Neighbour - Hold-out 
 |     |Reference| |
 |---|---|---|
|Prediction|   0|   1|
        | 0| 207|   66|
        | 1|  59| 86|

K-Nearest Neighbour - Cross-Validation
 |     |Reference| |
 |---|---|---|
|Prediction|   0|   1|
        | 0| 202|   67|
        | 1|  64| 85|
        
K-Nearest Neighbour - Leave-One-out Cross-Validation
 |     |Reference| |
 |---|---|---|
|Prediction|   0|   1|
        | 0| 209|   67|
        | 1|  57| 85|

##### Precision vs. Recall

In this part I have used `caret` package's precision and recall function. I have run a for loop for all of the methods.

```r
for(i in 1:length(predictions)){
  print(predictionNames[i])
  precision(predictions[i], expectedOutput$Survived)
  recall(predictions[i], expectedOutput$Survived)
}
```


Decision Trees
 |     |Decision Tree HO|Decision Tree CV | C4.5 CV |Decision Tree LOOCV|C4.5 LOOCV
 |---|---|---|---|---|---|
|Precision| 0.976378| 0.890566|0.9804688|0.9015748|NA*|
|Recall| 0.9323308|  0.887218|0.943609|0.8609023|NA*|
*NA: Was not able to get an output

        
Naive Bayes 
 |     |Hold-out|Cross-Validation |  Leave-One-Out Cross-Validation|
 |---|---|---|---|
|Precision| 0.6875| 0.7292818|0.7292818|
|Recall| 0.8270677| 0.9924812|0.9924812|


K-Nearest Neighbour 
 |     |Hold-out|Cross-Validation |  Leave-One-Out Cross-Validation|
 |---|---|---|---|
|Precision| 0.7582418| 0.7636364|0.7572464|
|Recall| 0.7781955| 0.7894737|0.7857143|

##### Accuracy
I have gathered this information from `caret` package's confusion matrix method.


Decision Trees
 |     |Decision Tree HO|Decision Tree CV | C4.5 CV |Decision Tree LOOCV|C4.5 LOOCV
 |---|---|---|---|---|---|
|Accuracy| 0.9426| 0.8589|0.9522|0.8517|NA*|

*NA: Was not able to get an output

        
Naive Bayes 
 |     |Hold-out|Cross-Validation |  Leave-One-Out Cross-Validation|
 |---|---|---|---|
|Accuracy| 0.6507| 0.7608|0.7608|



K-Nearest Neighbour 
 |     |Hold-out|Cross-Validation |  Leave-One-Out Cross-Validation|
 |---|---|---|---|
|Accuracy| 0.701| 0.6866|0.7033|

##### ROC and RAUC

In this part I have used `pROC` library to draw both ROC curve and RAUC line. 

```r
for(i in 1:length(predictions)){
  print(predictionNames[i])
  c = roc(predictor = as.numeric(predictions[i]), response = expectedOutput$Survived,
      levels = rev(levels(expectedOutput$Survived)))
  plot(c)
}
```
Decision Tree Hold-out
>Call:
roc.default(response = expectedOutput$Survived, predictor = as.numeric(predictTest),     levels = rev(levels(expectedOutput$Survived)))

>Data: as.numeric(predictTest) in 152 controls (expectedOutput$Survived 1) > 266 cases (expectedOutput$Survived 0).
Area under the curve(RAUC): 0.9464

![](https://github.com/mpinar/MachineLearning/blob/master/CW2/roc%20of%20DTreeHO.png?raw=true)

Decision Tree Cross-Validation
>Call:
roc.default(response = expectedOutput$Survived, predictor = as.numeric(testPredCV),     levels = rev(levels(expectedOutput$Survived)))

>Data: as.numeric(testPredCV) in 152 controls (expectedOutput$Survived 1) > 266 cases (expectedOutput$Survived 0).
Area under the curve(RAUC): 0.8482

![](https://github.com/mpinar/MachineLearning/blob/master/CW2/ROC%20of%20DTreeCV.png?raw=true)


Decision Tree Leave-One-Out Cross-Validation
>Call:
roc.default(response = expectedOutput$Survived, predictor = as.numeric(testPredLOO),     levels = rev(levels(expectedOutput$Survived)))

>Data: as.numeric(testPredLOO) in 152 controls (expectedOutput$Survived 1) > 266 cases (expectedOutput$Survived 0).
Area under the curve(RAUC): 0.8482

![](https://github.com/mpinar/MachineLearning/blob/master/CW2/roc%20of%20DTreeLOO.png?raw=true)

Naive Bayes Hold-out

>Call:
roc.default(response = expectedOutput$Survived, predictor = as.numeric(nbPredictHO$class),     levels = rev(levels(expectedOutput$Survived)))

>Data: as.numeric(nbPredictHO$class) in 152 controls (expectedOutput$Survived 1) < 266 cases (expectedOutput$Survived 0).
Area under the curve(RAUC): 0.4154

![](https://github.com/mpinar/MachineLearning/blob/master/CW2/roc%20of%20nbHO.png?raw=true)

Naive Bayes Cross-Validation

>Call:
roc.default(response = expectedOutput$Survived, predictor = as.numeric(nbPredictCV),     levels = rev(levels(expectedOutput$Survived)))

>Data: as.numeric(nbPredictCV) in 152 controls (expectedOutput$Survived 1) < 266 cases (expectedOutput$Survived 0).
Area under the curve(RAUC): 0.3261

![](https://github.com/mpinar/MachineLearning/blob/master/CW2/roc%20of%20nbCV.png?raw=true)

Naive Bayes Leave-One-Out Cross Validation

>Call:
roc.default(response = expectedOutput$Survived, predictor = as.numeric(nbPredictLOO),     levels = rev(levels(expectedOutput$Survived)))

>Data: as.numeric(nbPredictLOO) in 152 controls (expectedOutput$Survived 1) < 266 cases (expectedOutput$Survived 0).
Area under the curve(RAUC): 0.3261

![](https://github.com/mpinar/MachineLearning/blob/master/CW2/roc%20of%20nbLOO.png?raw=true)

K-NN Hold-out

>Call:
roc.default(response = expectedOutput$Survived, predictor = as.numeric(predictionHO),     levels = rev(levels(expectedOutput$Survived)))

>Data: as.numeric(predictionHO) in 152 controls (expectedOutput$Survived 1) > 266 cases (expectedOutput$Survived 0).
Area under the curve(RAUC): 0.672

![](https://github.com/mpinar/MachineLearning/blob/master/CW2/roc%20of%20knnHO.png?raw=true)

K-NN Cross-Validation

>Call:
roc.default(response = expectedOutput$Survived, predictor = as.numeric(predictionCV),     levels = rev(levels(expectedOutput$Survived)))

>Data: as.numeric(predictionCV) in 152 controls (expectedOutput$Survived 1) > 266 cases (expectedOutput$Survived 0).
Area under the curve(RAUC): 0.6593

![](https://github.com/mpinar/MachineLearning/blob/master/CW2/roc%20of%20knnCV.png?raw=true)

K-NN Leave-One-Out Cross-Validation

>Call:
roc.default(response = expectedOutput$Survived, predictor = as.numeric(predictionLOO),     levels = rev(levels(expectedOutput$Survived)))

>Data: as.numeric(predictionLOO) in 152 controls (expectedOutput$Survived 1) > 266 cases (expectedOutput$Survived 0).
Area under the curve(RAUC): 0.6739

![](https://github.com/mpinar/MachineLearning/blob/master/CW2/roc%20of%20knnLOO.png?raw=true)














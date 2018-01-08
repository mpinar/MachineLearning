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


> Dataset summary image

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








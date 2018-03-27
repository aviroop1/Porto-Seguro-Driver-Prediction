#Read Train and Test files
Train = read.csv('Train.csv')
Test = read.csv('Test.csv')

#Understand the data 
length(table(Train$id))
sapply(Train, function(x){class(x)})

#Plot and understand the correlation
library(corrplot)
corrplot(cor(Train))
Correlation = data.frame(cor(Train))

#Load regular expression library
library(regex)

#Initialize count variable
count = 0

#Loading all correlation values into a single array
a = character(0)
for (i in (1:59))
{
  for (j in (1:59))
  {
   a = append(a,Correlation[i,j])
  }
}

#Convert 'a' to numeric array
a = as.numeric(a)

#Arrange the correlations in descending order
a = a[order(a, decreasing = T)]

#Remove all correlations with values as 1, since those indicate variable correlated with itself
a = a[a != 1]

#Check distribution of the correlation values
quantile(a)

#Define b and c as numeric array
b = numeric(0); c=numeric(0)

#Take all the correlation values greater than 0.5 and load into b and c
for (i in (1:3422))
{
ifelse((a[i]>=0.5 || a[i]<= -0.5), b <- append(b, a[i]), c <- append(c, a[i]))
}

#Convert the values to 2 decimal places and returns a matrix
m = data.frame(sapply(Correlation, function(x){as.numeric(format(x, digits = 2))}))

#Define a new data frame DF
DF = data.frame(RowName = character(), ColName = character(), Correlation = numeric(), stringsAsFactors = F)
i=0; j=0; k=0

#The DF should consist of the critical correlation values and the variable names having those strong correlation values
for (i in (1:59))
{
  for (j in (1:59))
  {
    for (k in (1:20))
    {
    if(b[k] == as.numeric(format(Correlation[i,j], digits = 6)))
       {
           DF[k,1] = rownames(Correlation)[i]
           DF[k,2] = colnames(Correlation)[j]
           DF[k,3] = b[k]
      }
    }
  }
}

b = numeric(0)
count = 0
for (i in (1:3481))
{
  len = length(strsplit(a[i],"")[[1]])
  for (j in (1:len))
  {
    if (strsplit(a[i],"")[[1]][j] == "e")
      strsplit(a[i], "")[1:(j-1)]
      b = append(b, as.numeric(a[i]))
      count = count +1
  }
}



length(strsplit(a[2],"")[[1]])
length(strsplit(a[1],"")[[1]])
strsplit(a[2],"")[2]

strsplit(a[1:5], "")[[1]][1]
which(strsplit(a[1],"")[[1]] == "e")

#Load the required libraries for running the ML algorithms
library(xgboost)
library(caTools)

#Splitting the data
split = sample.split(Train$target, SplitRatio = 0.7)
Training = subset(Train, split == T)
Testing = subset(Train, split == F)

#xgboost model
param = list("objective" = "binary:logistic", "eval_metric" = "logloss", "eta" = 0.1, "max.depth" = 2)
xgboostcv = xgb.cv(params = param, data = as.matrix(Training), nfold = 10, nrounds = 40, label = Training$target, verbose = T )
xgboost = xgboost(data = as.matrix(Training), nrounds = 30, label = Training$target, nthread = 4, objective = "binary:logistic")
names = dimnames(Training)[[2]]
importance.matrix = xgb.importance(names, model = xgboost)
xgb.plot.importance(importance.matrix)

#Predict using xgboost model
Predict = predict(xgboost, newdata = as.matrix(Testing))

#Confusion Matrix
table(Testing$target, Predict>0.5)

#Tell R to not use the scientific notation
options(scipen = 3)

#Combining the prediction columns with ID column of Test dataset
TestPrediction = data.frame(cbind(Test$id, Predict))

#Change column names to 'id' and 'target'
colnames(TestPrediction) = c("id", "target")

#Write the Test Prediction values to a csv file
write.csv(TestPrediction, "TestPredictionPortoSeguro.csv")

#Load the DF Matrix value containing highest correlation values, to another data frame called 'HighestCorrDF'
HighestCorrDF = DF
DF = NULL

#Logistic Regression
a = which(colnames(Training) %in% HighestCorrDF[,1])
Training = Training[,-a]
Testing = Testing[,-a]

LogReg = glm(target ~., data = Training[,-1], family = binomial)
summary(LogReg)

#Predict using Logistic Regression model
PredictLogReg = predict(LogReg, newdata = Testing, type = "response")

#Understand the prediction
quantile(PredictLogReg)
sum(Testing$target==0)/nrow(Testing)
mean(PredictLogReg)

#Confusion Matrix
cf = table(Testing$target, PredictLogReg>0.0364173)
sum(diag(cf))/sum(cf)

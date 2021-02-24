library(caret)
library(kernlab)
data(spam)
inTrain<-createDataPartition(spam$type,p=0.75,list=FALSE)
training<-spam[inTrain,]
testing<-spam[-inTrain,]
set.seed(32343)
modelFit<-train(type~.,data=training,method="glm")


library(caret)
library(kernlab)
library(ISLR)
dat(Wage)
inTrain<-createDataPartition(Wage$wage,p=0.7,list = FALSE)
training<-Wage[inTrain,]
testing<-Wage[-inTrain,]
featurePlot(x=training[,c("age","education","jobclass")],
            y=training$wage,plot="paris")


library(caret)
library(kernlab)
data(spam)
inTrain<-createDataPartition(spam$type,p=0.75,list=FALSE)
training<-spam[inTrain,]
testing<-spam[-inTrain,]

hist(train)

set.seed(13343)
training$capAVe<-training$capitalAve
selectNA<-rbinom(dim(training)[1],size = 1,prob = 0.05)==1
training$capAVe[selectNA]<-NA
preObj<-preProcess(training[,-58],method = "knnImpute")
capAve<-predict(preObj,training[,-58])
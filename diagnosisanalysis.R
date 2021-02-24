library(text2vec)
library(caret)
library(tm)
library(glmnet)
library(pROC)
diagnosis.clean<-function(x){
  x<-removePunctuation(x)
  x<-stripWhitespace(x)
  return(x)
}

diabetes<-read.csv("diabetes_subset_8500.csv")
diabetes$diag.text<-
  as.character(paste(diabetes$diag_1_desc,
                     diabetes$diag_2_desc, diabetes$diag_3_desc, sep=" "))
diabetes$diag.text<-diagnosis.clean(
  diabetes$diag.text)

train<-createDataPartition(diabetes$readmitted,p=.7,
                           list=F)
train.diabetes<-diabetes[train,]
test.diabetes<-diabetes[-train,]

iter.maker<-itoken(train.diabetes$diag.text,
                   preprocess_function = tolower, tokenizer =
                     word_tokenizer)

v <-
  create_vocabulary(iter.maker,stopwords=stopwords('en'))

vectorizer <- vocab_vectorizer(v)


it <- itoken(train.diabetes$diag.text,
             preprocess_function = tolower,
             tokenizer = word_tokenizer)
dtm <- create_dtm(it, vectorizer)

text.cv<-cv.glmnet(dtm,y=as.factor(
  train.diabetes$readmitted), alpha=0.9,family="binomial",
  type.measure="auc", nfolds=5, intercept=F)

plot(text.cv)

no.text<-as.matrix(train.diabetes[,1:132])

no.text.cv<-cv.glmnet(no.text, y=
                        as.factor(train.diabetes$readmitted), alpha=0.9,
                      family='binomial',type.measure='auc', nfolds=5,
                      intercept=F)

plot(no.text.cv)
title("GLMNET No Text")
all.data<-cBind(dtm,no.text)

all.cv<-cv.glmnet(all.data,y=as.factor(
  train.diabetes$readmitted), alpha=0.9, family="binomial",
  type.measure="auc", nfolds=5, intercept=F)

plot(all.cv)


text.preds<-as.logical(predict(text.cv,
                               dtm,type='class', s=text.cv$lambda.min))

text.roc<-roc((train.diabetes$readmitted*1),
              text.preds*1)


no.text.preds<-as.logical(predict(no.text.cv,
                                  no.text,type='class', s=no.text.cv$lambda.min))
no.text.roc<-roc((train.diabetes$readmitted*1),
                 no.text.preds*1)
all.data.preds<-as.logical(predict(all.cv,
                                   all.data,type='class', s=all.cv$lambda.min))
all.data.roc<-roc((train.diabetes$readmitted*1),
                  all.data.preds*1)

plot(text.roc,col="blue",main="BLUE = Text, RED = No
Text, GREEN=All",adj=0)
plot(no.text.roc, add=TRUE,col="red", lty=2)
plot(all.data.roc,add=TRUE,col="darkgreen", lty=3)

confusion<-confusionMatrix(as.factor(all.data.preds),
                           as.factor(train.diabetes$readmitted))
#########Verificacion

test.it<-itoken(test.diabetes$diag.text,
                preprocess_function = tolower,
                tokenizer = word_tokenizer)
test.dtm<-create_dtm(test.it,vectorizer)
                         

test.no.text<-as.matrix(test.diabetes[,1:132])

new.patients<-cBind(test.dtm,test.no.text)

test.preds<-predict(all.cv,new.patients,
                    type='class',s=all.cv$lambda.min)

test.confusion<-confusionMatrix(as.factor(test.preds),
                                as.factor(test.diabetes$readmitted))
test.precision <- test.confusion$byClass["Pos Pred Value"]
test.recall <- test.confusion$byClass['Sensitivity']
test.f1 <- 2 * ((test.precision * test.recall) /
                  (test.precision + test.recall))

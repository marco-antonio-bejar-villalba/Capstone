library(tm)
library(Matrix)
library(glmnet)
library(caret)
library(pROC)
library(ggthemes)
library(ggplot2)
library(arm)

headline.clean<-function(x){
  x<-tolower(x)
  x<-removeWords(x,stopwords('en'))
  x<-removePunctuation(x)
  x<-stripWhitespace(x)
  return(x)
}
install.packages("RTextTools")
library(RTextTools)
trace("create_matrix",edit=T) # no necesario

match.matrix <- function(text.col,
                         original.matrix=NULL,
                         weighting=weightTf)
{
  control <- list(weighting=weighting)
  training.col <-
    sapply(as.vector(text.col,mode="character"),iconv,
           to="UTF8",sub="byte")
  corpus <- VCorpus(VectorSource(training.col))
  matrix <- DocumentTermMatrix(corpus,control=control);
  if (!is.null(original.matrix)) {
    terms <-
      colnames(original.matrix[,
                               which(!colnames(original.matrix) %in% colnames(matrix))])
    weight <- 0
    if (attr(original.matrix,"weighting")[2] =="tfidf")
      weight <- 0.000000001
    amat <- matrix(weight,nrow=nrow(matrix),
                   ncol=length(terms))
    colnames(amat) <- terms
    rownames(amat) <- rownames(matrix)
    fixed <- as.DocumentTermMatrix(
      cbind(matrix[,which(colnames(matrix) %in%
                            colnames(original.matrix))],amat),
      weighting=weighting)
    matrix <- fixed
  }
  matrix <- matrix[,sort(colnames(matrix))]
  gc()
  return(matrix)
  }

headlines<-read.csv('all_3k_headlines.csv')
train<-createDataPartition(headlines$y,p=0.5,list=F)
train.headlines<-headlines[train,]
test.headlines<-headlines[-train,]

clean.train<-headline.clean(train.headlines$headline)
train.dtm <- match.matrix(clean.train,
                          weighting=tm::weightTfIdf)

train.matrix<-as.matrix(train.dtm)
train.matrix<-Matrix(train.matrix, sparse=T)


dim(train.matrix)
train.matrix[1:5,1:25]

cv<-cv.glmnet(train.matrix,
              y=as.factor(train.headlines$y), alpha=1,
              family='binomial', nfolds=10, intercept=F,
              type.measure = 'class')

plot(cv)

preds<-predict(cv,train.matrix,type="class",
               s=cv$lambda.1se)

train.auc<-roc(train.headlines$y,as.numeric(preds))
train.auc
plot(train.auc)

clean.test<-headline.clean(test.headlines$headline)
test.dtm<-match.matrix(clean.test,
                       weighting=tm::weightTfIdf,
                       original.matrix=train.dtm)

test.matrix<-as.matrix(test.dtm)
test.matrix<-Matrix(test.matrix)

preds<-predict(cv,test.matrix,type="class",
               s=cv$lambda.min)
headline.preds<-data.frame(doc_row =
                             rownames(test.headlines),class=preds[,1])

confusion<-table(headline.preds[,2],test.headlines$y)
sum(diag(confusion))/sum(confusion)
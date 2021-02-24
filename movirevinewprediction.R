library(data.table)
library(pbapply)
library(text2vec)
library(caret)
library(glmnet)
library(qdap)
library(tm)
library(Metrics)
library(tidyr)
library(ggthemes)

movie.data<-fread('2k_movie_reviews.csv')

review.clean<-function(x){
  x<-replace_contraction(x)
  x<-removePunctuation(x)
  x<-stripWhitespace(x)
  x<-removeNumbers(x)
  x<-tolower(x)
  x<-stemmer(x)
  return(x)
}
clean.text<-review.clean(movie.data$train.movies)
y<-movie.data$opening_weekend
train<-createDataPartition(y,p=0.8,list=F)
train.movies<-clean.text[train]
train.y<-y[train]
test.movies<-clean.text[-train]
test.y<-y[-train]

iter.maker<-itoken(train.movies, tokenizer =
                     word_tokenizer)
v <- create_vocabulary(iter.maker,
                       stopwords=c(stopwords('SMART'),'movie','movies'))

pruned.v<-prune_vocabulary(v, term_count_min = 10,
                           doc_proportion_max = 0.5, doc_proportion_min = 0.001)

vectorizer <- vocab_vectorizer(pruned.v)
it <- itoken(train.movies, tokenizer = word_tokenizer)
dtm <- create_dtm(it, vectorizer)

text.cv<-cv.glmnet(dtm,train.y,alpha=1,family=
                     'gaussian', type.measure='mse', nfolds=5, intercept=T)

plot(text.cv)
title("Movie Reviews predict Revenue")

text.preds<-predict(text.cv,dtm,s=text.cv$lambda.min)

train.dat<-data.frame(actual=train.y,
                      preds=text.preds[,1])

train.dat<-data.frame(actual=train.y,
                      preds=text.preds[,1])
rmse(train.dat$actual,train.dat$preds)


mae(train.dat$actual,train.dat$preds)

train.tidy<-gather(train.dat)

ggplot(train.tidy, aes(x=key, y=value, fill=key)) +
  geom_boxplot()+theme_gdocs()

ggplot(train.dat, aes(x=actual, y=preds)) +
  geom_point(color='darkred',shape=1) +
  stat_smooth(method=lm) + theme_gdocs()


#Validation
test.text<-review.clean(test.movies)
test.it<-itoken(test.text, preprocess_function =
                  tolower, tokenizer = word_tokenizer)
test.dtm<-create_dtm(test.it,vectorizer)

test.preds<-predict(text.cv,test.dtm,
                    s=text.cv$lambda.min)

rmse(test.y,test.preds)
mae(test.y,test.preds)

test.dat<-data.frame(actual=test.y,
                     preds=test.preds[,1])
ggplot(test.dat, aes(x=actual, y=preds)) +
  geom_point(color='darkred',shape=1) +
  stat_smooth(method=lm) + theme_gdocs()

install.packages('openNLP')
install.packages("openNLPmodels.en",repos = "http://
datacube.wu.ac.at/",type = "source")

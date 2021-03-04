library(tm)
library(SnowballC)
library(data.table)
library(tidytext)
library(dplyr)
library(stringr)
library(text2vec)

source("constants.R")
source("utils.R")

downloadTextData()

rawTextData<-readTextData(ENGLISH_ID)

dataf<-data.table(text=linesToProcess)

#dataf$text<-removeWords(dataf$text,stopwords('en'))
dataf$text<-str_remove_all(dataf$text,"#")
dataf$text<-str_remove_all(dataf$text,"–")
dataf$text<-str_remove_all(dataf$text,"-")
dataf$text<-str_remove_all(dataf$text,"“")
dataf$text<-str_remove_all(dataf$text,"”")
dataf$text<- removePunctuation(dataf$text)
dataf$text<- removeNumbers(dataf$text)
dataf$text<- stripWhitespace(dataf$text)

dataf$text<- tolower(dataf$text)

tokens = space_tokenizer(dataf$text)

vocab <- create_vocabulary(itoken(tokens),ngram = c(3,3))
 
vocab<- prune_vocabulary(vocab, term_count_min = 5)

getnToken<-function(x,n)
{
  x[n]
}

splitted<-strsplit(vocab$term,"_")
vocabbis<-mutate(vocab,unigram=sapply(splitted,getnToken,1),bigram=sapply(splitted,getnToken,2),trigram=sapply(splitted,getnToken,3))

totalizedByUnigram<- vocabbis %>% group_by(unigram) %>% summarise(totalUnigram=sum(term_count))
totalizedByBigram<- vocabbis %>% group_by(unigram,bigram) %>% summarise(totalBigram=sum(term_count))

markovNet<-inner_join(vocabbis,totalizedByUnigram)
markovNet<-inner_join(markovNet,totalizedByBigram)

markovNet<-mutate(markovNet,pbigram=totalBigram/totalUnigram,ptrigram=term_count/totalBigram)

markovNet<-mutate(markovNet,term=NULL,term_count=NULL,doc_count=NULL,totalUnigram=NULL,totalBigram=NULL)

# 
# iter <- itoken(tokens)
# 
# vectorizer <- vocab_vectorizer(vocab)
# tcm <- create_dtm(iter, vectorizer)
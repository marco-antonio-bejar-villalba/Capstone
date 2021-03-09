library(tm)
library(SnowballC)
library(data.table)
library(tidytext)
library(dplyr)
library(stringr)
library(text2vec)
library(wordcloud)
library(stringdist)

source("constants.R")
source("utils.R")

downloadTextData()

rawTextData<-readTextData(ENGLISH_ID)

dataTable<-data.table(text=rawTextData)

dataTable<-cleanText(dataTable)

tokens = space_tokenizer(dataTable$text)

vocab3gram <- create_vocabulary(itoken(tokens),ngram = c(3,3))

vocab2gram <- create_vocabulary(itoken(tokens),ngram = c(2,2))

vocab1gram <- create_vocabulary(itoken(tokens),ngram = c(1,1))
 
vocab3gram <- prune_vocabulary(vocab3gram, term_count_min = 5)

vocab2gram <- prune_vocabulary(vocab2gram, term_count_min = 5)

vocab1gram <- prune_vocabulary(vocab1gram, term_count_min = 5)


markovNet3Gram<-getMarkovNet(vocab3gram,3)

markovNet2Gram<-getMarkovNet(vocab2gram,2)

vocab1gram<-data.table(vocab1gram)

setkey(vocab1gram,term)
setindex(vocab1gram,term_count)

example<-"cream cheese wit"

tokensEx<-getTokens(str_trim(example))

wordFound="ss"

beginingWord<-isBeginingWord(example)

if(beginingWord && length(tokensEx[[1]])>=2){
  wordFound=findTrigramWord(markovNet3Gram,tokensEx[[1]])
  if(nrow(wordFound)==0){
    wordFound=findBigramWord(markovNet2Gram,tokensEx[[1]])
  }
}

if(!beginingWord &&  length(tokensEx[[1]])>=2){
  lenghtList<-length(tokensEx[[1]])
  wordsToProcess<-findBigramWordIndexes(markovNet2Gram,tokensEx[[1]],number=500,lenghtList-1)
  if(nrow(wordsToProcess)!=0){
    stringDistances<-stringdist(tokensEx[[1]][lenghtList],wordsToProcess$bigram,method="dl")
    wordFound<-wordsToProcess[order(stringDistances)]
  }
}



findTrigramWord<-function(markovNet3Gram,tokensEx,number=10){
  lenghtList<-length(tokensEx)
  findTrigramWordIndexes(markovNet3Gram,tokensEx,number=10,lenghtList-1,lenghtList)
}

findTrigramWordIndexes<-function(markovNet3Gram,tokensEx,number=10,indexWord1,indexWord2){
  wordsFound<-markovNet3Gram[unigram==tokensEx[indexWord1] & bigram==tokensEx[indexWord2],.(trigram,ptrigram), keyby=.(-ptrigram)]
  head(wordsFound,number)
}

findBigramWord<-function(markovNet2Gram,tokensEx,number=10){
  lenghtList<-length(tokensEx)
  findBigramWordIndexes(markovNet2Gram,tokensEx,number=10,lenghtList)
}

findBigramWordIndexes<-function(markovNet2Gram,tokensEx,number=10,indexWord1){
  wordsFound<-markovNet2Gram[unigram==tokensEx[indexWord1],.(bigram,pbigram), keyby=.(-pbigram)]
  head(wordsFound,number)  
}

isBeginingWord<-function(stringToProcess){
  wordLength<-nchar(stringToProcess)
  str_sub(example,wordLength)==" "
}

getTokens<-function(stringToProcess){
  tempTable<-data.table(text=stringToProcess)
  stringCleaned<-cleanText(tempTable)
  tokens<-space_tokenizer(stringCleaned)
  tokens
}


##WordDistance

stringdist('crabapple','apple',method="lcs")

##WordClouds

wordcloud(vocab1gram$term,vocab1gram$term_count, max.words =
            100, colors=c('black','darkred'))

pal <- brewer.pal(9,"PuBu")
pal <- pal[-(1:2)]
wordcloud(vocab3gram$term,vocab3gram$term_count,c(4,0.5),2,200,FALSE,,.15,pal)

pal <- brewer.pal(9,"BuPu")
pal <- pal[-(1:2)]
wordcloud(vocab2gram$term,vocab2gram$term_count,c(4,1),2,200,FALSE,,.15,pal)




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

save(markovNet2Gram, file = "MarkovNetworkBiGram.RData")
saveRDS(markovNet2Gram,file = "MarkovNetworkBiGram.RDS")
saveRDS(markovNet3Gram,file = "MarkovNetworkTriGram.RDS")
save(markovNet3Gram, file = "MarkovNetworkTriGram.RData")
save(vocab1gram, file = "MarkovNetworkUniGram.RData")
saveRDS(vocab1gram,file = "MarkovNetworkUniGram.RDS")


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




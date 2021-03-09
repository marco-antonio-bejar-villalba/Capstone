options(encoding = "UTF-8")

library(tm)
library(SnowballC)
library(data.table)
library(tidytext)
library(dplyr)
library(stringr)
library(text2vec)

strURL<-"https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
strZIPFile<-"dataCapstone.zip"
strDir<-"final"
########################### Files and directories ###############

strDirDeutsch<-"de_DE"
strDirEnglish<-"en_US"
strDirFinnish<-"fi_FI"
strDirRussian<-"ru_RU"

strFileDeutschBlogs<-file.path(strDir,strDirDeutsch,"de_DE.blogs.txt")
strFileDeutschNews<-file.path(strDir,strDirDeutsch,"de_DE.news.txt")
strFileDeutschTwitter<-file.path(strDir,strDirDeutsch,"de_DE.twitter.txt")

strFileEnglishBlogs<-file.path(strDir,strDirEnglish,"en_US.blogs.txt")
strFileEnglishNews<-file.path(strDir,strDirEnglish,"en_US.news.txt")
strFileEnglishTwitter<-file.path(strDir,strDirEnglish,"en_US.twitter.txt")

strFileFinnishBlogs<-file.path(strDir,strDirFinnish,"fi_FI.blogs.txt")
strFileFinnishNews<-file.path(strDir,strDirFinnish,"fi_FI.news.txt")
strFileFinnishTwitter<-file.path(strDir,strDirFinnish,"fi_FI.twitter.txt")

strFileRussianBlogs<-file.path(strDir,strDirRussian,"ru_RU.blogs.txt")
strFileRussianNews<-file.path(strDir,strDirRussian,"ru_RU.news.txt")
strFileRussianTwitter<-file.path(strDir,strDirRussian,"ru_RU.twitter.txt")


if(!file.exists(strZIPFile)){
  download.file(strURL,strZIPFile)
}

if(!dir.exists(strDir)){
  unzip(strZIPFile)
}

fileBlogsEn<-file(strFileEnglishBlogs)
linesBlogsEn<-readLines(fileBlogsEn)
close(fileBlogsEn)

fileBlogsEn<-file(strFileEnglishBlogs)
linesBlogsEn<-readLines(fileBlogsEn)
close(fileBlogsEn)

fileBlogsEn<-file(strFileEnglishBlogs)
linesBlogsEn<-readLines(fileBlogsEn)
close(fileBlogsEn)

set.seed(33234)
linesBlogsEn<-sample(linesBlogsEn,50000)

dataf<-data.table(text=linesBlogsEn)

dataf$text<-removeWords(dataf$text,stopwords('en'))
dataf$text<-str_remove_all(dataf$text,"–")
dataf$text<-str_remove_all(dataf$text,"-")
dataf$text<-str_remove_all(dataf$text,"“")
dataf$text<-str_remove_all(dataf$text,"”")
dataf$text<- removePunctuation(dataf$text)
dataf$text<- stripWhitespace(dataf$text)
dataf$text<- removeNumbers(dataf$text)
dataf$text<- tolower(dataf$text)

#tokens <- strsplit(dataf$text, split = " ",
#                   fixed = T)

tokens = space_tokenizer(dataf$text)

#vocab <- create_vocabulary(itoken(tokens),ngram = c(1,1))

vocabbigram <- create_vocabulary(itoken(tokens),ngram = c(3,3))

vocab<- prune_vocabulary(vocabbigram, term_count_min = 5)

iter <- itoken(tokens)

vectorizer <- vocab_vectorizer(vocab)
tcm <- create_tcm(iter, vectorizer)
dtm <- create_dtm(iter, vectorizer)

glove <- GlobalVectors$new(rank = 50, x_max = 10)

wv_main<-glove$fit_transform(tcm, n_iter=30, convergence_tol = 0.01, n_threads = 8)

wv_context<-glove$components

word_vectors<-wv_main +t(wv_context)

holy<-word_vectors["fish",,drop=FALSE]
ghost<-word_vectors["love",,drop=FALSE]
to<-word_vectors["to",,drop=FALSE]
cos_sim<-sim2(x=word_vectors,y=(holy),method="cosine",norm="l2")
head(sort(cos_sim[,1], decreasing = TRUE), 50)

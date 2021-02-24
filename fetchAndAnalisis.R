library(tm)
library(SnowballC)

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

txt <- system.file("texts", "txt", package = "tm")
(ovid <- Corpus(DirSource(txt),
                 readerControl = list(reader = readPlain,
                                        language = "la",
                                        load = TRUE)))


ovid <- appendMeta(ovid,
                    cmeta = list(test = c(1,2,3)),
                    dmeta = list(clust = c(1,1,2,2,2)))

reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- Corpus(DirSource(reut21578),readerControl = list(reader = readReut21578XML))

clean.corpus<-function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords,
                   stopwords('english'))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  return(corpus)
}
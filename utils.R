##### funtions just used for getting the data and utility stuff
source("constants.R")

downloadTextData<-function(){
  
  if(!file.exists(strZIPFile)){
    download.file(strURL,strZIPFile)
  }
  
  if(!dir.exists(strDir)){
    unzip(strZIPFile)
  }
  
}


getFileName<-function(language=ENGLISH_ID,type=TWITTER_ID){
  
  strIdiomCode<-switch (language,
                        "du" = IDIOM_CODE_DEUTSCH,
                        "fn" = IDIOM_CODE_FINNISH,
                        "en" = IDIOM_CODE_ENGLISH,
                        "ru" = IDIOM_CODE_RUSSIAN
  )
  
  strFileName<-paste(strIdiomCode,".",type,".txt",sep="")

  file.path(strDir,strIdiomCode,strFileName)
}

readTextData<-function(language=ENGLISH_ID){
  

  strFileBlogs<-getFileName(language,BLOGS_ID)
  strFileNews<-getFileName(language,NEWS_ID)
  strFileTwitter<-getFileName(language,TWITTER_ID)
  
  
  fileBlogs<-file(strFileBlogs)
  linesBlogs<-readLines(fileBlogs,skipNul = TRUE,n=linesToReadBlogs,encoding = "UTF-8")
  close(fileBlogs)
  
  fileNews<-file(strFileNews)
  linesNews<-readLines(fileNews,skipNul=TRUE,n=linesToReadNews,encoding = "UTF-8")
  close(fileNews)
  
  fileTwitter<-file(strFileTwitter)
  linesTwitter<-readLines(fileTwitter,skipNul=TRUE,n=linesToReadTwitter,encoding = "UTF-8")
  close(fileTwitter)
  
  set.seed(33234)
  linesBlogs<-sample(linesBlogs,linesToReadBlogs)
  linesTwitter<-sample(linesTwitter,linesToReadTwitter)
  linesNews<-sample(linesNews,linesToReadNews)
  
  linesToProcess<-c(linesBlogs,linesNews,linesTwitter)
  
  linesToProcess
}

cleanText<-function(dataTable){
  #dataTable$text<-removeWords(dataTable$text,stopwords('en'))
  dataTable$text<- str_remove_all(dataTable$text,"#")
  dataTable$text<- str_remove_all(dataTable$text,"–")
  dataTable$text<- str_remove_all(dataTable$text,"-")
  dataTable$text<- str_remove_all(dataTable$text,"“")
  dataTable$text<- str_remove_all(dataTable$text,"”")
  dataTable$text<- str_remove_all(dataTable$text,"“")
  dataTable$text<- removePunctuation(dataTable$text)
  dataTable$text<- removeNumbers(dataTable$text)
  dataTable$text<- stripWhitespace(dataTable$text)
  dataTable$text<- tolower(dataTable$text)
  dataTable
}

getnToken<-function(x,n)
{
  x[n]
}

getMarkovNet<-function(vocab, ngram=2){
  
  splitted<-strsplit(vocab$term,"_")
  
  if(ngram==3){
    
    vocabbis<-mutate(vocab,unigram=sapply(splitted,getnToken,1),bigram=sapply(splitted,getnToken,2),trigram=sapply(splitted,getnToken,3))
    
    totalizedByUnigram<- vocabbis %>% group_by(unigram) %>% summarise(totalUnigram=sum(term_count))
    
    totalizedByBigram<- vocabbis %>% group_by(unigram,bigram) %>% summarise(totalBigram=sum(term_count))
    
    markovNet<-inner_join(vocabbis,totalizedByUnigram)
    
    markovNet<-inner_join(markovNet,totalizedByBigram)
    
    markovNet<-mutate(markovNet,pbigram=totalBigram/totalUnigram,ptrigram=term_count/totalBigram)
    
    markovNet<-mutate(markovNet,term=NULL,term_count=NULL,doc_count=NULL,totalUnigram=NULL,totalBigram=NULL)
    
    markovNet<-data.table(markovNet)
    
    setkey(markovNet,unigram,bigram,trigram)
    
    setindex(markovNet,unigram)
    
    setindex(markovNet,unigram,bigram)
  }
  else{
    
    vocabbis<-mutate(vocab,unigram=sapply(splitted,getnToken,1),bigram=sapply(splitted,getnToken,2))
    
    totalizedByBigram<- vocabbis %>% group_by(unigram) %>% summarise(totalBigram=sum(term_count))
    
    markovNet<-inner_join(vocabbis,totalizedByBigram)
    
    markovNet<-mutate(markovNet,pbigram=term_count/totalBigram)
    
    markovNet<-mutate(markovNet,term=NULL,term_count=NULL,doc_count=NULL,totalBigram=NULL)
    
    markovNet<-data.table(markovNet)
    
    setkey(markovNet,unigram,bigram)
    
    setindex(markovNet,unigram)
  }
  
  

  
  markovNet
}
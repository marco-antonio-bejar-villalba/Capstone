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
  linesBlogs<-readLines(fileBlogs,skipNul = TRUE,n=linesToReadBlogs)
  close(fileBlogs)
  
  fileNews<-file(strFileNews)
  linesNews<-readLines(fileNews,skipNul=TRUE,n=linesToReadNews)
  close(fileNews)
  
  fileTwitter<-file(strFileTwitter)
  linesTwitter<-readLines(fileTwitter,skipNul=TRUE,n=linesToReadTwitter)
  close(fileTwitter)
  
  set.seed(33234)
  linesBlogs<-sample(linesBlogs,linesToReadBlogs)
  linesTwitter<-sample(linesTwitter,linesToReadTwitter)
  linesNews<-sample(linesNews,linesToReadNews)
  
  linesToProcess<-c(linesBlogs,linesNews,linesTwitter)
  
  linesToProcess
}
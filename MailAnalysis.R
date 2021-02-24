options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')
library(gridExtra)
library(ggmap)
library(ggthemes)
library(NLP)
library(openNLP)
library("openNLPmodels.en", lib.loc="~/R/
win-library/3.2")
library(pbapply)
library(stringr)
library(rvest)
library(doBy)
library(tm)
library(cshapes)

temp <- file.path("C8_final_txts",list.files(path="C8_final_txts",pattern='*.txt'))
for (i in 1:length(temp)) assign(temp[i],
                                 readLines(temp[i]))
all.emails<-pblapply(temp, get)

txt.clean<-function(x){
  x<-x[-1]
  x<-paste(x,collapse= " ")
  x<-str_replace_all(x,
                     "[a-zA-Z0-9_.+-]+@[a-zAZ0-9-]+\\.[a-zA-Z0-9-.]+", "")
  x<-str_replace_all(x,
                     "Doc No.","")
  x<-str_replace_all(x,
                     "UNCLASSIFIED U.S. Department of State Case No.","")
  x<-removeNumbers(x)
  x<-as.String(x)
  return(x)
}

all.emails<-pblapply(all.emails,txt.clean)

all.emails[[3]][18,24]
names(all.emails)<-temp

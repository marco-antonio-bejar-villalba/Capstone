source("utils.R")

markovNet3Gram<-readRDS("MarkovNetworkTriGram.RDS")
markovNet2Gram<-readRDS("MarkovNetworkBiGram.RDS")
markovNet1Gram<-readRDS("MarkovNetworkUniGram.RDS")

example<-"new man in "

getPredictedWords(example,20)


getPredictedWords<- function(string,numberofsamples=10){
  
  tokensEx<-getTokens(str_trim(example))[[1]]
  
  wordFound=NULL
  
  beginingWord<-isBeginingWord(example)
  
  if(beginingWord && length(tokensEx)>=2){
    wordFound=findTrigramWord(markovNet3Gram,tokensEx, numberofsamples)
    print("fase 1")
    if(nrow(wordFound)==0){
      wordFound=findBigramWord(markovNet2Gram,tokensEx, numberofsamples)
      print("fase 2")
    }
  }
  
  ##TODO cuando hay una palabra y se va a agregar otra el caso no está cubierto

  if(beginingWord && length(tokensEx)==1){
    wordFound=findBigramWord(markovNet2Gram,tokensEx, numberofsamples)
    print("fase 2 bis")
  }
  
  
  if(!beginingWord &&  length(tokensEx)>=2){
    lenghtList<-length(tokensEx)
    wordsToProcess<-findBigramWordIndexes(markovNet2Gram,tokensEx,1000,lenghtList-1)
    print("fase 3")
    if(nrow(wordsToProcess)!=0){
      stringDistances<-stringdist(tokensEx[lenghtList],wordsToProcess$bigram,method="dl")
      wordFound<-head(wordsToProcess[order(stringDistances)],numberofsamples)
      print("fase 4")
    }
  }
  
  if(is.null(wordFound) || nrow(wordFound)==0){
    print("fase 5")
    if(!beginingWord){
      word<-tokensEx[lenghtList]
      stringDistances<-stringdist(word,substr(markovNet1Gram$term,1,nchar(word))+1,method="dl")
      wordFound<-head(markovNet1Gram[order(stringDistances)],numberofsamples)
      print("fase 6")
    }
  }

  wordFound
}

findTrigramWord<-function(markovNet3Gram,tokensEx,number=10){
  lenghtList<-length(tokensEx)
  findTrigramWordIndexes(markovNet3Gram,tokensEx,number,lenghtList-1,lenghtList)
}

findTrigramWordIndexes<-function(markovNet3Gram,tokensEx,number=10,indexWord1,indexWord2){
  wordsFound<-markovNet3Gram[unigram==tokensEx[indexWord1] & bigram==tokensEx[indexWord2],.(trigram,ptrigram), keyby=.(-ptrigram)]
  head(wordsFound,number)
}

findBigramWord<-function(markovNet2Gram,tokensEx,number=10){
  lenghtList<-length(tokensEx)
  findBigramWordIndexes(markovNet2Gram,tokensEx,number,lenghtList)
}

findBigramWordIndexes<-function(markovNet2Gram,tokensEx,number=10,indexWord1){
  wordsFound<-markovNet2Gram[unigram==tokensEx[indexWord1],.(bigram,pbigram), keyby=.(-pbigram)]
  head(wordsFound,number)  
}

isBeginingWord<-function(stringToProcess){
  wordLength<-nchar(stringToProcess)
  str_sub(example,wordLength)==" " || nchar(str_trim(stringToProcess,side="both"))==0
}

getTokens<-function(stringToProcess){
  tempTable<-data.table(text=stringToProcess)
  stringCleaned<-cleanText(tempTable)
  tokens<-space_tokenizer(stringCleaned)
  tokens
}

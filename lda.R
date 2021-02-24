##Topic preticytion with lda
library(tm)
library(qdap)
library(lda)
library(GuardianR)
library(pbapply)
library(LDAvis)
library(treemap)
library(car)
options(stringsAsFactors = F)
text<-read.csv(
  'Guardian_articles_11_14_2015_12_1_2015.csv')

articles <- iconv(text$body, "latin1", "ASCII", sub="")
articles <- gsub('http\\S+\\s*', '', articles)
articles <- bracketX(articles,bracket='all')
articles <- gsub("[[:punct:]]", "",articles)
articles <- removeNumbers(articles)
articles <- tolower(articles)
articles <-removeWords(articles,c(stopwords('en'),
                                  'pakistan','gmt','england'))

blank.removal<-function(x){
  x<-unlist(strsplit(x,' '))
  x<-subset(x,nchar(x)>0)
  x<-paste(x,collapse=' ')
}
articles<-pblapply(articles,blank.removal)

documents <- lexicalize(articles)

wc <- word.counts(documents$documents,
                  documents$vocab)
doc.length<- document.lengths(documents$documents)

k <- 4
num.iter <- 25
alpha <- 0.02
eta <- 0.02
set.seed(1234)
fit <- lda.collapsed.gibbs.sampler(documents =
                                     documents$documents, K = k, vocab = documents$vocab,
                                   num.iterations = num.iter, alpha = alpha, eta = eta,
                                   initial = NULL, burnin = 0,compute.log.likelihood =
                                     TRUE)

plot(fit$log.likelihoods[1,])

top.topic.words(fit$topics, 7, by.score=TRUE)

top.topic.documents(fit$document_sums,1)

theta <- t(pbapply(fit$document_sums + alpha, 2,
                   function(x) x/sum(x)))
phi <- t(pbapply(t(fit$topics) + eta, 2, function(x)
  x/sum(x)))

article.json <- createJSON(phi = phi,theta = theta,
                           doc.length = doc.length, vocab =
                             documents$vocab,
                           term.frequency = as.vector(wc))
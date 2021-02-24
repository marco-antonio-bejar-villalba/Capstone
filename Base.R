
#####clustering
options(stringsAsFactors = F)
set.seed(1234)
library(skmeans)
library(tm)
library(clue)
library(cluster)
library(fpc)
library(clue)
library(wordcloud)
clean.corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords,
                   c(stopwords("en"), "customer",
                     "service","customers","calls"))
  return(corpus)
}
wk.exp<-read.csv('1yr_plus_final4.csv', header=T)
wk.source <- VCorpus(VectorSource(wk.exp$text))
wk.corpus<-clean.corpus(wk.source)
wk.dtm<-DocumentTermMatrix(wk.corpus,
                           control=list(weighting= weightTfIdf))
wk.dtm.s<-scale(wk.dtm,scale=T)
wk.clusters<-kmeans(wk.dtm.s,3)
barplot(wk.clusters$size, main='k-means')
plotcluster(cmdscale(dist(wk.dtm)),
            wk.clusters$cluster)
dissimilarity.m <- dist(wk.dtm.s)
plot(silhouette(wk.clusters$cluster, dissimilarity.m))
work.clus.proto<-t(cl_prototypes(wk.clusters))
comparison.cloud(work.clus.proto, max.words=100)
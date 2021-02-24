wk.dtm<-DocumentTermMatrix(wk.corpus,
                           control=list(weighting= weightTfIdf))
wk.mediods<-pamk(wk.dtm, krange=2:4, critout = T)
dissimilarity.m <- dist(wk.dtm)
plot(silhouette(wk.mediods$pamobject$clustering,
                dissimilarity.m))
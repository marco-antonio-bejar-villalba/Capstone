library(skmeans)
library(clue)
wk.dtm<-DocumentTermMatrix(wk.corpus,
                           control=list(weighting= weightTfIdf))
soft.part <- skmeans(wk.dtm, 3, m = 1.2, control =
                       list(nruns = 5, verbose = T))
barplot(table(soft.part$cluster), main='Spherical
k-means')
plotcluster(cmdscale(dist(wk.dtm)), soft.part$cluster)
plot(silhouette(soft.part))
s.clus.proto<-t(cl_prototypes(soft.part))
comparison.cloud(s.clus.proto, max.words = 100)

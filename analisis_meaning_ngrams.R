library(data.table)
library(text2vec)
library(tm)
text<-fread('Airbnb-boston_only.csv')

airbnb<-data.table(review_id=text$review_id,
                   comments=text$comments,
                   review_scores_rating=text$review_scores_rating)

airbnb$comments<-
  removeWords(airbnb$comments,c(stopwords('en'),'Boston'))
airbnb$comments<- removePunctuation(airbnb$comments)
airbnb$comments<- stripWhitespace(airbnb$comments)
airbnb$comments<- removeNumbers(airbnb$comments)
airbnb$comments<- tolower(airbnb$comments)

tokens <- strsplit(airbnb$comments, split = " ",
                   fixed = T)

vocab <- create_vocabulary(itoken(tokens),ngram = c(1,1))

vocab<- prune_vocabulary(vocab, term_count_min = 5)

vocab[[1]][221:225]

iter <- itoken(tokens)
vectorizer <- vocab_vectorizer(vocab)
tcm <- create_tcm(iter, vectorizer)

fit.glove <- glove(tcm = tcm,
                   word_vectors_size = 50,
                   x_max = 10, learning_rate = 0.2,
                   num_iters = 15)

glove = GlobalVectors$new(rank = 50, x_max = 10)
glove$fit(tcm, n_iter = 20)
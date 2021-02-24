songs <- read.csv("data.csv")

songs$popularity <- (songs$popularity > 80)==TRUE
songs$popularity <- factor(songs$popularity)
levels(songs$popularity)
levels(songs$popularity) <- c("0", "1")
dim(songs)

pop.index <- songs$popularity == "1"
pop.index
pop_songs <- songs[pop.index=="TRUE", ]
dim(pop_songs)

#text mining
library(tm)
pop_songs$name <- gsub("[^[:alnum:]]"," ", pop_songs$name)
corp <- Corpus(VectorSource(pop_songs$name))
inspect(corp)

corp <- tm_map(corp, tolower)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeWords, stopwords("english"))
corp <- tm_map(corp, removeWords, c("feat"))
corp <- tm_map(corp, stripWhitespace)

library(SnowballC)
corp <- tm_map(corp, stemDocument)


dtm <- DocumentTermMatrix(corp)
inspect(dtm)

tfidf <- weightTfIdf(dtm)
inspect(tfidf)

#revoving sparse terms
tfidf <- removeSparseTerms(tfidf, 0.93)
inspect(tfidf)

#wordcloud
m <- as.matrix(tfidf)
library(wordcloud)
head(m)
# find out the importance of each term by summing up the tf-idf scores over the corpus
v <- sort(colSums(m),decreasing=TRUE)
importance <- data.frame(word = names(v), tfidf = v)
importance

wordcloud(importance$word, importance$tfidf,random.order=FALSE, min.freq = 1, max.words=1000, colors=brewer.pal(8, "Dark2"))

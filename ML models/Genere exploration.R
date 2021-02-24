
music_artict_genere <- read.csv("artists.csv")
genere.df <- read.csv("processed_data_by_genres.csv")

#preprocessing
music_artict_genere$genres <- gsub("\\[|\\]", "", music_artict_genere$genres)
music_artict_genere$genres
music_artict_genere$genres <- gsub("\\'|\\'", "", music_artict_genere$genres)
library(stringr)
music_artict_genere$genres <- str_replace_all(music_artict_genere$genres, " ", "_")
music_artict_genere$genres <- str_replace_all(music_artict_genere$genres, ",_", " ")
music_artict_genere$genres[100]
music_artict_genere$genres <- strsplit(music_artict_genere$genres, "\\s+")
music_artict_genere$genres

write.csv(music_artict_genere,"C:\\Users\\nmala\\P2Records\\Datasets\\test1.csv", row.names = FALSE)



#text mining
library(tm)
corp <- Corpus(VectorSource(music_artict_genere$genres))
inspect(corp)

corp <- tm_map(corp, tolower)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removePunctuation)
corp
corp <- tm_map(corp, stripWhitespace)


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
importance$word
importance$tfidf <- music_artict_genere$count * genere.df$popularity[genere.df$genres == music_artict_genere$genres_new]
(genere.df$genres == importance$word) == TRUE
head(genere.df$genres)
freq
head(importance$word)
head(importance)
wordcloud(music_artict_genere$gen, freq = freq,random.order=FALSE, min.freq = 1, max.words=1000, colors=brewer.pal(8, "Dark2"))

for (genre in genere.df$genres){
  
}

freq <- importance$tfidf * 
wordcloud(words = genere.df$genres, freq = genere.df$popularity, min.freq = 1, max.words=1000, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))



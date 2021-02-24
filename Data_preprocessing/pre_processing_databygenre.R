music_genre <- read.csv("data_by_genres.csv")

#removing nulls
music_genre[is.na(music_genre)] <- 0

#combining different terms in a genere
library(stringr)
music_genre$genres <- str_replace_all(music_genre$genres, " ", "_")

#writing dataset to a file
write.csv(music_genre,"C:\\Users\\nmala\\P2Records\\Datasets\\processed_data_by_genres.csv", row.names = FALSE)

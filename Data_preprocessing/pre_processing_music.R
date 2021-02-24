music.df <- read.csv("data.csv")
dim(music.df) #170653 rows
summary(music.df)

#viz
small.index <- sample(1:nrow(music), nrow(music)*0.1)
sample_music <- music[small.index, ]
ggplot(data=sample_music)+ geom_point(aes(x=valence, y=popularity))
ggplot(data=sample_music)+ geom_point(aes(x=year, y=popularity))
ggplot(data=sample_music)+ geom_point(aes(x=acousticness, y=popularity))
ggplot(data=sample_music)+ geom_point(aes(x=danceability, y=popularity))
ggplot(data=sample_music)+ geom_point(aes(x=energy, y=popularity))
ggplot(data=sample_music)+ geom_point(aes(x=loudness, y=popularity))

#removing nulls
music.df[is.na(music.df)] <- 0

#filtering out rows with incomplete date
temp.df <- grep("^[0-9]*?-[0-9]*?-[0-9]*?", music$release_date, value=TRUE, ignore.case=T, perl=T) #extracting all complete
date_index <- which(music$release_date %in% temp.df) #finding indexed of the dates extracted in the last line
processed.df <- music[date_index, ] #filtering the dataset using the indexes obtained
dim(processed.df) #118188 rows

#removing special charecters values from song names
library(stringr)
processed.df$name <- str_replace_all(processed.df$name, "[^[:alnum:]]", " ")
processed.df$name <- str_replace_all(processed.df$name, "[[:punct:]]", " ")

#writing dataset to a file
write.csv(processed.df,"C:\\Users\\nmala\\P2Records\\Data sets\\processed_music.csv", row.names = FALSE)


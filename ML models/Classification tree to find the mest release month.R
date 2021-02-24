library(caret)
music <- read.csv("processed_music.csv")
music <- na.omit(music)

#preprocess
music.p.df <- subset(music, select = -c(year, artists, name, mode, release_date))
#music.p.df <- scale(music.p.df)

#categorise features
music.p.df$release_month <- factor(music.p.df$release_month)
levels(music.p.df$release_month) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

music.p.df$explicit <- factor(music.p.df$explicit)
#music.p.df$explicit <- relevel(music.p.df$explicit, ref = 1)

#music.p.df$year <- factor(music.p.df$year)
music.p.df$key <- factor(music.p.df$key)
#music.p.df$mode <- factor(music.p.df$mode)
str(music.p.df)

#consider only popular songs
music.p.df$popularity <- (music.p.df$popularity > 70)==TRUE
music.p.df$popularity <- factor(music.p.df$popularity)
levels(music.p.df$popularity)
levels(music.p.df$popularity) <- c("0", "1")
str(music.p.df)
dim(music.p.df)
pop.index <- music.p.df$popularity == "1"
pop.index
pop_songs <- music.p.df[pop.index=="TRUE", ]
pop_songs <- subset(pop_songs, select = -c(popularity))
dim(pop_songs)
colnames(pop_songs)


#rename average duration
library(tidyverse)
pop_songs <- pop_songs %>% 
  rename(
    "Song_duration" =  "Avg.song.duration..sec."
  )

#prepare training and test dataset
library(rpart)
library(rpart.plot)
#set.seed(12)
set.seed(134234)
train.index <- sample(1:nrow(pop_songs), nrow(pop_songs)*0.6)  
train.df <- pop_songs[train.index, ]
valid.df <- pop_songs[-train.index, ]  

#model

default.ct <- rpart(release_month ~ ., data = train.df, method = "class")
prp(default.ct)
prp(default.ct, type=1, extra = 1)

full.ct <- rpart(release_month ~ ., data = train.df, method = "class", control = rpart.control(minsplit = 1))
prp(full.ct)


#perfomance evaluation
default.ct.point.pred <- predict(default.ct, valid.df, type = "class")
confusionMatrix(default.ct.point.pred, factor(valid.df$release_month))

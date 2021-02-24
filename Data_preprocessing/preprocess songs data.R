music <- read.csv("data.csv")
dim(music)
summary(music)
str(music)
View(music)

library(ggplot2)
#preproessing

music$loudness[is.na(music$loudness)] <- 0
music$popularity[is.na(music$popularity)] <- 0

#viz
small.index <- sample(1:nrow(music), nrow(music)*0.1)
sample_music <- music[small.index, ]
ggplot(data=sample_music)+ geom_point(aes(x=valence, y=))
ggplot(data=sample_music)+ geom_point(aes(x=year, y=popularity))
ggplot(data=sample_music)+ geom_point(aes(x=acousticness, y=popularity))
ggplot(data=sample_music)+ geom_point(aes(x=danceability, y=popularity))
ggplot(data=sample_music)+ geom_point(aes(x=energy, y=popularity))
ggplot(data=sample_music)+ geom_point(aes(x=loudness, y=popularity))


music$popularity <- (music$popularity > 3)==TRUE
music$popularity <- factor(music$popularity)

levels(music$popularity) <- c("N", "Y")
music$popularity



selected.index <- c(1,2,3,5,6,7,8, 11,12, 13, 14, 16, 18, 19)
selected.df <- music[,selected.index]

train.index <- sample(1:nrow(music), nrow(music)*0.5)

train.df <- selected.df[train.index, ]
valid.df <- selected.df[-train.index, ]

logit <- glm(popularity ~. , data=train.df, family = "binomial")
summary(logit)

logit.pred <- predict(logit, valid.df, type = "response")
logit.pred
pred <- ifelse(logit.pred >0.6, 1, 0)
pred
library(caret)
confusionMatrix(factor(pred), factor(valid.df$popularity), positive = "1")

#Reference
#Prediction     0     1
#0 15636  3094
#1  1384 65213

#Accuracy : 0.9475


selected.index1 <- c(2,3,5,7,8, 16, 18, 19)
selected.df1 <- music[,selected.index1]

train.index1 <- sample(1:nrow(music), nrow(music)*0.8)

train.df1 <- selected.df1[train.index1, ]
valid.df1 <- selected.df1[-train.index1, ]

logit1 <- glm(popularity ~. , data=train.df1, family = "binomial")
summary(logit1)

logit.pred1 <- predict(logit1, valid.df1, type = "response")
logit.pred1
pred1 <- ifelse(logit.pred1 >0.6, 1, 0)
pred1
confusionMatrix(factor(pred1), factor(valid.df1$popularity), positive = "1")

#Reference
#Prediction     0     1
#0  6228  1274
#1   565 26064
#
#Accuracy : 0.9461 

# Time of year -> popularity depending on month of release?
library(lubridate)
some_date <- c("20-03-1921")
month(as.POSIXlt(some_date, format="%d-%m-%Y"))

music$release_date 
a <- grep("^[0-9]*?-[0-9]*?-[0-9]*?", music$release_date, value=TRUE, ignore.case=T, perl=T)

#date_index <- match(a, music$release_date)
date_index <- which(music$release_date %in% a)
processed.df <- music[date_index, ]
dim(processed.df)

write.csv(processed.df,"C:\\processed_music.csv", row.names = FALSE)
processed.df[is.na(processed.df)] <- 0

a <- grep("^[a-z-A-Z-0-9]*?-[a-z-A-Z-0-9]*?-[a-z-A-Z-0-9]*?", processed.df$name, value=TRUE, ignore.case=T, perl=T)
summary(a)
#replace alpha numeric chars
library(stringr)
processed.df$name <- str_replace_all(processed.df$name, "[^[:alnum:]]", " ")
processed.df$name <- str_replace_all(processed.df$name, "[[:punct:]]", " ")
#processed.df[processed.df.name.str.isalnum()]
processed.df$name

#combining different terms in song name
library(stringr)
processed.df$name <- str_replace_all(processed.df$name, " ", "_")


write.csv(processed.df,"C:\\Users\\nmala\\P2Records\\Data sets\\processed_music.csv", row.names = FALSE)




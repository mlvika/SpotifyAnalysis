genre.df <- read.csv("processed_data_by_genres.csv")
View(genre.df)
genre_trim <- subset(genre.df, select = -c(genres))
g <- na.omit(genre_trim)
g <- scale(g)

#finding colrelated values to genre
library(factoextra)
install.packages("factoextra")
distance <- get_dist(g)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#Normalizing data
# compute mean and standard deviation of each column
library(caret)
norm.values <- preProcess(genre_trim, method=c("center", "scale"))

# we perform the transformation/normalization
genre.df.norm <- predict(norm.values, genre_trim)

set.seed(1234)

km <- kmeans(genre.df.norm, 4)


km$cluster
km$centers
km$withinss

plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 13))

# label x-axes
axis(1, at = c(1:13), labels = names(genre_trim))
names(genre_trim)
# plot centroids
for (i in c(1:4))
  lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3,5),
                                                       "black", "dark grey"))

# name clusters
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:4)))


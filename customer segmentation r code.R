library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyr")
install.packages("DT")
install.packages("scales")
Mall_customers <- read.csv("C:/Users/BRIGHTY/Downloads/Mall_Customers.csv")

# Summary Statistics
sd(Mall_customers$Age)
summary(Mall_customers$Annual.Income..k..)
sd(Mall_customers$Annual.Income..k..)
summary(Mall_customers$Age)
sd(Mall_customers$Spending.Score..1.100.)

# Customer Gender Visualization
a <- table(Mall_customers$Gender)
barplot(a, main="Using BarPlot to display Gender Comparison",
        ylab="Count",
        xlab="Gender",
        col=rainbow(2),
        legend=rownames(a))

pct <- round(a / sum(a) * 100)
lbs <- paste(c("Female", "Male"), " ", pct, "%", sep=" ")
pie3D(a, labels=lbs,
      main="Pie Chart Depicting Ratio of Female and Male")

# Visualization of Age Distribution
summary(Mall_customers$Age)

hist(Mall_customers$Age,
     col="blue",
     main="Histogram to Show Count of Age Class",
     xlab="Age Class",
     ylab="Frequency",
     labels=TRUE)

boxplot(Mall_customers$Age,
        col="#ff0066",
        main="Boxplot for Descriptive Analysis of Age")

# Analysis of the Annual Income of the Customers
summary(Mall_customers$Annual.Income..k..)
hist(Mall_customers$Annual.Income..k..,
     col="#660033",
     main="Histogram for Annual Income",
     xlab="Annual Income Class",
     ylab="Frequency",
     labels=TRUE)

plot(density(Mall_customers$Annual.Income..k..),
     col="yellow",
     main="Density Plot for Annual Income",
     xlab="Annual Income Class",
     ylab="Density")
polygon(density(Mall_customers$Annual.Income..k..),
        col="#ccff66")

boxplot(Mall_customers$Spending.Score..1.100.,
        horizontal=TRUE,
        col="#990000",
        main="BoxPlot for Descriptive Analysis of Spending Score")

hist(Mall_customers$Spending.Score..1.100.,
     main="Histogram for Spending Score",
     xlab="Spending Score Class",
     ylab="Frequency",
     col="#6600cc",
     labels=TRUE)

# K-means Algorithm
set.seed(123)
iss <- function(k) {
  kmeans(Mall_customers[, 3:5], k, iter.max=100, nstart=100, algorithm="Lloyd")$tot.withinss
}

k.values <- 1:10
iss_values <- map_dbl(k.values, iss)

plot(k.values, iss_values,
     type="b", pch=19, frame=FALSE,
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")

# Average Silhouette Method
k2 <- kmeans(Mall_customers[, 3:5], 2, iter.max=100, nstart=50, algorithm="Lloyd")
plot(silhouette(k2$cluster, dist(Mall_customers[, 3:5], "euclidean")))

k3 <- kmeans(Mall_customers[, 3:5], 3, iter.max=100, nstart=50, algorithm="Lloyd")
plot(silhouette(k3$cluster, dist(Mall_customers[, 3:5], "euclidean")))

k4 <- kmeans(Mall_customers[, 3:5], 4, iter.max=100, nstart=50, algorithm="Lloyd")
plot(silhouette(k4$cluster, dist(Mall_customers[, 3:5], "euclidean")))

k5 <- kmeans(Mall_customers[, 3:5], 5, iter.max=100, nstart=50, algorithm="Lloyd")
plot(silhouette(k5$cluster, dist(Mall_customers[, 3:5], "euclidean")))

k6 <- kmeans(Mall_customers[, 3:5], 6, iter.max=100, nstart=50, algorithm="Lloyd")
plot(silhouette(k6$cluster, dist(Mall_customers[, 3:5], "euclidean")))

k7 <- kmeans(Mall_customers[, 3:5], 7, iter.max=100, nstart=50, algorithm="Lloyd")
plot(silhouette(k7$cluster, dist(Mall_customers[, 3:5], "euclidean")))

k8 <- kmeans(Mall_customers[, 3:5], 8, iter.max=100, nstart=50, algorithm="Lloyd")
plot(silhouette(k8$cluster, dist(Mall_customers[, 3:5], "euclidean")))

k9 <- kmeans(Mall_customers[, 3:5], 9, iter.max=100, nstart=50, algorithm="Lloyd")
plot(silhouette(k9$cluster, dist(Mall_customers[, 3:5], "euclidean")))

k10 <- kmeans(Mall_customers[, 3:5], 10, iter.max=100, nstart=50, algorithm="Lloyd")
plot(silhouette(k10$cluster, dist(Mall_customers[, 3:5], "euclidean")))

fviz_nbclust(Mall_customers[, 3:5], kmeans, method="silhouette")

set.seed(125)
stat_gap <- clusGap(Mall_customers[, 3:5], FUN=kmeans, nstart=25,
                    K.max=10, B=50)
fviz_gap_stat(stat_gap)

# Visualizing the Clustering Results
k6 <- kmeans(Mall_customers[, 3:5], 6, iter.max=100, nstart=50, algorithm="Lloyd")

pcclust <- prcomp(Mall_customers[, 3:5], scale=FALSE)
summary(pcclust)

ggplot(Mall_customers, aes(x=Annual.Income..k.., y=Spending.Score..1.100.)) + 
  geom_point(stat="identity", aes(color=as.factor(k6$cluster))) +
  scale_color_discrete(name="Clusters",
                       breaks=c("1", "2", "3", "4", "5", "6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle="Using K-means Clustering")

ggplot(Mall_customers, aes(x=Spending.Score..1.100., y=Age)) + 
  geom_point(stat="identity", aes(color=as.factor(k6$cluster))) +
  scale_color_discrete(name="Clusters",
                       breaks=c("1", "2", "3", "4", "5", "6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle="Using K-means Clustering")

kCols <- function(vec) {
  cols <- rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

digCluster <- k6$cluster
dignm <- as.character(digCluster)

plot(pcclust$x[, 1:2], col=kCols(digCluster), pch=19, xlab="K-means", ylab="Classes")
legend("bottomleft", unique(dignm), fill=unique(kCols(digCluster)))
This script includes loading the necessary libraries, reading the data, performing summary statistics, visualizing gender distribution, age distribution, annual income, and clustering analysis with Mall_customers.












































































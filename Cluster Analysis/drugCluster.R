library(NbClust)
library(flexclust)
library(cluster)
library(factoextra)

data <- read.csv("drug_outputs.csv")

df <- scale(data) 

wssplot <- function(data, nc=15, seed=1234){
     wss <- (nrow(data)-1)*sum(apply(data,4,var))
     for (i in 2:nc){
          set.seed(seed)
          wss[i] <- sum(kmeans(data, centers=i)$withinss)}
     plot(1:nc, wss, type="b", xlab="Number of Clusters",
          ylab="Within groups sum of squares")}
print(head(df))
set.seed(1234)
wssplot(df)

set.seed(1234)
fit.km <- kmeans(df, 3, nstart=25)
fit.km$siz
fit.km$centers

result <- aggregate(data, by=list(cluster=fit.km$cluster), mean)
print(result)

print(fit.km)
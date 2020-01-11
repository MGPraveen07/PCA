install.packages("gdata")
library(gdata)
library(readr)
wine <- read_csv("C:/Users/Admin/Desktop/Assignments/PCA/wine.csv")
head(wine)
str(wine)
wine1<- scale(wine[1:178,2:14])
head(wine1)
str(wine1)
summary(wine1)
pcawine<- princomp(wine1,cor = TRUE,scores = TRUE,covmat = NULL)
head(pcawine)
summary(pcawine)
loadings(pcawine)
plot(pcawine)
biplot(pcawine)
pca1<-pcawine$scores[,c(1:3)]
pca1


Wss<- NULL
for (i in 1:3){Wss<- c(Wss,kmeans(pca1,centers = i)$tot.withinss)}
Wss
plot(Wss,type="b",xlab = "Number of clusters",ylab = "With in groups sum of squares")
title(sub = "k-means clustering scree plot")

fit<- kmeans(pca1,3)
fit
str(fit)
wine_fin<- data.frame(fit$cluster,wine)
wine_fin
aggregate(wine[,2:14],by=list(fit$cluster),FUN=mean)


library(WriteXLS)
library(xlsx)
write.xlsx(wine_fin,file="wine_1.xlsx")
library(readr)
write_csv(wine_fin,"wine_2.csv")



h_dist<- dist(pca1,method = "euclidean")
h_dist
fit1<-hclust(h_dist,method = "complete")
fit1
plot(fit1,hang=-1)
groups<- cutree(fit1,k=3)
groups
rect.hclust(fit1,k=3,border="red")
membership<- as.matrix(groups)
membership
h_wine<- data.frame(membership,wine)
h_wine
aggregate(wine[,2:14],by=list(h_wine$membership),FUN = mean)

library(readr)
write_csv(h_wine,"H_CLUST.csv")
getwd()

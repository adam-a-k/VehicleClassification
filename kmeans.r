library(readxl)
library(NbClust)
library(tidyverse) 
library(gridExtra) 
library(ggplot2)   
library(rlang)       
library(fpc)
library(MASS)
library(flexclust)
library(caret)
library(factoextra)
library(cluster)
vehicles <- read_excel("/Users/Adz/Documents/University/Year 2/2. Machine Learning and Data Mining/Coursework/vehicles.xlsx")

set.seed(1)

normalise <- function(x) 
{
  return ((x - min(x)) / (max(x) - min(x)))
}

outliers <- function(x) {
  
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  
  x > upper_limit | x < lower_limit
}

remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers(df[[col]]),]
  }
  df
}


vehicles <- remove_outliers(vehicles, c('Comp','Circ','D.Circ','Rad.Ra','Pr.Axis.Ra','Max.L.Ra','Scat.Ra','Elong','Pr.Axis.Rect',
                                        'Max.L.Rect','Sc.Var.Maxis','Sc.Var.maxis','Ra.Gyr','Skew.Maxis','Skew.maxis','Kurt.maxis','Kurt.Maxis','Holl.Ra'))


vehicles[c(2:19)] <- lapply(vehicles[c(2:19)], function(x) c(normalise(x)))


data.train <- vehicles[2:19]


euclidean <- NbClust(data.train,
                     distance="euclidean", 
                     min.nc=2,
                     max.nc=15,
                     method="kmeans",
                     index="all")

manhattan <- NbClust(data.train,
                     distance="manhattan",
                     min.nc=2,
                     max.nc=15,
                     method="kmeans",
                     index="all")

table(euclidean$Best.n[1,])

barplot(table(euclidean$Best.n[1,]),
        xlab="Number of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters Chosen (Euclidean Distance)")

table(manhattan$Best.n[1,])

barplot(table(manhattan$Best.n[1,]),
        xlab="Number of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters Chosen (Manhattan Distance)")

wss <- 0
for (i in 1:15){
  wss[i] <-
    sum(kmeans(data.train, centers=i)$withinss)
}
plot(1:15,
     wss,
     type="b",     
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#2 Clusters
fit.km <- kmeans(data.train, 2)

fit.km

plotcluster(data.train, fit.km$cluster)

parcoord(data.train, fit.km$cluster)

confuseTable.km <- table(vehicles$Class, fit.km$cluster)
confuseTable.km

fit.km$centers

#3 Clusters
fit.km <- kmeans(data.train, 3)

fit.km

plotcluster(data.train, fit.km$cluster)

parcoord(data.train, fit.km$cluster)

confuseTable.km <- table(vehicles$Class, fit.km$cluster)
confuseTable.km

fit.km$centers

#4 Clusters
fit.km <- kmeans(data.train, 4)

fit.km

plotcluster(data.train, fit.km$cluster)

parcoord(data.train, fit.km$cluster)

confuseTable.km <- table(vehicles$Class, fit.km$cluster)
confuseTable.km

fit.km$centers


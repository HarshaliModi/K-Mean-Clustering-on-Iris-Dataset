#Installing the required packages
install.packages("stats")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")

#Running the installed libraries
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)

#Convertiong the iris dataset into unlabelled dataset as per unsupervised learning algorithm
View(iris)
mydata = select(iris,c(1,2,3,4))
mydata

#Choosing the appropriate number of clusters with the help of WSS plot function
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

#Observing the kink to finalize the number of clusters to be formed
wssplot(mydata)

#K-Mean Cluster
KM = kmeans(mydata,2)
KM

#Evaluating Cluster Plot
autoplot(KM, mydata, frame=TRUE)

#Distinction among clusters by finding the centers
KM$centers

#K-means Algorithm

# Load Packages
library(purrr)
set.seed(4444)

# 
# calculate total within-cluster sum of squares by number of clusters — we'll use this 
  #to optimize k
iss <- function(k) {
  kmeans(customer_data[,3:5], k, iter.max=100, nstart=100, algorithm="Lloyd")$tot.withinss
}

k_values <- 1:20

iss_values <- map_dbl(k_values, iss)

plot(k_values, iss_values,
     type = "b", pch = 19, frame = FALSE, 
     main = "Optimal clusters",
     xlab = "Number of clusters K",
     ylab = "Total intra-clusters sum of squares")

library(cluster) 
library(grid)
library(gridExtra)

k2<-kmeans(customer_data[,3:5],2,iter.max=100,nstart=50,algorithm="Lloyd")
s2<-plot(silhouette(k2$cluster,dist(customer_data[,3:5],"euclidean")))

k3<-kmeans(customer_data[,3:5],3,iter.max=100,nstart=50,algorithm="Lloyd")
s3<-plot(silhouette(k3$cluster,dist(customer_data[,3:5],"euclidean")))


library(NbClust)
library(factoextra)
?fviz_nbclust
fviz_nbclust(customer_data[,3:5], kmeans, method = "silhouette")
stat_gap <- cluster::clusGap(customer_data[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(stat_gap)

k6<-kmeans(customer_data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
k6


#Visualizing the Clustering Results using the First Two Principle Components

pcclust=prcomp(customer_data[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)

pcclust$rotation[,1:2]
ggplot(customer_data, aes(x =`Annual Income (k$)`, y = `Spending Score (1-100)`)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=paste0("Cluster ", seq(1:6))) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering") +
  ggthemes::theme_fivethirtyeight()



ggplot(customer_data, aes(x =`Spending Score (1-100)`, y =Age)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster)), size = 2.5, alpha = .7) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering") +
  theme_minimal()



kCols=function(vec){cols=rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])}

digCluster<-k6$cluster; dignm<-as.character(digCluster); # K-means clusters

plot(pcclust$x[,1:2], col =kCols(digCluster), 
     pch =19, xlab ="K-means", ylab="classes")
legend("bottomleft",unique(dignm),fill=unique(kCols(digCluster)))
?rainbow
cols=rainbow (length (unique (vec)))
              
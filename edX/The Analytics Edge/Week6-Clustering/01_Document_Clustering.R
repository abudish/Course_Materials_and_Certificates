
# 1
dailykos <- read.csv('dailykos.csv')
distances <- dist(dailykos, method='euclidean')
  #1.1 We have a lot of observations, so it takes a long time to compute 
  # the distance between each pair of observations. 
  # We have a lot of variables, so the distance computation is long

docCluster <- hclust(distances, method="ward.D")
plot(docCluster) #1.2 2 and 3

#1.3 7 and 8

clusterGroups <-  cutree(docCluster, k = 7)

subset.by.cluster <- function(clstr_group) {
  subset(dailykos, clusterGroups == clstr_group)
}

cluster1 <- subset.by.cluster(1)
cluster2 <- subset.by.cluster(2)
cluster3 <- subset.by.cluster(3)
cluster4 <- subset.by.cluster(4)
cluster5 <- subset.by.cluster(5)
cluster6 <- subset.by.cluster(6)
cluster7 <- subset.by.cluster(7)

nrow(cluster3) #1.4 374
nrow(cluster1) #1.4 1266 the most
nrow(cluster2)
nrow(cluster4) #1.4 139 the fewest
nrow(cluster5)
nrow(cluster6)
nrow(cluster7)

tail(sort(colMeans(cluster1))) #1.5 bush

tail(sort(colMeans(cluster2))) #1.6 november, poll, vote, challenge
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))#1.6 Cluster 5 - Iraq war cluster
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))#1.6 Cluster 7 - Democratic Party cluster

#2
set.seed(1000)
KMC <- kmeans(dailykos, centers=7)
table(KMC$cluster)#2.1 Cluster 3 277 obs, 2 cluster - the smalest,
                  #    4 cluster - the biggest
#or
KmeansCluster <- split(dailykos, KMC$cluster)
dim(KmeansCluster[[1]]) # and so on

tail(sort(colMeans(KmeansCluster[[1]])))
tail(sort(colMeans(KmeansCluster[[2]])))#2.2 Cluster 2 - Democratic Party
tail(sort(colMeans(KmeansCluster[[3]])))#2.2 Cluster 3 - Iraq war
tail(sort(colMeans(KmeansCluster[[4]])))
tail(sort(colMeans(KmeansCluster[[5]])))
tail(sort(colMeans(KmeansCluster[[6]])))
tail(sort(colMeans(KmeansCluster[[7]])))

table(clusterGroups, KMC$cluster)
116/(116 +11 + 10 + 5 + 2) #2.3 K-means cluster 2 corresponds to Hier Cluster 7

table(clusterGroups, KMC$cluster)
171/(171 + 42 + 64) #2.4 K-means cluster 3 corresponds to Hier Cluster 5

table(clusterGroups, KMC$cluster)
123/(123 + 111 + 1 + 24 + 39 + 10)
111/(123 + 111 + 1 + 24 + 39 + 10)#2.5 No Hierarchical Cluster contains at least
                                  #    half of the points in K-Means Cluster 7.

table(clusterGroups, KMC$cluster)
320/(320 + 8 + 1) #2.6 K-means cluster 6 corresponds to Hier Cluster 2

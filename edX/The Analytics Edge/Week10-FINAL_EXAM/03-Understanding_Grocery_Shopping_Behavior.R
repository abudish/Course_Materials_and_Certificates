# 1 #
orders <- read.csv('orders.csv')
# What time of day are most orders placed?
hist(orders$order_hour_of_day)
# What is the average days since prior order?
mean(orders$days_since_prior_order) # 17.093


# 2 #
# What's the correlation between the orders of "fresh.fruits" and "fresh.vegetables"?
cor(orders$fresh.fruits, orders$fresh.vegetables) # 0.3955114

# In the dataset, what proportion of orders have 
# at least one item from the frozen.pizza aisle?
nrow(orders[orders$frozen.pizza >= 1, ])/nrow(orders) # 0.0522


# 3 #
orders.aisle <- orders[, 5:ncol(orders)]
library(caret)

preproc <- preProcess(orders.aisle)
ordersNorm <- predict(preproc, orders.aisle)
# What is the maximum value of frozen.dessert after normalization?
max(ordersNorm$frozen.dessert) # 11.74144
# What is the minimum value of soft.drinks in the normalized dataset?
min(ordersNorm$soft.drinks) # -0.2873327


# 4 #
distances <- dist(ordersNorm, method = "euclidean")
ClusterProducts <- hclust(distances, method = "ward.D")
plot(ClusterProducts, labels = FALSE)
# Based on the dendrogram, how many clusters do you think would NOT be appropriate for this problem?
# 5


# 5 #
set.seed(200)
KMC <- kmeans(ordersNorm, centers=4)
sort(table(KMC$cluster))
# How many observations are in the smallest cluster? - 36
# How many observations are in the largest cluster? - 3409


# 6 #
KmeansCluster <- split(orders.aisle, KMC$cluster)
# Which cluster best fits the description "orders mostly consistents of 
# cleaning supplies, beauty, and some pantry foods"? - Cluster 1
tail(sort(KMC$centers[1, ]))
tail(sort(KMC$centers[2, ]))
tail(sort(KMC$centers[3, ]))
tail(sort(KMC$centers[4, ]))


# 7 #
# Which cluster best fits the description "frozen desserts"? - Cluster 4
tail(sort(KMC$centers[4, ]))


# 8 #
# Which cluster on average has the smallest amount of items ordered?
rowSums(KMC$centers) # Cluster 3


# 9 #
# If we ran hierarchical clustering a second time without making any additional 
# calls to set.seed, we would expect:
  # Identical results to the first hierarchical clustering

# If we ran k-means clustering a second time without making any additional calls
# to set.seed, we would expect:
  # Different results from the first k-means clustering

# If we ran k-means clustering a second time, again running the command set.seed(200)
# right before doing the clustering, we would expect:
  # Identical results to the first k-means clustering correct

# If we ran k-means clustering a second time, running the command set.seed(100) 
# right before doing the clustering, we would expect:
  # Different results from the first k-means clustering


# 10 #
# Suppose it was decided that the 4 clusters were too general, and they wanted
# more specific clusters to describe the order behavior. 
# Would they want to increase or decrease the number of clusters?
  # Increase the number of clusters


# 11 #
orders.other <- orders[, 2:4]
KmeansCluster_other <- split(orders.other, KMC$cluster)
# Which cluster has the latest average hour of the day? - Cluster 4
mean(KmeansCluster_other[[1]]$order_hour_of_day)
mean(KmeansCluster_other[[2]]$order_hour_of_day)
mean(KmeansCluster_other[[3]]$order_hour_of_day)
mean(KmeansCluster_other[[4]]$order_hour_of_day)


# 12 #
# Why do we typically use cluster centroids to describe the clusters?
  # The cluster centroid captures the average behavior in the cluster, and can
  # be used to summarize the general pattern in the cluster.


# 13 #
# Which of the following visualizations could be used to observe 
# the distribution of days_since_prior_order, broken down by cluster?
orders.other$cluster <- KMC$cluster
boxplot(days_since_prior_order ~ cluster, data=orders.other)
ggplot(data=orders.other, aes(x=cluster, y=days_since_prior_order, group=cluster)) +
  geom_boxplot()
  # A box plot of the variable days_since_prior_order, subdivided by cluster;
  # ggplot with the cluster number on the x-axis and days_since_prior_order on 
  # the y-axis, cluster number as group, plotting with geom_boxplot() correct

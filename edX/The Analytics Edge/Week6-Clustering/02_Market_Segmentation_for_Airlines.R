#1
airlines <- read.csv('AirlinesCluster.csv')
summary(airlines)#1.1 smallest - BonusTrans, FlightTrans
                 #    largest - Balance, BonusMiles

#1.2 If we don't normalize the data, the clustering will be dominated 
#    by the variables that are on a larger scale

library(caret)
preproc  <- preProcess(airlines)
airlinesNorm <- predict(preproc, airlines)
summary(airlinesNorm)#1.3 largest max - FlightMiles, smallest min - DaysSinceEnroll

#2
distances <- dist(airlinesNorm, method='euclidean')
hierClust <- hclust(distances, method="ward.D")
plot(hierClust)#2.1 6

clusterGroups <-  cutree(hierClust, k = 5)
table(clusterGroups)#2.2 776

tapply(airlines$Balance, clusterGroups, mean)
tapply(airlines$QualMiles, clusterGroups, mean)
tapply(airlines$BonusMiles, clusterGroups, mean)
tapply(airlines$BonusTrans, clusterGroups, mean)
tapply(airlines$FlightMiles, clusterGroups, mean)
tapply(airlines$FlightTrans, clusterGroups, mean)
tapply(airlines$DaysSinceEnroll, clusterGroups, mean)
#2.3 Cluster 1 has largest avg of variable DaysSinceEnroll
# Infrequent but loyal customers. 

#2.4 Cluster 2 has largest avg of variables QualMiles, FlightMiles, FlightTrans
# Customers who have accumulated a large amount of miles, and the ones with 
# the largest number of flight transactions

#2.5 Cluster 3 has largest avg of variables Balance, BonusMiles, BonusTrans
# Customers who have accumulated a large amount of miles, 
# mostly through non-flight transactions

#2.6 Cluster 4 has largest avg of None variables
# Relatively new customers who seem to be accumulating miles, 
# mostly through non-flight transactions

#2.7 Cluster 5 has largest avg of None variables
# Relatively new customers who don't use the airline very often

#3
set.seed(88)
KMC <- kmeans(airlinesNorm, centers=5, iter.max = 1000)
table(KMC)
table(KMC$cluster) #3.1 2


tapply(airlines$Balance, KMC$cluster, mean)
tapply(airlines$QualMiles, KMC$cluster, mean)
tapply(airlines$BonusMiles, KMC$cluster, mean)
tapply(airlines$BonusTrans, KMC$cluster, mean)
tapply(airlines$FlightMiles, KMC$cluster, mean)
tapply(airlines$FlightTrans, KMC$cluster, mean)
tapply(airlines$DaysSinceEnroll, KMC$cluster, mean)
#3.2 No, because cluster ordering is not meaningful in either k-means clustering
#    or hierarchical clustering
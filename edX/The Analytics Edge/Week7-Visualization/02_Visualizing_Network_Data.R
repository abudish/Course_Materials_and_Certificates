library(igraph)

# 1 #
edges <- read.csv('edges.csv')
users <- read.csv('users.csv')
length(unique(users$id)) #1.1 59
#1.1 From str(edges) or nrow(edges), we see that there are 146 pairs of users 
#    in our dataset who are Facebook friends. However, each pair (A, B) must be
#    counted twice, because B is a friend of A and A is a friend of B. 
#    To think of this in simpler terms, consider a network with just new people,
#    A and B, and a single edge (A, B). Even though there are two vertices and 
#    one edge, each user has on average one friend.
#    For our network, the average number of friends per user is 292/59=4.95.

table(users$locale) #1.2 Locale B

table(users$gender, users$school)#1.3 No, both genders A and B have attended 
                                 #    schools A and B


# 2 #
?graph.data.frame
g = graph.data.frame(edges, FALSE, users)# <- this is the answer to 2.1

plot(g, vertex.size=5, vertex.label=NA) #2.2 4 connected components and 7 with no friends

sum(degree(g) >= 10)
table(degree(g) >= 10)#2.3 9

V(g)$size <- degree(g)/2+2
plot(g, vertex.label=NA)
max(degree(g))/2 + 2 #2.4 11
min(degree(g))/2 + 2 #2.4 2

# 3 #

V(g)$color <- "black"
V(g)$color[V(g)$gender == "A"] <- "red"
V(g)$color[V(g)$gender == "B"] <- "gray"
plot(g, vertex.label=NA) #3.1 Gender B

V(g)$color = "black"
V(g)$color[V(g)$school == "A"] <- "green"
V(g)$color[V(g)$school == "AB"] <- "yellow"
plot(g, vertex.label=NA) #3.2 Yes, 'AB' guys are friends
#3.2 Some, but not all, of the high-degree users attended school A

V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] <- "white"
V(g)$color[V(g)$locale == "B"] <- "orange"
plot(g, vertex.label=NA) #3.3 Locale B - largest, 4 connected component- locale A

# 4 $
?igraph.plotting
install.packages('rgl')
library(rgl)
rglplot(g) # plot our graph in 3-D
plot(g, vertex.label=NA, edge.width = 2) # edge.width



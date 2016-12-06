
###### Part 1: Euclidean Distance ######

# First create a 2*2 matrix A, where each row represent an observation point
A <- matrix(c(1.7, 5, 4, 72), 2, 2, byrow = T) 
A
# Then calculate the Euclidean distance between the two observation points
d <- dist(cbind(A))
print(d)


###### Part 2: Generate a Fake Dataset ######

set.seed(5)
x <- matrix(rnorm(300), 150, 2) # The dataset contains 150 observations, each of which is described by a two-element vector
xmean <- matrix(rnorm(8), 4, 2) # Generate a deviation matrix
which <- sample(1:4, 150, replace = T) # Randomly sample of number 1-4 for 150 times with replacement
x <- x + xmean[which, ] # Add the deviation to dataset in four randomly formed groups
plot(x, col = which, pch = 19) # Plot the distribution of the observation points


###### Part 3: K-Means Clustering ######

x.cluster <- cbind(x, which) # Add the randomly sampled 1-4 (a new column) to the dataset as the initial clusters (in this case, we specify 4 clusters)
while(TRUE){
  centroid <- c() # Create an empty vector to put the centroids
  for(g in 1:4){ # In this for loop, the position of each centroid is calculated - mean of observations assigned to each centroid
    centroid <- c(centroid, mean(x.cluster[x.cluster[, 3] == g, 1]), mean(x.cluster[x.cluster[, 3] == g, 2]))
  }
  centroid <- matrix(centroid, 4, 2, byrow = T) # Reform the vector into a matrix where each row is one centroid
  
  distance <- c()
  for(i in 1:nrow(x)){ # In this for loop, each observation is reassigned its closest centroid
    for(j in 1:4)
      { # First to calculate the Euclidean distance between each observation and each centroid
      dis<- sqrt((x.cluster[i,1]-centroid[j,1])^2+(x.cluster[i,2]-centroid[j,2])^2)
      distance <- c(distance, dis)
    }
  }
  distance <- matrix(distance, 150, 4, byrow = T) # Create the matrix where each row is an observation and each column is its distance to the four centroids
  centroid.label <- apply(distance, 1, which.min) # Choose the centroid with the shortest distance
  if(all(centroid.label == x.cluster[, 3])){ # If the centroid is not changing any more for each observation, stop the iteration
    km.clusters <- centroid.label
    centroid.matrix <- centroid
    break
  }
  else
    { # Otherwise, assign the new centroids to the dataset, and continue the iteration
    x.cluster[, 3] <- centroid.label
  }  
}


plot(x, col = centroid.label , pch = 19) # Plot the clustered observation points 
points(centroid.matrix, pch = 19, col = 6, cex = 2) # Add centroids to the plot

#Easy Solution
km.out1 <- kmeans(x, 4)
km.out1
plot(x, col = km.out1$cluster, pch = 19)
points(km.out1$centers, pch = 19, col = 6, cex = 2)



km.out2 <- kmeans(x, 3)
km.out2
plot(x, col = km.out2$cluster, pch = 19)
points(km.out2$centers, pch = 19, col = 6, cex = 2)

km.out3 <- kmeans(x, 5)
km.out3
plot(x, col = km.out3$cluster, pch = 19)
points(km.out3$centers, pch = 19, col = 6, cex = 2)

# Plot original data and clustered data together #
par(mfcol = c(1, 2)) # create plotting structure of two plots in a row
plot(x, col = which, pch = 19) # plot observations with randomly assigned clusters
plot(x, col = centroid.label, pch = 19) # plot observations after clustering
points(centroid.matrix, pch = 19, col = 6, cex = 2)
dev.off() # quit the plotting structure settings

  
###### Part 4: K-medoids clustering ######

# install package "cluster"
library(cluster)
# generate a dissimilarity matrix for all the data points in data x, using the Euclidean distance. Hint: use the dist() function is most straight forward.
dismat <- dist(x, method = "euclidean")    
# run the pam() function with 3 clusters using the dismat generated above
med <- pam(x, k=3, diss = inherits(x,"dismat"))   
clusters <- med$clustering 
medoids <- med$medoids  
# plot the clustered data points, where color is decided by the clusters, and add the resulted medoids 
plot(x, col = clusters, pch = 19)  
points(medoids, pch=19, col=6, cex=2)


###### Part 5: Hierarchical Clustering ######

# Calculate Inter-Observation Distances #
## If there are n observations, we need to calculate n*(n-1)/2 unique inter-observation distances. 
## It is because the distance between the same observation is zero, and distance between A and B is the same with the distance between B and A.  
#Let D be the mXn distance matrix, with m= nrow(x1) and n=nrow( x2). The elements are the Euclidean distances between the all locations x1[i,] and x2[j,]. That is,


# Euclidean distance function 1:

  ed1 <- c()
  for(i in 1:nrow(x)){
    for(j in 1:nrow(x)){
      if(i < j){
        ed <- sqrt((x[i,1]-x[j,1])^2 +(x[i,2]-x[j,2])^2) #D.ij = sqrt( sum.k (( x1[i,k] - x2[j,k]) **2 ).
        ed1 <- c(ed1, ed)
      }
    }
  }

ed1
# Euclidean distance function 2:
ed2 <- dist(x, method = "euclidean") # the default method

# Plot Cluster Dendrogram #
# First take out a subset of the original observations. In this case, we randomly select 20 observations for the purpose of clear demonstration in the dendrogram
where <- sample(1:nrow(x), 20, replace = F)
x.part <- x[where, ]
# Then calculate the inter-observation distances
y <- dist(x.part) # use either method 1 or 2 to calculate the Euclidean distance
# Function for hierarchical clustering
hc.complete <- hclust(y, method = "complete")
# Plot the dendrogram
plot(hc.complete)


# Try single with hclust function and plot the dendrogram
where <- sample(1:nrow(x), 20, replace = F)
x.part <- x[where, ]
# Then calculate the inter-observation distances
y <- dist(x.part) # use either method 1 or 2 to calculate the Euclidean distance
# Function for hierarchical clustering
hc.single <- hclust(y, method = "single")
# Plot the dendrogram
plot(hc.single)

# Try average with hclust function and plot the dendrogram
where <- sample(1:nrow(x), 20, replace = F)
x.part <- x[where, ]
# Then calculate the inter-observation distances
y <- dist(x.part) # use either method 1 or 2 to calculate the Euclidean distance
# Function for hierarchical clustering
hc.average <- hclust(y, method = "average")
# Plot the dendrogram
plot(hc.average)

########################################### END ##############################################

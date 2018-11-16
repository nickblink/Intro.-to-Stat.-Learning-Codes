# K-means Clustering Basic Practice
# Started on 9/13/2017
#
# The purpose of this file is to practice coding the basic K-means algorithm on a simple simulated set of data.
# This will be easy and messy, but it will be fun practice!
graphics.off()

n <- 100
k <- 2
sample_sd <-0.3

# simulate data
simulate = 'clustered'

if (simulate == 'uniform'){
  # The easy way - x and y values randomly spread across [0,1]
  x1 <- runif(n)
  x2 <- runif(n)
  X <- data.frame(x1,x2, rep(0,n))
} else if (simulate == 'clustered'){
  k_starting_pts <- data.frame(runif(k),runif(k))
  
  X <- data.frame(rep(0,n),rep(0,n),rep(0,n),rep(0,n))
  names(X) <- c('X1','X2','Cluster','Distance')

  # For first k-1 points, cluster ~ n/k points around it.
  
  cluster_pts_all <- data.frame(rep(0,k),rep(0,k))
  nk <- floor(n/k)
  for (i in 1:(k-1)){
    cluster_pt <- c(runif(1),runif(1))
    cluster_pts_all[i,] <- cluster_pt
    #X[iter,1:2] <- cluster_pt
    # Cluster points around this!
    X[((i-1)*nk+1):(i*nk),1:2] <- (
      data.frame(rnorm(nk, mean = cluster_pt[1], sd = sample_sd), 
                 rnorm(nk, mean = cluster_pt[2], sd = sample_sd)))
  }
  cluster_pt <- c(runif(1),runif(1))
  cluster_pts_all[k,] <- cluster_pt
  X[((k-1)*nk+1):n,1:2] <- (
    data.frame(rnorm(n-(k-1)*nk, mean = cluster_pt[1], sd = sample_sd), 
               rnorm(n-(k-1)*nk, mean = cluster_pt[2], sd = sample_sd)))
  
  # Shuffle points around
  X <- X[sample(n,replace=FALSE),]
  
  #plot(X[,1:2])
  #points(cluster_pts_all, col = 'red')
  #title(sprintf('Simulation of clustering around %i points with sd = %.2f',k,sample_sd))
  
} else {
  print('Not a proper point simulation')
  break
}

# Now for the algorithm! The algorithm starts by randomly assigning points
# to clusters. Next, it will compute the mean of each cluster. Then it will
# cycle through each point and move it to the cluster closest to it.
# I need a way to check if anything has changed - I could store the third 
# column of my X matrix every cycle and check if it's the same as the previous - genious!
# You know what would be a fun challenge - what is the most complex code you 
# can write without going back and without ever debugging?


# Try this procedure 6 times to compare results
par(mfrow = c(2,2), oma=c(0,0,2,0))
#par(mfrow = c(2,2))

for (tries in 1:4) {
  
# randomly assign clusters
X[,3] <- sample(1:k, n, replace=TRUE)

prev_clusters <- rep(0,n)
cluster_means <- data.frame(rep(0,k),rep(0,k), rep(0,k)) # How can I use apply to make this better?
names(cluster_means) <- c('means x1', 'mean x2', 'n (cluster)')

iter <- 0
distance_from_means <- rep(0,k)



while(!identical(X[,3],prev_clusters) && iter < 30){
  
  # Reset cluster means and previous clusters
  cluster_means <- data.frame(rep(0,k),rep(0,k), rep(0,k))
  prev_clusters <- X[,3]
  iter <- iter + 1
  
  # Calculate cluster means. First sum all cluster points up
  for (i in 1:n){
    clust <- X[i,3]
    cluster_means[clust,1:2] <- cluster_means[clust,1:2] + X[i,1:2]
    cluster_means[clust,3] <- cluster_means[clust,3] + 1
  }
  # Average out means
  for (m in 1:k){
    if (cluster_means[m,3]==0){
      cluster_means[m,1:2] <- rep(0,2)
    } else {
      cluster_means[m,1:2] <- cluster_means[m,1:2]/cluster_means[m,3]
    }
  }
  
  # Generate new clusters
  
  for (i in 1:n){
    for (j in 1:k){
      x <- unname(X[i,1:2]); y <- unname(cluster_means[j,1:2])
      distance_from_means[j] <- dist(rbind(data.frame(x),data.frame(y)))
    }
    # Update cluster and distance from mean
    X[i,3] <- which.min(distance_from_means)
    X[i,4] <- min(distance_from_means)
  }
  
  print(sum(X[,4]))
  if(is.na(sum(X[,4]))){browser()}
}
iter

#pt_color <- c('red','blue','green') #fixed for k = 3, but I'll cross that bridge later
pt_color <- 2:(k+1)

#The truly lazy way of graphing this all
#plot(X[1:2,],col=pt_color[X[,3]])
plot(X[1,1:2], col = pt_color[X[1,3]], asp = 1, xlim = c(-sample_sd,1+sample_sd), ylim=c(-sample_sd,1+sample_sd))
for (i in 2:n){
  points(X[i,1:2], col = pt_color[X[i,3]])
}

title(sprintf('Take %i: total distance = %0.2f',i,sum(X[,4])))

}
      
title(sprintf('K-means clustering with k = %i and n = %i',k,n),outer=TRUE)
      
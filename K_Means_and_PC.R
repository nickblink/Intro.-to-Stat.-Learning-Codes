# PCA and K-means

simulation = function(n=60, num_class = 3){
  clust1 = matrix(rnorm(20*50, 1, 2), ncol = 50)
  clust2 = matrix(rnorm(20*50, 2, 2), ncol = 50)
  clust3 = matrix(rnorm(20*50, 3, 2), ncol = 50)
  clusters = cbind(rep(c(1,2,3), each=20), rbind(clust1,clust2,clust3))
  clusters = clusters[sample(60),]
  return(clusters)
}

km= function(data, data.pc, k){
  data.km = kmeans(data[,2:ncol(data)],k)
  
  # par(mfrow=c(1,2))
  # plot(data.pc$x[,1],data.pc$x[,2],col=data[,1], main = 'First Two Principal Components', xlab='Z1', ylab='Z2')
  # title = sprintf('K-means with %i clusters', k)
  # plot(data.pc$x[,1],data.pc$x[,2],col=data.km$cluster, main = title)
  # print(table(data[,1],data.km$cluster))
  
  tbl_score = sum(apply(table(data[,1],data.km$cluster),1,max))
  return(tbl_score)
}

data <- simulation()
data.pc = prcomp(data[,2:ncol(data)])

graphics.off()
newdat = cbind(data[,1],data.pc$x[,c(1,2)])

n = 1000
t_pc = 0
t_og = 0
for (i in 1:n){
  data <- simulation()
  data.pc = prcomp(data[,2:ncol(data)])
  newdat = cbind(data[,1],data.pc$x[,1:3])
  t_pc = t_pc + km(newdat,data.pc, 3)
  t_og = t_og + km(data,data.pc,3)
}
avg_t_pc = t_pc/n
avg_t_og = t_og/n
avg_t_pc
avg_t_og



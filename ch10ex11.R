### Read in gene data and perform hierarchical clustering.

data = read.csv(    ,header="FALSE")
methods = c('complete','single','centroid', 'average')
for (met in methods){
  print("using method %s", %met)
  data.hc = hclust(cor(data), method = met)
  tbl = table(data.hc$labels, rep(c(1,2), each=20))
  tbl_score = max(tbl[1,])+max(tbl[2,])
  print("table score: %i",tbl_score)
  print(tbl)
  
}



### to find which genes differ the most across (so not within??) the two groups:
# Option a - find the correlation of each with the centroid of the group.
# Option b - find the euclidean distance with the centroid of the group.
# Option c - for each obs w/i each group, perform a regression on all the 
#   other obs and find the regressions with lower r^2 values
### ah this is all wrong. I was looking at difference in obs.
# Well I could perform a simple two sample t-test for each gene between both 
# groups of n = 20. Then choose the largest effect sizes out of the ones with
# statistical significance.
# What else? What could principal component analysis do? Not for this question?
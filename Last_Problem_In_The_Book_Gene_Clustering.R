setwd('C:/Users/Nick/Documents/Intro Statistical Learning')
gene_data <- read.csv('ch10Ex11.csv',header=F)
head(gene_data)
head(gene_data[,1:5])
dim(gene_data)
?hclust
dd = as.dist(1-cor(gene_data))
gene_hc <- hclust(dd)
plot(gene_hc)
gene_single <- hclust(dd, method='single')
plot(gene_single)
table(cutree(gene_hc,2),cutree(gene_single,2))
gene_hc$clusters
cutree(gene_hc, 2)

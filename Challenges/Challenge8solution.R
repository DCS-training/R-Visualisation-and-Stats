#We can use the UPMGA cluster algorithm to test the similarities of the books we look at today
#Use the matrix resulted from the Morista-horn and Jaccard analyses to plot 2 dendrogram.
#are them equal?
#Results of the Morista-Horn (Distances)
Clust <- hclust(Distances, method="average")#Use the UPMGA algorithm to create the hierarchical cluster
print(Clust)
#View the results
install.packages("ggdendro")
library(ggdendro)

ggdendrogram(Clust, rotate=T)

#Results of the Jaccard (Dist)
Clust2 <- hclust(Dist, method="average")#Use the UPMGA algorithm to create the hierarchical cluster
print(Clust2)
#View the results
install.packages("ggdendro")
library(ggdendro)

ggdendrogram(Clust2, rotate=T)

?ggdendrogram

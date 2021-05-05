#Data. I set it with the college dataset but if you have a datset with more than 3 numerical column try do the same on that
#College
college <- read_csv('http://672258.youcanlearnit.net/college.csv')
college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))
collegePCA <- college[c(5,7,12,13,14,15)]
collegePCA <- na.omit(collegePCA)

#1 Compute the PCA that will analyse if the combination of the tuition,salary average, loan default rate, and Median debt are connected to the type o the university (public/private) 
measures <- collegePCA [c(3:6)]
#Compute the PCS
pcs <- prcomp(measures)
plot(pcs)
print(pcs)
#let's add the PC1 and PC2 info to the Iris dataset
pcCollege <- collegePCA
pcCollege$pc1 <- pcs$x[,1]
pcCollege$pc2 <- pcs$x[,2]
str(pcCollege)
#Finally let's plot it 
ggplot(pcCollege, aes(x=pc1, y=pc2, color=control)) + geom_point(size=2, alpha=0.5)+theme_bw()+ labs(title = "College") 

#Better see the clusters
library(plyr)

find_hull <- function(pcCollege) pcCollege[chull(pcCollege$pc1, pcCollege$pc2), ]
hulls <- ddply(pcCollege,"control", find_hull)

ggplot(pcCollege, aes(x=pc1, y=pc2, color=control)) + geom_point(size=2, alpha=0.5)+theme_bw()+ labs(title = "college")+ geom_polygon(data=hulls, alpha=.2, aes(fill=control))

#other option
install.packages("ggfortify")
library(ggfortify)
autoplot(pcs, data=collegePCA, size=4, alpha=0.5, colour='control', loadings=TRUE,loadings.label = TRUE )+theme_bw()
#2 USe the K-means to do a cluster analysis of the data 
#K-Means===============
measures <- collegePCA[c(3:6)]
numGroups = 2
myKMeans <- kmeans(measures,numGroups)
myKMeans


kmeanCollege <- pcCollege
kmeanCollege$cluster <- myKMeans$cluster
ggplot(kmeanCollege, aes(x=pc1, y=pc2, col=control)) + geom_point(size=4, alpha=0.5)+theme_bw()+ labs(title = "College")+ facet_grid(control~cluster)

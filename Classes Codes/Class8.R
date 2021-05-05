library(tidyverse)
#PCA ############
#Prepare the sample
str(iris)
#extract the continuous variables
measures <- iris[,1:4]#Extract all rows and colum 1 to 4
measures <- iris[c(1:4)]
#Compute the PCS
pcs <- prcomp(measures)
plot(pcs)
print(pcs)
#let's add the PC1 and PC2 info to the Iris dataset
pcIris <- iris
pcIris$pc1 <- pcs$x[,1]
pcIris$pc2 <- pcs$x[,2]
str(pcIris)
#Finally let's plot it 
ggplot(pcIris, aes(x=pc1, y=pc2, color=Species)) + geom_point(size=6, alpha=0.5)+theme_bw()+ labs(title = "Iris") 

ggplot(pcIris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + geom_point(size=6, alpha=0.5)+theme_bw()+ labs(title = "Iris") 

#Better see the clusters
library(plyr)

find_hull <- function(pcIris) pcIris[chull(pcIris$pc1, pcIris$pc2), ]
hulls <- ddply(pcIris,"Species", find_hull)

ggplot(pcIris, aes(x=pc1, y=pc2, color=Species)) + geom_point(size=6, alpha=0.5)+theme_bw()+ labs(title = "Iris")+ geom_polygon(data=hulls, alpha=.2, aes(fill=Species))

#Plot the variance percentage impact of PC1 and PC2
pcIris <- subset(pcIris, select=c(pc1,pc2,Sepal.Length:Species))
percentage <- round((pcs$sdev*pcs$sdev) / sum((pcs$sdev*pcs$sdev)) * 100, 2)
percentage <- paste( colnames(pcIris), "(",paste(as.character(percentage), "%", ")", sep="") )

ggplot(pcIris, aes(x=pc1, y=pc2, color=Species)) + geom_point(size=6, alpha=0.5)+theme_bw()+ labs(title = "Iris")+ xlab(percentage[1]) + ylab(percentage[2])

#other option
install.packages("ggfortify")
library(ggfortify)
autoplot(pcs, data=iris, size=4, alpha=0.5, colour='Species', loadings=TRUE,loadings.label = TRUE )+theme_bw()



#Clustering Analysis####################
#K-Means===============
measures <- iris[,1:4]
numGroups = 3
myKMeans <- kmeans(measures,numGroups)
myKMeans

#The algorithm gives you quite a lot of information. We can use what we need to plot it and make it more easy to see 
#Let's create a new dataset to see the results 
kmeanIris <- pcIris
kmeanIris$cluster <- myKMeans$cluster
ggplot(kmeanIris, aes(x=pc1, y=pc2, col=Species)) + geom_point(size=4, alpha=0.5)+theme_bw()+ labs(title = "Iris")+ facet_wrap(~cluster)
#Even better we can use a facet_grid
ggplot(kmeanIris, aes(x=pc1, y=pc2, col=Species)) + geom_point(size=4, alpha=0.5)+theme_bw()+ labs(title = "Iris")+ facet_grid(Species~cluster)

#What if performe it on only 2 variables
measures2 <- with(measures, data.frame(Petal.Length, Sepal.Length))
numGroups = 3
myKMeans2 <- kmeans(measures2,numGroups)
myKMeans2
kmeanIris2 <- pcIris
kmeanIris2$cluster <- myKMeans2$cluster
ggplot(kmeanIris2, aes(x=Petal.Length, y=Sepal.Length, col=Species)) + geom_point(size=4, alpha=0.5)+theme_bw()+ labs(title = "Iris")+ facet_wrap(~cluster)
#Even better we can use a facet_grid
ggplot(kmeanIris2, aes(x=Petal.Length, y=Sepal.Length, col=Species)) + geom_point(size=4, alpha=0.5)+theme_bw()+ labs(title = "Iris")+ facet_grid(Species~cluster)


#We can use shape and color to distingusish
iris_clustered <- data.frame(kmeanIris, clustered=as.factor(kmeanIris$cluster))
ggplot(iris_clustered , aes(x=pc1, y=pc2, color=clustered, shape=Species)) + geom_point(size=4, alpha=0.5)+
  theme_bw()

kmName <- iris_clustered %>%
  mutate(ClustName = str_replace_all(clustered, c("1"="virginica","2" = "setosa","3" = "versicolor")))
  
Results <- as.factor(ifelse(kmName$ClustName==kmName$Species,"correct","wrong"))

KmNameResults <- cbind(kmName,Results)

#let's plot it 
ggplot(KmNameResults, aes(x=pc1, y=pc2, color=Results, shape=Species)) + geom_point(size=4)+
  theme_bw()+
  scale_color_manual(values=c("black","red" ))


#Alternatively we can use the k means results to draw a cricle around
library(cluster)
autoplot(pam(measures, 3), frame = TRUE, frame.type = 'norm', color=kmeanIris$Species)+theme_bw()

?pam

#UPMGA hierarchical analysis
distMeasures <- dist(measures)#distances between observations based on the 4 original measures
IrisHClust <- hclust(distMeasures, method="average")#Use the UPMGA algorithm to create the hierarchical cluster
print(IrisHClust)
#View the results
install.packages("ggdendro")
library(ggdendro)
library (tidyverse)
ggdendrogram(IrisHClust)
IrisHClust$labels <- iris$Species

#save the dendrogram as a dendrogram
dend <- as.dendrogram(IrisHClust)
dend_data <- dendro_data(dend, type = "rectangle")
names(dend_data)
head(dend_data$segments)
head(dend_data$labels)
# Let's add some color:
ggplot(dend_data$segments) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend))+
  geom_text(data = dend_data$labels, aes(x, y, label = label, color=label),
            hjust = 1, angle = 90, size = 3)+
  ylim(-1, 5)+theme_dendro()

# But sort them based on their order in dend:
install.packages('dendextend')
library(dendextend)


dend%>% 
  set("branches_k_color", k = 3) %>% 
  plot(main = "Default ")

ggd1 <- as.dendrogram(IrisHClust)
ggplot(ggd1, theme = theme_minimal()) 

# Create a radial plot and remove labels
ggplot(ggd1, labels = FALSE) + 
  scale_y_reverse(expand = c(0.2, 0)) +
  coord_polar(theta="x")

#Discriminant analysis==================
#1Train the model with the variation of species across the 4 numeric variables
#2 USe the Discriminant analyisis to predict the species based on those variables
library(MASS)
daModel <- lda(Species~Petal.Width+Sepal.Width+Petal.Length+
                 Sepal.Length, iris)
daModel

#let's see how well we trained it
prediction <- predict(daModel, iris)#assigns to each observation on the dataset a probability of belonging to each of the original 3 groups
#addint the prediction class to our Iris with PCS values
daIris <- pcIris
daIris$predicted <- prediction$class
#Plot it 
ggplot(daIris, aes(x=pc1, y=pc2, color=predicted)) +
  geom_point(size=3) + facet_wrap(~Species)+theme_bw()
#See how correct it was
install.packages("caret")
install.packages("e1071")
library(caret)
library(e1071)
confusionMatrix(daIris$Species, daIris$predicted)


?confusionMatrix
#END OF CLASS 8


#Challenge 7 try it I set it with the college dataset but if you have a datset with more than 3 numerical column try do the same on that
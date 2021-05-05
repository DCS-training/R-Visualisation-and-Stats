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
 
  
#1 Compute the PCA that will analyse if the combination of the tuition,salary average, loan default rate, and Median debt are connected to the type of university (public/private) 
#Remember that you need to first extract the numerical variables you want to use
#Then compute the PCA
#Then add PC1 and PC2 to the original dataset
#Finally plot the result
#If you want you can use a Chull to draw the borders of the subsets

#2 USe the K-means to do a cluster analysis of the data 
#Start from extracting the numerical variables
#Compute the K-means (in this case the number of clusters is 2)
#Then add the cluster to the the original dataset
#Finally plot the results


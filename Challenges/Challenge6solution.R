#Challenge 6 
#Datasets  
library(tidyverse)
#Rodents
rodents <- read_csv('Datasets/RodentJoined.csv')
rodents <- rodents %>%
  mutate(species_id=as.factor(species_id), sex=as.factor(sex),genus=as.factor(genus), species=as.factor(species), taxa=as.factor(taxa), plot_type=as.factor(plot_type))

#We want to check the relationship between weight and sex but the sample need some cleaning
ggplot(rodents, aes(x=weight, fill=sex))+ geom_density(alpha=0.5) +theme_bw() 

#1)Remove the null vaules but only for sex and weight 
RodentsCleaned<- rodents[!(is.na(rodents$sex) | rodents$sex=="") & !(is.na(rodents$weight) | rodents$weight==""), ]

#2) Print a graph of the cleaned dataset that will show the difference in weight across male and female as boxplots
ggplot(RodentsCleaned, aes(x=sex,y=weight, fill=sex))+ geom_boxplot(alpha=0.5) +theme_bw()

#3) The two subsets (male and female) are both right skewed. Use the log transformation to remove the issue and then replot both a density graph and a boxplot
#calculate the natural log of weight
Weightlog <- log(RodentsCleaned$weight)
#Add it to the dataset
Rodenttotal <- cbind(Weightlog, RodentsCleaned)
#plot the logweight
ggplot(Rodenttotal , aes(Weightlog, fill=sex))+geom_density(alpha=0.5)+theme_bw()

ggplot(Rodenttotal, aes(x=sex,y=Weightlog, fill=sex))+ geom_boxplot(alpha=0.5) +theme_bw()

#4) Replot the boxplot that will print the male results first and then the female
Rodenttotal$sex <- factor(Rodenttotal$sex, levels(Rodenttotal$sex)[c(2,1)])
ggplot(Rodenttotal, aes(x=sex,y=Weightlog, fill=sex))+ geom_boxplot(alpha=0.5) +theme_bw()

boxplot.stats(Rodenttotal$Weightlog)

#5) Do 2 t-tests one that will test the weight against the sex and one the weightlog against the sex. Are the result different?
t.test(Rodenttotal$weight ~ Rodenttotal$sex)
t.test(Rodenttotal$Weightlog ~ Rodenttotal$sex)

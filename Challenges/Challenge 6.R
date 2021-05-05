#Datasets  
library(tidyverse)
#Rodents
rodents <- read_csv('Datasets/RodentJoined.csv')
rodents <- rodents %>%
  mutate(species_id=as.factor(species_id), sex=as.factor(sex),genus=as.factor(genus), species=as.factor(species), taxa=as.factor(taxa), plot_type=as.factor(plot_type))

#We want to check the relationship between weight and sex but the sample need some cleaning
ggplot(rodents, aes(x=weight, fill=sex))+ geom_density(alpha=0.5) +theme_bw() 

#1)Remove the null values but only for sex and weight 

#2) Print a graph of the cleaned dataset that will show the difference in weight across male and female as boxplots

#3) The two subsets (male and female) are both right skewed. Use the log transformation to remove the issue and then replot both a density graph and a boxplot

#4) Replot the boxplot that will print the male results first and then the female

#5) Do 2 t-tests one that will test the weight against the sex and one the weightlog against the sex. Are the result different?

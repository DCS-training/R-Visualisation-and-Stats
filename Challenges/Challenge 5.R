#Challenge 5 
#Datasets  
library(tidyverse)
#Rodents
rodents <- read_csv('Datasets/RodentJoined.csv')
rodents <- rodents %>%
  mutate(species_id=as.factor(species_id), sex=as.factor(sex),genus=as.factor(genus), species=as.factor(species), taxa=as.factor(taxa), plot_type=as.factor(plot_type))
rodents <- na.omit(rodents)
rodentsCleaned <-subset(rodents, genus!="Sigmodon" & genus!="Baiomys" & genus!="Neotoma")#Use this one for the Anova test I removed the problematic genus

#College
college <- read_csv('http://672258.youcanlearnit.net/college.csv')
college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

#Rember prior to any test you need to explicitate the null-hypothesis and check if the data fit the requirement (-i.e. they are normally distributed for parametric tests)

#1) Use the t-test to check if it is possible to dermine the sex of the rodent based on the weight of the animal. Remember to check the distribution and to formulate a null-hypothesis
#1a) Null Hypothesis
#1b) Is it normally distributed?
#1c)if the distribution is not normalised try answer the same question with the K.S. test. is there a difference in the results?

#2) Use the Anova test with Tukey multiple pairwise-comparisons to check if it is possible to dermine the genus of the rodent based on the weight of the animal. Remember to check the distribution and to formulate a null-hypothesis. Also remember to use the Use the RodentsCleaned for the Anova test where I removed the problematic genus
#2a) Null Hypothesis
#2b) Is it normally distributed?

#2c)Remember to use the Tukey multiple pairwise-comparisons and you can use the visual representation too. If you are using the visual representation use this lines of code to print it. Where TuckeyTest is however you are going to name the result of the Tukey multiple pairwise-comparisons results (the rest of the code will make sure to have the pairwise comparison visibe in the results)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))
plot(TukeyTest, las=1) 

#2d)Which are the pairs of genus for which it is not possible to use the weight to distinguish them? basically wich are the pairs for which the test fails and you have p-values above the thereshold?


#3 using the chi-squared test can you assess if there is a relationship between the region and the type (control) of the univeristies. 
#Remember to set the null-hypothesis

#This challenge is quite a lot so I will suggest you to do it in different moments

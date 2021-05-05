#Challenge 5 
library(tidyverse)
#Rodents
rodents <- read_csv('Datasets/RodentJoined.csv')
rodents <- rodents %>%
  mutate(species_id=as.factor(species_id), sex=as.factor(sex),genus=as.factor(genus), species=as.factor(species), taxa=as.factor(taxa), plot_type=as.factor(plot_type))
rodents <- na.omit(rodents)
rodentsCleaned <-subset(rodents, genus!="Sigmodon" & genus!="Baiomys" & genus!="Neotoma")
#College
college <- read_csv('http://672258.youcanlearnit.net/college.csv')
college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

#Rember prior to any test you need to explicitate the null-hypothesis and check if the data fit the requirement (-i.e. they are normally distributed for parametric tests)

#1) Use the t-test to check if it is possible to dermine the sex of the rodent based on the weight of the animal. Remember to check the distribution and to formulate a null-hypothesis
#1a) Null Hypothesis: the weight of the rodents is not influenced by their sex
#1b) Is it normally distributed? 
ggplot(rodents, aes(x=weight, fill=sex))+ geom_density(alpha=0.5)+theme_bw()#not really but let's try anyway

Male <- subset(rodents, sex=="M")
Female <- subset(rodents, sex=="F")

t.test(Male$weight, Female$weight)
t.test(rodents$weight ~ rodents$sex)
#1c)if the distribution is not normalised try answer the same question with the K.S. test. is there a difference in the results?
ks.test(Male$weight, Female$weight)


#2) Use the Anova test with Tukey multiple pairwise-comparisons to check if it is possible to dermine the genus of the rodent based on the weight of the animal. Remember to check the distribution and to formulate a null-hypothesis

#2a) Null Hypothesis: the weight of the animal is not influenced by the genus it belongs to 
#2b) Is it normally distributed? More or less
ggplot(rodentsCleaned, aes(x=weight, fill=genus))+ geom_boxplot(alpha=0.5) +theme_bw()

ggplot(rodentsCleaned, aes(x=weight, fill=genus))+ geom_density(alpha=0.5) +theme_bw()+facet_wrap(~genus)

ggplot(rodentsCleaned, aes(x=weight, fill=genus))+ geom_histogram(binwidth =10 ) +theme_bw()+facet_wrap(~genus, ncol=4)

anovaResult <- aov(rodentsCleaned$weight ~ rodentsCleaned$genus)
summary(anovaResult)
#Now use Tukey multiple pairwise-comparisons
TukeyTest <- TukeyHSD(anovaResult)
TukeyHSD(anovaResult)
print(TukeyTest)
par(mar=c(4,14,4,2)+0.9,mgp=c(4,2,0))
plot(TukeyTest, las=1) 

#Which are the pairs of genus for which it is not possible to use the weight to distinguish them? basically wich are the pairs for which the test fails and you have p-values above the thereshold? none


#4 using the chi-squared test can you assess if there is a relationship between the region and the type (control) of the univeristies. 
#Remember to set the null-hypothesis: the diffusion of private and public university is evenly distributed across the 4 region of the USA
#chi-squared test. 
chisq <- chisq.test(college$control, college$region)
# Observed counts
chisq$observed
#expected (evenly distributed-NullHypothesis)
chisq$expected
#Residual expected -observed
chisq$residuals
#Visualise the Chi_squared Results
install.packages("corrplot")
library(corrplot)
round(chisq$residuals, 3)
corrplot(chisq$residuals, is.cor = FALSE)

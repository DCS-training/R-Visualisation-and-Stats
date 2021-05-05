#HOMEWORKS from Tuesday
library(tidyverse)
college <- read_csv('http://672258.youcanlearnit.net/college.csv')
college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

p <-ggplot(college, aes(x=control, y=region))+ geom_bin2d() +theme_bw()
newdata <- ggplot_build(p)$data[[1]]
p + geom_text(data=newdata, aes((xmin + xmax)/2, (ymin + ymax)/2, 
                                label=count), col="white")+
  scale_fill_gradient(low = "grey80", high = "grey20")



##Our last Graph============
Books <- read_csv("Datasets/Books.csv")
ggplot(Books, aes(x=Pages, fill=Type))+ geom_density() +theme_bw()+facet_wrap(~Type, ncol=1)

#Again a 3rd way to save and export the summarising stats data 
#subplotting according to the type of Book and get the descriptive stats of the samples
Noir <- subset(Books, Type=="Noir")
Romance <- subset(Books, Type=="Romance")
ShortStories <- subset(Books, Type=="Short Stories")

means <- aggregate(Pages ~ Type, Books, mean)#Generate a table of the means according to type
names(means)[2] <- "Mean"#rename the mean column

median <- aggregate(Pages ~ Type, Books, median)#Generate a table of the median according to type
names(median)[2] <- "Median"#rename the median column
median[1] <- NULL  #removing the type column

sd<- aggregate(Pages ~ Type, Books, sd)#Generate a table of the sd according to type
names(sd)[2] <- "Sd"#rename the sd column
sd[1] <- NULL  #removing the type column

#To export the table created
TableValues <- cbind(means, median, sd)
write.csv(TableValues, "Datasets\\SumStat.csv", row.names = FALSE)




#TESTING THE DATASET#######
##Student T-test==========
#♦Can   
Books <- read_csv("Datasets/BooksB.csv")
ggplot(Books, aes(x=Weight, fill=Binding))+ geom_density() +theme_bw()+facet_wrap(~Binding, ncol=1)

#Are them normally distributed?
#Yes 
#I can go haed
#Null-Hypothesis: the type of binding does not influence the weight of the books
#Let's perform a t-test
#Divide the 2 samples 
GluedBook <- subset(Books, Binding=="Glued")
SewnBook <- subset(Books, Binding=="Sewn")

#T-Test
t.test(GluedBook$Weight, SewnBook$Weight)
t.test(Books$Weight ~ Books$Binding)

#What if I want to perform a t test on variables that has more than 2 values
#Need to do all the possible comparisons 
#T test on type of book and number of pages (3 Variables)

Noir <- subset(Books, Type=="Noir")
Romance <- subset(Books, Type=="Romance")
ShortStories <- subset(Books, Type=="Short Stories")

#Test 1 Noir and Romance
t.test(Noir$Weight, Romance$Weight)
#Test 1 second version
NoirRomance <- rbind(Noir, Romance)
t.test(NoirRomance$Weight ~ NoirRomance$Type)

#Test 2 Noir and Shortstories
t.test(Noir$Weight, ShortStories$Weight)
#Test 2 second version
NoirShortStories <- rbind(Noir, ShortStories)
t.test(NoirShortStories$Weight ~ NoirShortStories$Type)

#Test 3 Romance and Shortstories 
t.test(Romance$Weight, ShortStories$Weight)
#Test 3 second version
RomanceShortStories <- rbind(Romance, ShortStories)
t.test(RomanceShortStories$Weight ~ RomanceShortStories$Type)

#Since you repeat more than one test apply Bonferroni correction 
pValuesTypes <- c(2.2e-16, 2.2e-16, 2.2e-16)
p.adjust(pValuesTypes, method="bonferroni")
print(AdjutedPValuesTypes)



##Anova Test ==========
#♦Can I predict the lenght of a book based on the type of binding  
Books<- read_csv("Data/BooksB.csv")
ggplot(Books, aes(x=Pages, fill=Type))+ geom_density() +theme_bw()+facet_wrap(~Type, ncol=1)

#Are them normally distributed?
#Yes 
#I can go haed
#Null-Hypothesis: the type of book does not influence the number of pages
#Let's perform an anova test
anovaResult <- aov(Books$Pages ~ Books$Type)
summary(anovaResult)
#Now use Tukey multiple pairwise-comparisons
TukeyTest <- TukeyHSD(anovaResult)
TukeyHSD(anovaResult)
print(TukeyTest)
plot(TukeyTest)


#chi-squared test
chisq.test(Books$Type, Books$Binding)
chisq <- chisq.test(Books$Type, Books$Binding)
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


#Kolmogorov-Smirnov test for not normally distributed curves
ggplot(Books, aes(x=Pages, fill=Binding))+ geom_density() +theme_bw()+facet_wrap(~Binding, ncol=1)

#Not normally distributed
#use the subplot
ks.test(GluedBook$Pages, SewnBook$Pages)
t.test(GluedBook$Pages, SewnBook$Pages)#Still gives you a low p-values but only cause t.test is quite a robust test 

#Let's make a better example
Horses<- data.frame(legsize=rnorm(500, mean=400, sd=50), Type="Horse")
Pigs<- data.frame(legsize=runif(500, min=200, max=600), Type="Pig")
Animal<- rbind(Horses,Pigs)

ggplot(Animal, aes(x=legsize, fill=Type))+ geom_density(alpha=0.5) +theme_bw()

#Perform the tests

ks.test(Horses$legsize, Pigs$legsize)
t.test(Horses$legsize, Pigs$legsize)

#END OF CLASS 6
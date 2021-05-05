#CHALLENGE 2 SOLUTION#######
library(tidyverse)
college <- read_csv('http://672258.youcanlearnit.net/college.csv')
college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))
#3- Compute the mean, median, and standard deviation of the salary average across the 4 regions
ggplot(college, aes(x=faculty_salary_avg, fill=region)) + 
  geom_histogram(alpha=0.8, color="black", binwidth=750)+
  geom_vline(aes(xintercept = mean(faculty_salary_avg)),col='red',size=2)+
  theme_bw()+ 
  facet_wrap(~region, ncol = 1)+ 
  labs(title = "USA University", subtitle = "Average Income of the professors across the different regions", caption = "(based on data from the 2005 survey of USA Universities)", x = "Income average",  color = "Regions")+ 
  geom_vline(data=filter(college, region=="Midwest"),aes(xintercept =mean(faculty_salary_avg)) ,col='orange',size=2)+
  geom_vline(data=filter(college, region=="Northeast"),aes(xintercept = mean(faculty_salary_avg)),col='green',size=2)+
  geom_vline(data=filter(college, region=="South"),aes(xintercept = mean(faculty_salary_avg)),col='light blue',size=2)+
  geom_vline(data=filter(college, region=="West"),aes(xintercept = mean(faculty_salary_avg)),col='purple',size=2) 

 
#BOXPLOT##########
#Boxplots are an other way to visualise summarising statistic of the dataset breaked out according to a categorical value 
library(tidyverse)


#Let's get back to our iris dataset
##Subsetting the sample to see the difference across a categorical values

virginica <- subset(iris, Species=="virginica")
versicolor <- subset(iris, Species=="versicolor")
setosa <- subset(iris, Species=="setosa")

#Now lets calculate again and compare 
#Values of Virginica
mean(virginica$Petal.Width)
median(virginica$Petal.Width)
var(virginica$Petal.Width)
sd (virginica$Petal.Width)

#Values of versicolor
mean(versicolor$Petal.Width)
median(versicolor$Petal.Width)
var(versicolor$Petal.Width)
sd (versicolor$Petal.Width)

#Values of setosa
mean(setosa$Petal.Width)
median(setosa$Petal.Width)
var(setosa$Petal.Width)
sd (setosa$Petal.Width)


ggplot(iris, aes(x=Petal.Width, fill=Species)) + 
  geom_histogram(alpha=0.8, color="black", binwidth=0.08)+
  geom_vline(aes(xintercept = mean(Petal.Width)),col='red',size=2)+
  theme_bw()+
  geom_vline(data=filter(iris, Species=="setosa"),aes(xintercept =mean(Petal.Width)) ,col='orange',size=2)+
  geom_vline(data=filter(iris, Species=="versicolor"),aes(xintercept = mean(Petal.Width)),col='green',size=2)+
  geom_vline(data=filter(iris, Species=="virginica"),aes(xintercept = mean(Petal.Width)),col='light blue',size=2)+
  facet_wrap(~Species, ncol = 1) 

#an other way to visualise it is using the Boxplot 
ggplot(iris, aes(x=Petal.Width, y=Species)) + 
  geom_boxplot()+
  theme_bw()

#adding some color-coded info 
ggplot(iris, aes(x=Species, y=Petal.Width, color=Species)) + 
  geom_boxplot()+
  theme_bw()

#you can also have a multilayered representation by plotting also the single observations
ggplot(iris, aes(x=Species, y=Petal.Width, color=Species)) + 
  geom_boxplot(outlier.color = "black")+
  geom_jitter(alpha =0.3)+
  theme_bw()

#removing the outliers from graph cause they can be confusing
ggplot(iris, aes(x=Species, y=Petal.Width, color=Species)) + 
  geom_boxplot(outlier.alpha = 0)+
  geom_jitter(size= 2, alpha =0.3)+
  theme_bw()


#what if I want to plot the mean too
means <- aggregate(Petal.Width ~ Species, iris, mean)

ggplot(iris, aes(x=Species, y=Petal.Width, color=Species)) + 
  geom_boxplot()+
  theme_bw()+
  stat_summary(fun=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = FALSE) + 
  geom_text(data = means, aes(label = Petal.Width, y = Petal.Width + 0.08))


#let's play with the colours 
#using palettes
means <- aggregate(Petal.Width ~ Species, iris, mean)
ggplot(iris, aes(x=Species, y=Petal.Width, color=Species)) + 
  geom_boxplot(outlier.alpha = 0)+
  geom_jitter(size= 3, alpha =0.3)+
  theme_bw()+
  stat_summary(fun=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = FALSE) + 
  geom_text(data = means, aes(label = Petal.Width, y = Petal.Width + 0.08))+
  scale_color_brewer(palette="Accent")#you need to use a scale_fill_brewer if the colours you are using are in the fill comand 
?scale_color_brewer
#in this case you shall use scale_fill_brewer because the color coded comand is fill and not colour
ggplot(iris, aes(x=Petal.Width, fill=Species)) + 
  geom_histogram(alpha=0.8, color="black", binwidth=0.08)+
  facet_wrap(~Species, ncol = 1)+
  scale_fill_brewer(palette="Dark2") +
  theme_bw()

#More palettes: wesanderson Palette
install.packages("wesanderson")
library(wesanderson)

ggplot(iris, aes(x=Species, y=Petal.Width, color=Species)) + 
  geom_boxplot(outlier.alpha = 0)+
  geom_jitter(size= 3, alpha =0.3)+
  theme_bw()+
  stat_summary(fun=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = FALSE) + 
  geom_text(data = means, aes(label = Petal.Width, y = Petal.Width + 0.08))+
  scale_color_manual(values = wes_palette("BottleRocket2", n = 3))

#many other palettes available 
browseURL("https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/")

#Print in Black and white [aes(shape=Species),]
ggplot(iris, aes(x=Species, y=Petal.Width, color=Species)) + 
  geom_boxplot(outlier.alpha = 0)+
  geom_jitter(aes(shape=Species),size= 3, alpha =0.3)+
  theme_bw()+
  stat_summary(fun=mean, colour="black", geom="point", 
               shape=18, size=3,show.legend = FALSE) + 
  geom_text(data = means,colour="black", aes(label = Petal.Width, y = Petal.Width + 0.08))+
  scale_color_grey(start = 0.6, end = 0.01) 

#Manually select colors 
ggplot(iris, aes(x=Species, y=Petal.Width, color=Species)) + 
  geom_boxplot(outlier.alpha = 0)+
  geom_jitter(size= 3, alpha =0.3)+
  theme_bw()+
  stat_summary(fun=mean, colour="black", geom="point", 
               shape=18, size=3,show.legend = FALSE) + 
  geom_text(data = means,colour= "black", aes(label = Petal.Width, y = Petal.Width + 0.08))+
  scale_color_manual(values=c("#80ec65", "#10ea31", "#700015"))

browseURL("https://www.hexcolortool.com/")

#Use gradients
ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Sepal.Width)) + geom_point(size=3)+theme_bw()

ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Sepal.Width, shape=Species)) + geom_point(size=4, alpha=0.8)+
  scale_color_gradientn(colours = rainbow(7))+
  theme_dark()

#export graphs
ggsave(filename='Figure1.tiff', dpi=600, path='Graphs')
#ggsave(
#filename,
#plot = last_plot(),
#device = NULL,
#path = NULL,
#scale = 1,
#width = NA,
#height = NA,
#units = c("in", "cm", "mm"),
#dpi = 300,
#limitsize = TRUE,
#...
#)


#Time for the 3rd challenge 

#END OF CLASS 4


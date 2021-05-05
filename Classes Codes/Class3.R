#READING A GRAPH###########
library(tidyverse)

college <- read_csv('http://672258.youcanlearnit.net/college.csv')
college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))


ggplot(college, aes(x=sat_avg, y=admission_rate, color=control )) + geom_point(size=3, alpha=0.2)+theme_bw()+facet_wrap(~control) + labs(title = "USA University", subtitle = "The relation between university rate of admission and the sat results", caption = "(based on data from the 2005 survey of USA Universities)", x = "SAT average", y= "Admission Rate", color = "Type of University")+geom_smooth()


#is there any relationship between SAT average and amdmission rate? 
#is this relationship influenced by the type of university (private/public)? and or the area of the USA where they are located?

#+geom_smooth()

browseURL("http://guessthecorrelation.com/")




#DESCRIPTIVE STATISTICS #############

library(datasets)  # For example datasets so that we can use iris
iris


## Measuring the central location =============

### mean ------------------
#mean(x, trim = 0, na.rm = FALSE, ...)
#x is the input vector, trim is used to drop some observations from both end of the sorted vector, na.rm is used to remove the missing values from the input vector

mean(iris$Petal.Width)#using mean formula

iris <- iris


#the mean is the sum of the values of one variable divided the number of observations
sum(iris$Petal.Width)/nrow(iris)#using mathematical mean formula

### Median ------------------
#median(x, na.rm = FALSE)
median(iris$Petal.Width)#using median formula

#Why do you think they are different?


### mode --------------------
#Unfortunately there is not a straightforward formula but you can use this 
# Create the function.
Modes <- function(Petal) {
  uniqv <- unique(iris$Petal.Width)
  uniqv[which.max(tabulate(match(iris$Petal.Width, uniqv)))]
  
}

# Calculate the mode using the user function.
result <- Modes(Petal)
print(result)

iris

## Measuring the dispersion =====================

###variance ---------------------
differences <- iris$Petal.Width - mean(iris$Petal.Width) #each observation minus mean of observation
differences

squareDiff <- differences ^2 #Squared previous values
SumSqaredDiffs <- sum(squareDiff) #Sum previous values
NumObs <- nrow(iris)-1 #number of rows - 1 cause we are considering one observation
variance <- SumSqaredDiffs/NumObs #sum of squared variance divided num observations
variance

var(iris$Petal.Width)

### Standar Deviation ---------------------
#A large standard deviation indicates that the data points are far from the mean, and a small standard deviation indicates that they are clustered closely around the mean. It is the squared root of the previous value

sqrt(variance)

sd (iris$Petal.Width)

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


#Print them as a table
Species <- c("setosa","versicolor", "virginica")

Mean <- c(mean(setosa$Petal.Width), mean(versicolor$Petal.Width), mean(virginica$Petal.Width))

Median <- c(median(setosa$Petal.Width), median(versicolor$Petal.Width), median(virginica$Petal.Width))

Variance <- c(round(var(setosa$Petal.Width), digits = 2), round(var(versicolor$Petal.Width), digits = 2), round(var(virginica$Petal.Width), digits = 2))

SD <- c(round(sd(setosa$Petal.Width), digits = 2), round(sd(versicolor$Petal.Width), digits = 2),round(sd(virginica$Petal.Width), digits = 2))

FullPlot <- as.data.frame(cbind(Species, Mean, Median, Variance, SD))


FullPlot 
##Graphic visualisation of summarising stats: histograms and density curves
#histogram
ggplot(iris, aes(x=Petal.Width, fill=Species))+ 
  geom_histogram(alpha=0.8,color="black", binwidth= sd(iris$Petal.Width)/10)+
  geom_vline(aes(xintercept = mean(Petal.Width)),col='red',size=2)+
  theme_bw()+facet_wrap(~Species, ncol = 1) 
 

#  color="black",
#+facet_wrap(~species, ncol = 1) 

#Density plot
ggplot(iris, aes(x=Petal.Width, fill=Species))+ 
  geom_density(alpha = 0.5)+
  geom_vline(aes(xintercept = mean(Petal.Width)),col='red',size=2)+
  theme_bw()+facet_wrap(~Species, ncol = 1)

#, fill=Species +facet_wrap(~Species, ncol = 1) 

#Using the subplot mean values to plot the differences
ggplot(iris, aes(x=Petal.Width, fill=Species)) + 
  geom_histogram(alpha=0.8, color="black", binwidth=0.08)+
  geom_vline(aes(xintercept = mean(Petal.Width)),col='red',size=2)+
  theme_bw()+
  geom_vline(aes(xintercept = mean(setosa$Petal.Width)),col='orange',size=2) +
  geom_vline(aes(xintercept = mean(versicolor$Petal.Width)),col='green',size=2) +
  geom_vline(aes(xintercept = mean(virginica$Petal.Width)),col='light blue',size=2)+ facet_wrap(~Species, ncol = 1)

+
  facet_wrap(~Species, ncol = 1) 

#let's fix the visualisation
ggplot(iris, aes(x=Petal.Width, fill=Species)) + 
  geom_histogram(alpha=0.8, color="black", binwidth=0.08)+
  geom_vline(data=filter(iris, Species=="setosa"),aes(xintercept = mean(Petal.Width)),col='red',size=2)+
  geom_vline(data=filter(iris, Species=="versicolor"),aes(xintercept = mean(Petal.Width)),col='green',size=2)+
  geom_vline(data=filter(iris, Species=="virginica"),aes(xintercept = mean(Petal.Width)),col='blue',size=2)+
  theme_bw()+
  facet_wrap(~Species, ncol = 1) 


#Challenge 2

#END OF CLASS 3



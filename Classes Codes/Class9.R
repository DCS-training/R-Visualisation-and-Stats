#Correlation and Regression###########


#let's compute the correlation
library(tidyverse)
ggplot(iris, aes(x=Petal.Width, y=Petal.Length)) + geom_point(size=4)+theme_bw()

#Lets calculate how much is it
cor(iris$Petal.Width, iris$Petal.Length)#By default it use Pearson

#How would it look on different variables
ggplot(iris, aes(x=Sepal.Width, y=Petal.Length)) + geom_point(size=4)+theme_bw()
cor(iris$Sepal.Width, iris$Petal.Length)

#let's use other methods 
cor(iris$Petal.Width, iris$Petal.Length, method = "kendall")
cor(iris$Petal.Width, iris$Petal.Length, method = "spearman")

#Quick challenge 
#Compute the correlation between Sepal.Width and Sepal.Length is it similar to what you see in for the two Petal variables? 
cor(iris$Sepal.Width, iris$Sepal.Length)


#Regression ################à
#can we guess the petal width measure based on the measures of the Petal length, Sepal length and Sepal width?
#Let's analyse our 4 variables 
cor(iris$Petal.Width, iris$Petal.Length)
ggplot(iris, aes(x=Petal.Length, y=Petal.Width)) + geom_point(size=4)+theme_bw()

cor(iris$Petal.Width, iris$Sepal.Width)
ggplot(iris, aes(x=Sepal.Width, y=Petal.Width)) + geom_point(size=4)+theme_bw()

cor(iris$Petal.Width, iris$Sepal.Length)
ggplot(iris, aes(x=Sepal.Length, y=Petal.Width)) + geom_point(size=4)+theme_bw()


#Let's calculate the model between Petal Width and Petal length
fit <- lm(Petal.Width ~ Petal.Length,  data=iris)
summary(fit)


#Visualise the regression line
ggplot(iris, aes(x=Petal.Length, y=Petal.Width)) + geom_point(size=4)+theme_bw()+geom_abline(slope=0.415755, intercept=-0.363076, col="red", size=2)
#Visualise the model results 
library(ggfortify)
autoplot(fit)

ggplot(iris, aes(x=Petal.Width, fill=Species)) + geom_density()+theme_bw()

# Multiple Linear Regression Example
Versicolor <- subset(iris, Species=="versicolor")
Virginica <- subset(iris, Species=="virginica")
Setosa <- subset(iris, Species=="setosa")

fitVersicolor <- lm(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data=Versicolor)
summary(fitVersicolor)

fitVirginica <- lm(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data=Virginica)
summary(fitVirginica)

fitSetosa <- lm(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data=Setosa)
summary(fitSetosa)


fit2 <- lm(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data=iris)
summary(fit2) # show results

autoplot(fitVirginica)
autoplot(fitSetosa)
autoplot(fitVersicolor)
autoplot(fit2)

#Check the interdependence indipendent variables
#Independent variables shall have a VIF values below 2
install.packages("car")
library(car)
car::vif(fit2)
#Oh no let's see what happen if I take out Sepal Length
fit3 <- lm(Petal.Width ~ Petal.Length + Sepal.Width, data=iris)
summary(fit3)
car::vif(fit3)
autoplot(fit3)

#There are other type of regressions we do not have time to talk about you can find more info here
browseURL("https://data-flair.training/blogs/r-nonlinear-regression/")

#Similarities and Difference coefficients#########

#Let 's import a new dataset 
library(tidyverse)
BookAndPublisher <- read_csv('Datasets/BookAndPublisher.csv')
BookAndPublisher <- BookAndPublisher %>%
  mutate(Idbla=as.factor(Id), Publisher=as.factor(Publisher),
         specific=as.factor(specifics))

#The package we are going to use is an old one so it wants matrixs and not dataframes 

#Firstly reshape the data
library(reshape2)

BookMatrix <- acast(BookAndPublisher, Id ~ specifics, value.var = "values")
BookMatrix

#Look at our data
ggplot (BookAndPublisher, aes(x=Id, y=values, fill=specifics))+
  geom_bar(stat="identity")+ theme_bw()+
  facet_wrap(~Publisher, scales="free_x")#scales="free_x" free the scale of x otherwise it will expect 9 bar for each subplot


#Simpson diversity coefficient========== 
install.packages("vegan")
library(vegan)
diversity(BookMatrix, index="simpson")

#Which Publisher offer more comodities?
#Save the result as a new objetc
diversityIndex <- diversity(BookMatrix, index="simpson")
#Transform it in a data frame
diversityIndex <- as.data.frame(diversityIndex)
#Get the rowname as variable and call it Id
diversityIndex  <- rownames_to_column(diversityIndex , var = "Id")
#Create a new vector name Editor with the editor info
Editor <- c("Brill", "Brill", "Brill", "Newton", "Newton", "Newton","Archaeopress","Archaeopress","Archaeopress")
#Bind them 
Total <- cbind(diversityIndex, Editor)
#Aggregate results for mean 
means <- aggregate(diversityIndex ~ Editor, Total, mean)
#So Archaeopress will be the editor that will offer you more comodities


#Jaccard Similariy Coefficients========== 
vegdist(BookMatrix, method="jaccard", binary=TRUE)
#To plot them we need to revert to tidy fortmats
Dist <- vegdist(BookMatrix, method="jaccard", binary=TRUE)
Dist2 <- as.matrix(Dist)
DistTidy <- melt(Dist2, varnames = c("BooksA", "BooksB"), value.name = "distance" )

p <- ggplot(DistTidy, aes(x=BooksA, y=BooksB, fill=distance, label=round(distance,2)))+geom_raster()+geom_text(color="black")+
  scale_fill_gradient(low = "white", high = "red")
p

p + geom_vline(xintercept=c(0,3.5,6.5), size=2)+
  geom_hline(yintercept=c(0,3.5,6.5), size=2)


#Morista-Horn overlap coefficients========== 
#this will take into account also the abundance not only the presence or not
Distances <- vegdist(BookMatrix, method="horn")
Distances
#To plot them we need to revert to tidy fortmats
Distances2 <- as.matrix(Distances)
DistancesTidy <- melt(Distances2, varnames = c("BooksA", "BooksB"), value.name = "distance" )

ggplot(DistancesTidy, aes(x=BooksA, y=BooksB, fill=distance, label=round(distance,2)))+geom_raster()+geom_text(color="black")+
  scale_fill_gradient(low = "white", high = "red")+ geom_vline(xintercept=c(0,3.5,6.5), size=2)+
  geom_hline(yintercept=c(0,3.5,6.5), size=2)


#If you want to further analise our new dataset go to the Challenge 8
#THE END 
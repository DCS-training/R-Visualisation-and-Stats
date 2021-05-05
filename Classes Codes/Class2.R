## The tidyverse assamblage  ==========================
install.packages("tidyverse")


# DATA STRUCTURE ######################

#Vector ==============================================
#One dimentional collection of data
y <- c(6,7,8,9,10)
is.vector(y)

x <- c("b","d","a","f","h")
is.vector(x)


#Matrix==============================================
#bidimential collection of data
m1 <- matrix(c(T, T, F, F, T, F), nrow = 2)
m1

m2 <- matrix(c("a", "b", 
               "c", "d"), 
             nrow = 2,
             byrow = T)
m2

#A matrix has data all of the same type

m3 <- matrix(c(1, "b", 
               "c", "d"), 
             nrow = 2,
             byrow = T)
m3
## Array more than 2 dimentsions===================================
#multidimentional collection of data 
# Give data, then dimensions (rows, columns, tables)
a1 <- array(c( 1:24), c(4, 3, 2))
a1

## Data Frame ==============================================

# Can combine vectors of the same length
#different type variables (similar to a table in spreadsheet)
vNumeric   <- c(1, 2, 3)
vCharacter <- c("a", "b", "c")
vLogical   <- c(T, F, T)

df1 <- cbind(vNumeric, vCharacter, vLogical)
df1  # Coerces all values to most basic data type

df2 <- as.data.frame(cbind(vNumeric, vCharacter, vLogical))
df2  # Makes a data frame with three different data types


# Clear environment
rm(list = ls()) 

# Clear console
cat("\014")  # ctrl+L


# Load base packages manually
library(datasets)  # For example datasets
?datasets
library(help = "datasets")

# SOME SAMPLE DATASETS #####################################

# iris data
?iris
iris
head(iris)

# UCBAdmissions
?UCBAdmissions
UCBAdmissions

# Titanic
?Titanic
Titanic

# state.x77
?state.x77
state.x77

#swiss
?swiss
swiss



# IMPORTING DATA FROM SPREADSHEET

library(tidyverse)
# Import CSV files with readr::read_csv() from tidyverse
df <- read_csv("Datasets/RodentSimplified.csv")
df <- read_csv("Datasets/StateData.csv")

# Import CSV files and modify the form and names of variables
df2 <- read_csv("Datasets/RodentSimplified.csv") %>%
  select(mo,dy,yr,period,species) %>% 
  mutate(period = as.factor(period)) %>%
  rename(month = mo) 

# Import CSV files from online repo
college <- read_csv('http://672258.youcanlearnit.net/college.csv')
college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

rm (list = ls()) #Remove everything



#PLOTTING GRAPHS WITH GGPLOT2##############
library(tidyverse)


# THE PROBLEM WITH PIE CHARTS
# Three data sets
pie.a <- c(22, 14, 18, 20, 14, 12)
pie.b <- c(20, 18, 16, 18, 16, 12)
pie.c <- c(12, 14, 20, 18, 14, 22)

# Changing graphical parameters for a minute
oldpar <- par()   # Stores old graphical parameters
par(mfrow    = c(1, 3),  # Num. rows/cols
    cex.main = 3)   #  Main title 3x bigger
colors <- c("grey98", "grey90", "lightskyblue", "lightgreen", "grey98", "grey90")
?colors

# Three pie charts side by side
# Is the green slice or blue slice bigger?
pie(pie.a, main = "Pie A", col = colors)
pie(pie.b, main = "Pie B", col = colors)
pie(pie.c, main = "Pie C", col = colors)

# Three bar charts side by side
# Is the green bar or blue bar bigger?
barplot(pie.a, main = "Bar A", col = colors)
barplot(pie.b, main = "Bar B", col = colors)
barplot(pie.c, main = "Bar C", col = colors)





##The Grammar of Graphics==========================
###Level 1 Data --------------------------
iris
###Level 2 and 3 aesthetics and Geometries --------------------------
ggplot(iris, aes(x=Petal.Length, y=Petal.Width)) + 
  geom_point()

###Level 2 aesthetics: use a third variable to color code --------------------------
ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species)) + geom_point()

###Level 2 aesthetics: use a forth variable to set the size of the dots --------------------------
ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species, size=4)) + geom_point()

###Level 3 geometry: increase the size of all dots --------------------------
ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species)) + geom_point(size=4)

###Level 4 faceting: Subdivide the visualisation in subplots --------------------------
ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species, size=Sepal.Width)) + geom_point()+facet_wrap(~Species)

###Level 4 faceting: Subdivide the visualisation in subplots according to 2 variables --------------------------
ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species, size=Sepal.Width)) + geom_point()+facet_grid(Petal.Width~Species)

###Level 5 Statistics: plot summ stats instead of the data --------------------------
ggplot(iris, aes(x=Petal.Length, y=Petal.Width)) + stat_summary_bin(fun.y="mean", geom = "bar")

###Level 6 Coordinates: attributes of the coordinate axis --------------------------
ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species, size=Sepal.Width)) + geom_point()+facet_wrap(~Species)+coord_polar()

###Level 7 Themes: change the global setting of the charts --------------------------
ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species, size=Sepal.Width)) + geom_point()+theme_bw()


ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species, size=Sepal.Width)) + geom_point()+theme_bw() + labs(title = "New plot title", subtitle = "A subtitle", caption = "(based on data from ...)", x = "New x label", y= "New y label", color = "Colours", size= "Sepal Width")

#END OF CLASS 2

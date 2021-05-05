# WELCOME TO R STUDIO ################################
## This is the interface you are going to get used to ======
# This is color coded in green beacuse it is a comment
# You can change the visualisation by going on Tools>Golobal Options

# Flag the soft-wrap R source files option to have the code going authomatically to the next raw when it is too long 

# To comment and un-comment strings you can use the shift+ctrl+c shortcut
#comments can be used to create paragraph and subparagraphs in your document

# THIS IS A LEVEL 1 HEADER #################################

## This Is a Level 2 Header ================================

### This is a level 3 header. ------------------------------

# BASIC OPERATIONS WITH R################################
## Math ================
8+5 #Basic math; press ctrl+enter to run it
#R works in vector [1] is the index number ctrl Enter

## Create a sequence ================
1:250 #Print numbers between 1 and 250 across several lines
#To Clear the console press ctrl+l
100:1 #Print numbers from 100 to 1
seq(10)#Print numbers from 1 to 10
seq(30, 0, by = -3)#Print numbers from 30 to 0 every 3

## Print a String ================
print('Hello World!')
#Prints "Hello World" in the console basically a string variable 

## Define Variables ================
x <- 1:5 #Put the numbers between 1-5 in the variable x
x #Displays the values we have set in x

#NB R is Case sensitive Y is not the same of y

y <- c(6,7,8,9,10) #puts numbers between 6-10 in y
y
#Short cut for <- is alt+-
### Assign the same value to multiple vectors---------------
a <- b <- c <- 3


## Vector math ================
x+y #sum the x sequence plus the y sequence
x*2 #multiply for 2 each number in x

#To remove variables from the environment
rm(x) #Remove an object from the workspace
rm (a, b) #Remove more than one
rm (list = ls()) #Remove everything


#INSTALLING PACKAGES ###################
#list of packages available by subject
browseURL("https://cran.r-project.org/web/views/")

#list of packages available by name
browseURL("https://cran.r-project.org/web/packages/available_packages_by_name.html")

## See current packages ==========================
library()#List the available ones
search()#List the ready to go ones 

## Install packages ==========================
#you can use: tools>Install packages
#or directly
install.packages("ggplot2")
install.packages("tidyverse")
?install.packages

## Activate packages ==========================
library(ggplot2)
library(tidyverse)
#to get info on the packages and browse available examples
browseVignettes(package = "ggplot2" )
browseVignettes()#this would open all available vignettes of all installed packages in your browser

## Check for updates ==========================
#tools>check for updates
update.packages()

## To unload packages ==========================
#unflag or reboot (everytime you close the system the non default packages get unloaded)
detach ("package:ggplot2", unload = TRUE)

## To Remove packages ==========================
#x in the packages windows 
remove.packages("nameofthepackage")

## The tidyverse assamblage  ==========================
install.packages("tidyverse")
browseURL("https://www.tidyverse.org/")
#END OF CLASS 1 
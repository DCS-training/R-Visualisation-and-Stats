#Create a chart that would show the relationship between the SAT average and the admissino rate 
#Can you see any relationship between the two values?
#Now see if this relationship is also influenced by the type of control of the University "control" and the region where the university is located.
#Plot this last info on separate subplot to better see the trends
#Make sure that the chart has a white background
#Give your Graph a title and a subtitle
#Rename the x and y axis and add a caption saying that it is based on the data from the University census.
 library(tidyverse)

ggplot(college, aes(x=sat_avg, y=admission_rate, color=control)) + geom_point(size=3, alpha=0.2)+theme_bw()+facet_grid(region~control) + labs(title = "USA University", subtitle = "The relation between university rate of admission and the sat results", caption = "(based on data from the 2005 survey of USA Universities)", x = "SAT average", y= "Admission Rate", color = "Type of University")


#If you want to have some fun learning how to eyeballing the correlation between 2 variables here is a nice website to play with 
browseURL("http://guessthecorrelation.com/")




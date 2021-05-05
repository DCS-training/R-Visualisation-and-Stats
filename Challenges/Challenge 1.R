#CHALLENGE 1#################
install.packages("tidyverse")
library("tidyverse")

#Using the following data 
# Import CSV files from online repo
college <- read_csv('http://672258.youcanlearnit.net/college.csv')
college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

#Create a chart that would show the relationship between the SAT average and the admission  rate 
#Can you see any relationship between the two values?
#Now see if this relationship is also influenced by the type of control of the University "control" and the region where the university is located.
#Plot this last info on separate subplot to better see the trends
#Make sure that the chart has a white background
#Give your Graph a title and a subtitle
#Rename the x and y axis and add a caption saying that it is based on the data from the University census.
#Add some trasparency to better see the overlapping points search here on how to do it
browseURL("https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html")

#Discuss on Teams the results
#We are going to discuss the result Next Tuesday (30/03)


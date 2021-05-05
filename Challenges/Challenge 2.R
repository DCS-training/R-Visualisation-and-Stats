#Dataset 
library(tidyverse)
college <- read_csv('http://672258.youcanlearnit.net/college.csv')
college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

#1- Create a graph that would analyse the central tendency and dispersion of the faculty salary average 


#2- Check if the Region where the universities are located influences the salary average


#3- Compute the mean, median, and standard deviation of the salary average across the 4 regions. Can you understand better the differences across the 4 areas now? Which way of showing the summarising stats of the sample do you find more intuitive 



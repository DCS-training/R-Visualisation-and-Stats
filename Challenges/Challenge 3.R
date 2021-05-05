# Using the following dataset 

library(tidyverse)
college <- read_csv('http://672258.youcanlearnit.net/college.csv')
college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

#1 Create a boxplot visualisation that would analyse the variation of the faculty salary average across the different regions

#2 Add to the visualisation the representation of the single observations (Tip: use geom_jitter)

#3 Set the parameters to make the graph more readable (size and alpha of the jitters, hide the outliers of the boxplot, change the colors to something you like more)

#4 Export the final graph 


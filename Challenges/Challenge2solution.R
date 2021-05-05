#Challenge 2 Solution

#Dataset 
library(tidyverse)
college <- read_csv('http://672258.youcanlearnit.net/college.csv')
college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

#1- Create a graph that would analyse the central tendency and dispersion of the faculty salary average 
ggplot(college, aes(x=faculty_salary_avg, fill=region)) + 
  geom_histogram(alpha=0.8, color="black", binwidth=750)+
  geom_vline(aes(xintercept = mean(faculty_salary_avg)),col='red',size=2)+
  theme_bw()+ 
  labs(title = "USA University", subtitle = "Average Income of the professors across the different regions", caption = "(based on data from the 2005 survey of USA Universities)", x = "Average Income")+facet_wrap(~region, ncol = 1) 

#2- Check if the Region where the universities are located influencesthe salary average
#,fill=region
#+facet_wrap(~region, ncol = 1) 

#3- Compute the mean, median, and standard deviation of the salary average across the 4 regions
Midwest <- subset(college, region=="Midwest")
Northeast <- subset(college, region=="Northeast")
South <- subset(college, region=="South")
West <- subset(college, region=="West")


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


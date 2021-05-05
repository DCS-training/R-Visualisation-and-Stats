# Using the following dataset 

library(tidyverse)
college <- read_csv('http://672258.youcanlearnit.net/college.csv')
college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

#1 Create a boxplot visualisation that would analyse the variation of the faculty salary average across the different regions
ggplot(college, aes(x=region, y=faculty_salary_avg, fill=region))+
  geom_boxplot()+
  theme_bw()

#2 Add to the visualisation the representation of the single observations (Tip: use geom_jitter)
ggplot(college, aes(x=region, y=faculty_salary_avg, color=region))+
  geom_boxplot()+
  geom_jitter()+
  theme_bw()

#3 Set the parameters to make the graph more readable (size and alpha of the jitters, hide the outliers of the boxplot, change the colors to something you like more)
#Bonus: Visualise the mean of the different boxplots
meansCollege <- aggregate(faculty_salary_avg ~ region, college, mean)

ggplot(college, aes(x=region, y=faculty_salary_avg, color=region))+
  geom_boxplot(outlier.alpha = 0)+
  geom_jitter(size= 3, alpha =0.3)+
  scale_color_manual(values = wes_palette("BottleRocket2", n = 4))+
  theme_bw()+
  stat_summary(fun.y=mean, colour="Black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + 
  geom_text(data = meansCollege, aes(label = faculty_salary_avg, y = faculty_salary_avg + 500), color="Black")


#4 Export the final graph 
ggsave(filename='Figure3.tiff', dpi=600, path='Graphs')



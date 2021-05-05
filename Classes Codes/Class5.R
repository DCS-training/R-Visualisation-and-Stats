#Recap class 4############
#3 Set the parameters to make the graph more readable (size and alpha of the jitters, hide the outliers of the boxplot, change the colors to something you like more)
library(tidyverse)
library(wesanderson)
college <- read_csv('http://672258.youcanlearnit.net/college.csv')
college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

meansCollege <- aggregate(faculty_salary_avg ~ region, college, mean)
meansCollege <- meansCollege %>%
  mutate(faculty_salary_avg= round(faculty_salary_avg,1))


ggplot(college, aes(x=region, y=faculty_salary_avg, color=region))+
  geom_boxplot(outlier.alpha = 0)+
  geom_jitter(size= 3, alpha =0.3)+
  scale_color_manual(values = wes_palette("BottleRocket2", n = 4))+
  theme_bw()+
  stat_summary(fun=mean, colour="Black", geom="point", 
               shape=18, size=3,show.legend = FALSE) + 
  geom_text(data = meansCollege, aes(label = faculty_salary_avg, y = faculty_salary_avg + 500), color="Black")+ labs(title = "Salary and Areas of US", x = "Areas of US", y= "Average income for Professors", color = "Areas of US")

#export graph
ggsave(filename='Figure1.tiff', dpi=600, path='Graphs')

#VISUALISING PROBABILITY######
library(tidyverse)
#Creating 3 dataframes with r norm for 3 different population 
PinkRoses<- data.frame(Petal.Width=rnorm(500, mean=2.5, sd=0.7), Type="Pink")
RedRoses<- data.frame(Petal.Width=rnorm(500, mean=5, sd=0.3), Type="Red")

YellowRoses<- data.frame(Petal.Width=rnorm(500, mean=5.6, sd=1.2), Type="Yellow")

Roses <- rbind(PinkRoses,YellowRoses,RedRoses)


ggplot(Roses, aes(x=Petal.Width,  fill=Type))+ geom_histogram(binwidth = 0.2, color="black") +theme_bw()+facet_wrap(~Type, ncol=1)

ggplot(Roses, aes(x=Petal.Width, fill=Type))+ geom_density() +theme_bw()

#Challege 4

#nice youtube channel with good and easy video to explain Statistical Concepts 
browseURL("https://www.youtube.com/channel/UCtYLUTtgS3k1Fg4y5tAhLbw")


##Other types of distribution =======
###Uniform-------------
Uniform<- data.frame(Petal.Width=runif(500, min=20, max=30), Type="Pink")

ggplot(Uniform, aes(x=Petal.Width))+ geom_histogram(binwidth = 0.4, color="black") +theme_bw()

###Lognorm-------------
Lognorm <- data.frame(Petal.Width=rlnorm(500, meanlog=0, sdlog=1))

ggplot(Lognorm, aes(x=Petal.Width))+ geom_histogram(binwidth =0.25, color="black") +theme_bw()


# SOME MORE NICE GRAPHS OPTIONS

#dataset 
college <- read_csv('http://672258.youcanlearnit.net/college.csv')
college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

##Visualise the density on 2 numerical variables=======
#using +geom_density_2d() to convey to highlight the distribution
#basically is performing a 2d version of the density plot we were looking at for single variables last week

ggplot(college, aes(x=sat_avg, y=admission_rate, color=region)) + geom_point(alpha=0.5)+theme_bw()+facet_wrap(~region)+ geom_density_2d()

ggplot(college, aes(x=sat_avg, y=admission_rate, color=region))  +theme_bw()+facet_wrap(~region)+ stat_density_2d(aes(fill = ..level..), geom = "polygon")

install.packages("hexbin")
library(hexbin)
ggplot(college, aes(x=sat_avg, y=admission_rate) ) +
  geom_hex(bins = 25) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()
+facet_wrap(~region)

##Assessing the incidence of the subplots =======
###Bubble Graph----------
#when you analyse the sample you need to assess also how many data you have for each cluster of analysis (i.e. our species)
ggplot(college, aes(x=region, y=control, color=region))+ geom_count() +theme_bw()

ggplot(college, aes(x=region, y=state, color=region))+ geom_count() +theme_bw()
###Heatmap----------
ggplot(college, aes(x=control, y=region))+ geom_bin2d() +theme_bw()

#plot
p <-ggplot(college, aes(x=control, y=region))+ geom_bin2d() +theme_bw()

#Add the information 

# Get data from the plot created (save the results as a new dataframe)
newdata <- ggplot_build(p)$data[[1]]

# add in text labels
p + geom_text(data=newdata, aes((xmin + xmax)/2, (ymin + ymax)/2, 
                               label=count), col="white")+scale_fill_gradientn(colours = rainbow(7))

#quick challenge for yourself for home
#Using the information learned last week change the color-scheme of this last graph 

#END OF CLASS 5




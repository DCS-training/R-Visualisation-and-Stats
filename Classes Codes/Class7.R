library(tidyverse)

#Grouped, stacked and percent stacked barplot###########
#Dataset
Books <- read_csv("Datasets/BooksB.csv")
#Modify a little bit the dataset
BooksR <- subset(Books, Pages>30 & Pages<500)

ggplot(BooksR, aes(fill=Type, y=Weight, x=Type)) + 
  geom_bar(stat="identity")+theme_bw()

#Check the Type ad Type of Binging in relation to the weight
ggplot(BooksR, aes(fill=Binding, y=Weight, x=Type)) + 
  geom_bar(position="dodge", stat="identity")+theme_bw()
#Make it stacked
ggplot(BooksR, aes(fill=Binding, y=Weight, x=Type)) + 
  geom_bar(position="Stack", stat="identity")+theme_bw()
#Using percentage
ggplot(BooksR, aes(fill=Binding, y=Weight, x=Type)) + 
  geom_bar(position="fill", stat="identity")+theme_bw()

#Using the counts
ggplot(BooksR, aes(x=Type, fill=Binding))+ 
  geom_bar()+theme_bw()

#Add labels count
ggplot(BooksR, aes(x=Type, fill=Type)) + 
  geom_bar()+
  geom_text(aes(label=stat(count)),stat='count',  nudge_y=-20, color="white")+theme_bw()

#Using the percentage for categorical variable
#Percentage for bar
ggplot(BooksR, aes(x=Type, fill=Binding)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), position="fill")+theme_bw()+ scale_y_continuous(labels = scales::percent)+ labs(y="Percentage by Novel types", x="Type")

#Total percentage
ggplot(BooksR, aes(x=Type, fill=Binding)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)))+theme_bw()+ scale_y_continuous(labels = scales::percent)+ labs(y="Percentage books", x="Type")


#Adding legend
ggplot(BooksR, aes(x=Type, fill=Type)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  theme_bw()+ 
  scale_y_continuous(labels = scales::percent)+ labs(y="Percentage books", x="Type")+
  geom_text(aes(y = ((..count..)/sum(..count..)),label=scales::percent((..count..)/sum(..count..))), stat = "count", color="black", vjust = -0.25)


#Order your results###########
#Order result for specific graph
p <- ggplot(BooksR, aes(x=Type, fill=Type)) + 
  geom_bar()+
  geom_text(aes(label=stat(count)),stat='count',  nudge_y=-20, color="white", size=5)
p
p + scale_x_discrete(limits=c("Romance", "Short Stories", "Noir"))

#Order result for all dataset=========================
#If I replot it 
ggplot(BooksR, aes(x=Type, fill=Type)) + 
  geom_bar()+
  geom_text(aes(label=stat(count)),stat='count',  nudge_y=-20, color="white")


#Make sure the variable is recognise as factor
newBooks <- BooksR %>% 
  mutate(Type = as.factor(Type))
# Examine levels
newBooks %>% 
  pull(Type) %>% 
  levels()
#Let's check
levels(newBooks$Type)
levels(BooksR$Type)

#Reorder
newBooks$Type <- factor(newBooks$Type, levels(newBooks$Type)[c(2,3,1)])
#New plot version
ggplot(newBooks, aes(x=Type, fill=Type)) + 
  geom_bar()+
  geom_text(aes(label=stat(count)),stat='count',  nudge_y=-20, color="white")

levels(newBooks$Type)

#How to correct the dataset#############

#Use log scale==================
install.packages("sn")#package need to randomly create a skewed dataset
library(sn)
#create 2 right skewed datasets 
Parrot <- data.frame(weight=rsn(n=1000, xi=6, omega=2, alpha=3, tau=0, dp=NULL), type="parrot")
Albatros <- data.frame(weight=rsn(n=1000, xi=6.3, omega=2, alpha=3, tau=0, dp=NULL), type="albatros")
#Merge them
Birds <- rbind(Parrot, Albatros)
#Plot them 
ggplot(Birds, aes(weight, fill=type))+geom_density(alpha=0.5)+theme_bw()
ggplot(Birds, aes(type, weight))+geom_boxplot()+theme_bw()
#calculate the natural log of weight
Weightlog <- log(Birds$weight)
#Add it to the dataset
Birdstotal <- cbind(Weightlog, Birds)
#plot the logweight
ggplot(Birdstotal, aes(Weightlog, fill=type))+geom_density(alpha=0.5)+theme_bw()
ggplot(Birdstotal, aes(type, Weightlog))+geom_boxplot()+theme_bw()
#See the difference on a t-test
t.test(Birdstotal$weight ~ Birdstotal$type)
t.test(Birdstotal$Weightlog ~ Birdstotal$type)

#Remove Outliers===============
boxplot.stats(Birdstotal$weight)
Birdstotalrmout <- subset(Birdstotal, weight<11.1 ) 
ggplot(Birdstotalrmout, aes(type, weight))+geom_boxplot()+theme_bw()
ggplot(Birdstotalrmout, aes(type, Weightlog))+geom_boxplot()+theme_bw()
boxplot.stats(Birdstotalrmout$Weightlog)


#Remove null values##########
IrisNull <- read.csv("Datasets\\IrisNulls.csv")
#compute the mean
mean(IrisNull$Petal.Length)
#clean the whole sample
IrisCleaned<- na.omit(IrisNull)
#re compute on the cleaned sample
mean(IrisCleaned$Petal.Length)
mean(IrisCleaned$Petal.Width)
#Clean single columns
IrisCleaned2<- IrisNull[!(is.na(IrisNull$Petal.Length) | IrisNull$Petal.Length==""), ]
#calculate mean again
mean(IrisCleaned$Petal.Length)
mean(IrisCleaned2$Petal.Length)


#Change scale of Graphs axis######################

#Initial graph
Mygraph <- ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species)) + geom_point(size=6, alpha=0.5)+theme_bw()+ labs(title = "Iris", subtitle = "Relationship between Petal Lengh and Petal Width", x = "Petal Length", y= "Petal Width")
Mygraph
#Define limits axis
Mygraph +xlim(0, 10)+ylim(0, 3)
#Define limits and set intercept of X and Y
Mygraph +expand_limits(x=c(0,10), y=c(0, 3))
#Plot in the log scale
Mygraph + coord_trans(x="log10", y="log10")
#Quickly check if log would fix curve
ggplot(Birds, aes(weight, fill=type))+geom_density(alpha=0.5)+theme_bw()+ coord_trans(x="log")+expand_limits(x=c(3,15))

#Change unit of measurment
ggplot(iris, aes(x=Petal.Length/2.54, y=Petal.Width/2.54, color=Species)) + geom_point(size=6, alpha=0.5)+theme_bw()+ labs(title = "Iris", subtitle = "Relationship between Petal Lengh and Petal Width", x = "Petal Length (Inches)", y= "Petal Width(Inches)")+expand_limits(x=c(0,3), y=c(0,1.5))

#Set the thicks in the grid
ggplot(iris, aes(x=Petal.Length/2.54, y=Petal.Width/2.54, color=Species)) + geom_point(size=6, alpha=0.5)+theme_bw()+ labs(title = "Iris", subtitle = "Relationship between Petal Lengh and Petal Width", x = "Petal Length (Inches)", y= "Petal Width(Inches)")+expand_limits(x=c(0,4), y=c(0,1.5))+
  scale_x_continuous(minor_breaks = c(0,1,1.5,4), breaks = seq(0,4,2))+ scale_y_continuous(minor_breaks = seq(0,2, 0.25), breaks = seq(0,2,0.5))

#END OF CLASS 7



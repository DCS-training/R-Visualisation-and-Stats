#CHALLENGE 4#########
#Dataset 

Books<- read_csv("Datasets/Books.csv")

#1)Analyse the content of the Books file
#Extract mean and SD of the pages in the sample 
#Do the same for each category
mean(Books$Pages)
sd(Books$Pages)

means <- aggregate(Pages ~ Type, Books, mean)
SDs <- aggregate(Pages ~ Type, Books, sd)

analysis <- as.data.frame(cbind(means$Type, means$Pages, SDs$Pages))
analysis <- setNames(analysis, c("type","mean","sd"))


#2)Which type of distribution does the variable Pages have?
ggplot(Books, aes(x=Pages))+ geom_density() +theme_bw()
#3)Is it  a normal distribution?

#4)What about when you plot sepeparetly the different type of books?
#Can you describe the distribution of Romance books, Noirs and ShortStories
ggplot(Books, aes(x=Pages, fill=Type))+ geom_density() +theme_bw()

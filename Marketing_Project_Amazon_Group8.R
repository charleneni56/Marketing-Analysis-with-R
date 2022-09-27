---
## "Marketing Project - Group 8"
## "Chia-Ling Ni, Mihir Kungulwar, Tejaswini Edupuganti, Yi-Hsuan Tseng"
## "Mar 7, 2022"
---
  
###### set up environment ######
  
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse) 
library(tidyr) 
library(lubridate)
library(here)
library(skimr)
library(janitor)
library(rmarkdown)
library(corrplot)
library(reshape2)
library(expss)
library(textdata)
library(tidytext)
library(stargazer)
library(topicmodels)
library(tm)
library(treemapify)
library(ggthemes)
library(ggraph)
library(ggrepel)
library(ggwordcloud)
library(wordcloud)
library(wordcloud2)
library(widyr)
library(yardstick)
library(InformationValue)
library(pscl)
library(glm.deploy)
library(caret)
library(plyr)
library(cvms)


###### data set up and examine ######

# Read data from bestsellers with categories.csv using readcsv
bestsellers_with_categories <- read_csv("bestsellers with categories.csv")
mydata <- bestsellers_with_categories

# Check structure of the data
str(mydata)

# Print names of all the columns in the dataset
colnames(mydata)

# Clean the column names: Renaming User.Rating to Rating for easier use 
names(mydata)[3] <- c("Rating")

# Summary statistics of mydata
summary(mydata)

sum(is.na(mydata)) # checking if there's missing values
na.omit(mydata) # removing missing values

###### EDA and visualization ######
library(ggplot2)
library(tidyverse) 
library(tidyr) 
library(lubridate)
library(here)
library(skimr)
library(janitor)
library(rmarkdown)
library(corrplot)
library(reshape2)
library(ggthemes)
library(treemapify)
library(stargazer)
library(ggcorrplot)
library(dplyr)

# examine the correlation between Rating, Reviews, Price, Year in the dataset

cor_df = round(cor(mydata[c("Price","Rating","Reviews","Year")]),3)
ggcorrplot(cor_df,colors = c("black", "white", "orange"),hc.order=TRUE,lab=TRUE)

### EDA and Visualization of insights on Amazon Bestsellers data


# plot 1-1: Distribution of Genre
# Remove duplicate data based on distinct book names 
mydata_distinct<- mydata%>%distinct(Name,.keep_all =TRUE)
mydata_distinct <- as.data.frame(mydata_distinct)

mydata_distinct %>%
  select(Name,Genre) %>%
  distinct(Name,Genre) %>%
  group_by(Genre) %>%
  summarise(Count=n(), .groups = "drop")%>% 
  mutate(Percent=prop.table(Count) * 100) %>%
  ggplot(mapping = aes(x="", y=Percent,fill = Genre))+
  geom_bar(stat="identity",width = 1)+
  coord_polar("y",start = pi / 3)+
  theme_pander()+
  geom_label(aes(label = paste0(round(Percent,2), "%")), position = position_stack(vjust = 0.5),fontface = "italic")+
  theme(plot.title = element_text(hjust=0.5)) + 
  labs(title="Distribution of Genre")


# plot 1-2: Bestselling Books in terms of Genre across Years

# Transform Year from date to character datatype for analysis
mydata$Year <- as.character(mydata$Year)
# Sorting dataset by Year from 2009 to 2019
mydata$Year <- ordered(mydata$Year, levels = c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"))

ggplot(data=mydata, aes(y=Year, fill=Genre))+geom_bar(position="dodge")+coord_flip()+theme_pander()+
  labs(title="Number of Best Selling Fiction and Non Fiction Books across Years", x="Total Number of Books")


# plot 1-3: Trend in Number of Reviews for each User Rating

ggplot(data=mydata, aes(x=Rating, y=Reviews, fill=Rating))+geom_bar(stat="identity")+
  labs(title="Reviews Per User Rating", x="User Ratings", y="Total Reviews")+theme_pander()


# plot 1-4: Average price of fiction, non fiction and all genres from 2009 to 2019

totalbook<-mydata %>% mutate(Year = as.character(Year)) %>% group_by(Year)%>%
  summarise(Avg_Price_All_Genres= mean(Price),.groups = "drop") 

nonfictionbook<-mydata %>% filter(Genre=="Non Fiction") %>% mutate(Year = as.character(Year)) %>% 
  group_by(Year) %>% summarise(Non_Fiction_Avg_Price= mean(Price),.groups = "drop")

fictionbook<-mydata %>% filter(Genre=="Fiction") %>% mutate(Year = as.character(Year)) %>%
  group_by(Year) %>% summarise(Fiction_Avg_Price= mean(Price),.groups = "drop") 

merge_1<- merge(totalbook, nonfictionbook, by.x="Year", by.y="Year")
merge_2<- merge(merge_1, fictionbook, by.x="Year", by.y="Year")
mydata_avg_price<- melt(merge_2, id.vars="Year")
mydata_avg_price<- mydata_avg_price %>% mutate_if(is.numeric, ~round(., 2))

ggplot(data=mydata_avg_price,aes(x=Year, y=value,fill=variable))+geom_bar(stat="identity", position=position_dodge(0.8))+
  theme_pander()+theme(plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c('#FFFF00','#56B4E9','#00FF00'))+ 
  labs(x="Year", y="Average Price", title="Average Price Distribution by Year")+
  theme(legend.position="right")

# plot 1-5: Best seller's review numbers by Genre across year
ggplot(data=mydata, aes(x=Year,y=Reviews, fill=Genre))+geom_col(position="dodge")+labs(y="Total number of reviews", title="Best Selling Boooks' Review Numbers by Genre across year")+theme_pander()

# plot 1-6: Top10 Authors based on Number of Reviews and Genre
# filter the top 10
mydata2  <- mydata %>% group_by(Author, Genre) %>% summarise(Reviews) %>% arrange(desc(Reviews)) %>% head(n=20)

ggplot(data=mydata2, aes(x=Author, y=Reviews, fill=Genre))+geom_bar(stat="identity")+facet_wrap(~Genre)+
  coord_flip()+labs(title="Top 10 Authors", subtitle="Based on Genre and Number of Reviews", y="Total Reviews")+theme(axis.text.x=element_text(hjust=1,vjust=1,angle=35))


### additional plot just for analysis: Top 10 Expensive Fiction and Non Fiction Books, didn't put in the paper

fiction<-mydata %>%
  filter(Genre=="Fiction")%>%
  group_by(Name) %>%
  summarise(Avg_Price=mean(Price),.groups = "drop") %>%
  arrange(desc(Avg_Price)) %>% 
  top_n(10, wt=Avg_Price)

ggplot(fiction, aes(x=reorder(Name, -Avg_Price), y=Avg_Price, fill=Name))+
  geom_point(stat='identity', col="Darkblue", size=4)+
  geom_segment(aes(x=Name,xend=Name,y=min(Avg_Price),yend=max(Avg_Price)),linetype="longdash",size=0.1)+
  coord_flip() +
  theme_pander()+
  scale_color_pander()+
  labs(x='Book Name', y='Average Price', title='Most Expensive Fiction Books')+
  geom_text(aes(x=Name, y=0.01, label=Avg_Price ),hjust=-3, vjust=1.2, size=4,colour="Darkblue", fontface="bold")+
  theme(plot.title = element_text(hjust=0.5),legend.position="none")

nonfiction<- mydata %>%
  filter(Genre=="Non Fiction")%>%
  group_by(Name) %>%
  summarise(Avg_Price=mean(Price),.groups = "drop") %>%
  arrange(desc(Avg_Price)) %>% 
  top_n(10, wt=Avg_Price)

ggplot(nonfiction, aes(x=reorder(Name, -Avg_Price), y=Avg_Price, fill=Name))+
  geom_point(stat='identity', col="Darkgreen", size=4)+
  geom_segment(aes(x=Name,xend=Name,y=min(Avg_Price),yend=max(Avg_Price)),linetype="longdash",size=0.1)+
  coord_flip() +
  theme_pander()+
  scale_color_pander()+
  labs(x='Book Name', y='Average Price', title='Most Expensive Non Fiction Books')+
  geom_text(aes(x=Name, y=0.01, label=Avg_Price ),hjust=-3, vjust=1.2, size=4,colour="Darkgreen", fontface="bold")+
  theme(plot.title = element_text(hjust=0.5),legend.position="none")


###### EDA text mining on "The Common Book Names of Top Selling Books" ######

# Remove duplicate data
mydata<-mydata%>%distinct(Name,.keep_all =TRUE)
mydata <- as.data.frame(mydata)

### EDA - Text Mining for Book Names

# Data Cleaning Process
mydata$Name <- tolower(mydata$Name)
mydata$Name <- gsub('books', 'book', mydata$Name)
mydata$Name <- gsub('5th', '', mydata$Name)
mydata$Name<-  gsub('6th', '', mydata$Name)
mydata$Name <- gsub("ii's", '', mydata$Name)
mydata$Name <- gsub("ii", '', mydata$Name)
mydata$Name <- gsub("[[:punct:]]", "", mydata$Name) 
mydata$Name <- gsub("[[:digit:]]+", " ", mydata$Name)

# Remove stop words and count the total number of each word
word_counts <- mydata %>%
  unnest_tokens(output=word, input=Name) %>%
  anti_join(stop_words, by='word') %>%
  count(word,sort=TRUE)

# Tokenize the book names
name_word<-mydata %>%
  unnest_tokens(output=word, input=Name) %>%
  anti_join(stop_words, by='word') %>%
  filter(str_detect(word,"[:alpha:]")) %>%
  distinct()

# Each word total count by different authors
author_mention_word <- name_word%>%
  count(word, name='author_count',sort=TRUE) %>% 
  filter(author_count>=5) # Higher Count

author_mention_all <- name_word%>%
  count(word, name='author_count',sort=TRUE) #Overall Count

# Remove outliers
author_mention_word <- author_mention_word[-c(1),]
author_mention_all<- author_mention_all[-c(1),]

### Visualization 1
# cloud graph using worldcloud2
wordcloud2(data=author_mention_all, size=1.6, color='random-dark')

### Visualization 2

# Find correlation between words across users
words_correlations <- name_word %>%
  semi_join(author_mention_word, by = 'word') %>%
  pairwise_cor(item=word, feature=Author,sort=TRUE) %>%
  filter(correlation >= 0.2 & correlation < 1)

# High correlation words across users
words_correlations_high <- name_word %>%
  semi_join(author_mention_word, by = 'word') %>%
  pairwise_cor(item=word, feature=Author,sort=TRUE) %>%
  filter(correlation >= 0.5 & correlation < 1)

correlation<- format(round(words_correlations$correlation, 3))
correlation_high<- format(round(words_correlations_high$correlation, 3))

# Correlation Network plot   
ggraph(words_correlations, layout = 'fr') +
  geom_edge_link(aes(colour = factor(round(correlation,3)))) +
  geom_node_point() +
  geom_node_text(aes(label=name),repel = TRUE)+
  labs(title="Correlation between words across authors")+
  guides(colour = guide_legend(title = 'Correlation'))


# High Correlation Network plot   
ggraph(words_correlations_high, layout = 'fr') +
  geom_edge_link(aes(colour = factor(round(correlation,3)))) +
  geom_node_point() +
  geom_node_text(aes(label=name),repel = TRUE)+
  labs(title="High correlation between words across authors")+
  guides(colour = guide_legend(title = 'Correlation'))

###### Logistic Regression classification Analysis on Genre ######
library(readr)
library(tidyverse)
library(dplyr)
library(ggrepel)
library(ggplot2)
library(tidyr)
library(yardstick)
library(topicmodels)
library(tm)
library(caret) 
library(InformationValue)
library(pscl)
library(glm.deploy)
library(caret)
library(plyr)
library(cvms)

df <- read_csv(file='bestsellers with categories.csv')

df$my_genre = ifelse(df$Genre == 'Fiction',1,0)
write.csv(df,'df.csv')

mylogit<-glm(`my_genre`~`User Rating`+`Reviews`+`Price`, 
             family="binomial",data=df) 

summary(mylogit)

exp(coef(mylogit))

newdata1<-df[1:5,] 

probabilities1<-predict(mylogit,newdata=newdata1,type="response")
############################################################################
df <- read_csv(file = 'bestsellers with categories.csv')
df$my_genre = ifelse(df$Genre == 'Fiction',1,0)
df$my_genre = as.factor(df$my_genre)

set.seed(2022) 
trainfold<-trainControl(method="cv",number=10,search = "random", savePredictions = TRUE) 
#describe model for training - can use LeapForward or LeapBackward as method 
modelfold<-train(`my_genre`~`User Rating`+`Reviews`+`Price`,data=df,method="glm",trControl=trainfold) 

#print model summary and predictions 
summary(modelfold) 

#view final model 
modelfold$finalModel 
predictions = modelfold$pred 

#view predictions for each fold 
modelfold$resample 

Normal_Accuracy <- ((sum(predictions$pred == predictions$obs)) / nrow(predictions))*100

#create scatterplot of x1 vs. y1
plot(predictions$rowIndex, predictions$obs,main = "Without Author",xlab = 'Row Number',ylab = "Genre", col='red') 

#add scatterplot of x2 vs. y2
points(predictions$rowIndex, predictions$pred, col='blue')
legend(x=450,y=1.9,c("Misclassified","Classified"),title = 'Legend',cex=.8,col=c("red","blue"),pch=c(1,1))

############################################################################

dfa <- read_csv(file = 'bestsellers with categories.csv')
dfa$my_genre = ifelse(dfa$Genre == 'Fiction',1,0)
dfa$my_genre = as.factor(dfa$my_genre)

set.seed(2022) 
trainfolda<-trainControl(method="cv",number=10, savePredictions = TRUE) 
#describe model for training - can use LeapForward or LeapBackward as method 
modelfolda<-train(`my_genre`~`User Rating`+`Reviews`+`Price`+`Author`,data=dfa,method="glm",trControl=trainfolda) 
#print model summary and predictions 
summary(modelfolda) 

#view final model 
modelfolda$finalModel 
predictionsa = modelfolda$pred 

#view predictions for each fold 
modelfolda$resample 

After_Author <- ((sum(predictionsa$pred == predictionsa$obs)) / nrow(predictionsa))*100

#create scatterplot of x1 vs. y1
plot(predictionsa$rowIndex, predictionsa$obs,main = "With Author",xlab = 'Row Number',ylab = "Genre", col='red')

#add scatterplot of x2 vs. y2
points(predictionsa$rowIndex, predictionsa$pred, col='blue')

legend(x=450,y=1.9,c("Misclassified","Classified"),title = 'Legend',cex=.8,col=c("red","blue"),pch=c(1,1))

plottinga <- modelfolda$resample[c("Resample","Accuracy")]
plottinga$Resample = as.factor(plottinga$Resample)
class(plottinga$Accuracy)
plot(plottinga$Resample, plottinga$Accuracy, col='red')

temp_df <- data.frame(Normal_Accuracy,After_Author)
barplot(t(as.matrix(temp_df)),xlab = 'Variables',names.arg = c('Before Adding Author','After Adding Author'),ylab = 'Accuracy in %',beside=TRUE)

############################################################################

df <- read_csv(file = 'bestsellers with categories.csv')
df$my_genre = ifelse(df$Genre == 'Fiction',1,0)
df$my_genre = as.factor(df$my_genre)
sample<-sample.int(n=nrow(df), replace=FALSE,size=.75*(nrow(df))) 
trainset<-df[sample,] 
testset<-df[-sample,] 
model<-glm(`my_genre`~`User Rating`+`Reviews`+`Price`, 
           family="binomial",data=trainset) 

predicted <- predict(model, testset, type="response")
optimal <- optimalCutoff(testset$my_genre, predicted)[1]

cfm <- (confusionMatrix(testset$my_genre, predicted))
names(cfm)[1] <- 'Non Fiction'
names(cfm)[2] <- 'Fiction'
rownames(cfm)[1] <- 'Non Fiction'
rownames(cfm)[2] <- 'Fiction'

misClassError(testset$my_genre, predicted, threshold=optimal)
plotROC(testset$my_genre, predicted)

noauth_fi <- varImp(modelfold, scale = FALSE)
yesauth_fi <- varImp(modelfolda, scale = FALSE)
noauth_fi <- noauth_fi$importance
yesauth_fi <- yesauth_fi$importance
yesauth_fi$features <- rownames(yesauth_fi)
yesauth_fi <- head(arrange(yesauth_fi,desc(Overall)), n = 5)
barplot(t(as.matrix(noauth_fi)),xlab = 'Features',names.arg = c('User Rating','Reviews','Price'),ylab = 'Importance',beside=TRUE)
barplot(t(as.matrix(yesauth_fi$Overall)),xlab = 'Features',names.arg = c(yesauth_fi$features),ylab = 'Importance',beside=TRUE)


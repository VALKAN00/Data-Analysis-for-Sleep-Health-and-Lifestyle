
# Load the library
library(readxl)
library(rpart.plot)
library(magrittr)
library(rpart)
library(tidymodels)
library(plyr)
library(dplyr)
library(tidyverse)
library(here)
library(RColorBrewer)
library(VisitorCounts)
library(ISLR2)
library("zoom")
library(ggtext)
library(factoextra)
library(ggplot2)
library(ggpubr)

#read the data 
my_data<-read_xlsx("dataset.xlsx")

#### Starting Cleaning operation ####

#remove duplicated data
sum(duplicated(my_data))
duplicated(my_data)
my_data <-distinct(my_data)

#clean NUll or NA values in data
sum(is.na(my_data))
is.na(my_data)
my_data <-na.omit(my_data)

#info about data 
str(my_data)


#boxplot to detect outliers in data
boxplot(my_data$`Person ID`,my_data$Age,my_data$`Sleep Duration`,my_data$`Quality of Sleep`,my_data$`Physical Activity Level`,
        my_data$`Stress Level`,my_data$`Heart Rate`,my_data$`Daily Steps`)
#we can see that only 'heart rate'column contain some outlayers

#plot for outliers before cleaning
boxplot(my_data$`Heart Rate`)

#to get info to clean outliers
summary(my_data)

#outliers cleaning
which(my_data$`Heart Rate`>=80)
which(my_data$`Heart Rate`<65)
which(scale(my_data$`Heart Rate`)>3.3)
my_data2 <- my_data[-c(4,5,6,7,17,19,81,82,94,146,148,265,267,277,278),]

#plot for outlyers after cleaning
boxplot(my_data2$`Heart Rate`)

#another check after remove outliers form column
hist(my_data2$`Heart Rate`,
     xlab = "heart rate",
     main = "outliers check",
     breaks = sqrt(nrow(my_data2))
)

#plot for column need to normalize
boxplot(my_data2$`Daily Steps`)

#normalizing or scaling big numeric data
my_data2 <-cbind(my_data2,'Daily steps /100'=my_data2$`Daily Steps`/100)
my_data2<-my_data2[,-c(12)]


#plot for normalized column
boxplot(my_data2$`Daily steps /100`)

#another check for normalized column
hist(my_data2$`Daily steps /100`,
     xlab = "Daily steps /100",
     main = "normalization check",
)

#data plot after clean outlyers and normailze the data
boxplot(my_data2$`Person ID`,my_data2$Age,my_data2$Age,my_data2$`Sleep Duration`,my_data2$`Quality of Sleep`,my_data2$`Physical Activity Level`,
        my_data2$`Stress Level`,my_data2$`Heart Rate`,my_data2$`Daily steps /100`,ylim=c(0,400))


#info about data after cleaning
summary(my_data2)

#export the data in an excel sheet after cleaning
write.csv(my_data2,"sleep_Health.csv")

#### Strating Classification 'supervised learning' ####

#reading the dataset
data<-read.csv("Sleep_Health.csv")

#deleting a specific colmn
new_data<-data[,c(-1,-2)]

#viewing the new dataset
view(new_data)

#creating the trainset and testset
data_train_test<-function(new_data,size=0.75,train=TRUE){
  n_rows=nrow(new_data)#count number of rows in dataset
  total_rows=size*n_rows#return nth row to construct the train set 
  train_sample<-1:total_rows #from the first row to the nth one 
  if(train==TRUE){
    return(new_data[train_sample,])
  }
  else{
    return(new_data[-train_sample,])
  }
}

train_data<-data_train_test(new_data,size=0.75,train=TRUE)
test_data<-data_train_test(new_data,size=0.75,train=FALSE)

#printing the dimention of both
dim(train_data)
dim(test_data)


### Classification Decision Tree on Sleep Disorder ###

# Create a decision tree model specification
tree_specification <- decision_tree()%>%
  set_engine("rpart")%>%
  set_mode("classification")
tree_model <- tree_specification %>%
  fit(as.factor(Sleep.Disorder) ~Blood.Pressure+Age+Occupation, data = train_data ,minsplit=2)

#Prints a table of optimal prunings based on a complexity parameter
printcp(tree_model$fit)

#print the rules 
rules <- rpart.rules(tree_model$fit)
print(rules)


# Plot the decision tree
prp(tree_model$fit)
rpart.plot(tree_model$fit,type = 2, extra = 101, cex = 0.8, box.palette = "auto",box.col=c("royalblue3","steelblue1","slategray1"))

#prediction and accuracy for the model
predict_unseen <-predict(tree_model$fit, test_data, type = 'class')
table_mat <- table(test_data$Sleep.Disorder, predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test*100))

#print the summary 
summary(tree_model$fit)


### Regression Decision Tree on Heart Rate ###

# Create a decision tree model specification
tree_spec <- decision_tree()%>%
  set_engine("rpart")%>%
  set_mode("regression")
tree_fit <- tree_spec %>%
  fit(Heart.Rate ~Blood.Pressure+Stress.Level+Physical.Activity.Level+Gender , data = train_data,minsplit=2)

#Prints a table of optimal prunings based on a complexity parameter
printcp(tree_fit$fit)

#print the rules 
rules <- rpart.rules(tree_fit$fit)
print(rules)

# Plot the decision tree
prp(tree_fit$fit)
rpart.plot(tree_fit$fit,type = 2, extra = 101, cex = 0.8, box.palette = "auto",box.col=c("orchid", "plum1"))

#prediction and the accuracy of the model
predict<-predict(tree_fit$fit, test_data)

# Calculate RMSE and R-squared to see the preformance
metrics <- metric_set(rmse, rsq)
model_performance <- test_data %>%
  mutate(predictions = predict) %>%
  metrics(truth = Heart.Rate, estimate = predictions)

print(model_performance)

#print the summary 
summary(tree_fit$fit)

#### Starting Clustering 'unsupervised learning' by Kmeans ####

new_data<-data[,c(-1,-2)]
#using the set.seed() function in order to set a seed for R’s random number generator
set.seed(1234)
#select the numeric data 
k_data<-select(new_data,c(2,4,5,6,7,10,12))
#applaying kmeans
km_res<-kmeans(k_data,centers=3)
#visualise the clusters
fviz_cluster(km_res, data = k_data,
             palette = c("red3", "snow4", "deepskyblue4"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

#### Starting the Visualization ####

#Gender 
x<-table(data$Gender)
G_percentage<- paste0(round(100*x/sum(x),digits =2),"%")
pie(x,labels =G_percentage,main="Gender",col=c("pink","lightblue"))
legend("bottomright", legend = c("female", "male"), fill = c("pink", "lightblue"))
#Age{
par(mfrow=c(2,2))
hist(data$Age,col="skyblue",xlab="Age",main="Age Distribution") #age histogram
boxplot(data$Age,main="Age Distribution",col = "skyblue")
barplot(data$Age,name=data$Person.ID,xlab ="Person ID",ylab="Age",col="skyblue")
plot(data$Person.ID,data$Age,main="Ages",xlab ="ID",ylab="Age",col="skyblue")

ggplot(data=data,mapping = aes(Person.ID,Age))+geom_line(col="navy")
ggplot(data=data,mapping = aes(Person.ID,Age))+geom_point(aes(col=Age),alpha=0.5)+geom_smooth(se = FALSE)
ggplot(data=data,mapping = aes(Person.ID,Age))+geom_col(aes(col=Age),position =position_dodge())+geom_smooth(se = FALSE)


#}

#visualizing things that affects sleep duration:
#plot for the relationship between age and sleep duration
ggplot(data =data, mapping=aes(x=Age,y=Sleep.Duration))+ geom_point(aes(color =Sleep.Duration),alpha = 1.5)+
  theme_minimal()+labs(subtitle="27:59years old")
ggplot(data =data,mapping=aes(x=Age,y=Sleep.Duration))+geom_col(aes(fill =Quality.of.Sleep),position = position_dodge())+theme_minimal() 

#Gender and sleep duration
ggplot(data =data, mapping=aes(x=Gender,y=Sleep.Duration))+geom_col(aes(fill =Quality.of.Sleep),position = position_dodge())+theme_minimal()

#occupatuion 
oc<-table(data$Occupation)
oc
percentage<- paste0(round(100*oc/sum(oc),digits = 2),"%")
pie(oc,labels =percentage,main="occupations",col=c("red","green","blue","pink","yellow","skyblue","purple","gray","lightgreen","brown"))
legend("bottomright", legend = c("Accountant", "Doctor","Engineer","Lawyer","Manager","Nurse","Salesperson","Scientist","Software Engineer","Teacher"),
       fill=c("red","green","blue","pink","yellow","skyblue","purple","gray","lightgreen","brown"),cex=0.5)

#occupation and sleep duration:
ggplot(data =data, mapping=aes(x=Occupation,y=Sleep.Duration))+geom_col(aes(fill=Sleep.Duration),position = position_dodge())

#visualizing things that affects quality of sleep:
#plot for the relationship between age and quality of sleep
ggplot(data =data, mapping=aes(x=Age,y=Quality.of.Sleep))+geom_col(aes(fill=Quality.of.Sleep),position = position_dodge())+theme_minimal()
ggplot(data =data, mapping=aes(x=Age,y=Quality.of.Sleep))+geom_line(col="navy")+theme_minimal()
barplot(data$Age,name=data$Quality.of.Sleep,col = "lightblue")

#plot for the relationship between sleep duration and quality of sleep
ggplot(data =data, mapping=aes(x=Sleep.Duration,y=Quality.of.Sleep))+geom_col(aes(fill=Quality.of.Sleep),position =position_dodge())



#visualizing things that affects physical activity level:
#plot for the relationship between quality of sleep and physical activity level 
ggplot(data =data, mapping=aes(x=Quality.of.Sleep,y=Physical.Activity.Level))+geom_col(aes(fill=Physical.Activity.Level),position =position_dodge())

#plot for the relationship between sleep duration and physical activity level
ggplot(data =data, mapping=aes(x=Sleep.Duration,y=Physical.Activity.Level))+geom_col(aes(fill=Physical.Activity.Level),position =position_dodge())
ggplot(data =data, mapping=aes(x=Sleep.Duration,y=Physical.Activity.Level))+geom_col(aes(fill=BMI.Category),position =position_dodge()) #+BMI


#plot for the relationship between physical activity level and heart rate
ggplot(data =data, mapping=aes(x=Physical.Activity.Level,y=Heart.Rate))+geom_line()+theme_minimal()
ggplot(data =data, mapping=aes(x=Physical.Activity.Level,y=Heart.Rate))+geom_col(aes(fill=Heart.Rate),position =position_dodge())

#visualizing things that affects stress level:
#plot for the relationship between sleep duration and stress level  
ggplot(data =data, mapping=aes(x=Sleep.Duration,y=Stress.Level))+geom_line()
ggplot(data =data, mapping=aes(x=Sleep.Duration,y=Stress.Level))+geom_col(aes(fill=Stress.Level),position =position_dodge())

#plot for the relationship between quality of sleep and stress level 
ggplot(data =data, mapping=aes(x=Quality.of.Sleep,y=Stress.Level))+geom_col(aes(fill=Stress.Level),position =position_dodge())

#visualizing things that affects blood pressure:
#plot for the relationship between quality of sleep and blood pressure 
ggplot(data =data, mapping=aes(x=Quality.of.Sleep,y=Blood.Pressure))+geom_col(aes(fill=Blood.Pressure),position =position_dodge()) 

#plot for the relationship between sleep duration and blood pressure 
ggplot(data =data, mapping=aes(x=Sleep.Duration,y=Blood.Pressure))+geom_col(aes(fill=Blood.Pressure),position =position_dodge()) 


#plot for the relationship between blood pressure and physical activity level
ggplot(data =data, mapping=aes(x=Physical.Activity.Level,y=Blood.Pressure))+geom_col(aes(fill=Blood.Pressure),position =position_dodge()) 


#plot for the relationship between blood pressure and stress
ggplot(data =data, mapping=aes(x=Blood.Pressure,y=Stress.Level))+geom_col(aes(fill=Stress.Level),position =position_dodge())


#visualizing things that affects heart rate:
#plot for the relationship between blood pressure and heart rate
ggplot(data =data, mapping=aes(x=Blood.Pressure,y=Heart.Rate))+geom_col(aes(fill=Heart.Rate),position =position_dodge())



#plot for the relationship between quality of sleep  and heart rate
ggplot(data =data, mapping=aes(x=Quality.of.Sleep,y=Heart.Rate))+geom_line()+theme_minimal()
ggplot(data =data, mapping=aes(x=Quality.of.Sleep,y=Heart.Rate))+geom_col(aes(fill=Heart.Rate),position =position_dodge())

#plot for the relationship between sleep duration and heart rate
ggplot(data =data, mapping=aes(x=Sleep.Duration,y=Heart.Rate))+geom_line()+theme_minimal()
ggplot(data =data, mapping=aes(x=Sleep.Duration,y=Heart.Rate))+geom_col(aes(fill=Heart.Rate),position =position_dodge())

#plot for BMI category:
ggplot(data =data, mapping=aes(x=BMI.Category))+geom_bar(aes(fill=BMI.Category))

#BMI & physical activity level
ggplot(data =data, mapping=aes(x=BMI.Category,y=Physical.Activity.Level))+geom_col(aes(fill=Physical.Activity.Level),position =position_dodge())
ggplot(data =data, mapping=aes(x=BMI.Category,y=Physical.Activity.Level))+geom_point(aes(color=Physical.Activity.Level),position =position_dodge(width=0.5))

#BMI & occupation
ggplot(data =data, mapping=aes(x=BMI.Category,y=Occupation))+geom_col(aes(fill=Physical.Activity.Level),position =position_dodge())

#zoom
zm()

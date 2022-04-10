library(corrplot)
library('fastDummies')
library(ISLR)
library(caret)
library(glmnet)
library(ggplot2)
library(vip)
library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(ggpubr)


insurance_data = read.csv('F:/statCS/3rd year/Sem2/statistical learning/finalproject/insurance.csv')
View(insurance_data)
dim(insurance_data)
sum(is.na(insurance_data))
#no missing values
set.seed(1234)
train_index <- createDataPartition(insurance_data$charges,p=0.8,list = FALSE,times = 1)
train_data <- insurance_data[train_index,]
test_data <- insurance_data[-train_index,]

View(train_data)
dim(train_data)
lapply(train_data,function(x) { length(which(is.na(x)))})

glimpse(train_data)
freq(train_data)

ggplot(data = train_data,aes(age))+ geom_density(kernel="gaussian",fill = "lightblue" ,color ="black")
ggplot(data = train_data,aes(age))+ geom_histogram(binwidth = 5,fill = "lightblue" ,color ="black")
ggplot(data = train_data,aes(age))+ geom_freqpoly()
ggplot(data = train_data,aes(age))+ geom_boxplot(fill = "#FF34A0" ,color ="black")

ggplot(data = train_data,aes(bmi))+ geom_density(kernel="gaussian",fill = "lightblue" ,color ="black")
ggplot(data = train_data,aes(bmi))+ geom_histogram(binwidth = 5,fill = "lightblue" ,color ="black")
ggplot(data = train_data,aes(bmi))+ geom_freqpoly()
ggplot(data = train_data,aes(bmi))+ geom_boxplot(fill = "#F034A0" ,color ="black")
quantile(train_data$bmi,c(.25,.50,.75,.95,.99))

ggplot(data = train_data,aes(charges))+ geom_density(kernel="gaussian",fill = "lightblue" ,color ="black")
ggplot(data = train_data,aes(charges))+ geom_histogram(binwidth =100 ,fill = "lightblue" ,color ="black")
ggplot(data = train_data,aes(charges))+ geom_freqpoly()
ggplot(data = train_data,aes(charges))+ geom_boxplot(fill = "#F034A0" ,color ="black")
quantile(train_data$charges,c(.25,.50,.75,.95,.99))

ggplot(data = train_data,aes(as.factor(children)))+ geom_bar(fill = "skyblue" ,color ="black")

ggplot(data = train_data,aes(age,charges))+geom_point(color= "red",shape=2,size=0.5)
ggplot(data = train_data,aes(age,charges))+geom_point(color= "blue")+geom_quantile(color= "red")
ggplot(data = train_data,aes(age,charges))+geom_point(color= "blue")+geom_smooth(color= "red")

ggplot(data = train_data,aes(bmi,charges))+geom_point(color= "red")
ggplot(data = train_data,aes(bmi,charges))+geom_point(color= "blue")+geom_quantile(color= "red")
ggplot(data = train_data,aes(bmi,charges))+geom_point(color= "blue")+geom_smooth(color= "red")


ggplot(data = train_data,aes(sex,charges))+geom_boxplot(color= "black",fill="skyblue")+
  theme_bw()+theme(axis.text.x = element_text(angle = 90,vjust=0.5))+
  theme(axis.text.x = element_text(angle=90,vjust=1.0,color="black"))+
  theme(axis.text.y = element_text(vjust=1.0,color="black"))


ggplot(data = train_data,aes(smoker,charges))+geom_boxplot(color= "black",fill="darkslateblue")+
  theme_bw()+theme(axis.text.x = element_text(angle = 90,vjust=0.5))+
  theme(axis.text.x = element_text(angle=90,vjust=1.0,color="black"))+
  theme(axis.text.y = element_text(vjust=1.0,color="black"))



ggplot(data = train_data,aes(region,charges))+geom_boxplot(color= "black",fill="magenta")+
  theme_bw()+theme(axis.text.x = element_text(angle = 90,vjust=0.5))+
  theme(axis.text.x = element_text(angle=90,vjust=1.0,color="black"))+
  theme(axis.text.y = element_text(vjust=1.0,color="black"))

ggplot(data = train_data,aes(age,bmi))+geom_point(color= "red",shape=2,size=0.5)
ggplot(data = train_data,aes(age,bmi))+geom_point(color= "blue")+geom_quantile(color= "red")
ggplot(data = train_data,aes(age,bmi))+geom_point(color= "blue")+geom_smooth(color= "red")

ggplot(train_data, aes(x=sex,fill=smoker)) + 
  geom_bar(position="dodge")
      
ggplot(train_data, aes(x=sex,fill=region)) + 
  geom_bar(position="dodge") 

ggplot(train_data, aes(x=smoker,fill=region)) + 
  geom_bar(position="dodge") 


ggplot(data = train_data,aes(region,bmi))+geom_boxplot(color= "black",fill="skyblue")+
  theme_bw()+theme(axis.text.x = element_text(angle = 90,vjust=0.5))+
  theme(axis.text.x = element_text(angle=90,vjust=1.0,color="black"))+
  theme(axis.text.y = element_text(vjust=1.0,color="black"))

ggplot(data = train_data,aes(smoker,bmi))+geom_boxplot(color= "black",fill="skyblue")+
  theme_bw()+theme(axis.text.x = element_text(angle = 90,vjust=0.5))+
  theme(axis.text.x = element_text(angle=90,vjust=1.0,color="black"))+
  theme(axis.text.y = element_text(vjust=1.0,color="black"))

ggplot(data = train_data,aes(sex,age))+geom_boxplot(color= "black",fill="skyblue4")+
  theme_bw()+theme(axis.text.x = element_text(angle = 90,vjust=0.5))+
  theme(axis.text.x = element_text(angle=90,vjust=1.0,color="black"))+
  theme(axis.text.y = element_text(vjust=1.0,color="black"))

ggplot(data = train_data,aes(smoker,age))+geom_boxplot(color= "black",fill="sienna2")+
  theme_bw()+theme(axis.text.x = element_text(angle = 90,vjust=0.5))+
  theme(axis.text.x = element_text(angle=90,vjust=1.0,color="black"))+
  theme(axis.text.y = element_text(vjust=1.0,color="black"))


ggplot(data = train_data,aes(region,age))+geom_boxplot(color= "black",fill="steelblue4")+
  theme_bw()+theme(axis.text.x = element_text(angle = 90,vjust=0.5))+
  theme(axis.text.x = element_text(angle=90,vjust=1.0,color="black"))+
  theme(axis.text.y = element_text(vjust=1.0,color="black"))

ggplot(data = train_data,aes(region,bmi))+geom_boxplot(color= "black",fill="steelblue")+
  theme_bw()+theme(axis.text.x = element_text(angle = 90,vjust=0.5))+
  theme(axis.text.x = element_text(angle=90,vjust=1.0,color="black"))+
  theme(axis.text.y = element_text(vjust=1.0,color="black"))

train_data$sex<-recode(train_data$sex,"male"=1, "female"=0)
train_data$smoker<-recode(train_data$smoker, "yes"=1, "no"=0)
train_data <- dummy_cols(train_data, select_columns = 'region')
train_data<-subset(train_data, select = -region )
corrplot(cor(train_data, method = c("spearman")))
palette = colorRampPalette(c("blue", "white", "red")) (20)
heatmap(x = cor(train_data, method = c("spearman")), col = palette, symm = TRUE)
cor(train_data)

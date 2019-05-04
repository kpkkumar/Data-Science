#=========remove all from R environment
rm(list=ls(all=TRUE))

#Read the file
setwd("D:/DataScience/Imarticus/Final_Project_Mar19/Attrition")
infile = read.csv("Attrition.csv")
head(infile)
str(infile)
summary(infile)

##################################################missing values treatment
#install.packages("Amelia")
library(Amelia)
missmap(infile, y.at = 1,y.labels = "",col=c("red","green"),legend = FALSE)
sum(is.na(infile))
###No misisng values in the data set 
library(dplyr)

#feature engineering - OUTLIER ANALYSIS (Numeric data)
boxplot(infile$Age)
boxplot(infile$DailyRate)
boxplot(infile$DistanceFromHome)
boxplot(infile$HourlyRate)
boxplot(infile$MonthlyIncome)
summary(infile$MonthlyIncome)
infile$MonthlyIncome = scale(infile$MonthlyIncome)
str(infile$MonthlyIncome)
###Salary has outliers, need not to be treated 
boxplot(infile$MonthlyRate)
boxplot(infile$NumCompaniesWorked)
infile$NumCompaniesWorked = scale(infile$NumCompaniesWorked)
str(infile$NumCompaniesWorked)
####one of the employee has no. of companies morethan 8
boxplot(infile$PercentSalaryHike)
boxplot(infile$TotalWorkingYears)
infile$TotalWorkingYears = scale(infile$TotalWorkingYears)
##There are many employees who have 30+ years of working years 
boxplot(infile$TrainingTimesLastYear)
infile$TrainingTimesLastYear = scale(infile$TrainingTimesLastYear)

### Couple of employyes has done more than 4 hrs of training 
boxplot(infile$YearsAtCompany)
infile$YearsAtCompany = scale(infile$YearsAtCompany)
str(infile$YearsAtCompany)
boxplot(infile$YearsAtCompany)
###Manay employees are haivng more than 20+ Years at company 
boxplot(infile$YearsInCurrentRole)
###There are outliers with employees having > 15 years exp 
boxplot(infile$YearsSinceLastPromotion)
###There are outliers with employees having >6 years exp since last promition 
boxplot(infile$YearsWithCurrManager)
###There are outlier with employees woring with same manager more than 14 years 

############ Analysis of factor data #########
library(ggplot2)
## Attrition Vs Business Level 
table(infile$BusinessTravel)
ggplot(infile,aes(BusinessTravel,fill=Attrition))+geom_bar()+
ggtitle("Attrition Vs Business Level")->p1
p1   # Attrition high in employees travel rarely 

## Attrition Vs Department  
table(infile$Department)
ggplot(infile,aes(Department,fill=Attrition))+geom_bar()+
  ggtitle("Attrition Vs Department")->p2
p2   # Attrition high in employees working in R&D and then followed by Sales


## Attrition Vs EducationField 
table(infile$EducationField)
ggplot(infile,aes(EducationField,fill=Attrition))+geom_bar()+
  ggtitle("Attrition Vs EducationField")->p3
p3   
# Attrition high in employees in the filed of Lifescience, Medical followed 
# Marketing, Technical degree has employees<200 however the attrition is high

## Attrition Vs Gender 
table(infile$Gender)
ggplot(infile,aes(Gender,fill=Attrition))+geom_bar()+
  ggtitle("Attrition Vs Gender")->p4
p4
# More attrition in Male population than female

## Attrition Vs JobRole
table(infile$JobRole)
ggplot(infile,aes(JobRole,fill=Attrition))+geom_bar()+
  ggtitle("Attrition Vs JobRole")->p5
p5
     # More attrition in Laboratory Technitian, Sales Executive, 
     # SalesRepresentive and Research Scientist 


## Attrition Vs MaritalStatu
table(infile$MaritalStatus)
ggplot(infile,aes(MaritalStatus,fill=Attrition))+geom_bar()+
  ggtitle("Attrition Vs MaritalStatus")->p6
p6
     # More attrition in employees who are single 

## Attrition Vs Overtime
table(infile$OverTime)
ggplot(infile,aes(OverTime,fill=Attrition))+geom_bar()+
  ggtitle("Attrition Vs OverTime")->p8
p8
# More attrition in employees who are single 


#~~~~~~~~~~~~~~~~~~~~~~COnsolidate all plots in one single page 
library(gridExtra)
g1= grid.arrange(p1,p2,p3) 
g1
g2 = grid.arrange(p1,p2,p3)
g2
g3 = grid.arrange(p4,p5,p6)
g3
grid.arrange(p8)->g3
g3


#```````````````````````Draw Consolidated Corelation Marix````````````````
#correaltion matrix
library(corrplot)
library(psych)
infile = infile[,c(-10, -9, -22, -27)]
Infile_cor = infile

for(i in 1:ncol(Infile_cor)){
  
  Infile_cor[,i]<- as.integer(Infile_cor[,i])
}
corrplot(cor(Infile_cor))

# Years at current role, Years at company, Years with current manager
# are highly  corelated 


##@@@@@@@@@@@@@@@@@@@Modeling@@@@@@@@@@@@@@@@@@@@@##
#calculate base accuracy
prop.table(table(infile$Attrition))
summary(infile$Attrition)
#83.8% Accuracy 
head(infile$Attrition)
table(infile$Attrition)
##convert Yes or No to 1 and 0 befor modeling (Logistic dristibution is based on 0 or 1)
infile$Attrition= ifelse(infile$Attrition=="Yes",1,0)
str(infile$Attrition)
str(infile)
colnames(infile)

#infile = infile[,c(-10, -9, -22, -27)]
colnames(infile)
#splitting the data to train and test 
#install.packages("caTools")
library(caTools)
set.seed(1234)
split <- sample.split(infile$Attrition, SplitRatio = 0.7)
train <- subset(infile, split == TRUE)
test <- subset(infile, split == FALSE)
dim(train)
dim(test)
table(infile$Attrition)
table(infile$Attrition)
str(train)
str(test)

####################Build model#####################
colnames(train)
lm1 = glm(Attrition ~ ., data=train, family = binomial(link='logit'))

summary(lm1)
print(lm1)
# lm1 AIC is 664.2 

lm2 = glm(Attrition ~ BusinessTravel + DistanceFromHome + EnvironmentSatisfaction + JobInvolvement + 
            JobSatisfaction + JobLevel + MaritalStatus + NumCompaniesWorked + OverTime + RelationshipSatisfaction + 
            WorkLifeBalance + YearsInCurrentRole + YearsSinceLastPromotion , data=train, 
          family = binomial(link='logit'))
summary(lm2)
# lm2 AIC is 685.99

### lm2 has higher AIC tham lm1

library(caret)
###prediction using the model with train data 
pred1 = predict(lm1, data=train, type = "response")
#pred1 = predict(lm2, data=train, type = "response")
train$predict = ifelse(pred1 >= 0.5, 1,0)
table(train$Attrition, train$predict)
tab = table(train$Attrition, train$predict)
sum(diag(tab))/sum(tab)
str(train$Attrition)
train$predict = as.factor(train$predict)
str(train$predict)
str(train$Attrition)
train$Attrition = as.factor(train$Attrition)
str(train$Attrition)
confusionMatrix(train$Attrition, train$predict)


####Accuracy is 89.7% while using lm1 
####Accuracy is 88.4% while using lm2

###prediction using the model with test data 
pred2 = predict(lm1, newdata = test, type = "response")
pred2 = predict(lm2, newdata = test, type = "response")
test$predict = ifelse(pred2 >=0.5, 1,0)
str(test$predict)
str(test$Attrition)
test$predict = as.factor(test$predict)
test$Attrition = as.factor(test$Attrition)
table(test$Attrition, test$predict)
tab = table(test$Attrition, test$predict)
sum(diag(tab))/sum(tab)
confusionMatrix(test$Attrition,test$predict)

####Accuracy is 85.9% while using lm1 
####Accuracy is 84.5% while using lm2


###prediction using the model with test data @ thushold alue of 0.7
pred3 = predict(lm1, newdata = test, type = "response")
#pred3 = predict(lm2, newdata = test, type = "response")
test$predict = ifelse(pred3 >=0.7, 1,0)
table(test$Attrition, test$predict)
tab = table(test$Attrition, test$predict)
sum(diag(tab))/sum(tab)
test$predict = as.factor(test$predict)
str(test$predict)
str(test$Attrition)
confusionMatrix(test$Attrition,test$predict)
####Accuracy is 86.6% while using lm1 
####Accuracy is 85.2% while using lm2



# ROC Curve - Area under the curve
install.packages("ROCR")
library("ROCR")

str(pred3)

predic1 = prediction(pred3,test$Attrition)


# creating ROC curve
roc1<-performance(predic1,"tpr","fpr")
plot(roc1)
title("ROC Curve")
auc1<- performance(predic1, measure = "auc")
auc1 <- auc1@y.values[[1]]
auc1
###Roc is 0.82 is good while applying using lm1 model 
###RoC is 0.81 is same while applying using 1m2 model 

###False positive and True positive is nothing but Sensisitiviy and specifivity
###If sensitivity increase, specifivity decrease 
###ROC > 0.5 is good 

########################
library(caret)
ctrl1 <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
#install.packages("e1071")
require("e1071")
infile$Attrition<-as.factor(infile$Attrition)
mod_fit1 <- train(Attrition~.,  data=train, method="glm", family="binomial",
                  trControl = ctrl1)
summary(mod_fit1)


##predict with test data 
pred_cross1 = predict(mod_fit1, newdata=test)
unique(pred_cross1)
pred_cross2<-as.numeric(pred_cross1)
summary(pred_cross2)
pred_cross3<-pred_cross2-1
summary(pred_cross3)
pred_cross1<-pred_cross3
max(pred_cross1)

unique(pred_cross1)
pred_cross1<-as.numeric(pred_cross1)
summary(pred_cross1)
pred_cross1<-ifelse(pred_cross1>=0.5, 1,0)
test$Attrition<-as.factor(test$Attrition)

unique(test$Attrition)
unique(pred_cross1)
pred_cross1<-as.factor(pred_cross1)
confusionMatrix(pred_cross1, test$Attrition)
###Sensitivity is for positive where all positive is covered, Attrition detection
###Sepecicivity is for negative where all negatives
#variable importance
varImp(log.model1,scale=FALSE)
infile_cor <- train

for(i in 1:ncol(infile_cor)){
  
  infile_cor[,i]<- as.integer(infile_cor[,i])
}
cor(infile_cor)


##AIC after cross validation 658
##Accuracy is 86% after cross validation 
####Accuracy is 86.7% while using lm1 
####Accuracy is 85.2% while using lm2
## Concluding with intial model (lm1 which has accuracy of 86.7%) 




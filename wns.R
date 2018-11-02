library(xgboost)
library(data.table)
library(funModeling)
library(Matrix)
library(caret)
library(dummies)
library(neuralnet)

train <- read.csv("train_LZdllcl.csv",header=TRUE)
test <- read.csv("test_2umaH9m.csv",header=TRUE)
test$is_promoted <- 1
data <- rbind(train,test)
summary(data)
summary(train)

data$previous_year_rating <- ifelse(is.na(data$previous_year_rating)==TRUE,0,data$previous_year_rating)
data$education <- ifelse(data$education=="",'not_there',as.character(data$education))
data$education <- as.factor(data$education)

#library(Information)
#IV_dance_studio_data<-create_infotables(data=train, y="is_promoted", bins=10, parallel=FALSE)
#IV_values<-IV_dance_studio_data$Summary
#print(IV_dance_studio_data$Tables$region,row.names=FALSE)


data$department <- ifelse(data$department=='Operations'|data$department=='Procurement'|data$department=='Analytics','op_pr_ana',as.character(data$department))


data$department <- ifelse(data$department=='HR'|data$department=='Legal','hr_legal',as.character(data$department))

data$department <- ifelse(data$department=='R&D'|data$department=='Sales & Marketing','r&d_sales',as.character(data$department))


data$region <- ifelse(data$region =='region_18' |
                      data$region =='region_34' |
                      data$region =='region_24' |  
                      data$region =='region_33' |  
                      data$region =='region_29' |  
                      data$region =='region_32' |  
                      data$region =='region_21' |  
                      data$region =='region_5' |  
                      data$region =='region_6'  |
                      data$region =='region_9' ,'low',as.character(data$region))

data$region <- ifelse(  data$region =='region_31'|
                        data$region =='region_20'|
                        data$region =='region_11'|  
                        data$region =='region_12'|  
                        data$region =='region_19'|  
                        data$region =='region_14'|  
                        data$region =='region_15'|  
                        data$region =='region_16'|  
                        data$region =='region_2'|
                        data$region =='region_13'|
                        data$region =='region_26'|
                        data$region =='region_27'|
                        data$region =='region_30'|
                        data$region =='region_8'|
                        data$region =='region_1'|
                        data$region =='region_10' ,'Medium',as.character(data$region))
data$region <- ifelse(data$region =='region_17'|
                        data$region =='region_22'|
                        data$region =='region_23'|
                        data$region =='region_25'|
                        data$region =='region_28'|
                        data$region =='region_3'|
                        data$region =='region_4'|
                        data$region =='region_7'
                        ,'high',as.character(data$region))

data$education <- ifelse(data$education=='','not_availabale',as.character(data$education))
data$education <- as.factor(data$education)
data$department <- as.factor(data$department)
data$region <- as.factor(data$region)

data$agebytenure <- (data$age/data$length_of_service)
data$previous_year_rating <- ifelse(is.na(data$previous_year_rating)==TRUE,0,data$previous_year_rating)

summary(data)
data_1 <- dummy.data.frame(data, sep = "_")

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

maxmindf <- as.data.frame(lapply(mydata, normalize))

train <- maxmindf[1:54808,]
test <- maxmindf[54809:78298,]

names(train)[5]<-paste("department_r_d_sales")
names(train)[10]<-paste("education_Bachelors")
names(train)[11]<-paste("education_BelowSecondary")
names(train)[12]<-paste("education_Mastersabove")
names(train)[23]<-paste("KPIs_met")

names(test)[5]<-paste("department_r_d_sales")
names(test)[10]<-paste("education_Bachelors")
names(test)[11]<-paste("education_BelowSecondary")
names(test)[12]<-paste("education_Mastersabove")
names(test)[23]<-paste("KPIs_met")

library(neuralnet)
nn <- neuralnet(is_promoted ~
                  no_of_trainings+
                  age+
                  previous_year_rating+
                  length_of_service+
                  KPIs_met+
                  awards_won.+
                  avg_training_score+
                  agebytenure, data=train, hidden=c(1,1),err.fct = "ce", linear.output=FALSE)
nn$result.matrix
plot(nn)




summary(pred)
library(pROC)
my_roc <- roc(train$is_promoted, pred_train)
coords(my_roc, "best", ret = "threshold")

employee_id <- test$employee_id
a<-ifelse(pred>0.5,1,0)
submission <- as.data.frame(cbind(employee_id,a))
submission$is_promoted <- a

names(submission)[1]<-paste("employee_id")
names(submission)[2]<-paste("is_promoted")


write.csv(submission,"pred2_dt.csv")













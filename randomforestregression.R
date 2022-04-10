library("caret")
library("vip")
library("ISLR")
library("dplyr")
library('fastDummies')
library("randomForest")

insurance_data = read.csv('F:/statCS/3rd year/Sem2/statistical learning/finalproject/insurance.csv')
View(insurance_data)
dim(insurance_data)
sum(is.na(insurance_data))
#no missing values

insurance_data$sex<-recode(insurance_data$sex,"male"=1, "female"=0)
insurance_data$smoker<-recode(insurance_data$smoker, "yes"=1, "no"=0)
insurance_data <- dummy_cols(insurance_data, select_columns = 'region')
insurance_data<-subset(insurance_data, select = -region )



set.seed(1234)
train_index <- createDataPartition(insurance_data$charges,p=0.8,list = FALSE,times = 1)
train_data <- insurance_data[train_index,]
test_data <- insurance_data[-train_index,]

Train_x = model.matrix(charges~. ,train_data)
Train_y =train_data$charges
Test_x = model.matrix(charges~.,test_data)
Test_y = test_data$charges


#tuning parameter mtry

# Algorithm Tune (tuneRF)
set.seed(1234)
bestmtry <- tuneRF(Train_x,Train_y, stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)


ran_mod_tuned <-randomForest(charges~.,data = train_data,mtry = 4, 
                       importance = TRUE, na.action = na.omit)

predicted_RF_tuned <- predict(ran_mod_tuned,Test_x)
predicted_RF
SSE <- sum((predicted_RF_tuned - Test_y)^2)
SST <- sum((Test_y - mean(Test_y))^2)
R_square <- 1 - SSE / SST
RMSE = sqrt(SSE/nrow(test_data))
R_square
RMSE



#Grid search
set.seed(1234)
mtry <- sqrt(ncol(Train_x))
tunegrid <- expand.grid(.mtry=mtry)
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(1:9))
rf_gridsearch <- train(charges~., data=train_data, method="rf", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

ran_mod_tuned <-randomForest(charges~.,data = train_data,mtry = 4, 
                             importance = TRUE, na.action = na.omit)



predicted_RF_tuned <- predict(ran_mod_tuned,Test_x)
SSE <- sum((predicted_RF_tuned - Test_y)^2)
SST <- sum((Test_y - mean(Test_y))^2)
R_square_RF <- 1 - SSE / SST
RMSE = sqrt(SSE/nrow(test_data))
R_square
RMSE
library("caret")
library("glmnet")
library("vip")
library("ISLR")
library("dplyr")
library('fastDummies')
library("VIF")
library("xgboost")
library("gbm")      
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

#################################################################################################################
xgb_train = xgb.DMatrix(data = Train_x, label = Train_y)
xgb_test = xgb.DMatrix(data = Test_x, label = Test_y)

Xgb.model <- xgboost(data = xgb_train, max.depth = 2, nrounds = 50)
print(Xgb.model)
pred_y = predict(Xgb.model, xgb_test)
SSE <- sum((pred_y - Test_y)^2)
SST <- sum((Test_y - mean(Test_y))^2)
R_square <- 1 - SSE / SST
RMSE = sqrt(SSE/nrow(test_data))
R_square
RMSE
##################################################################################################################

############## GRADIENT BOOSTING ############################################################################

# create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(3, 5, 7),
  n.minobsinnode = c(3,5,7),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune <- gbm(
    formula = charges ~ .,
    distribution = "gaussian",
    data = train_data,
    n.trees = 1000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}


hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)


set.seed(123)

# train GBM model
gbm.fit.final <- gbm(
  formula = charges ~ .,
  distribution = "gaussian",
  data = train_data,
  n.trees = 16,
  interaction.depth = 5,
  shrinkage = 0.3,
  n.minobsinnode = 3,
  bag.fraction = 0.65,
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  
#PREDICT
pred <- predict(gbm.fit.final, n.trees = gbm.fit.final$n.trees, test_data)
pred
Test_y
# results
caret::RMSE(pred, Test_y)
SSE <- sum((pred - Test_y)^2)
SST <- sum((Test_y - mean(Test_y))^2)
R_square_gbm <- 1 - SSE / SST
R_square_gbm

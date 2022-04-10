library("caret")
library("glmnet")
library("vip")
library("ISLR")
library("dplyr")
library('fastDummies')

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

# Setting the range of lambda values
lambda_seq <- 10^seq(2, -2, by = -.1)
Ridge.fit = glmnet(Train_x,Train_y,alpha = 0,lambda  = lambda_seq)
summary(Ridge.fit)
lambdas <- Ridge.fit$lambda
#Doing k fold cross validation to select the best lambda

# Using cross validation glmnet
ridge_cv <- cv.glmnet(Train_x,Train_y,alpha = 0, lambda = lambdas)
# Best lambda value
best_lambda <- ridge_cv$lambda.min
best_lambda

best_fit <- ridge_cv$glmnet.fit
head(best_fit)


# Rebuilding the model with optimal lambda value
best_ridge <- glmnet(Train_x,Train_y, alpha = 0, lambda = best_lambda)
coef(best_ridge)

#predicting the test using ridge model
predicted_Ridge <- predict(best_ridge,s=best_lambda,Test_x)
predicted_Ridge


# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

RMSE(predicted_Ridge,Test_y)
eval_results(Test_y, predicted_Ridge, test_data)




# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(charges~., data = train_data,
                    method = "leapSeq", 
                    tuneGrid = data.frame(nvmax = 1:9),
                    trControl = train.control
)
step.model$results
step.model$bestTune
step.model
summary(step.model$finalModel)
coef(step.model$finalModel,7)
best_mod <- lm(charges~age+sex+bmi+children+smoker+region_northeast+region_northwest,data = train_data)
summary(best_mod)
View(test_data)
text_x <- test_data[,-6]
text_y <-test_data[,6]
text_x
text_y
predicted <- predict(step.model,text_x)
eval_results(text_y, predicted, test_data)



#stepAIC
step.model.AIC <- train(charges~., data = train_data,
                    method = "lmStepAIC",
                    trControl = train.control,
                    trace = FALSE
)
step.model.AIC$results
step.model.AIC$finalModel
summary(step.model.AIC$finalModel)

predicted_AIC<- predict(step.model.AIC,text_x)
eval_results(text_y, predicted_AIC, test_data)




library(leaps)

#Import prostate cancer dataset from ElemStatLearn library  
library(ElemStatLearn)
data("prostate", package = "ElemStatLearn")

#Convert prostate data from list to dataframe 
typeof(prostate)
prostate <-  as.data.frame(prostate,byrow=T)

#Seperate train data and test data of prostate data
train_data <- prostate[which(prostate$train == TRUE),]
train_data$train <- NULL

test_data <- prostate[which(prostate$train == FALSE),]
test_data$train <- NULL

#linear regression to train data
lm_prostate <- lm(lpsa ~ .,train_data)
summary(lm_prostate)

# ordinary least squared(OLS) error calculation
ols_error <- function(y,y_predicted){
  return((y-y_predicted)^2)
}

#train error
training_error <- mean(ols_error(train_data$lpsa, predict(lm_prostate,train_data[1:8])))
cat("Standard linear regression using OLS\n")
cat("training error of standard linear regression using OLS: ", training_error)


#test error
test_error <- mean(ols_error(test_data$lpsa, predict(lm_prostate,test_data[1:8])))
cat("\ntest error of standard linear regression using OLS: ", test_error)

#model training by coefficients of forward selection
model_train <- function(coefs){
  err <- c()
  train.frame <- cbind(train_data[9],train_data[names(coefs[-1])])
  model <- lm(lpsa ~ .,train.frame)
  yhat <- predict(model,train_data[names(coefs[-1])])
  y <- train_data[9]
  err <- append(err,ols_error(yhat,y))
  mse <- mean(err)
  return(mse)
}


#forward selection
library(leaps)
forward_selection <- regsubsets(train_data[1:8],train_data$lpsa,method='forward')
summary(forward_selection)
model_coeff <- coef(forward_selection,id=1:8)
model_coeff
forward_mse <- sapply(model_coeff, model_train)
forward_mse

#BIC and AIC
n <- nrow(train_data) 
bic <- rep(0,8)
aic <- rep(0,8)

#calculating AIC and BIC
for (i in 1:8){
  bic[i] = n*log(forward_mse[i])+log(n)*(1+i)
  aic[i] = n*log(forward_mse[i])+2*(1+i)
}


#BIC - choosing best model based on BIC
bic_forward_predictions <- names(model_coeff[[which.min(bic)]])[-1]
bic_forward_predictions
cat("\n\n BIC")
cat("\nthe best model by BIC consists of variables:",bic_forward_predictions)
bic_forward_test <- cbind(test_data[9],test_data[,bic_forward_predictions,drop=F])
bic_forward_best_model <- lm(lpsa ~ .,data=bic_forward_test)

#test error based on best BIC
bic_forward_yhat <- predict(bic_forward_best_model,test_data[,bic_forward_predictions,drop=F])
bic_forward_mse <- mean(ols_error(bic_forward_yhat,test_data[9]))
bic_forward_mse
cat("\ntest error by the model choosen by BIC: ",bic_forward_mse)

#AIC - choosing best model based on AIC
aic_forward_predictions <- names(model_coeff[[which.min(aic)]])[-1]
aic_forward_predictions
cat("\n\n AIC")
cat("\nthe best model by AIC consists of variables:",aic_forward_predictions)
aic_forward_test <- cbind(test_data[9],test_data[,aic_forward_predictions,drop=F])
aic_forward_best_model <- lm(lpsa ~ .,data=aic_forward_test)

#test error based on best AIC
aic_forward_yhat <- predict(aic_forward_best_model,test_data[,aic_forward_predictions,drop=F])
aic_forward_mse <- mean(ols_error(aic_forward_yhat,test_data[9]))
aic_forward_mse
cat("\ntest error by the model choosen by AIC: ",aic_forward_mse)


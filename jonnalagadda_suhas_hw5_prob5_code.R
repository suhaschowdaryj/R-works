library(lars)

#Import prostate cancer dataset from ElemStatLearn library  
library(ElemStatLearn)
data("prostate", package = "ElemStatLearn")

#Convert prostate data from list to dataframe 
head(prostate)
typeof(prostate)
prostate <-  as.data.frame(prostate,byrow=T)

#Seperate train data and test data of prostate data
train_data <- prostate[which(prostate$train == TRUE),]
train_data$train <- NULL

test_data <- prostate[which(prostate$train == FALSE),]
test_data$train <- NULL

#Cross validation for lasso regresion
model_seq <- seq(0,1,length = 100)
model1    <- cv.lars(as.matrix(train_data[,1:8]),as.matrix(train_data[,9]), K=5, index = model_seq, type = "lasso",
                     mode = "fraction")
model_mcv <- which.min(model$cv)

# ordinary least squared(OLS) error calculation
ols_error <- function(y,y_predicted){
  return((y-y_predicted)^2)
}

#lasso using minimum cv rule
prost_lasso <- lars(as.matrix(train_data[1:8]),as.matrix(train_data[9]),type='lasso')
best1 <- model_seq[model_mcv]
lasso_coeff <- predict(prost_lasso , s = best1, type = "coef", mode = "frac")
cat("best lambda using minimum cv rule:",lasso_coeff$s)
cat("\nestimated regression coefficients using minimum cv rule:\n",lasso_coeff$coefficients)

#test error
lasso_yhat_mincv <- predict(prost_lasso , as.matrix(test_data[1:8]), s = best1, type = "fit", mode = "frac")
test_error <- mean(ols_error(test_data$lpsa, lasso_yhat_mincv$fit))
cat("\ntest error using 5 fold minimum cv rule : ", test_error)
cat("\n\nOne standard deviation rule for CV")


# one-standard rule
bound<-model1$cv[model_mcv] + model1$cv.error[model_mcv]
bests2<-model_seq[min(which(model1$cv<bound))]
lasso_coef2 <- coef(prost_lasso, s=bests2, mode="frac")
lasso_coeff_onestandard <- predict(prost_lasso , s = bests2, type = "coef", mode = "frac")
cat("\nbest lambda using one standard deviation rule:",lasso_coeff_onestandard$s)
cat("\nestimated regression coefficients using one standard deviation rule:\n",lasso_coeff_onestandard$coefficients)

#test error
lasso_yhat_onestandard <- predict(prost_lasso , as.matrix(test_data[1:8]), s = bests2, type = "fit", mode = "frac")
test_error <- mean(ols_error(test_data$lpsa, lasso_yhat_onestandard$fit))
cat("\ntest error using 5 fold cv one standard deviation rule : ", test_error)

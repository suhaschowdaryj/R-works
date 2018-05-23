#hw 6 prob 6 : Adaptive lasso from lars

library(lars)

#Import prostate cancer dataset from ElemStatLearn library  
library(ElemStatLearn)
data("prostate", package = "ElemStatLearn")

#Convert prostate data from list to dataframe 
head(prostate,4)
typeof(prostate)
prostate <-  as.data.frame(prostate,byrow=T)

#Seperate train data and test data of prostate data
train_data <- prostate[which(prostate$train == TRUE),]
train_data$train <- NULL

test_data <- prostate[which(prostate$train == FALSE),]
test_data$train <- NULL

# ordinary least squared(OLS) error calculation
ols_error <- function(y,y_predicted){
  return((y-y_predicted)^2)
}

#initial weights using OLS
ols_model <- lm(as.matrix(train_data[9])~as.matrix(train_data[1:8]))
initial_beta <- ols_model$coefficients
initial_weights <- 1/initial_beta

#adaptive lasso initial train data
x_adalasso <- train_data[1:8]/initial_weights

#Cross validation for ada lasso regresion
model_seq <- seq(0,1,length = 100)
model1    <- cv.lars(as.matrix(x_adalasso),as.matrix(train_data[,9]), 
                     K=5, index = model_seq, type = "lasso", mode = "fraction")
model_mcv <- which.min(model1$cv)

#ada lasso using minimum cv rule
ada_lasso <- lars(as.matrix(x_adalasso),as.matrix(train_data[9]),type='lasso')
best1 <- model_seq[model_mcv]
ada_lasso_coeff <- predict(ada_lasso , s = best1, type = "coef", mode = "frac")
cat("best lambda using minimum cv rule:",ada_lasso_coeff$s)
cat("\nestimated regression coefficients using minimum cv rule:\n",ada_lasso_coeff$coefficients)

#test error
lasso_yhat_mincv <- predict(ada_lasso , as.matrix(test_data[1:8]), s = best1, type = "fit", mode = "frac")
test_error <- mean(ols_error(test_data$lpsa, lasso_yhat_mincv$fit))
cat("\ntest error using 5 fold minimum cv rule : ", test_error)
cat("\n\nOne standard deviation rule for CV")

# one-standard rule
bound<-model1$cv[model_mcv] + model1$cv.error[model_mcv]
bests2<-model_seq[min(which(model1$cv<bound))]
ada_lasso_coef2 <- coef(ada_lasso, s=bests2, mode="frac")
ada_lasso_coeff_onestandard <- predict(ada_lasso , s = bests2, type = "coef", mode = "frac")
cat("\nbest lambda using one standard deviation rule:",ada_lasso_coeff_onestandard$s)
cat("\nestimated regression coefficients using one standard deviation rule:\n",ada_lasso_coeff_onestandard$coefficients)

#test error
lasso_yhat_onestandard <- predict(ada_lasso , as.matrix(test_data[1:8]), s = bests2, type = "fit", mode = "frac")
test_error <- mean(ols_error(test_data$lpsa, lasso_yhat_onestandard$fit))
cat("\ntest error using 5 fold cv one standard deviation rule : ", test_error)

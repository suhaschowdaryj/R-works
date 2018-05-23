#question-6 
#Linear regression model

library(MASS)
library(ggplot2)

#generation of training data 

#train data - green labels
set.seed(2000)
mean1  =  t(c(2,1))
cov1   =  matrix(c(1,0,0,1),nrow=2)
green_train_data =  mvrnorm(100,mean1,cov1)
green_train_data =  data.frame(green_train_data)
green_train_data$label  =  1


#train data - red labels
set.seed(2000)
mean2  =  t(c(1,2))
cova2  =  matrix(c(1,0,0,1),nrow=2)
red_train_data  =  mvrnorm(100,mean2,cova2)
red_train_data  =  data.frame(red_train_data)
red_train_data$label  =  0

train_data  =  rbind(green_train_data,red_train_data)

#linear regression

relation= lm(train_data$label~train_data$X1 + train_data$X2 ,  train_data)

x1_coeff  =  relation$coefficients[2]
x2_coeff  =  relation$coefficients[3]
intercept_1 = relation$coefficients[1]


a = -(intercept_1-0.5)/x2_coeff
b = -(x1_coeff)/x2_coeff


#linear decision boundary and bayes decision boundary

plot(train_data[train_data$label == 1, "X1"] , train_data[train_data$label == 1, "X2"],
     xlab='X1',ylab='X2',col="green")
par(new=TRUE)
points(train_data[train_data$label == 0, "X1"] ,train_data[train_data$label == 0, "X2"],
       col="red")
abline(0,1,col = "blue")
abline(a,b,col="orange")
title('linear classifier and bayes classifier')
legend("topright",cex=0.7,legend=c("label = 1","label = 0","bayes","linear classifier"), 
        col=c("green","red","blue","orange"), pch = c("o","o","-","-"))


#generation of test data 

#test data - green labels
set.seed(2014)
mean_t1  =  t(c(2,1))
cov_t1   =  matrix(c(1,0,0,1),nrow=2)
green_test_data =  mvrnorm(500,mean_t1,cov_t1)
green_test_data =  data.frame(green_test_data)
green_test_data$label  =  1


#test data - red labels
set.seed(2014)
mean_t2  =  t(c(1,2))
cova_t2  =  matrix(c(1,0,0,1),nrow=2)
red_test_data  =  mvrnorm(500,mean_t2,cova_t2)
red_test_data  =  data.frame(red_test_data)
red_test_data$label  =  -1


test_data  =  rbind(green_test_data,red_test_data)
write.csv(test_data, file = "MyData.csv")


# linear classifier training error 
train_data$predicted = 1


train_data$predicted[(((train_data$X1 * x1_coeff) + (train_data$X2 * x2_coeff) +
                     intercept_1) > 0.5) &  train_data$label == 0] = 0


train_data$predicted[(((train_data$X1 * x1_coeff) + (train_data$X2 * x2_coeff) +
                         intercept_1) < 0.5) &  train_data$label == 1] = 0


linear_training_errorr=(nrow(train_data)-sum(train_data$predicted))/(nrow(train_data))
cat("linear classifier training error is ", linear_training_errorr)



# linear classifier test data error 
test_data$predicted = 1

test_data$predicted[(((test_data$X1 * x1_coeff) + (test_data$X2 * x2_coeff) +
                         intercept_1) > 0.5) &  test_data$label == 0] = 0


test_data$predicted[(((test_data$X1 * x1_coeff) + (test_data$X2 * x2_coeff) +
                        intercept_1) < 0.5) &  test_data$label == 1] = 0


linear_test_errorr=(nrow(test_data)-sum(test_data$predicted))/(nrow(test_data))
cat("\nlinear classifier test error is ", linear_test_errorr)



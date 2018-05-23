#hw 2, question-5 
#Bayesian decision boundary

library(MASS)
library(ggplot2)

#generation of training data 

#training data - green labels
set.seed(2000)
mean1  =  t(c(2,1))
cov1   =  matrix(c(1,0,0,1),nrow=2)
green_train_data =  mvrnorm(100,mean1,cov1)
green_train_data =  data.frame(green_train_data)
green_train_data$label  =  1


#training data - red labels
set.seed(2000)
mean2  =  t(c(1,2))
cova2  =  matrix(c(1,0,0,1),nrow=2)
red_train_data  =  mvrnorm(100,mean2,cova2)
red_train_data  =  data.frame(red_train_data)
red_train_data$label  =  0


train_data  =  rbind(green_train_data,red_train_data)


#Bayes classifier 
plot(train_data[train_data$label == 1, "X1"] , train_data[train_data$label == 1, "X2"],
     xlab='X1',ylab='X2',col="green")
par(new=TRUE)
points(train_data[train_data$label == 0, "X1"] ,train_data[train_data$label == 0, "X2"],
       col="red")
abline(0,1,col = "blue")
title('Bayes classifier')
legend("topright",legend=c("label = 1","label = 0"), col=c("green","red"),pch = 'o')


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
red_test_data$label  =  0


test_data  =  rbind(green_test_data,red_test_data)
write.csv(test_data, file = "MyData.csv")


# bayes training error 
# bayes decision boundary is x=y

train_data$predicted = 1
train_data$predicted[((train_data$X2>train_data$X1) & (train_data$label==1))]  = 0
train_data$predicted[((train_data$X2<train_data$X1) & (train_data$label==0))] = 0

bayes_training_errorr=(nrow(train_data)-sum(train_data$predicted))/(nrow(train_data))
cat("bayes training error is ", bayes_training_errorr)


# bayes test error 
test_data$predicted = 1
test_data$predicted[((test_data$X2>test_data$X1) & (test_data$label==1))]  = 0
test_data$predicted[((test_data$X2<test_data$X1) & (test_data$label==0))] = 0

bayes_test_errorr=(nrow(test_data)-sum(test_data$predicted))/(nrow(test_data))
cat("\nbayes test error is ",bayes_test_errorr)


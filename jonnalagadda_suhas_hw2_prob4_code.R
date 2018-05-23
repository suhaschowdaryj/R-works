#question-4 

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


#scatter plot of training data
plot(train_data[train_data$label == 1, "X1"] , train_data[train_data$label == 1, "X2"],
                     xlab='X1',ylab='X2',col="green")
par(new=TRUE)
points(train_data[train_data$label == 0, "X1"] ,train_data[train_data$label == 0, "X2"],
                    col="red")
title('Training data')
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


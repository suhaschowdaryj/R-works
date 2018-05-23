#question-4 LDA and Logistic regression

library(MASS)

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


#LDA 

#TRAIN ERROR
relation= lda(label~X1 + X2 , train_data)

prediction= predict(relation, newdata = train_data[,c(1,2)])$class

table(prediction, train_data$label)

lda_trainerror = mean(prediction != train_data$label)

cat("\nLDA training error is ", lda_trainerror)


#test error
prediction1= predict(relation, newdata = test_data[,c(1,2)])$class

table(prediction1, test_data$label)

lda_testerror = mean(prediction1 != test_data$label )

cat("\nLDA test error is ", lda_testerror)


#logistic regression

#train error
glm_relation= glm(label~X1 + X2 , train_data,family = "binomial")

glm_prediction= predict(glm_relation, newdata = train_data[,c(1,2)])

glm_prediction = ifelse(glm_prediction > 0.5,1,0)

table(glm_prediction,train_data$label)

glm_trainerror = mean(glm_prediction != train_data$label)

cat("\nLogistic regression training error is ", glm_trainerror)


#test error
glm_prediction1= predict(glm_relation, newdata = test_data[,c(1,2)])

glm_prediction1 = ifelse(glm_prediction1 > 0.5,1,0)

table(glm_prediction1,test_data$label)

glm_testerror = mean(glm_prediction1 != test_data$label)

cat("\nLogistic regression test error is ", glm_testerror)









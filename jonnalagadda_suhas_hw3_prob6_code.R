#hw3 question 6

library(MASS)

#generate ten centers, which are treated as fixed parameters
Sig <- matrix(c(1,0,0,1),nrow=2)
seed_center <- 16
set.seed(seed_center)
center_green <- mvrnorm(n=10,c(1,0),Sig)
center_red <- mvrnorm(n=10,c(0,1),Sig)
##define a function "gendata2" first
gendata2 <-function(n,mu1,mu2,Sig1,Sig2,myseed)
{
  set.seed(myseed)
  mean1 <- mu1[sample(1:10,n,replace=T),]
  mean2 <- mu2[sample(1:10,n,replace=T),]
  green <- matrix(0,ncol=2,nrow=n)
  red <- matrix(0,ncol=2,nrow=n)
  for(i in 1:n){
    green[i,] <- mvrnorm(1,mean1[i,],Sig1)
    red[i,] <- mvrnorm(1,mean2[i,],Sig2)
  }
  x <- rbind(green,red)
  return(x)
}

#generate the training set
seed_train <- 2000
ntrain <- 100
train2 <- gendata2(ntrain,center_green,center_red,Sig/5,Sig/5,seed_train)
ytrain <- c(rep(1,ntrain),rep(0,ntrain))

train2
ytrain

colnames(train2) = c("X1","X2")


#generate the test set
seed_test <- 2014
ntest <- 500
test2 <- gendata2(ntest,center_green,center_red,Sig/5,Sig/5,seed_test)
ytest <- c(rep(1,ntest),rep(0,ntest))
colnames(test2) = c("X1","X2")

test2
ytest

#linear regression for scenario 2
#fit data

relation= lm(ytrain ~ train2[,1] + train2[,2])

x1_coeff  =  relation$coefficients[2]
x2_coeff  =  relation$coefficients[3]
intercept_1 = relation$coefficients[1]


a = -(intercept_1-0.5)/x2_coeff
b = -(x1_coeff)/x2_coeff


#scatter plot with linear classifier
plot(train2[ytrain == 1, "X1"] , train2[ytrain == 1, "X2"],
     xlab='X1',ylab='X2',col="green", ylim = c(-2,3))
par(new=TRUE)
points(train2[ytrain == 0, "X1"] ,train2[ytrain == 0, "X2"], col="red")
abline(a,b,col="blue")
title('linear classifier')
legend("topright",cex=0.7,legend=c("label = 1","label = 0","linear classifier"), 
       col=c("green","red","blue"), pch = c("o","o","-"))


# linear classifier training error 

train_data =  cbind(train2,ytrain)
train_data = as.data.frame(train_data)
train_data$predicted = 1

train_data$predicted[(((train_data$X1 * x1_coeff) + (train_data$X2 * x2_coeff) +
                         intercept_1) > 0.5) &  train_data$ytrain == 0] = 0

train_data$predicted[(((train_data$X1 * x1_coeff) + (train_data$X2 * x2_coeff) +
                         intercept_1) < 0.5) &  train_data$ytrain == 1] = 0

linear_training_errorr=(nrow(train_data)-sum(train_data$predicted))/(nrow(train_data))

cat("linear classifier training error for scenario 2 is ", linear_training_errorr)



# linear classifier test data error 
test_data =  cbind(test2,ytest)
test_data = as.data.frame(test_data)

test_data$predicted = 1

test_data$predicted[(((test_data$X1 * x1_coeff) + (test_data$X2 * x2_coeff) +
                        intercept_1) > 0.5) &  test_data$ytest == 0] = 0

test_data$predicted[(((test_data$X1 * x1_coeff) + (test_data$X2 * x2_coeff) +
                        intercept_1) < 0.5) &  test_data$ytest == 1] = 0

linear_test_errorr=(nrow(test_data)-sum(test_data$predicted))/(nrow(test_data))

cat("\nlinear classifier test error for scenario 2 is ", linear_test_errorr)


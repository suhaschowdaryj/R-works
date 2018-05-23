#hw-3 , question-5 

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

plot(train2[ytrain == 1, "X1"] , train2[ytrain == 1, "X2"],
     xlab='X1',ylab='X2',col="green", ylim = c(-2,3))
par(new=TRUE)
points(train2[ytrain == 0, "X1"] ,train2[ytrain == 0, "X2"],
       col="red")
title('Scatter plot')
legend("topright",cex=0.5,legend=c("label = 1","label = 0"), 
       col=c("green","red"), pch = c("o","o"))


#generate the test set
seed_test <- 2014
ntest <- 500
test2 <- gendata2(ntest,center_green,center_red,Sig/5,Sig/5,seed_test)
ytest <- c(rep(1,ntest),rep(0,ntest))
colnames(test2) = c("X1","X2")

test2
ytest

write.csv(test_data, file = "MyData.csv")




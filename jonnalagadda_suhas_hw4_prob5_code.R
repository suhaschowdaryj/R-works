#question-5 knn for scenario 1 and 2 

library(MASS)
library(class)

#Scenario 1 

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


#knn train 
n_neighbors = c(1,4,7,10,13,16,30,45,60,80,100,150,200)
knn_train_err = c()

for (i in (1:length(n_neighbors))){ 
  
  y_train_pred_knn = knn(train = train_data[,-3],
                         test = train_data[,-3], 
                         cl = train_data[,3], k= n_neighbors[i]  )
  
  train_data$knnpredicted = 1
  
  train_data$knnpredicted[ y_train_pred_knn != train_data$label] = 0 
  err = 1 - mean(train_data$knnpredicted)
  cat('training error for', n_neighbors[i] , 'neighbors is' , err, '\n')
  knn_train_err = c(knn_train_err, err)
  train_data = train_data[,-4]
  
}


cat('\n')

#knn test error
n_neighbors = c(1,4,7,10,13,16,30,45,60,80,100,150,200)
knn_test_err = c()

for (i in (1:length(n_neighbors))){ 
  y_test_pred_knn = knn(train = train_data[,-3],
                        test = test_data[,-3], 
                        cl = train_data[,3], k= n_neighbors[i]  )
  
  test_data$knnpredicted = 1
  test_data$knnpredicted[ y_test_pred_knn != test_data$label] = 0 
  err = 1 - mean(test_data$knnpredicted)
  cat('test error for', n_neighbors[i] , 'neighbors is' , err, '\n')
  knn_test_err = c(knn_test_err, err)
  test_data = test_data[,-4]
}


degrees_of_freedom = 200 / n_neighbors

library(ggplot2)
ggplot()+
  geom_line(aes(degrees_of_freedom,knn_train_err),linetype = 'dashed', colour = 'red')+
  geom_point(aes(degrees_of_freedom,knn_train_err), colour = 'red', size = 1)+
  geom_line(aes(degrees_of_freedom,knn_test_err),linetype = 'dashed', colour = 'blue')+
  geom_point(aes(degrees_of_freedom,knn_test_err), colour = 'blue', size = 1)+
  ggtitle('k- Number of nearest neighbors')+
  xlab('degrees of freedom - N/k')+
  ylab('knn error')



#Scenario 2

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

colnames(train2) = c("X1","X2")
train_data =  cbind(train2,ytrain)
train_data = as.data.frame(train_data)

#generate the test set
seed_test <- 2014
ntest <- 500
test2 <- gendata2(ntest,center_green,center_red,Sig/5,Sig/5,seed_test)
ytest <- c(rep(1,ntest),rep(0,ntest))
colnames(test2) = c("X1","X2")

test_data =  cbind(test2,ytest)
test_data = as.data.frame(test_data)

#knn train 
cat('\n\n Scenario-2 \n\n')
n_neighbors = c(1,4,7,10,13,16,30,45,60,80,100,150,200)
knn_train_err = c()

for (i in (1:length(n_neighbors))){ 
  
  y_train_pred_knn = knn(train = train_data[,-3],
                         test = train_data[,-3], 
                         cl = train_data[,3], k= n_neighbors[i]  )
  
  train_data$knnpredicted = 1
  
  train_data$knnpredicted[ y_train_pred_knn != train_data$ytrain] = 0 
  err = 1 - mean(train_data$knnpredicted)
  cat('training error for', n_neighbors[i] , 'neighbors is' , err, '\n')
  knn_train_err = c(knn_train_err, err)
  train_data = train_data[,-4]
  
}

cat('\n')

#knn test error
n_neighbors = c(1,4,7,10,13,16,30,45,60,80,100,150,200)
knn_test_err = c()

for (i in (1:length(n_neighbors))){ 
  y_test_pred_knn = knn(train = train_data[,-3],
                        test = test_data[,-3], 
                        cl = train_data[,3], k= n_neighbors[i]  )
  
  test_data$knnpredicted = 1
  test_data$knnpredicted[ y_test_pred_knn != test_data$ytest] = 0 
  err = 1 - mean(test_data$knnpredicted)
  cat('test error for', n_neighbors[i] , 'neighbors is' , err, '\n')
  knn_test_err = c(knn_test_err, err)
  test_data = test_data[,-4]
}



library(ggplot2)
ggplot()+
  geom_line(aes(degrees_of_freedom,knn_train_err),linetype = 'dashed', colour = 'red')+
  geom_point(aes(degrees_of_freedom,knn_train_err), colour = 'red', size = 1)+
  geom_line(aes(degrees_of_freedom,knn_test_err),linetype = 'dashed', colour = 'blue')+
  geom_point(aes(degrees_of_freedom,knn_test_err), colour = 'blue', size = 1)+
  ggtitle('k- Number of nearest neighbors')+
  xlab('degrees of freedom - N/k')+
  ylab('knn error')

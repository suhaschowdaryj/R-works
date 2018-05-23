# Question 6 - classifying digits 1,2,3 with knn using 1,3,5,7,15 neighbors 
#import train data and test which consists of digits 0-9
library(MASS)
library(class)


k1 = read.table('zip.train.gz')
k2 = read.table('zip.test.gz') 


#filter training data which consists of only 1, 2 and 3 digits
train_data  = k1[which(k1$V1 == 1 | k1$V1 == 2 | k1$V1 == 3), ]


#filter test data which consists of only 1, 2 and 3 digits
test_data  = k2[which(k2$V1 == 1  | k2$V1 == 2 | k2$V1 == 3), ]

#linear regression classifier
relation= lm(train_data$V1 ~ . , data = train_data)

#knn train 
n_neighbors = c(1,3,5,7,15)
knn_train_err = c()

for (i in (1:5)){ # vec <- c(vec, 1:10)
  
  y_train_pred_knn = knn(train = train_data[,-1],
                         test = train_data[,-1], 
                         cl = train_data[,1], k= n_neighbors[i]  )
  
  train_data$knnpredicted = 1
  
  train_data$knnpredicted[ y_train_pred_knn != train_data$V1] = 0 
  err = 1 - mean(train_data$knnpredicted)
  cat('training error for', n_neighbors[i] , 'neighbors is' , err, '\n')
  knn_train_err = c(knn_train_err, err)
  train_data = train_data[,-258]
  
}

cat('\n')

#knn test error
n_neighbors = c(1,3,5,7,15)
knn_test_err = c()

for (i in (1:5)){ 
  train_data = train_data[,-258]
  y_test_pred_knn = knn(train = train_data[,-1],
                        test = test_data[,-1], 
                        cl = train_data[,1], k= n_neighbors[i]  )
  
  test_data$knnpredicted = 1
  
  test_data$knnpredicted[ y_test_pred_knn != test_data$V1] = 0 
  err = 1 - mean(test_data$knnpredicted)
  cat('test error for', n_neighbors[i] , 'neighbors is' , err, '\n')
  knn_test_err = c(knn_test_err, err)
  test_data = test_data[,-258]
}



#Part b 
#LDA
train_data <- train_data[, -17]


#TRAIN ERROR
relation= lda(train_data[,1] ~ . , data = train_data[,c(-1,-17)])
prediction= predict(relation, newdata = train_data[,-1])$class
table(prediction, train_data[,1])
lda_trainerror = mean(prediction != train_data[,1])
cat("\nLDA training error is ", lda_trainerror)


#test error
prediction1= predict(relation, newdata = test_data[,c(-1,-17)])$class
table(prediction1, test_data[,1])
lda_testerror = mean(prediction1 != test_data[,1] )
cat("\nLDA test error is ", lda_testerror)



# Question 4 - text book exercise 2.8
#import train data and test which consists of digits 0-9
k1 = read.table('zip.train.gz')
k2 = read.table('zip.test.gz') 


#filter training data which consists of only 2 and 3 digits
train_data  = k1[which(k1$V1 == 2 | k1$V1 == 3), ]


#filter test data which consists of only 2 and 3 digits
test_data  = k2[which(k2$V1 == 2 | k2$V1 == 3), ]

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

cat('\n')


#linear classification train data performance
y_predict_lm =  predict(relation)
y_predict_lm[y_predict_lm > 2.5] = 3
y_predict_lm[y_predict_lm < 2.5] = 2

train_data$predicted = 1

train_data$predicted[ y_predict_lm != train_data$V1] = 0 
train_error_lm = 1 - mean(train_data$predicted)
cat('training error for linear classifier',train_error_lm)
cat('\n')

#linear classification test data performance
y_test_predict_lm =  predict(relation,test_data[,2:ncol(test_data)])
y_test_predict_lm[y_test_predict_lm > 2.5] = 3
y_test_predict_lm[y_test_predict_lm < 2.5] = 2

test_data$predicted = 1

test_data$predicted[ y_test_predict_lm != test_data$V1] = 0 
test_error_lm = 1 - mean(test_data$predicted)
cat('\ntest error for linear classifier',test_error_lm)

# Question 5 - classifying digits 1, 2 and 3 using classification tree  
library(tree)

#import train data and test which consists of digits 0-9
k1 = read.table('zip.train.gz')
k2 = read.table('zip.test.gz') 


#filter training data which consists of only 2 and 3 digits
train_data <- k1[which(k1$V1 == 1 | k1$V1 == 2 | k1$V1 == 3), ]
train_data <- train_data[, -17]

#filter test data which consists of only 2 and 3 digits
test_data  <- k2[which(k2$V1 == 1 | k2$V1 == 2 | k2$V1 == 3), ]
test_data <- test_data[,-17]

#tree model
tree1 = tree(as.factor(V1) ~ ., data = train_data)
summary(tree1)

#train error
predicted_train_y <- predict(tree1,train_data[,2:256],type = "class" ) 
tree_trainerror = mean(predicted_train_y != train_data$V1)
cat("\nclassification tree train error is ", tree_trainerror)

#test error
predicted_y <- predict(tree1,test_data[,2:256],type = "class" ) 
tree_testerror = mean(predicted_y != test_data$V1)
cat("\nclassification tree test error is ", tree_testerror)

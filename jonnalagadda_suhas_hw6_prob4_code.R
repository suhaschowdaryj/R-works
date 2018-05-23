# Question 4 - classifying digits 2 and 3 with linear SVM, gaussian kernel and polynomial kernel  
#import train data and test which consists of digits 0-9
library(e1071)

#read dataset
k1 = read.table('zip.train.gz')
k2 = read.table('zip.test.gz') 


#filter training data which consists of only 2 and 3 digits
train_data <- k1[which(k1$V1 == 2 | k1$V1 == 3), ]
train_data <- train_data[, -17]

#filter test data which consists of only 2 and 3 digits
test_data  <- k2[which(k2$V1 == 2 | k2$V1 == 3), ]
test_data <- test_data[,-17]

#linear SVM
svm_linear_tune <- tune(svm, as.factor(V1)~ .,data = train_data,type = 'C', ranges = list(cost = 2^(-2:2)))
print(svm_linear_tune)
tunedModel <- svm_linear_tune$best.model
tunedModelY <- predict(tunedModel,test_data[,2:256] ) 
svmlinear_testerror = mean(tunedModelY != test_data$V1)
cat("\nlinear svm test error is ", svmlinear_testerror)


#gaussian SVM
svm_gaussian_tune <- tune(svm, V1 ~ .,data = train_data,type = 'C', kernel = 'radial',
                        ranges = list(gamma = seq(0.1,0.4,0.05), cost = 2^(2:4)))
print(svm_gaussian_tune)
gauss_tunedModel <- svm_gaussian_tune$best.model
gauss_tunedModelY <- predict(gauss_tunedModel,test_data[,2:256] ) 
svmgaussian_testerror = mean(gauss_tunedModelY != test_data$V1)
cat("\ngaussian svm test error is ", svmgaussian_testerror)



#polynomial SVM
svm_poly_tune <- tune(svm, V1 ~ .,data = train_data,type = 'C', kernel = 'polynomial',
                        ranges = list(degree = 3:5, epsilon = seq(0.1,0.4,0.05), cost = 2^(2:4)))
print(svm_linear_tune)
poly_tunedModel <- svm_poly_tune$best.model
poly_tunedModelY <- predict(poly_tunedModel,test_data[,2:256] ) 
svmpoly_testerror = mean(poly_tunedModelY != test_data$V1)
cat("\npolynomial svm test error is ", svmpoly_testerror)






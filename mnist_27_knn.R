#Classifying 2 and 7 using K nearest neigbors algorithm
library(dslabs)
library(tidyverse)
library(caret)

data("mnist_27")
#Scatterplot of training data
mnist_27$train %>% ggplot(aes(x_1,x_2, color = y)) + geom_point()

#Basic KNN with K = 5
fit <- knn3(y ~ ., data = mnist_27$train, k = 5)

y_hat <- predict(fit, mnist_27$test, type = "class")
confusionMatrix(y_hat, mnist_27$test$y)
#This gives about 81% accuracy, lets compare that with a linear model

lm_fit <- mnist_27$train %>% mutate(y = ifelse(y==7, 1, 0)) %>% lm(y ~ x_1 + x_2, data = .)
lm_p_hat <- predict(lm_fit, mnist_27$test)
#lm_p_hat has 0 and 1 for 2 and 7 respectively, we convert back
lm_y_hat = factor(ifelse(lm_p_hat>0.5,7,2)) 
confusionMatrix(lm_y_hat, mnist_27$test$y)
#Linear model gives about 75 % accuracy

#Now we play around with k and cross validation - using train function in caret package

#Default - it finds the best k in 5:9
fit_c <- train(y ~ ., method = "knn", data = mnist_27$train)
ggplot(fit_c)
#Here we notice that best accuracy is at k = 9
#Now let us play around with k
k_df <- data.frame(k = seq(1,75,2))
k_fit_knn <- train(y~ ., method = "knn", data = mnist_27$train, tuneGrid = k_df)
ggplot(k_fit_knn, highlight = T)
# Best accuracy obtained when k = 33
#Lets run on test set and see
y_hat_33 <- predict(k_fit_knn, mnist_27$test)
confusionMatrix(y_hat_33, mnist_27$test$y) #results


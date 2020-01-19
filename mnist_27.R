#Classifying 2 and 7 based on proportion of dark pixels in upper left quadrant and lower right quadrant
#Upper left - X1 (=x_1)
#Lower right - X2 (=x_2)
library(dslabs)
library(tidyverse)
library(caret)

data("mnist_27")

#Plotting the training data
mnist_27$train %>% ggplot(aes(x_1,x_2,color=y)) + geom_point()
train_set <- mnist_27$train
#Using mutate to change y to 0 and 1 (1 if y=7, 0 if y=2)
train_set <- train_set %>% mutate(y = ifelse(y==7,1,0))

#fitting a linear regression model
fit <- train_set %>% lm(y ~ x_1+x_2, data=.)

p_hat <- predict(fit, mnist_27$test)
y_hat <- factor(ifelse(p_hat>0.5,7,2))

#Summary of results
confusionMatrix(y_hat, mnist_27$test$y)




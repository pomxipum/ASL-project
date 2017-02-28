## Logistic Regression
install.packages("glmnet")
library("glmnet")
library(caret)

x.train <- as.matrix(bow.train[,-1])
y.train <- as.vector(bow.train[,1])
x.val <- as.matrix(bow.val[,-1])
y.val <- as.vector(bow.val[,1])
glmnet.fit <- glmnet(x=x.train,y=y.train, family = "multinomial")  
glmnet.pred <- predict(glmnet.fit, newx = x.val, type="class")
matrix <- confusionMatrix( glmnet.pred[,50], y.val)
matrix$overall[1]
dim(glmnet.pred)

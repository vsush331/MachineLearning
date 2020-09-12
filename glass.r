install.packages('caTools')  
install.packages('dplyr')    
install.packages('ggplot2')  
install.packages('class')    
install.packages('corrplot)
library(dplyr)
library(caTools)
library(ggplot2)
library(caret)
library(class)
library(corrplot)
glass <- read.csv("C:/Users/Susheel/Desktop/KNN/glass.csv")
standard.features <- scale(glass[,1:9])
data <- cbind(standard.features,glass[10])
anyNA(data)
head(data)
corrplot(cor(data))
set.seed(101)
sample <- sample.split(data$Type,SplitRatio = 0.70)
train <- subset(data,sample==TRUE)
test <- subset(data,sample==FALSE)
predicted.type <- knn(train[1:9],test[1:9],train$Type,k=1)
error <- mean(predicted.type!=test$Type)
confusionMatrix(predicted.type,test$Type)
predicted.type <- NULL
error.rate <- NULL

for (i in 1:10) {
  predicted.type <- knn(train[1:9],test[1:9],train$Type,k=i)
  error.rate[i] <- mean(predicted.type!=test$Type)
    
}

knn.error <- as.data.frame(cbind(k=1:10,error.type =error.rate))

   ggplot(knn.error,aes(k,error.type))+ 
      geom_point()+ 
geom_line() + 
scale_x_continuous(breaks=1:10)+ 
theme_bw() +
xlab("Value of K") +
ylab('Error')
predicted.type <- knn(train[1:9],test[1:9],train$Type,k=3)
error <- mean(predicted.type!=test$Type)
confusionMatrix(predicted.type,test$Type)







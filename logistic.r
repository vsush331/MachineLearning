
> bank_data <- read.csv("C:/Users/Susheel/Desktop/regression/bank_data.csv", sep=";")
>   View(bank_data)
> summary(bank_data)
str(bank_data)
> logit<- glm(y~age+balance+day+duration+campaign+pdays+previous+factor(job)+factor(marital)+factor(education)+factor(default)+factor(housing)+factor(loan)+factor(contact)+factor(month)+factor(poutcome),family ="binomial",data = bank_data)
> summary(logit)
library(MASS)
stepAIC(logit)
> prob_y <- as.data.frame(predict(logit, type = c("response"), bank_data))
> final_y <- cbind(bank_data, prob_y)
> 
  > confusion_y <- table(prob_y>0.5, bank_data$y)
> 
  > table(prob_y>0.5)
confusion_y
> accuracy_y <- sum(diag(confusion_y)/sum(confusion_y))
> 
  > accuracy_y
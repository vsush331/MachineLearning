crime_data <- read.csv("C:/Users/Susheel/Desktop/clusters/crime_data.csv")
View(crime_data)
> summary(crime_data)
library(data.table)
ncol(crime_data)
norm_crime_data_sub <- scale(crime_data[,2:5])
d <- dist(norm_crime_data_sub, method = "euclidean")
str(d)
fit <- hclust(d, method = "complete")
plot(fit, hang=-1)
rect.hclust(fit,plot(fit,hang=-1),k=4,border="red"

            
crime_data_final <- cbind(crime_data, groups)
aggregate(crime_data_final[,2:6], by=list(crime_data_final$groups), FUN = mean) 

#####GROUP 2 HAVE HIGH CRIME RATE########            
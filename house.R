df<-read.csv(file ="C:/Users/lenovo/UNHAS/SEMESTER 6/DATA MINING/house.csv", 
             header=TRUE, sep =";")
setwd('C:/Users/lenovo/UNHAS/SEMESTER 6/DATA MINING')
dfi<-read.csv('house.csv')
View(dfi)
data <- dfi[-c(1,15,16,17,18)]
data$price <- ifelse(data$price == 0, NA, data$price)
View(df)
deta <- df[-c(1,15,16,17,18)]
deta$price <- ifelse(deta$price == 0, NA, deta$price)
deta$price <- ifelse(deta$price > 1000000, NA, deta$price)
deta <- deta[complete.cases(deta), ]

deta$price2 <- ifelse(deta$price > mean(deta$price), 1, 0)
deta$renovated <- ifelse(deta$yr_renovated > 0, 1, 0)
deta$build <- ifelse(deta$yr_built> 1970, 1, 0)
deta$basement <- ifelse(deta$sqft_basement > 0, 1, 0)

deta$basement <- as.factor(deta$basement)
deta$build <- as.factor(deta$build)
deta$renovated <- as.factor(deta$renovated)
deta$price2 <- as.factor(deta$price2)
deta<-deta[,-c(1,11,12,13)]
s <- sample(2, nrow(deta), replace = T, prob = c(0.8, 0.2)) 
train_TP1 <- deta[s == 1,]
test_TP1 <- deta[s == 2,]
library(party)
library(rpart)
library(caret)
pohon <-ctree(price2~.,data=train_TP1)
plot(pohon)

predict_model<-predict(pohon, test_TP1)
predict_model
m_at <- table(test_TP1$price2, predict_model)
m_at


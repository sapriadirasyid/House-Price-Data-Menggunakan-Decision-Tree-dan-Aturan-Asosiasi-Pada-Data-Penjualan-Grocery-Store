grocery<-read.csv(file ="C:/Users/lenovo/UNHAS/SEMESTER 6/DATA MINING/grocery.csv", 
             header=F, sep =";")
View(grocery)
colnames(grocery) <- c('Items')
grocery <- data.frame(grocery)
write.csv(grocery,file="market_basket_transactions.csv", quote = FALSE, row.names = FALSE)
df <- read.transactions("market_basket_transactions.csv", format = 'basket', sep=',')
summary(df)
df
rules = apriori(data = df, parameter = list(support = 0.005, confidence = 0.25))
inspect(rules)
summary(rules)
sortir <- sort(rules, by="lift")
redundant <- is.redundant(sortir)
rules.pruned <- sortir[!redundant]
summary(rules.pruned)
inspect(rules.pruned)
plot(rules.pruned)

#Set working directory
setwd("/Users/yyh/Documents/GitHub/QTM200Spring2020/problem_sets/PS4")
#load dataset
install.packages(car)
library(car)
data(Prestige)
help(Prestige)
#############Question 1###############
#Create new variable
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
#run linear regression
Reg1 <- lm(prestige ~ income + professional + income:professional, data = Prestige)
summary(Reg1)
#Calculate marginal effect of a $1000 increase in income
effect1 <- 1000*0.003 -1000*0.002
effect1# 1
##Calculate marginal effect for change in the dummy variable
effect2 <- 37.781 - 0.002*6000
effect2#25.781

#############Question 2###############
#Testing beta1 = 0 or not
TS1 <- 0.042/0.016
n <- 131
k <- 3
P_value1 <- 2*(pt(abs(TS1), n-k, lower.tail = F))
P_value1# 0.00972002 < 0.05, affected

#Testing beta2 = 0 or not
TS2 <- 0.042/0.013
P_value2 <- 2*(pt(abs(TS2), n-k, lower.tail = F))               
P_value2# 0.00156946 < 0.05, affected

#F-test for the overal model fit
F.test <- ((0.094/k)/((1-0.094)/(n-k-1))) 
df1 <- k
df2 <- n-k-1
F.pvalue <- df(F.test, df1, df2)
F.pvalue #0.007141957 < 0.05
#We can reject the null hypothesis and conclude that there's at least one coinficient is significant.

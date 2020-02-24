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

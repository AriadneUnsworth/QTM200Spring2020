#Set working directory
setwd("/Users/yyh/Documents/GitHub/QTM200Spring2020/problem_sets/PS3")
#load dataset
incumb <- read.csv("incumbents_subset.csv", header = T)
summary(incumb)
#############Question 1###############
X1 <- incumb$difflog
Y1 <- incumb$voteshare
#1.regression
Reg1 <- lm(Y1~X1)
summary(Reg1)
#2.plot regression
plot(Y1~X1, main = "incumbents' voteshare vs. difference in spending", xlab = "difference in spending", ylab = "voteshare")
abline(Reg1)
#3.residual analysis
residual1 <- residuals(Reg1)
#4.prediction equation
#Coefficients:
  #Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.579031   0.002251  257.19   <2e-16 ***
  #X1          0.041666   0.000968   43.04   <2e-16 ***

#Residual standard error: 0.07867 on 3191 degrees of freedom
#Multiple R-squared:  0.3673,	Adjusted R-squared:  0.3671 
#F-statistic:  1853 on 1 and 3191 DF,  p-value: < 2.2e-16

#############Question 2###############
X2 <- incumb$difflog
Y2 <- incumb$presvote
#1.regression
Reg2 <- lm(Y2~X2)
summary(Reg2)
#2.plot regression
plot(Y2~X2, main = "Incumbents' presidential vote vs. difference in spending", xlab = "difference in spending", ylab = "voteshare")
abline(Reg2)
#3.residual analysis
residual2 <- residuals(Reg2)
#4.prediction equation
#Coefficients:
  #Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.507583   0.003161  160.60   <2e-16 ***
  #X2          0.023837   0.001359   17.54   <2e-16 ***

#Residual standard error: 0.1104 on 3191 degrees of freedom
#Multiple R-squared:  0.08795,	Adjusted R-squared:  0.08767 
#F-statistic: 307.7 on 1 and 3191 DF,  p-value: < 2.2e-16

#############Question 3##############
X3 <- incumb$presvote
Y3 <- incumb$voteshare
#1.regression
Reg3 <- lm(Y3~X3)
summary(Reg3)
#2.plot regression
plot(Y3~X3, main = "incumbents' voteshare vs. ", xlab = "presvote", ylab = "voteshare")
abline(Reg3)
#3.residual analysis
residual3 <- residuals(Reg3)
#4.prediction equation
#Coefficients:
  #Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.441330   0.007599   58.08   <2e-16 ***
  #X3          0.388018   0.013493   28.76   <2e-16 ***

#Residual standard error: 0.08815 on 3191 degrees of freedom
#Multiple R-squared:  0.2058,	Adjusted R-squared:  0.2056 
#F-statistic:   827 on 1 and 3191 DF,  p-value: < 2.2e-16

#############Question 4##############
X4 <- residual2
Y4 <- residual1
#1.regression
Reg4 <- lm(Y4~X4)
summary(Reg4)
#2.plot regression
plot(Y4~X4, main = "Q1 residuals vs. Q2 residual", xlab = "Q1 residuals", ylab = "Q2 residuals")
abline(Reg4)
#3.residual analysis
residual4 <- residuals(Reg4)
#4.prediction equation
#Coefficients:
  #Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -4.860e-18  1.299e-03    0.00        1    
#X4           2.569e-01  1.176e-02   21.84   <2e-16 ***

#Residual standard error: 0.07338 on 3191 degrees of freedom
#Multiple R-squared:   0.13,	Adjusted R-squared:  0.1298 
#F-statistic:   477 on 1 and 3191 DF,  p-value: < 2.2e-16

#############Question 5##############
X5a <- incumb$difflog
X5b <- incumb$presvote
Y5 <- incumb$voteshare
#1.regression
Reg5 <- lm(Y5~X5a+X5b)
#2.prediction equation
summary(Reg5)

#Coefficients:
  #Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.4486442  0.0063297   70.88   <2e-16 ***
  #X5a         0.0355431  0.0009455   37.59   <2e-16 ***
  #X5b         0.2568770  0.0117637   21.84   <2e-16 ***

#Residual standard error: 0.07339 on 3190 degrees of freedom
#Multiple R-squared:  0.4496,	Adjusted R-squared:  0.4493 
#F-statistic:  1303 on 2 and 3190 DF,  p-value: < 2.2e-16



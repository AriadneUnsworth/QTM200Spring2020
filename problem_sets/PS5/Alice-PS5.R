#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
library(car)
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("faraway"),  pkgTest)

# set working directory
setwd("C:/Users/yyh/Documents/GitHub/QTM200Spring2020/problem_sets/PS5")


#####################
# Problem 1
#####################

# load data
gamble <- (data=teengamb)
# run regression on gamble with specified predictors
model1 <- lm(gamble ~ sex + status + income + verbal, gamble)
#check the constant variance, normality assumptions and large leverage points
#constant variance by residual vs. fitted & nomality assessment by Q-Q plot
plot(model1)
#Plotting hat-values
plot(hatvalues(model1), pch = 16, cex = 1, main = "Hat values plot for regression model 'gamble'")
abline(h = 2*4/47, lty = 2)
abline(h= 3*4/47, lty = 2)
identify(1:47, hatvalues(model1), row.names(gamble))
#[1] 31 33 35 42 have high leverage using the thresholds 2(k + 1)/n and 3(k + 1)/n

#Plotting cook's distances for outliers
summary(model1)
plot (cooks.distance(model1), pch = 16, cex = 1)
abline(h=4/(47-4-1), lty = 2) #showing any cook's distance > 4/(n-k-1)
identify(1:47, cooks.distance(model1), row.names(gamble)) #finding which observations are the outliers
#Outlier test
outlierTest(model1, row.names(model1))
#Creating a bubble plot
plot(hatvalues(model1), rstudent(model1), type = "n", main = "Individual influences in regression model 'gamble'",
     xlab = "h values", ylab = "studentized residuals")
cook <- sqrt(cooks.distance(model1))
points(hatvalues(model1), rstudent(model1), cex = 10*cook/max(cook))
abline(h = c(-2, 0, 2), lty = 2)
abline(v = c(2, 3)*4/47, lty = 2)
identify(hatvalues(model1), rstudent(model1), row.names (gamble))


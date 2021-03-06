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
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set working directory
setwd("C:/Users/yyh/Documents/GitHub/QTM200Spring2020/problem_sets/PS1")


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#find t statistics for a 90% confidence level
t90 <- qt((1-0.9)/2, df= 24, lower.tail = FALSE)
n <- length(y) #sample size
ymean <- mean(y) # sample mean
ysd <- sd(y) # sample standard deviation
lower <- ymean - t90*(ysd/sqrt(n))
upper <- ymean + t90*(ysd/sqrt(n))
#construct the confidence interval
confint90 <- c(lower, upper)
confint90 #[1] 93.95993 102.92007

#####################
# Problem 2
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
#T test assuming that the school average IQ is higher than the country average (100)
t.test(y, mu=100, alternative="greater", conf.level=0.95, var.equal = FALSE)
#t = -0.59574, df = 24, p-value = 0.7215

#####################
# Problem 3
#####################

y <- c(1, 2, 1, 3, 4, 1, 1, 4, 2, 1, 3, 4, 3, 2, 1, 3, 4, 1, 2, 3, 1, 1, 2, 1, 1, 3, 4)
#import dataset
expenditure <- read.table("expenditure.txt", header=T)
summary(expenditure)
#Relationship between Y,X1,X2 and X3
plot(expenditure$X1, expenditure$Y, main = "Public education Expenditure vs. personal income", 
                                    xlab = "personal income per capita",
                                    ylab = "expenditure per capita")
#The best fit line
abline(lm(expenditure$Y~expenditure$X1))

#X2
plot(expenditure$X2, expenditure$Y, main = "Public education Expenditure vs. underaged population", 
                                    xlab = "number of resident per thousands under 18",
                                    ylab = "expenditure per capita")
#The best fit line
abline(lm(expenditure$Y~expenditure$X2))
#X3
plot(expenditure$X3, expenditure$Y, main = "Public education Expenditure vs. urban population", 
                                    xlab = "number of urban resident per thousands",
                                    ylab = "expenditure per capita")
#The best fit line
abline(lm(expenditure$Y~expenditure$X3))

#Relationship between Y and Regions
library(Rmisc)
summarySE(data = expenditure, measurevar = "Y", groupvars = "Region")
#Create a new factor variable YR
expenditure$YR <- expenditure$Y
expenditure$YR <- factor(NA, levels = c("Northeast", "North Central", "South", "West"))
expenditure$YR[expenditure$Region == 1] <- "Northeast"
expenditure$YR[expenditure$Region == 2] <- "North Central"
expenditure$YR[expenditure$Region == 3] <- "South"
expenditure$YR[expenditure$Region == 4] <- "West"
summary(expenditure$YR)
#A side by side boxplot
boxplot(expenditure$Y~expenditure$YR, main = "Public education Expenditure vs. Regions",
                                      xlab = "Regions",
                                      ylab = "expenditure per capita")
#Y vs X1 plus regional information
plot(expenditure$X1, expenditure$Y, main = "Public education Expenditure vs. personal income", 
     xlab = "personal income per capita",
     ylab = "expenditure per capita", col = expenditure$Region, pch = expenditure$Region)
legend(x="topright", legend = levels(expenditure$YR), col = c(1,2,3,4), pch= c(1,2,3,4))

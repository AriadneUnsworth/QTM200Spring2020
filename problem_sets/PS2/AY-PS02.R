#Set working directory
#setwd("C:/Users/hyan249/Documents")

#############Question 1##############
#Create the table
data1 <- c(14, 6, 7, 7, 7, 1)
bribe <- as.table(matrix(data1, nrow = 2, ncol = 3, byrow = T))
rownames(bribe) <- c("Lower class", "Upper class")
colnames(bribe) <- c("not-stopped","bribe requested", "stopped/given warnings")
bribe
#Calculate expected values
addmargins(bribe)
Expected_Value <- c(27*21/42, 27*13/42, 27*8/42, 15*21/42, 15*13/42, 15*8/42)
Expected_Value
#Calculate chi-square statistics
chi_sqr <- sum((data1-Expected_Value)^2/Expected_Value)
chi_sqr#[1] 3.791168

#Calculate the p-value
p_value <- pchisq(3.791168, df = 2, lower.tail = FALSE)
p_value#[1] 0.1502306 > alpha = 0.1
#We therefore cannot reject the null hypothesis and conclude that whether officers were more likely to solicit a bribe from drivers are not independent from their classes

#Check with R
chisq.test(bribe, correct = TRUE)#same result

#Calculate standardized residuals
bribeR <- c(data1-Expected_Value)/sqrt(Expected_Value)
#View standardized residuals
plot(bribeR)
mean(bribeR)#[1] -0.007950638
#reported it in a table
bribeR <- as.table(matrix(bribeR, nrow = 2, ncol = 3, byrow = T))
rownames(bribeR) <- c("Lower class", "Upper class")
colnames(bribeR) <- c("not-stopped","bribe requested", "stopped/given warnings")
bribeR
#Since our residuals are close to zero and relatively evenly distributed, we cannot decern a significant difference between our observed value and expected value

###############Question 2################
#Import dataset
data2 <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv", header = T)
summary(data2)
#Our null hypothesis is:
#There's no association between whether a village have female leaders position reserved or not and the number of new or repaired drinking water facilities in that village
#Our alternative hypothesis is:
#There's an association between whether a village have female leaders position reserved or not and the number of new or repaired drinking water facilities in that village
#correlation testing on regression model between reserved and water
lm1 <- lm(data2$water~data2$reserved)
summary(lm1)
cor.test(data2$water, data2$reserved)
#Test shows that we can reject the null hypothesis and conclude that there is an significant correlation between whether a village have female leaders position reserved or not and the number of new or repaired drinking water facilities in that village
#Coefficient estimate is 9.252, which means that for each unit increase in reserved female positions, there will be 9.252 more repaired or new drinking water facilities

################Question 3#################
#Import dataset
data3 <- read.csv()
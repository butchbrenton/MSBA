rm(list = ls())

#Import Packages
require(xlsx)

#Import Data
data <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/M4/buad502a-m4-novice-data-set-normtemp.xls", 1)

#Looking at the data types and overview of the data
str(data)

##Question 1
avg <- 512.5
sd <- 56
n <- 15000

#Part A
lower <- 440
upper <- 560
(pnorm(upper,avg,sd)-pnorm(lower,avg,sd))*n

#Part B
lower <- 380 
upper <- 620
(pnorm(upper,avg,sd)-pnorm(lower,avg,sd))*n

#Part C
lower <- 320 
upper <- 680
(pnorm(upper,avg,sd)-pnorm(lower,avg,sd))*n

#Part D
lower <- 410 
upper <- 590
(pnorm(upper,avg,sd)-pnorm(lower,avg,sd))*n

##Question 2
avg <- 3.11
sd <- 0.13
n <- 1500

#Part A
lower <- 2.8
upper <- 3.3
(pnorm(upper,avg,sd)-pnorm(lower,avg,sd))*100

#Part B
lower <- 2.11 
upper <- 3.5
(pnorm(upper,avg,sd)-pnorm(lower,avg,sd))

#Part C
upper <- 2.96
pnorm(upper,avg,sd)

#Part D
lower <- 3.4
1-pnorm(lower,avg,sd)

##Question 3
avg <- 115
sd <- 25
n <- 12000

#Part A
lower <- 115
upper <- 140
(pnorm(upper,avg,sd)-pnorm(lower,avg,sd))

#Part B
lower <- 90 
upper <- 115
(pnorm(upper,avg,sd)-pnorm(lower,avg,sd))

#Part C
lower <- 100 
upper <- 155
(pnorm(upper,avg,sd)-pnorm(lower,avg,sd))*n

#Part D
lower <- 140 
1-pnorm(lower,avg,sd)

#Part E
lower <- avg - sd
upper <- avg + sd
(pnorm(upper,avg,sd)-pnorm(lower,avg,sd))*n

##Question 4
gender1 <- data[data$Gender == 1,]
gender2 <- data[data$Gender == 2,]

bins <- seq(95,102)

gen1freq <- hist(gender1$Body.Temp, breaks = bins)
cbind(bins[-8],gen1freq$counts)

bins <- seq(95,102)

gen2freq <- hist(gender2$Body.Temp, breaks = bins)
cbind(bins[-8],gen2freq$counts)

reg1 <- lm(Body.Temp~., data=gender1)
res1 <- rstandard(reg1)
qqnorm(res1, ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(res1)

reg2 <- lm(Body.Temp~., data=gender2)
res2 <- rstandard(reg2)
qqnorm(res2, ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(res2)

gen1avg <- mean(gender1$Body.Temp)
gen1sd <- sd(gender1$Body.Temp)

gen2avg <- mean(gender2$Body.Temp)
gen2sd <- sd(gender2$Body.Temp)

lower1gen1 <- gen1avg - gen1sd
lower2gen1 <- gen1avg - 2*gen1sd
lower3gen1 <- gen1avg - 3*gen1sd

upper1gen1 <- gen1avg + gen1sd
upper2gen1 <- gen1avg + 2*gen1sd
upper3gen1 <- gen1avg + 3*gen1sd

lower1gen2 <- gen2avg - gen2sd
lower2gen2 <- gen2avg - 2*gen2sd
lower3gen2 <- gen2avg - 3*gen2sd

upper1gen2 <- gen2avg + gen2sd
upper2gen2 <- gen2avg + 2*gen2sd
upper3gen2 <- gen2avg + 3*gen2sd

sum(gender1$Body.Temp > lower1gen1 & gender1$Body.Temp < upper1gen1)/nrow(gender1)
sum(gender1$Body.Temp > lower2gen1 & gender1$Body.Temp < upper2gen1)/nrow(gender1)
sum(gender1$Body.Temp > lower3gen1 & gender1$Body.Temp < upper3gen1)/nrow(gender1)

sum(gender2$Body.Temp > lower1gen2 & gender2$Body.Temp < upper1gen2)/nrow(gender2)
sum(gender2$Body.Temp > lower2gen2 & gender2$Body.Temp < upper2gen2)/nrow(gender2)
sum(gender2$Body.Temp > lower3gen2 & gender2$Body.Temp < upper3gen2)/nrow(gender2)

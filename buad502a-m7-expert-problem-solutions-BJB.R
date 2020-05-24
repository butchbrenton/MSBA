rm(list = ls())

require(xlsx)
#read knob data deleting first row and assigning column names as header from row 2 data
data_knob <- read.xlsx(
  "C:/Users/17574/Documents/MSBA/Statistics/M7/buad502a-m7-expert-dataset1-knobs.xls",
  1, startRow = 2)

#read returned goods data into two data frames - open box and finish
data_RG_cond <- read.xlsx(
  "C:/Users/17574/Documents/MSBA/Statistics/M7/buad502a-m7-expert-dataset2-returned-goods.xls",
  1, startRow = 2, endRow = 5)
data_RG_finish <- read.xlsx(
  "C:/Users/17574/Documents/MSBA/Statistics/M7/buad502a-m7-expert-dataset2-returned-goods.xls",
  1, startRow = 8, endRow = 11)

#read sheep data into data frames - keep only columns 1 through 3
data_sheep <- read.xlsx(
  "C:/Users/17574/Documents/MSBA/Statistics/M7/buad502a-m7-expert-dataset3-sheep.xls", 1)
data_sheep <- data_sheep[,1:3]

##Problem 1
exp.percent <- c(0.55,0.325,0.125)
obs <- c(211,169,43)
n <- 423
#a What is the expected number of customers purchasing each type of knob
expected <- exp.percent * n
expected

#b Compute chi squared
residuals <- obs - expected
residuals.sq <- residuals**2
residual.exp <- residuals.sq/expected
chi <- sum(residual.exp)
chi

#c How many degrees of freedom does chi squared have?
dof <- length(obs) - 1
dof

#d If the customers purchase the historical proportions, 
#    what is the mean of the chi squared distribution?
#  The mean chi squared is equal to the dof
mean.chi <- dof
mean.chi

#e Does chi squared from part b. seem large in comparison to the mean in d.?
difference <- abs(chi - mean.chi)
difference
# yes the difference is greater than 9 which is large

#f What does that say about the null hypothesis?
# a large chi square difference means that the data does not fit very well
# and there is not a relationship

#g Find the alpha = 0.05 critical for the chi square
#    distribution with the appropriate df
alpha <- 0.05
qchisq(1 - alpha, df = dof)

#h Using the critical value, what can we conclude about the null hypothesis 
# at alpha = 0.05?
chi - qchisq(1 - alpha, df = dof)
# chi square statistic is greater than the critical value so there is not
#   a statistical significance.
1 - pchisq(chi, dof)
# this is confirmed with the p-value

## Problem 2 - random sample from five different promotions
# a The Null Hypothesis is that the five promotions are independent and have no association

#b This is a chi square fit test for independence

#c Are the assumptions and conditions met?
# Counted Data Condition - The data consists of counts of a categorical data
# Independence Assumption - The data is from independent promotions
# Randomization Condition - The data was chosen randomly
# Sample Size Assumption - The data is large enough to test
# Expected Cell Frequency Condition - there are at least 10 data points in each

#d What are the expected numbers for each cell if H0 is true?
prom1 <- c(data_knob[1,2], data_knob[1,3], data_knob[1,4])
prom2 <- c(data_knob[2,2], data_knob[2,3], data_knob[2,4])
prom3 <- c(data_knob[3,2], data_knob[3,3], data_knob[3,4])
prom4 <- c(data_knob[4,2], data_knob[4,3], data_knob[4,4])
prom5 <- c(data_knob[5,2], data_knob[5,3], data_knob[5,4])
obs <- rbind(prom1,prom2,prom3,prom4,prom5)
obs

exp.percent <- c(0.55,0.325,0.125)
n <- 325
exp <- matrix(NA, 5,3)
exp[,1] <- n*exp.percent[1]
exp[,2] <- n*exp.percent[2]
exp[,3] <- n*exp.percent[3]
exp

#e Find chi squared
residual <- obs - exp
residual.sq <- residual**2
residual.exp <- residual.sq/obs

chi <- sum(residual.exp)
chi

#f Find the degrees of freedom
dof <- (nrow(obs) -1)*(ncol(obs)-1)
dof

#g Find the critical value at alpha = 0.05
alpha <- 0.05
qchisq(1 - alpha, df = dof)

#h What do you conclude?  Explain.
# We can infer from the chi square value that we must reject the null
# hypothesis and conclude that the promotions are not independent

## Problem 3
#a Probability that a randomly selected item is an electronics item
# number of electronic items / total number of items
p.elec <- data_RG_cond[2,6] / data_RG_cond[3,6]
p.elec

#b Probability that a randomly selected item is a household item
# number of household items / total number of items
p.house <- data_RG_cond[1,6] / data_RG_cond[3,6]
p.house

#c Probability of a randomly selected item is a household item
# given that the phone is in new condition
# assume problem is asking for probability of a household item
# from the new condition items
# number of new household items / total number of new items
p.new.house <- data_RG_cond[1,2] / data_RG_cond[3,2]
p.new.house

#d If an item's chance of being an eletronics item were the same,
# regardless of condition, how many items would you expect to be
# electronics?
# answer is the probability of an electronics item * total number of items
elec <- p.elec * data_RG_cond[3,6]
elec

# e State the Null and alternate hypothesis Flick would like to test
# and give the name of the test
# Null - electronic and household items are independent and have no association
# Alternative - electronic and household items are dependent and have association
# Test - Chi squared test for independence

#f Calculate the chi-square value and the p-value.  State conclusions
# for equal chances of a returned item is  1 of 4x2=8 or 1/8
chance <- 1/8
# build matrix of equal probability and add to observed data
house.exp <- c(chance*data_RG_cond[3,6],chance*data_RG_cond[3,6],
               chance*data_RG_cond[3,6], chance*data_RG_cond[3,6],
               4*chance*data_RG_cond[3,6])
elec.exp <- house.exp
items <- rbind(data_RG_cond[1:2,2:6], house.exp, elec.exp)
items
#calculate chi squared
house.res <- ((items[1,] - items[3,])**2)/house.exp
elec.res <-  ((items[2,] - items[4,])**2)/elec.exp
chisq <- sum(house.res,elec.res)
chisq
#calculate p-value
dof <- 4 - 1
1 - pchisq(chisq, dof)
# Since chi squared is large and the p-value is 0, we can accept 
# H0 and conclude the electronic and household items returned 
# have no association

##Question 4
# a is the finish of the item independent of whether the item is electronics
# or a household item?
house.stain.per <- data_RG_finish[1,2] / data_RG_finish[1,4]
house.stain.per
elec.stain.per  <- data_RG_finish[2,2] / data_RG_finish[2,4]
elec.stain.per
# there are 34.8% returned stainless household items
# to 38.2% returned electronics items. The porportions are
# roughly equal

# b use the Chi Squares Test of Independence
data_RG_finish_1 <- data_RG_finish[1:2,2:3]
chisq.test(data_RG_finish_1)
# p-value is (slightly) greater than .05 we do not 
# reject H0 that stainless steel is independent
# of returned items

## Problem 5 SHEEP
# more apples means more wool?
# a What is the regression line?
# build a linear model, graph, and examine summary and coefficients

model1 <- lm(data_sheep[,3]~data_sheep[,2])
summary(model1)
coef(model1)

plot(data_sheep[,3],data_sheep[,2], xlab = "Farm Revenue",
     ylab = "Apples Per Day Per Month", main = "Apples to Revenue",)


# b Are the asumptions and conditions met?
# Linearity Assumption - The data appears to be linear from the scatterplot
#  with no obvious curve.  
# Quantative Variable Condition - The data is quantative and can be plotted
# Linearity Condition - The data appears to be linear
# Independence Assumption - The data apples fed and the revenues
#   are independent from one another
# Randomizatin Condition - The data is sampled from 60 months and 
    # covers avious seasons and different conditions
sheep.res = resid(lm(data_sheep[,3]~data_sheep[,2])) 
plot(sheep.res)
# Equal Spread Condition - The residuals
#   show no obvious patern compared to predicted values
# Normal Population Assumption - the population is normal
hist(sheep.res)
# Nearly Normal Condition - The residuals show a normal distribution


#c find the predicted revenues for a month with 1200 apples fed
# revenue = value * slope + y-intercept
month1200 <- 1200 * coef(model1)[2] + coef(model1)[1]
month1200   # revenue for a month of 1200 apples fed

#d 95% Confidence Interval in a month with 1200 apples fed
se <- as.numeric(summary(model1)[6])
print(paste("Confidence Interval: (", round(month1200 - 1.96*se, 3),
            ", ", round(month1200 + 1.96*se, 3), ")", sep = ""))

#e what is se (spread around the line)
# spread around the line is the standard error
# of the regression output
se

#f find the predicted revenues for a month with 1200 apples fed
# revenue = value * slope + y-intercept
month25k <- 25000 * coef(model1)[2] + coef(model1)[1]
month25k   # revenue for a month of 25,000 apples fed

#g 95% Confidence Interval in a month with 25,000 apples fed
se <- as.numeric(summary(model1)[6])
print(paste("Confidence Interval: (", round(month25k - 1.96*se, 3),
            ", ", round(month25k + 1.96*se, 3), ")", sep = ""))

#h Are these predictions likely to be useful?
# Since the line of linear regression is very accurate
# the prediction for 1200 apples per month can be taken 
# with a 95% confidence since this modelled point is well
# within an area for which we have data
#  The predicted revenue for 25,000 is less likely to be
# accurate since it is way outside of the range for which
# we have data so the model may not be accurate

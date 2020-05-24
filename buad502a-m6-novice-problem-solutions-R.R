rm(list = ls())

# import xlsx
require(xlsx)

# set working directory

# Problem 1
agedata <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/M6/buad502a-m6-novice-dataset-age.xls",1)
head(agedata)
colnames(agedata) <- c("CustomerID", "Age")
head(agedata)

# a
mean_age <- mean(agedata$Age)
print(mean_age)

# b
std_age <- sd(agedata$Age)
print(std_age)

# c
sterror_age <- std_age / sqrt(nrow(agedata))
print(sterror_age)

# d
sterror_age_100 <- std_age / sqrt(1000)
print(sterror_age_100)

# e
df_age <- nrow(agedata) - 1
print(df_age)


# Problem 2

# a
# Ho: mean age = 25

# b
# Ha: mean age =/= 25  This is two-sided because we want to know if mean is 
# equal to 25 or not. It can be greater than or less than 25.

# c
tstat <- (mean_age - 25) / sterror_age
print(tstat)

# d
pvalue <- 2 * pt(tstat, df_age, lower = FALSE)
print(pvalue)

# e
# Because the p-value is <.05, we reject the null hypothesis that the mean = 25

# f
tstar <- abs(qt(.05/2, df_age))
# current_ME <- tstar * sterror_age
goal_ME <- current_ME / 3
zstar <- 1.96
required_n <- (zstar * std_age / goal_ME) ^ 2
print(required_n)


# Problem 3
golfdata <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/M6/buad502a-m6-novice-dataset-golf.xls", 1)
head(golfdata)
golfdata <- golfdata[,1:2]
colnames(golfdata) <- c("GolferID", "DriveYards")
head(golfdata)

# a
hist(golfdata$DriveYards)
# The data is slightly skewed right, towards golfers who drive the ball farther

# b
# Randomization Condition: The data was found using a randomized experiment
# 10% Condition: the sample size is not more than 10% of the population
# Nearly Normal Condition: the data is not perfectly not but is also not extremely
# skewed. With a sample size of 237, it is close enough

# c
mean_dy <- mean(golfdata$DriveYards)
print(mean_dy)

# d
# We know that a skewed distribution can potentially affect the mean. This could 
# be a slight concern for our data.
# The data is not extremely skewed so it is probably ok to continue

# e
n <- nrow(golfdata)
sd_dy <- sd(golfdata$DriveYards)
se_dy <- sd_dy / sqrt(n)
tstar <- abs(qt(.05/2, n - 1))
lower_bound <- mean_dy - tstar * se_dy
upper_bound <- mean_dy + tstar * se_dy
print(paste("Confidence Interval: (", round(lower_bound, 2), ", ", round(upper_bound, 2), ")", sep = ""))


# Problem 4
nbhooddata <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/M6/buad502a-m6-novice-dataset-neighborhoods.xls", 1)
head(nbhooddata)
colnames(nbhooddata) <- c("Obs", "Neighborhood1", "Neighborhood2")
head(nbhooddata)

# a
# Randomization Condition: Data collected with suitable randomization - this is true
# 10% Condition: We have a random experiment and a reasonably sized sample
hist(nbhooddata$Neighborhood1)
hist(nbhooddata$Neighborhood2)
# Nearly Normal Condition: The data looks reasonably close to normal in our histograms
# Independent Groups Assumption: The two neighborhoods must be independent - we can assume this is true

# b
mean_n1 <- mean(nbhooddata$Neighborhood1, na.rm = TRUE)
mean_n2 <- mean(nbhooddata$Neighborhood2)
print(mean_n1)
print(mean_n2)

# c
mean_dif <- mean_n1 - mean_n2
print(mean_dif)

# d
var_n1 <- var(nbhooddata$Neighborhood1, na.rm = TRUE)
var_n2 <- var(nbhooddata$Neighborhood2)
print(var_n1)
print(var_n2)

# e
sd_n1 <- sqrt(var_n1)
sd_n2 <- sqrt(var_n2)
print(sd_n1)
print(sd_n2)

# f
n1 <- length(na.omit(nbhooddata$Neighborhood1))
n2 <- length(nbhooddata$Neighborhood2)
se_nbhoods <- sqrt(var_n1 / n1 + var_n2 / n2)
print(se_nbhoods)

# g
# Ho: mean N1 = mean N2
# Ha: mean N1 =/= mean N2
tstat <- mean_dif / se_nbhoods
print(tstat)

# Problem 5
df_nbhoods <- (sd_n1 / n1 + sd_n2 / n2)^2 / ((sd_n1 / n1)^2 / (n1 - 1) + (sd_n2 / n2)^2 / (n2 - 1))
print(df_nbhoods)

# b
pvalue <- 2 * pt(tstat, n1 + n2 - 2, lower = FALSE)
print(pvalue)

# c
pvalue <- 2 * pt(tstat, min(n1 - 1, n2 - 1), lower = FALSE)
print(pvalue)

# d
# c is more conservative because it uses a smaller degrees of freedom

# e
# both p-values are less than .05, se we reject the null hypothesis that the mean age
# of the houses in the two neighborhoods are the same


# Problem 6
# a
tstar <- abs(qt(.05/2, df_nbhoods))
lower_bound <- mean_dif - tstar * se_nbhoods
upper_bound <- mean_dif + tstar * se_nbhoods
print(paste("Confidence Interval: (", round(lower_bound, 2), ", ", round(upper_bound, 2), ")", sep = ""))

# b
# No, it is not

# c
# This says it is unlikely that the mean difference is 0 - we reject Ho

# d
# Ho: mean N1 - mean N2 = 0
# Ha: mean N1 - mean N2 =/= 0
pooled_var <- ((n1 - 1)*var_n1 + (n2 - 1)*var_n2) / ((n1 - 1) + (n2 - 1))
print(pooled_var)
pooled_se <- sqrt(pooled_var / n1 + pooled_var / n2)
print(pooled_se)
df_nbhoods <- n1 + n2 - 2
tstat <- mean_dif / pooled_se
print(tstat)
pvalue <- 2 * pt(tstat, df_nbhoods, lower = FALSE)
print(pvalue)
# Conclusion: Reject Ho, means are not equal

# e
tstar <- abs(qt(.05/2, df_nbhoods))
lower_bound <- mean_dif - tstar * pooled_se
upper_bound <- mean_dif + tstar * pooled_se
print(paste("Confidence Interval: (", round(lower_bound, 2), ", ", round(upper_bound, 2), ")", sep = ""))

# f
# The numbers are slightly different because we are assuming that the variances 
# of the two populations from which the samples have been drawn are equal. The 
# conclusions are the same because there is still a clear difference between the two means.

# Problem 7
bogodata <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/M6/bud502a-m6-novice-dataset-bogo.xls",1)
head(bogodata)
colnames(bogodata) <- c("Store", "WithProgram", "WithoutProgram")
head(bogodata)

# a
# Yes, traffic on each day (with and without program) is not independent of each other

# b
hist(bogodata$WithProgram, breaks = 10)
hist(bogodata$WithoutProgram, breaks = 10)
# Yes, the data was still randomized, the distributions are nearly normal, and 
# there are at least 15 obs but not greater than 10% of the population

# c
bogodata$Difference <- bogodata$WithProgram - bogodata$WithoutProgram
head(bogodata)
mean_dif <- mean(bogodata$Difference)
print(mean_dif)

# d
sd_dif <- sd(bogodata$Difference)
print(sd_dif)

# e
se_dif <- sd_dif / sqrt(nrow(bogodata))
print(se_dif)

# f
tstat <- mean_dif / se_dif
print(tstat)

# g
df_bogo <- nrow(bogodata) - 1
print(df_bogo)

# h
# One-sided, because the alternative hypothesis is that the program increases the
# mean traffic

# i
pvalue <- pt(tstat, df_bogo, lower = FALSE)
print(pvalue)

# j
# The p-value is much greater than .05, so we fail to reject the null and conclude
# that there is no statistical difference in traffic


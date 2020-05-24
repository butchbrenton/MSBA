rm(list = ls())

# import xlsx
require(xlsx)

## Problem 1
networthdata <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/M6/buad502a-m6-expert-dataset-net-worth.xls",1)
#a Create a Histogram
hist(networthdata$Net.Worth/1000, col = "blue",
     main = "Histogram of Net Worth",
     xlab = "1,000 Dollars")
# What does it tell us - That there is a nearly normal distribution
# centered just above $500,000 and not skewed

#b Report on assumptions and conditions necessary to use a t-test
# Randomization Condition - The survey was a random sample of customers so this is true
# 10% Condition - The sample size is not more than 10% of a large firm
# Nearly Normal Condition - The histogram shows a nearly normal distribution

#c Mean
meanNetWorth <- mean(networthdata$Net.Worth)
meanNetWorth

#d Standard Deviation
SDNetWorth <- sd(networthdata$Net.Worth)
SDNetWorth

#e Standard Error
SENetWorth <- SDNetWorth / sqrt(nrow(networthdata))
SENetWorth

#f Standar Error for 1000 sample size instead of 325
SENetWorth1000 <- SDNetWorth / sqrt(1000)
SENetWorth1000

#g Degrees of freedom (n - 1) for t-statistic 1 less than rows
dfNetWorth <- nrow(networthdata) - 1
dfNetWorth

#h Degrees of freedom (n- 1) for t-statistic with 1000 sample size
dfNetWorth1000 <- 1000 - 1
dfNetWorth1000

##Problem 2 - Boko Fittlesworth wants to know if mean net worth is $425,00
#a Null Hypothesis is that the mean net worth is $425,000

#b The alternative is two-sided because we want to check to see if the mean
#  is more or less than $425,000

#c value of test statistic
testStatNetWorth <- (meanNetWorth - 425000)/SENetWorth
testStatNetWorth

#d p-value of the test statistic
pv_NetWorth <- 2 * pt(testStatNetWorth, dfNetWorth, lower.tail = FALSE)
pv_NetWorth

#e Conclusion at alpha = 0.5?
# Because the p-value is << 0.5 we reject the Null Hypothesis that 
# the mean Net Worth is $425,000

#f How large a sample would cut the margin of error by 60%?
#calculate t*
tstarNetWorth <- abs(qt(.05/2, dfNetWorth, ))
tstarNetWorth
#calculate ME
MENetWorth <- tstarNetWorth * SENetWorth
MENetWorth
#calculate desired margin of error 
goal_MENetWorth <- 0.60 * MENetWorth
goal_MENetWorth
#calculate sample size for desired margin of error
zstar <- 1.96
goal_samplesize <- (zstar * SDNetWorth / goal_MENetWorth) ^ 2
goal_samplesize

## Problem 3 Basketball
bballdata <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/M6/buad502a-m6-expert-dataset-basketball.xls",1)
#a Create and interperet a histogram
hist(bballdata$Mean.3.pointers, col = "darkorange",
     main = "Histogram of Three-Point Goals per Game",
     xlab = "Mean number of 3-point goals per game")
# What does it tell us - That there is a nearly normal distribution
# centered just around 3.5 and no obvious skewing

#b Report on assumptions and conditions necessary to use a t-test
# Randomization Condition - The survey is of the top players and not a random sample of customers
# however the data was collected from a randomized experiment so this is true
# 10% Condition - The sample size is not more than 10% of players
# Nearly Normal Condition - The histogram shows a nearly normal distribution

#c Mean 
mean3p <- mean(bballdata$Mean.3.pointers)
mean3p

#d The data is the mean number of three-point goals per game for each player
# This is not a concern for interpreting the interval provided the data is not 
# skewed.  The histogram shows no obvious skewing so this should not be a concern.

#e Find the 95% confidence interval for the mean number of 3-point goals per game
n <- nrow(bballdata)
SDbball <- sd(bballdata$Mean.3.pointers)
SEbball <- SDbball / sqrt(n)
tstarbball <- abs(qt(0.5/2, n - 1))
lower_bound <- mean3p - tstarbball * SEbball
upper_bound <- mean3p + tstarbball * SEbball 
print(paste("95% Confidence Interval: (", round(lower_bound, 2), ", ", round(upper_bound, 2), ")", sep = ""))

## Problem 4 
winedata <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/M6/buad502a-m6-expert-dataset-wine.xls",1)
#a convert the vintage to age of wine
winedata <- 2020 - winedata[,2:3]
# create histograms
hist(winedata$Cellar.1, col = "darkred",
     main = "Histogram of Wine Age in Cellar 1",
     xlab = "Age of Wine")
hist(winedata$Cellar.2, col = "yellow",
     main = "Histogram of Wine Age in Cellar 2",
     xlab = "Age of Wine")

#b Report on assumptions and conditions for the difference between two means t-test
# Randomization Condition - The survey is a random sample of wines, this is true
# 10% Condition - The experiment is random and the sample size is assumed to be less than 10%
# Nearly Normal Condition - The histogram shows a mostly normal distribution so this is reasonable
# Independent Groups Assumption - The two friends each have independent wine collections so this is true

#c Find the sample means for age for the two cellars
mean_cellar1 <- mean(winedata$Cellar.1)
mean_cellar1
mean_cellar2 <- mean(winedata$Cellar.2,na.rm=TRUE)
mean_cellar2

#d Find the estimated differences between the two means
mean_agediff <- abs(mean_cellar1 - mean_cellar2)
mean_agediff

#e Find the sample variances for each wine cellar
var_cellar1 <- var(winedata$Cellar.1)
var_cellar1
var_cellar2 <- var(winedata$Cellar.2,na.rm=TRUE)
var_cellar2

#f Find the sample standard deviation for each wine cellar
sd_cellar1 <- sd(winedata$Cellar.1)
sd_cellar1
sd_cellar2 <- sd(winedata$Cellar.2,na.rm=TRUE)
sd_cellar2

#g Find the standard error of the difference between the two sample means
len_cellar1 <- length(winedata$Cellar.1)
len_cellar2 <- length(na.omit(winedata$Cellar.2))
se_cellars <- sqrt((var_cellar1/len_cellar1) + (var_cellar2/len_cellar2))
se_cellars

#h calculate the t-statistic for the observed diffrences in mean ages
# assuming the true mean difference is zero
tstat_wine <- mean_agediff / se_cellars
tstat_wine

## Problem 5
#a Test the hypothesis that the mean age of the wine
# in the two winecellars is the same
# [this would require considerable sampling and taste testing to be thorough ;) ]
# the natural way for comparing distributions of two groups is side by side boxplots
boxplot(winedata$Cellar.1, winedata$Cellar.2, na.rm = TRUE,
        names = c("Cellar 1","Cellar 2"), 
        col = c("darkred","yellow"),
        main = "Boxplots of Wine Age for Comparision")
#visually the mean wine ages are very close, cellar 2 has a wider range

#b How many degrees of freedom are there?
#variances are unequal (from problem 4)
df_cellars <- (sd_cellar1/len_cellar1 + sd_cellar2/len_cellar2)^2 /
        ((sd_cellar1/len_cellar1)^2 / (len_cellar1 -1)
         + (sd_cellar2/len_cellar2)^2 / (len_cellar2 - 1))
df_cellars

#c Calculate the p-value of the statistic using df=n1+n2-2
pvalue_cellars <- 2 * pt(tstat_wine, len_cellar1 + len_cellar2 - 2, lower.tail = FALSE)
pvalue_cellars

#d Calculate the p-value of the statistic using df=min(n1-1, n2-1)
pvalue_cellarsd <- pt(tstat_wine, min(len_cellar1 -1, len_cellar2 - 1), lower.tail = FALSE)
pvalue_cellarsd

#e Which is more conservative, of b and c?  
# assume comparison intended is of c and d, the two p-values to be compared
# method in d is more conservative, becasue it uses a lower value for the degrees of freedom

#f What do you conclude at alpha = 0.5
# Both p-values are greater than 0.5 so we can conclude that the 
# Null Hypothesis can be accepted and the mean age of the wines in the
# two cellars are the same.  

## Problem 6
#a Find the 95% confidence interval for the mean difference in ages of wines
# in the two wine cellars
tstar_wine <- abs(qt(.05/2, df_cellars))
lower_bound_wine <- mean_agediff - tstar_wine * se_cellars
upper_bound_wine <- mean_agediff + tstar_wine * se_cellars
print(paste("95% Confidence Interval: (", round(lower_bound_wine, 2),
            ", ", round(upper_bound_wine, 2), ")", sep = ""))

#b Is zero within the confidence window?
# Yes, in fact zero is near the middle of the confidence window

#c What does it say about the Null Hypothesis that the mean difference is zero
# The confidence window supports H0 since zero is within the 95% confidence window

#d Test the null hypothesis at alpha=0.05 using the pooled t-test
# show the t-statistic, p-value, and the conclusion
pooled_var_wine <- ((len_cellar1 - 1) * var_cellar1 + (len_cellar2 - 1) * var_cellar2) /
        ((len_cellar1 - 1) + (len_cellar2 - 1))
pooled_var_wine

pooled_se_wine <- sqrt(pooled_var_wine/len_cellar1 + pooled_var_wine/len_cellar2)
pooled_se_wine

df_cellars6 <- len_cellar1 + len_cellar2 - 2
df_cellars6

tstat_wine6 <- mean_agediff / pooled_se_wine
tstat_wine6

pvalue_cellars6 <- 2 * pt(tstat_wine6, df_cellars6, lower.tail = FALSE)
pvalue_cellars6

# Conclusion - Accept the Null Hypothesis, the means are almost equal

#e Find a 95% confidence interval using pooled degrees of freedom
tstar_wine_6 <- abs(qt(.05/2, df_cellars6, lower.tail = FALSE))
tstar_wine_6
lower_bound_wine <- mean_agediff - tstar_wine_6 * pooled_se_wine
upper_bound_wine <- mean_agediff + tstar_wine_6 * pooled_se_wine
print(paste("95% Confidence Interval: (", round(lower_bound_wine, 2),
            ", ", round(upper_bound_wine, 2), ")", sep = ""))

#f are the answers different from waht you found previously?
# The answers have aslight amount of vartiation from those previously
# calculated because we are assuming that the variances are equal.
# The differences are very minimal and do not change our conclusions
# and reinforce our observation that the means are very close

## Problem 7
bloomsdata <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/M6/buad502a-m6-expert-dataset-blooms.xls",1)
bloomsdata$diff <- bloomsdata$With.Fertilizer - bloomsdata$Without.Fertilizer

#a Are the data paired? Explain.
# Yes, the two randomly selected plants are the same except for the fertilizer

#b Do the data meet the other assumptions and conditions
# Paired Data Assumption - The data is paired because they are the same variety
# Independence Assumption - The plants are raised independently 
# Randomization Condition - The plants are chosen with a coin toss
# 10% Condition - The nursery grows a lot of plants, 2 is less than 10%
# Normal Population Assumption  - The sample population differences follow a normal distribution
# Nearly Normal Condition - Histograms show a nearly normal distribution of differences
hist(bloomsdata$With.Fertilizer, col = "blue",
     main = "Histogram with Fertilizer",
     xlab = "Number of Blooms")
hist(bloomsdata$Without.Fertilizer, col = "red",
     main = "Histogram without Fertilizer",
     xlab = "Number of Blooms")

# c. Compute the mean difference
mean_diff_bloom <- mean(bloomsdata$diff)
mean_diff_bloom

# d. Compute the standard deviation of the differences.
sd_dif_blooms <- sd(bloomsdata$diff)
sd_dif_blooms

# e. Compute the standard error of the mean difference.
se_dif_blooms <- sd_dif_blooms / sqrt(nrow(bloomsdata))
se_dif_blooms

# f. Find the value of the t-statistic.
tstat_blooms <- mean_diff_bloom / se_dif_blooms
tstat_blooms

# g. How many degrees of freedom does the t-statistic have?
df_blooms <- nrow(bloomsdata) - 1
df_blooms

# h. Is the alternative hypothesis one- or two-sided?
## THe alternative hypothesis is one-sided because we are considering
#if the mean of the differences is greater than zero

# i. What is the p-value associated with this statistic?
pvalue_blooms <- pt(tstat_blooms, df_blooms, lower.tail = FALSE)
pvalue_blooms

# j. At alpha = 0.05, what do you conclude?
# The p-value is less than 0.05 so we must reject the null hypothesis
# and conclude that there is a statistical difference in the two plants
        

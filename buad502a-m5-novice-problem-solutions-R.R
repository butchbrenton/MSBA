## Question 1 ###
#1. Yes
#2. 
n = 60
yes = 14
phat <- yes/n
print (phat)
phat
#3.
standard_error <- sqrt((phat*(1-phat))/n)
standard_error
#d.
conf <- 1.96
top <- phat + conf *standard_error
bottom <- phat - conf* standard_error
print(paste("Confidence Interval: (", round(bottom, 2), ", ", round(top, 2), ")", sep = ""))


###Question 2###
n = 350
perc_walk = 0.65
num_walk = n* perc_walk
standard_error = sqrt(perc_walk * (1- perc_walk)/ n)
standard_error
#1.
top <- perc_walk +1.96 *standard_error
bottom <- perc_walk - 1.96*standard_error
print(paste("Confidence Interval: (", round(bottom, 2), ", ", round(top, 2), ")", sep = ""))
#2.
top <- perc_walk +1.64 *standard_error
bottom <- perc_walk - 1.64*standard_error
print(paste("Confidence Interval: (", round(bottom, 2), ", ", round(top, 2), ")", sep = ""))
#3.
#We see that the 90% confidence interval is smaller meaning we are a little less confident that the actual number is within that interval. 
#The larger confidenceinterval in 95% means we are more confident that the actual number is within that confidence interval and thus, it is wider.
#4.
new_se <- sqrt(perc_walk * (1- perc_walk)/ 500)
top <- perc_walk +1.96 *new_se
bottom <- perc_walk - 1.96*new_se
print(paste("Confidence Interval: (", round(bottom, 2), ", ", round(top, 2), ")", sep = ""))
#The new confidence interval is smaller than before. 
#Thus the more people we survey, the smaller our phat becomes and the more narrow our confidence interval becomes.
#5.
#we need to quadruple the sample sie.
new_size <- 4 * n



###Question 3###
#1.
margin_error <- 0.02
confidence_interval <- 1.96 #95%
samp_size <- 0.5*0.5/((margin_error/confidence_interval)**2)

#2.
confidence_interval_two <- 2.58
samp_size <- 0.5*0.5/((margin_error/confidence_interval)**2)
samp_size
#3.
margin_error <- 0.05
samp_size <- 0.5*0.5/((margin_error/confidence_interval)**2)
samp_size
#4.
#In order to have a margin of error that is 1%, we would need to survey more students than in the entire student body. 
#So we would just want to find the true population proportion at that point.

###Question 3 ###
#1.
#H0: survey results = predicted results
#0.3 = 115/250

#2.
expected = 0.3
yes = 115
no = 250
samp_prop = yes/no
samp_prop
sd = sqrt((expected * (1- expected))/no)
sd

#3.
zstat <- ((yes/no)-expected)/sd
zstat
#4.
#This seems very large, which indicates that there is a high probability that 0.3 is probably 
#not the true population proportion.

###Question 5###
#1.
#H0: March 2018 results = September 2017 results
#63% = 64%

#2.
#H1: March 2018 ≠ September 2017
#63% ≠ 64%

twentyeighteen <- 0.63
twentyseventeen <- 0.64
n <- 1018
num_people2018 <- n * twentyeighteen
num_people2017 <- n * twentyseventeen
overall <- (num_people2017 + num_people2018) / (n *2)
overall
#3.
expected_difference <- twentyseventeen- twentyeighteen
variance <- (twentyseventeen*(1- twentyseventeen))/n + (twentyeighteen*(1-twentyeighteen))/n
variance
zscore <- expected_difference/variance
zscore
#4. #5.
#The critical value is 3.291, thus our z score is well above this meaning we should reject the
#null hypothesis and consider that the percentage has changed significantly.

#6.
#Type I error is rejecting that the percentages are equal, when they actually are. 
#For us, this means saying the change is significant in the new poll, when it is not.

#7.
#Type II error is failing to reject the null, when they actually are different. 
#For us, this means saying the change is not significant in the new poll, when it actually is.

#8. 
#As Polls-R-Us, we would consider a Type I error more serious. 
#We don't want to reject a null hypothesis unless we are very certain that it should be rejected, for our own credibility.
#9.
#Political commentators would be more worried about Type II errors, because they want to be aware of changes in the 
#American people before anyone else.







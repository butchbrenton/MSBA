rm(list = ls())

#Import Packages
require(xlsx)

##Question 1
#Import Data
data <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/M5/buad502a-m5-expert-data-zip-code-sim.xls", 1)

#a create a histogram for the data
hist(data$X..Successes, col = "blue"  , main = "Histogram of Success Rate", xlab = "Success Rate")
#d calculate p, phat, SD, SE
p <- .85
n <- 500
phat <- mean(data$X..Successes)/100
standard_deviation <- sqrt((p*(1 - p))/n)
standard_error <- sqrt((phat*(1-phat))/n)
cat(paste("p = ", p, "p hat = ", phat, "Standard Deviation = ",
          round(standard_deviation, 5), "Standard Error = ", 
          round(standard_error, 5), sep = "\n"))

##Question 2
#a 
margin_error <- 0.025
confidence_interval <- 1.96 #95%
samp_size <- 0.5*0.5/((margin_error/confidence_interval)**2)
print(paste("Sample Size is: " ,round(samp_size,0), "  Residents", sep = ""))

#b
margin_error <- 0.025
confidence_interval <- 2.576 #99%
samp_size <- 0.5*0.5/((margin_error/confidence_interval)**2)
print(paste("Sample Size is: " ,round(samp_size,0), "  Residents", sep = ""))

#c
margin_error <- 0.05
confidence_interval <- 1.96 #95%
samp_size <- 0.5*0.5/((margin_error/confidence_interval)**2)
print(paste("Sample Size is: " ,round(samp_size,0), "  Residents", sep = ""))

#d 
margin_error <- 0.01
confidence_interval <- 1.96 #99%
samp_size <- 0.5*0.5/((margin_error/confidence_interval)**2)
print(paste("Sample Size is: " ,round(samp_size,0), "  Residents", sep = ""))


## Question 3
#a
phat <- .65
n <- 867
standard_error <- sqrt((phat*(1-phat))/n)
conf <- 1.96  #95%
top <- phat + conf * standard_error
bottom <- phat - conf * standard_error
print(paste("Confidence Interval: (", round(bottom, 3), ", ", round(top, 3), ")", sep = ""))

#b
phat <- .65
n <- 867
standard_error <- sqrt((phat*(1-phat))/n)
conf <- 1.645  #90%
top <- phat + conf * standard_error
bottom <- phat - conf * standard_error
print(paste("Confidence Interval: (", round(bottom, 3), ", ", round(top, 3), ")", sep = ""))

#d
phat <- .65
n <- 500
standard_error <- sqrt((phat*(1-phat))/n)
conf <- 1.645  #90%
top <- phat + conf * standard_error
bottom <- phat - conf * standard_error
print(paste("Confidence Interval: (", round(bottom, 3), ", ", round(top, 3), ")", sep = ""))

#e
phat <- .65
n <- 867
standard_error <- sqrt((.5*.5)/n)
half_standard_error <- standard_error/2 #half of the margin of error
conf <- 1.96  #95% confidence interval 
samp_size <- (phat*(1-phat))/((half_standard_error/conf)**2)
print(paste("Sample Size is: " ,round(samp_size,0), "  College Students", sep = ""))

#f
phat <- .65
n <- 867
standard_error <- sqrt((phat*(1-phat))/n)
conf <- 2.326  #98%
top <- phat + conf * standard_error
bottom <- phat - conf * standard_error
print(paste("Confidence Interval: (", round(bottom, 3), ", ", round(top, 3), ")", sep = ""))

## Question 4
p <- 14
n <- 60
#b calculate the value of the sample proportion p-hat
phat <- p/n
print(phat)
#c standard error
standard_error <- sqrt((phat*(1-phat))/n)
print(standard_error)
#d construct confidence interval for 95%
conf <- 1.960  #95% = 2SE
top <- phat + conf * standard_error
bottom <- phat - conf * standard_error
print(paste("Confidence Interval: (", round(bottom, 3), ", ", round(top, 3), ")", sep = ""))

## Question 5
#b Find the standard deviation of the sample porportion based on the null hypothesis
expected = .32
yes = 195
total = 425
samp_prop <- yes/total
samp_prop
sd <- sqrt((expected * (1 - expected))/total)
sd
#c find the standard error
standard_error <- sqrt((samp_prop*(1-samp_prop))/total)
standard_error
#d difference between the SD and the SE
abs(sd - standard_error)
#e find the z-statistic
zstat <- ((yes/total)-expected)/sd
zstat

##Question 6
#Sept 2017 54%
# Feb 2018 53%
#c Find the z-score of the observed proportion
#set the variables
sept17 <- .54
feb18 <- .53
n <- 913
#nsept17 <- n * sept17
#nfeb18 <- n * feb18
#overall <- (nsept17 + nfeb18)/(n*2)
expected_difference <- sept17 - feb18
#calculate the variance
variance <- (sept17 * (1 - sept17))/n + (feb18 * (1 - feb18))/n
variance
#calculate z-score
zscore <- expected_difference/variance
zscore


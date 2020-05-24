rm(list = ls())

#Import Packages
require(xlsx)

#Import Data
data <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/M4/buad502a-m4-expert-dataset-bodyfat.xls", 1)

#Looking at the data types and overview of the data
str(data)

##Question 1  - Assume population of 100 students
avg <- 435
sd <- 72
n <- 100
  
# Passing score for the best 35% applicants pass?
x <- 462.74307
(1-pnorm(x,avg,sd))*100

# Produce a normal curve that represents this distribution,
#   label mean and 3 standard deviations

lb <- 219; ub <- 651
x <- seq(-4,4,length=100)*sd + avg
hx <- dnorm(x,avg,sd)
plot(x, hx, type="n", xlab="Real Estate Exam Test Scores", ylab="",
     main="Normal Distribution", axes=FALSE)
i <- x >= lb & x <= ub
lines(x, hx)
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="blue") 
area <- pnorm(ub, avg, sd) - pnorm(lb, avg, sd)
axis(1, at=seq(200, 700, 50), pos=0)
abline(v=c(219,435, 651), col=c("blue", "red", "blue"), 
       lty=c(1,2,1), lwd=c(1,3,1))
legend("left", legend=c("3 SD", "Mean"),
       col=c("blue", "red"), lty=1:2, cex=0.8)

#Part a
lower <- 440
upper <- 560
(pnorm(upper,avg,sd)-pnorm(lower,avg,sd))*n

#Part b
lower <- 380 
upper <- 620
(pnorm(upper,avg,sd)-pnorm(lower,avg,sd))*n

#Part c
lower <- 320 
upper <- 680
(pnorm(upper,avg,sd)-pnorm(lower,avg,sd))*n

#Part d
lower <- 410 
upper <- 590
(pnorm(upper,avg,sd)-pnorm(lower,avg,sd))*n

##Question 2
avg <- 700
sd <- 180
n <- 36

#Part a
lower <- 280
upper <- 3200
(pnorm(upper,avg,sd)-pnorm(lower,avg,sd))*100


#Part b
lower <- 520
upper <- 880
(pnorm(upper,avg,sd)-pnorm(lower,avg,sd))*100

#Part c
x <- 850
pnorm(x,avg,sd)*100

#Part d
x <- 850
(1-pnorm(x,avg,sd))*100

##Question 3
avg <- 11.5
sd <- 2.1
n <- 11500

#Part a
lower <- 11.5
upper <- 14.0
(pnorm(upper,avg,sd)-pnorm(lower,avg,sd))*100

#Part b
lower <- 9.0 
upper <- 11.6
(pnorm(upper,avg,sd)-pnorm(lower,avg,sd))*100

#Part c
lower <- 10.0 
upper <- 15.0
(pnorm(upper,avg,sd)-pnorm(lower,avg,sd))*n

#Part d
upper <- 14.0 
(1-pnorm(upper,avg,sd))*n

#Part e
lower <- avg - sd
upper <- avg + sd
(pnorm(upper,avg,sd)-pnorm(lower,avg,sd))*n

##Question 4

#a. BODYFAT

fatbreak <- seq(0, 48, by = 3)
fatFreq <- hist(data$BODYFAT, 
                breaks = fatbreak, 
                main = "Histogram of Body Fat",
                xlab = "Body Fat",
                col = "green"
                )

#b. WEIGHT

weightFreq <- hist(data$WEIGHT, 
                breaks = 15, 
                main = "Histogram of Weight Data",
                xlab = "Weigth",
                col = "blue"
                )

#c. HEIGHT
bins <- seq(28, 80.5, by = 3.5)
heightFreq <- hist(data$HEIGHT, 
                   breaks = bins, 
                   main = "Histogram of Height Data",
                   xlab = "Height",
                   col = "red"
                   )
# Height excluding 29.5inch outlier 
bins <- seq(64, 79, by = 1)
data1 <- subset(data, data$HEIGHT !="29.5")
heightFreq <- hist(data1$HEIGHT, 
                   breaks = bins, 
                   main = "Histogram of Height Data Excluding Outlier",
                   xlab = "Height",
                   col = "purple"
                   )

#d. Produce a normal probability plot for body fat
qqnorm(data$BODYFAT, ylab="Body Fat", 
       xlab="Normal Scores")
qqline(data$BODYFAT)
#e Produce a normal probability plot for weight
qqnorm(data$WEIGHT, ylab="Weight", 
       xlab="Normal Scores")
qqline(data$WEIGHT)
#f Produce a normal probability plot for height
qqnorm(data$HEIGHT, ylab="Height", 
       xlab="Normal Scores")
qqline(data$HEIGHT)
#  Remove Outlier at 29.5 inches
qqnorm(data1$HEIGHT, ylab="Height Excluding Outlier", 
       xlab="Normal Scores")
qqline(data1$HEIGHT)
#f What are the mean and standard deviations?
BFavg <- mean(data$BODYFAT)
BFsd <- sd(data$BODYFAT)
Wavg <- mean(data$WEIGHT)
Wsd <- sd(data$WEIGHT)
Havg <- mean(data$HEIGHT)
Hsd <- sd(data$HEIGHT)
meanAndSd = data.frame(row.names = c("Body Fat", "Weight", "Height"),
                       mean =c(BFavg, Wavg, Havg),
                       SD =c(BFsd, Wsd, Hsd))
print(meanAndSd)

#g How well do the observations fit the normal model

#BF
lowerBF1 <- BFavg - 1 * BFsd
lowerBF2 <- BFavg - 2 * BFsd
lowerBF3 <- BFavg - 3 * BFsd
upperBF1 <- BFavg + 1 * BFsd
upperBF2 <- BFavg + 2 * BFsd
upperBF3 <- BFavg + 3 * BFsd
sum(data$BODYFAT > lowerBF1 & data$BODYFAT < upperBF1)/nrow(data)
sum(data$BODYFAT > lowerBF2 & data$BODYFAT < upperBF2)/nrow(data)
sum(data$BODYFAT > lowerBF3 & data$BODYFAT < upperBF3)/nrow(data)
#Weight
lowerW1 <- Wavg - 1 * Wsd
lowerW2 <- Wavg - 2 * Wsd
lowerW3 <- Wavg - 3 * Wsd
upperW1 <- Wavg + 1 * Wsd
upperW2 <- Wavg + 2 * Wsd
upperW3 <- Wavg + 3 * Wsd
sum(data$WEIGHT > lowerW1 & data$WEIGHT < upperW1)/nrow(data)
sum(data$WEIGHT > lowerW2 & data$WEIGHT < upperW2)/nrow(data)
sum(data$WEIGHT > lowerW3 & data$WEIGHT < upperW3)/nrow(data)
#Height 
lowerH1 <- Havg - 1 * Hsd
lowerH2 <- Havg - 2 * Hsd
lowerH3 <- Havg - 3 * Hsd
upperH1 <- Havg + 1 * Hsd
upperH2 <- Havg + 2 * Hsd
upperH3 <- Havg + 3 * Hsd
sum(data$HEIGHT > lowerH1 & data$HEIGHT < upperH1)/nrow(data)
sum(data$HEIGHT > lowerH2 & data$HEIGHT < upperH2)/nrow(data)
sum(data$HEIGHT > lowerH3 & data$HEIGHT < upperH3)/nrow(data)

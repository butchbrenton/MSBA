rm(list = ls())

#Import Packages
require(xlsx)

#Import Data
#data for question 1
data <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/M2/buad502a-m2-novice-data.xls", 1)
#data for question 2
data2 <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/M2/buad502a-m2-novice-data.xls", 2)


#Question 1
#look at the structure and head of data
str(data)
head(data)

#b.
returns <- rep(0, nrow(data))

for (row in 1:(nrow(data)-1)){
  returns[row] <- (data[row + 1,2]-data[row, 2])/data[row,2]
}

newdata <- cbind(data, returns)

#c.
hist(newdata$returns)
#d.
#Yes, the stock market appears to follow a bell curve even though the top 
#of the "bell" is tall

#e.
#There are other things that we should know to determine if data is normally distributed.
#Such as the mean, median, and mode are all the same. 68% of the data lies within
#one standard deviation of the mean, and 95% of the data is within 2 standard 
#deviations of the mean.

#f.
datawithoutlast <- newdata[-nrow(newdata),]
meanReturn <- mean(datawithoutlast$returns)
sdReturn <- sd(datawithoutlast$returns)
#The small mean return of 0.00017 tells us that average return is low and thus can be difficult
#to make money in the stock market. The standard deviation of 0.011 tells us
#that there is some variation in returns relative to the average, but the 
#deviation is nothing that would allow for a large return.

#g.
boxplot(newdata$returns, col = "lightgreen")

#The histogram seems to be more informative than the box plot.
#While they both tell you similar things, the historgram shows
#that most of our data is falling within 2 bins and our variance is small,
#while the box plot can be more difficult to interpret because
#of the various observations that have not fallen within the 
#quartiles.

#h.
summary(newdata$returns)

#i.
plot(newdata$Date, newdata$DJIA)

#Question 2
#look at the data
head(data2)
str(data2)
sum(is.na(data2))
#notice when we brought the data in some NAs were introduced, we need to eliminate these.

data2 <- data2[,-4]
data2 <- data2[,-3]
cleandata <- na.omit(data2)
nrow(cleandata)
#we have 48 rows of data which matches what we have in our excel sheet
#a. 
plot(cleandata$Stock.risk..beta., cleandata$Monthly.stock.return....)

#b
cor(cleandata$Stock.risk..beta., cleandata$Monthly.stock.return....)
#This shows a fairly strong positive correlation between the stock risk and monthly stock return

#c
regress <- lm(cleandata$Monthly.stock.return.... ~ cleandata$Stock.risk..beta.)
summary(regress) # show regression coefficients table
#slope: 0.3481 
#intercept: 0.3632




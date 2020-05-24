rm(list = ls())

#Import Packages
require(xlsx)

#Import Data
#data for question 1
#First row not column labels; delete first two rows and assign column labels later
data <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/M2/EDITbuad502a-m2-expert-data-1-Morningstar-ETF-Returns.xlsx", 1,startRow=3, header=FALSE)
#data for question 2
data2 <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/M2/buad502a-m2-expert-data-2-AMZN.xlsx", 1)
#data for question 3
data3 <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/M2/buad502a-m2-expert-data-3-MLB2016.xlsx", 1)

#Question 1
str(data)

# establish column labels
colnames(data) <- c("Name",	"Category",	"YTD Return %" , "1 mo Return %" , "3 mo Return %" , "1 yr Return %" , "3 yr Return %" , "Trading Volume")

#clean data to eliminate  dashes and blanks
#data [ data == "---" ] <- NA
#data <- na.omit(data)
summary(data)

#Create Histograms
hist(data$`1 mo Return %`)
hist(data$`3 mo Return %`)
hist(data$`1 yr Return %`)
hist(data$`3 yr Return %`)

#Determine average YTD returns
hist(data$`YTD Return %`)
summary(data$`YTD Return %`)

#five-number summaries
summary(data$`1 mo Return %`)
sd(data$`1 mo Return %`, na.rm = TRUE)
summary(data$`3 mo Return %`)
sd(data$`3 mo Return %`, na.rm = TRUE)
summary(data$`1 yr Return %`)
sd(data$`1 yr Return %`, na.rm = TRUE)
summary(data$`3 yr Return %`)
sd(data$`3 yr Return %`, na.rm = TRUE)

#Create boxplot
boxplot(data$`1 mo Return %`,main="1 mo Return %")
boxplot(data$`3 mo Return %`,main="3 mo Return %")
boxplot(data$`1 yr Return %`,main="1 yr Return %")
boxplot(data$`3 yr Return %`,main="3 yr Return %")


#Question 2

#Average daily returns over 10 year period
amznAve <- mean(data2$AMZN.Daily.Percent.Return,na.rm=TRUE)
amznAve

#SD of Amazon data 
amznSDprice <- sd(data2$AMZN.Closing.Price)
amznSDprice
amznSDreturn <- sd(data2$AMZN.Daily.Percent.Return,na.rm=TRUE)
amznSDreturn

#boxplot of Amazon Daily Returns
boxplot(data2$AMZN.Daily.Percent.Return, na.rm=TRUE, main="Amazon Daily Returns")

#boxplot of Amazon Daily Closing Price
boxplot(data2$AMZN.Closing.Price, na.rm=TRUE, main="Amazon Closing Price")

#Time series plot of Amazon Closing Price
plot(data2$Date, data2$AMZN.Closing.Price)

#Question 3

#Calculate expected winning percentage and observed winning percentage
CalWin <- (data3$Runs.Scored) ^ 2 / (data3$Runs.Scored ^ 2 + data3$Runs.Allowed ^ 2)
ObsWin <- data3$Won/(data3$Won + data3$Lost) 

#Add new columns to data
newdata3 <- cbind(data3, CalWin, ObsWin)
write.xlsx(newdata3,"C:/Users/17574/Documents/MSBA/Statistics/M2/NEWbuad502a-m2-expert-data-3-MLB2016.xlsx" )

#Create Scatterplot for expected and observed winning percentage
plot(newdata3$ObsWin, newdata3$CalWin, main = "Calculated vs. Observed Winning Percentage")

#Calculate correlation, slope and intercept of regression model
CalvObsCor <- cor(newdata3$ObsWin, newdata3$CalWin)
CalvObsCor
regress <- lm(newdata3$ObsWin ~ newdata3$CalWin)
summary(regress)



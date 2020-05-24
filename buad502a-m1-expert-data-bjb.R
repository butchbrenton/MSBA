rm(list = ls())

#Import Packages
require(xlsx)

#Import Data
data <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/buad502a-m1-expert-data.xls", 1)

#Looking at the data types and overview of the data
str(data)

#Combined Frequency and Relative Freq Tables for AcctTyp
AcctTypFreq <- table(data$AcctTyp)
AcctTypFreq <- as.data.frame(AcctTypFreq)
names(AcctTypFreq)[1] <- "AcctTyp"
AcctTypFreq$RelativeFreq <- AcctTypFreq$Freq/sum(AcctTypFreq$Freq)
AcctTypFreq

#Combined Frequency and Relative Freq Tables for OpenedBy
OpenedByFreq <- table(data$OpenedBy)
OpenedByFreq <- as.data.frame(OpenedByFreq)
names(OpenedByFreq)[1] <- "OpenedBy"
OpenedByFreq$RelativeFreq <- OpenedByFreq$Freq/sum(OpenedByFreq$Freq)
OpenedByFreq

#Combined Frequency and Relative Freq Tables for Branch
BranchFreq <- table(data$Branch)
BranchFreq <- as.data.frame(BranchFreq)
names(BranchFreq)[1] <- "Branch"
BranchFreq$RelativeFreq <- BranchFreq$Freq/sum(BranchFreq$Freq)
BranchFreq

#Combined Frequency and Relative Freq Tables for Customer
CustomerFreq <- table(data$Customer)
CustomerFreq <- as.data.frame(CustomerFreq)
names(CustomerFreq)[1] <- "Customer"
CustomerFreq$RelativeFreq <- CustomerFreq$Freq/sum(CustomerFreq$Freq)
CustomerFreq

#Account Type Frequency Bar Chart
AcctTyp <- table(data$AcctType)
barplot (AcctTyp, main = "Account Type Frequency Bar Chart")

#Branch Frequency Bar Chart
Branch <- table(data$Branch)
barplot(Branch, main = "Branch Frequency Bar Chart")

#Customer Frequency Bar Chart
Customer <- table(data$Customer)
barplot (Customer, main = "Customer Frequency Bar Chart")

#Opened by Frequency Bar Chart
OpenedBy <- table(data$OpenedBy)
barplot (OpenedBy, main = "Opened by Frequency Bar Chart")

#Sales by Account Type Bar Chart
AcctTyp.Amount <- tapply(data$Amount, data$AcctType, sum)
barplot (AcctTyp.Amount, main = "Sales by Account Type")

#Sales by Branch Bar Chart
Branch.Amount <- tapply(data$Amount, data$Branch, sum)
barplot (Branch.Amount, main = "Sales by Branch")

#Sales by Customer Bar Chart
Customer.Amount <- tapply(data$Amount, data$Customer, sum)
barplot (Customer.Amount, main = "Sales by Customer")

#Sales by OpenedBy Bar Chart
OpenedBy.Amount <- tapply(data$Amount, data$OpenedBy, sum)
barplot (OpenedBy.Amount, main = "Sales by Opened By")

#Account Type Pie Chart
pie(AcctTyp, main="Account Type Frequency Pie Chart")

#Branch Frequency Pie Chart
pie(Branch, main="Branch Frequency Pie Chart")

#Customer Frequency Pie Chart
pie(Customer, main="Customer Frequency Pie Chart")

#Opened by Frequency Pie Chart
pie (OpenedBy, main = "Opened by Frequency Pie Chart")

#Account Type Pie Chart
pie(AcctTyp.Amount, main="Account Type Sales Pie Chart")

#Branch Frequency Pie Chart
pie(Branch.Amount, main="Branch Sales Pie Chart")

#Customer Frequency Pie Chart
pie(Customer.Amount, main="Customer Sales Pie Chart")

#Opened by Frequency Pie Chart
pie (OpenedBy.Amount, main = "Opened by Sales Pie Chart")

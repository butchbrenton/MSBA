rm(list = ls())

#Import Packages
require(xlsx)

#Import Data
data <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/buad502a-m1-novice-data.xls", 1)

#Looking at the data types and overview of the data
str(data)

#Frequency Tables for Region
RegionFreq <- table(data$Region)
RegionFreq <- as.data.frame(RegionFreq)
names(RegionFreq)[1] <- "Region"
RegionFreq$RelativeFreq <- RegionFreq$Freq/sum(RegionFreq$Freq)
RegionFreq

#Frequency Tables for Rep
RepFreq <- table(data$Rep)
RepFreq <- as.data.frame(RepFreq)
names(RepFreq)[1] <- "Rep"
RepFreq$RelativeFreq <- RepFreq$Freq/sum(RepFreq$Freq)
RepFreq

#Frequency Tables for Item
ItemFreq <- table(data$Item)
ItemFreq <- as.data.frame(ItemFreq)
names(ItemFreq)[1] <- "Item"
ItemFreq$RelativeFreq <- ItemFreq$Freq/sum(ItemFreq$Freq)
ItemFreq

#Region Bar Charts
region <- table(data$Region)
barplot(region, main="Region Frequency Bar Chart")
barplot(RegionFreq$Freq, main="Region Frequency Bar Chart", names.arg = RegionFreq$Region)

region.units <- tapply(data$Units, data$Region, sum)
barplot(region.units, main="Units per Region Bar Chart")

region.unitcost <- tapply(data$Unit.Cost, data$Region, sum)
barplot(region.unitcost, main="Unit Cost per Region Bar Chart")

region.total <- tapply(data$Total, data$Region, sum)
barplot(region.total, main="Total Cost per Region Bar Chart")

#Rep Bar Charts
rep <- table(data$Rep)
barplot(rep, main="Representative Frequency Bar Chart")
barplot(RepFreq$Freq, main="Representative Frequency Bar Chart", names.arg = RepFreq$Rep)

rep.units <- tapply(data$Units, data$Rep, sum)
barplot(rep.units, main="Units per Representative Bar Chart")

rep.unitcost <- tapply(data$Unit.Cost, data$Rep, sum)
barplot(rep.unitcost, main="Unit Cost per Representative Bar Chart")

rep.total <- tapply(data$Total, data$Rep, sum)
barplot(rep.total, main="Total Cost per Representative Bar Chart")

#Item Bar Charts
item <- table(data$Item)
barplot(item, main="Representative Frequency Bar Chart")
barplot(ItemFreq$Freq, main="Representative Frequency Bar Chart", names.arg = ItemFreq$Item)

item.units <- tapply(data$Units, data$Item, sum)
barplot(item.units, main="Units per Item Bar Chart")

item.unitcost <- tapply(data$Unit.Cost, data$Item, sum)
barplot(item.unitcost, main="Unit Cost per Item Bar Chart")

item.total <- tapply(data$Total, data$Item, sum)
barplot(item.total, main="Total Cost per Item Bar Chart")

#Region Pie Charts
pie(region, main="Region Frequency Bar Chart")
pie(RegionFreq$Freq, main="Region Frequency Bar Chart", labels = RegionFreq$Region)

pie(region.units, main="Units per Region Bar Chart")

pie(region.unitcost, main="Unit Cost per Region Bar Chart")

pie(region.total, main="Total Cost per Region Bar Chart")

#Rep Pie Charts
pie(rep, main="Representative Frequency Bar Chart")
pie(RepFreq$Freq, main="Representative Frequency Bar Chart", labels = RepFreq$Rep)

pie(rep.units, main="Units per Representative Bar Chart")

pie(rep.unitcost, main="Unit Cost per Representative Bar Chart")

pie(rep.total, main="Total Cost per Representative Bar Chart")

#Item Pie Charts
pie(item, main="Representative Frequency Bar Chart")
pie(ItemFreq$Freq, main="Representative Frequency Bar Chart", labels = ItemFreq$Item)

pie(item.units, main="Units per Item Bar Chart")

pie(item.unitcost, main="Unit Cost per Item Bar Chart")

pie(item.total, main="Total Cost per Item Bar Chart")


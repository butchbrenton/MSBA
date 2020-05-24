rm(list = ls())

#Import Packages
require(xlsx)
library(data.tree)

#Import Data Files
pizzData <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/M3/buad502a-m3-expert-data2-pizza-and-beer.xls", 1)
studentData <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/M3/buad502a-m3-expert-data3-students.xls", 1)
defectData <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/M3/buad502a-m3-expert-data1-defects.xls", 1)

#Pizza and Beer
#1 
n18to24total <- pizzData[4,2]
n25to34total <- pizzData[4,3]
n35to44total <- pizzData[4,4]
n45to54total <- pizzData[4,5]
n55to64total <- pizzData[4,6]
n65plustotal <- pizzData[4,7]
totalpizza <- pizzData[4,8]
more18to24 <- pizzData[3,2] 
more25to34 <- pizzData[3,3]
more35to44 <- pizzData[3,4]
more45to54 <- pizzData[3,5]
more55to64 <- pizzData[3,6]
more65plus <- pizzData[3,7]
moretotal <- pizzData[3,8]
same65plus <- pizzData[2,7]
#a 
print(moretotal/totalpizza)
#b
print(n18to24total/totalpizza)
#c
print(more18to24/totalpizza + more25to34/totalpizza)
#d
print(n18to24total/totalpizza + moretotal/totalpizza - more18to24/totalpizza)
#e
print(more35to44/totalpizza)
#f
print(1 - more55to64/n55to64total)
#g
moreolderthan35 <- more35to44 + more45to54 + more55to64 + more65plus 
olderthan35total <- n35to44total + n45to54total + n55to64total + n65plustotal
print(moreolderthan35/olderthan35total)
#h
print(same65plus/n65plustotal)

#Question 2 - Students
#a 
#create probability tree names
students <- Node$new ("University Marijuana Users")
Stem <- students$AddChild ("STEM")
SocBeh <- students$AddChild ("Social Sciences")
ArtsandHum <- students$AddChild ("Arts & Humanities")
Bus <- students$AddChild ("Business")
#assign probabilities
Stem$p <- studentData[1,2]
SocBeh$p <- studentData[1,3]
ArtsandHum$p <- studentData[1,4]
Bus$p <- studentData[1,5]
#create children
StemM <- Stem$AddChild ("STEM Users")
StemA <- Stem$AddChild ("STEM Abstainers")
SocBehM <- SocBeh$AddChild("SS Users")
SocBehA <- SocBeh$AddChild("SS Abstainers")
ArtsandHumM <- ArtsandHum$AddChild("A & H Users")
ArtsandHumA <- ArtsandHum$AddChild("A & H Abstainers")
BusM <- Bus$AddChild("Bus Users")
BusA <- Bus$AddChild("Bus Abstainers")
#assign probability to children
StemM$p <- studentData[2,2] * Stem$p
StemA$p <- (1 - studentData[2,2]) * Stem$p
SocBehM$p <- studentData[2,3] * SocBeh$p
SocBehA$p <- (1 - studentData[2,3]) * SocBeh$p
ArtsandHumM$p <- studentData[2,4] * ArtsandHum$p
ArtsandHumA$p <- (1 - studentData[2,4]) * ArtsandHum$p
BusM$p <- studentData[2,5] * Bus$p
BusA$p <- (1 - studentData[2,5]) * Bus$p
#print basic tree
print (students, "p")
#plot tree with probabilities
plot(students)
GetNodeLabel <- function(node) paste0('Prob\n', node$p)
GetEdgeLabel <- function(node) {
  label = node$name
  return (label)
}
SetEdgeStyle(students, fontname = 'helvetica', label = GetEdgeLabel)
SetNodeStyle(students, fontname = 'helvetica', label = GetNodeLabel, shape = "circle")
SetGraphStyle(students, rankdir = "LR")
plot(students)

#b - percentage of students are Maijuana users
marUsers = StemM$p +SocBehM$p + ArtsandHumM$p + BusM$p
print (marUsers)
#c - perentage of studnets abstaining
marAbs = StemA$p +SocBehA$p + ArtsandHumA$p + BusA$p
print(marAbs)
#d - probability a random user is in A & H
print(ArtsandHumM$p/marUsers)
#e - probability a random user is in Business
print(BusM$p/marUsers)
#f - probability a random user is in STEM
print(StemM$p/marUsers)
#g - probability a random abstainer is in A & H 
print(ArtsandHumA$p/marAbs)
#h - probability a random abstainer is in Social Sciences
print(SocBehM$p/marAbs)
#i - probability a random abstainer is in STEM
print(StemA$p/marAbs)
#j - For a random student what is the Probability the abstainer is in STEM
print(Stem$p/marAbs)

#Defects
#3 
#count defective monitors and determine probabilities
num0 <- sum(defectData$X..Defects == 0)
num1 <- sum(defectData$X..Defects == 1)
num2 <- sum(defectData$X..Defects == 2)
num3 <- sum(defectData$X..Defects == 3)
num4 <- sum(defectData$X..Defects == 4, defectData$X..Defects == 5,defectData$X..Defects == 6, defectData$X..Defects == 7)
totalmonitors = num0+num1+num2+num3+num4
#a - Probability of defects for each sample space
sample_space <- 0:4
prob0 = num0/totalmonitors
prob1 = num1/totalmonitors
prob2 = num2/totalmonitors
prob3 = num3/totalmonitors
prob4 = num4/totalmonitors
probs <- c(prob0,prob1,prob2,prob3,prob4)
cbind(sample_space, probs)

#b - How many defects to expect per monitor
expectedvalue <- sum(sample_space * probs)
print(expectedvalue)

#c - What is the standard deviation of the number of defects per monitor
deviations <- (sample_space - expectedvalue) ^ 2 * probs
st_dev <- sqrt(sum(deviations))
print(st_dev)

#d - What is the expected number of defects in the next 100 monitors
expected100 <- 100 * expectedvalue
print(expected100)

#e - What is the standard deviation of the nuber of defects in the next 100 screens
st_dev100 <- st_dev * 100
print(st_dev100)

#f - Defective monitors have 4 or more bad pixels, how many defective monitors to expect in the next 100
defMon100 = prob4 * 100
print(defMon100)

#g - QI finds 50% of defective monitors, the rest ship and cost $74.49 to warranty
CostToWarranty <- defMon100 * 0.5 * 74.49
print (CostToWarranty)

#4 - Smartphone Factory defect rate
compA <- .45
compB <- .35
compC <- .20
compADef <- .01
compBDef <- .02
compCDef <- .01
#a Percentage of chips overall defective?
totalDef <- compA*compADef + compB*compBDef + compC*compCDef
print(totalDef)
#b What is the probability for a random defective chip to be from each factory
FactAdef <- compA*compADef/totalDef 
print(FactAdef)
FactBdef <- compB*compBDef/totalDef
print(FactBdef)
FactCdef <- compC*compCDef/totalDef
print(FactCdef)
#c What is the probability for a random non-defective chip to be from each factory
totalNon <- compA*(1 - compADef) + compB*(1 - compBDef) + compC*(1 - compCDef)
FactANon <- compA*(1 - compADef)/totalNon 
print(FactANon)
FactBNon <- compB*(1 - compBDef)/totalNon 
print(FactBNon)
FactCNon <- compC*(1 - compCDef)/totalNon 
print(FactCNon)

#5 On time flight rate of 60% and a random sample of 25 flights selected
#a Find the probability that exactly 15 are on time
Probof15in25 <- dbinom(15,25,.6)
print(Probof15in25)

#b Find the probability that at most 5 are on time.
x <- 0:5
Probof5orLessin25 <- sum(dbinom(x,25,.6))
print(Probof5orLessin25) 

#c Find the probability that 9 are on time.
x <- 9
Probof15in25 <- dbinom(x,25,.6)
print(Probof9in25)

#d Find he probability that at least 7 are on time.
x <- 7:25
ProbofAtLEast7in25 <- sum(dbinom(x,25,.6))
print(ProbofAtLEast7in25) 

#e Find the standard distribution of this binomial distribution.
StnDev <- sqrt(25 * 0.6 * 0.4)
print(StnDev)

#f Is it unusual for only 9 of these flights to be on time? [Hint: x is "unusual" if it is more than 2 standard deviations from the mean]
FltMean <- 25 * 0.6
print(FltMean)
#two sd from the mean
UsualFLightProb <- range(FltMean - 2 * StnDev, FltMean + 2 * StnDev)
print(UsualFLightProb)
#9 on time flights is unusual becasue 9 outside of UsualFLightProb range

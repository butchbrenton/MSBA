rm(list = ls())

#Import Packages
require(xlsx)

# Set working directory


############################# Economic Conditions ##############################

#Import Data
economicdata <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/M3/buad502a-m3-novice-data1-current-economic-conditions.xls", 1)

#Looking at the data types and overview of the data
str(economicdata)
head(economicdata)

# Question 1
economicdata[,2] <- economicdata[,2] / economicdata[4,2]
print(economicdata)
P_poor <- economicdata[3,2]
print(P_poor)

# Question 2
P_fair <- economicdata[2,2]
P_fairorpoor <- P_fair + P_poor
print(P_fairorpoor)

# Question 3
P_poorandpoorandpoor <- P_poor ^ 3
print(P_poorandpoorandpoor)

# Question 4
P_notpoor <- 1 - P_poor
P_notpoorandnotpoorandnotpoor <- P_notpoor ^ 3
print(P_notpoorandnotpoorandnotpoor)

# Question 5
# We assumed that each observation is independent

# Question 6
# This is reasonable because there is no reason that one persons view on current
# economic conditions affects another persons views. The observations came from 
# random sampling of the US adult population.


############################ Household Composition #############################

# Import Data
householddata <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/M3/buad502a-m3-novice-data2-householdcomposition.xls", 1)

# Look at data
str(householddata)
print(householddata)
row.names(householddata) <- householddata[,1]
householddata <- householddata[,-1]
print(householddata)

# Question 1
P_Asian <- householddata["Asian","Total"] / householddata["Total","Total"]
print(P_Asian)

# Question 2
P_Hispanicand2Skip <- householddata["Hispanic","X2.Skip.Generations"] / householddata["Total","Total"]
print(P_Hispanicand2Skip)

# Question 3
# Part 1 - Marginal Probability, Part 2 - Joint Probability

# Question 4
P_2AdultGengivenBlack <- householddata["Black","X2.Adult.Generations"] / householddata["Black","Total"]
print(P_2AdultGengivenBlack)

# Question 5
P_whitegiven2Skip <- householddata["White","X2.Skip.Generations"] / householddata["Total","X2.Skip.Generations"]
print(P_whitegiven2Skip)

# Question 6
P_2SkipgivenHispanic <- householddata["Hispanic","X2.Skip.Generations"] / householddata["Hispanic","Total"]
print(P_2SkipgivenHispanic)


############################ US Labor Force ####################################

# Question 1
# install.packages("data.tree")
library(data.tree)

laborforce <- Node$new("US Labor Force")
hispanic <- laborforce$AddChild("Hispanic")
black <- laborforce$AddChild("Black")
white <- laborforce$AddChild("White")
asian <- laborforce$AddChild("Asian")
other <- laborforce$AddChild("Other")

hispanic$p <- .17
black$p <- .11
white$p <- .62
asian$p <- .056
other$p <- .044

hispanicmale <- hispanic$AddChild("Hispanic Male")
hispanicfemale <- hispanic$AddChild("Hispanic Female")
blackmale <- black$AddChild("Black Male")
blackfemale <- black$AddChild("Black Female")
whitemale <- white$AddChild("White Male")
whitefemale <- white$AddChild("White Female")
asianmale <- asian$AddChild("Asian Male")
asianfemale <- asian$AddChild("Asian Female")
othermale <- other$AddChild("Other Male")
otherfemale <- other$AddChild("Other Female")

hispanicmale$p <- .17 * .58
hispanicfemale$p <- .17 * .42
blackmale$p <- .11 * .52
blackfemale$p <- .11 * .48
whitemale$p <- .62 * .54
whitefemale$p <- .62 * .46
asianmale$p <- .056 * .49
asianfemale$p <- .056 * .51
othermale$p <- .044 * .48
otherfemale$p <- .044 * .52

# print tree
print(laborforce, "p")

# plot basic tree
plot(laborforce)

# Plot Tree with probabilities
GetNodeLabel <- function(node) paste0('Prob\n', node$p)
GetEdgeLabel <- function(node) {
        label = node$name
    return (label)
}
SetEdgeStyle(laborforce, fontname = 'helvetica', label = GetEdgeLabel)
SetNodeStyle(laborforce, fontname = 'helvetica', label = GetNodeLabel, shape = "circle")
SetGraphStyle(laborforce, rankdir = "LR")
plot(laborforce)

# Question 2
US_labor_force <- 187564231
print(asian$p)
Asians_lf <- US_labor_force * asian$p
print(Asians_lf)

# Question 3
print(blackfemale$p)
print(blackfemale$p * US_labor_force)

# Question 4
P_FemalegivenWhite <- whitefemale$p / white$p
print(P_FemalegivenWhite)

# Question 5
P_Female <- whitefemale$p + blackfemale$p + hispanicfemale$p + asianfemale$p + otherfemale$p
P_WhitegivenFemale <- whitefemale$p / P_Female
print(P_WhitegivenFemale)

############################ Repair Calls  #####################################

# Import Data
repaircalls <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/M3/buad502a-m3-novice-data3-repaircalls.xls", 1)

# Look at data
str(repaircalls)
head(repaircalls)
repaircalls$Time <- substring(repaircalls[,2], 12, 16)


# Question 1
total_hours <- nrow(repaircalls)
sample_space <- 0:3
prob0 <- sum(repaircalls$X..calls == 0) / total_hours
prob1 <- sum(repaircalls$X..calls == 1) / total_hours
prob2 <- sum(repaircalls$X..calls == 2) / total_hours
prob3 <- sum(repaircalls$X..calls == 3) / total_hours
probs <- c(prob0, prob1, prob2, prob3)
cbind(sample_space, probs)

# Question 2
expectedvalue <- sum(sample_space * probs)
print(expectedvalue)

# Question 3
deviations <- (sample_space - expectedvalue) ^ 2 * probs
st_dev <- sqrt(sum(deviations))
print(st_dev)

# Question 4
total_11amhours <- sum(repaircalls$Time == "11:00")
prob0_11 <- sum(repaircalls$Time == "11:00" & repaircalls$X..calls == 0) / total_11amhours
prob1_11 <- sum(repaircalls$Time == "11:00" & repaircalls$X..calls == 1) / total_11amhours
prob2_11 <- sum(repaircalls$Time == "11:00" & repaircalls$X..calls == 2) / total_11amhours
prob3_11 <- sum(repaircalls$Time == "11:00" & repaircalls$X..calls == 3) / total_11amhours
probs_11am <- c(prob0_11, prob1_11, prob2_11, prob3_11)
expectedvalue_11am <- sum(sample_space * probs_11am)
print(expectedvalue_11am)

# Question 5
deviations_11am <- (sample_space - expectedvalue_11am) ^ 2 * probs_11am
sqrt(sum(deviations_11am))

# Question 6
hours_in_day <- length(unique(repaircalls$Time))
expectedvalue * hours_in_day * 30


# Question 7
st_dev * hours_in_day * 30


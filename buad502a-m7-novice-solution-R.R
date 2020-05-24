rm(list = ls())

##Question 1

#Inputs
exp.percent <- c(0.55,0.325,0.125)
obs <- c(171,137,34)
n <- 342

#Part1
expected <- exp.percent*n
expected

#Part2
residuals <- obs - expected
residuals.sq <- residuals**2
residual.exp <- residuals.sq/expected
chi <- sum(residual.exp)
chi

#Part3
dof <- length(obs) - 1
dof

#Part4
mean.chi <- dof
mean.chi

#Part 5
diff <- chi - mean.chi
diff

#Part 7
qchisq(.95, df=dof) 

#Part 8
1-pchisq(chi, dof)


##Question 2

#Part 3
prom1 <- c(163,122,40)
prom2 <- c(175,115,35)
prom3 <- c(177,103,45)

n <- 325

obs <- rbind(prom1,prom2,prom3)
obs

exp.percent <- c(0.55,0.325,0.125)
exp <- matrix(NA, 3,3)
exp[,1] <- n*exp.percent[1]
exp[,2] <- n*exp.percent[2]
exp[,3] <- n*exp.percent[3]
exp

#Part 4
residual <- obs-exp
residual.sq <- residual**2
residual.exp <- residual.sq/obs

chi <- sum(residual.exp)
chi

#Part 5
dof <- (nrow(obs)-1)*(ncol(obs)-1)
dof

#Part 6
qchisq(.95, df=dof) 


##Question 3

#Inputs
iphone <- c(234,135, 363,97,829)
android <- c(543,266,284,129,1222)
coltotals <- iphone + android
phone <- rbind(iphone,android,coltotals)
colnames(phone) <- c("1st","2nd","3rd","Needs Repair","Total")
phone <- data.frame(phone)
head(phone)

#Part 1
p.and <- phone$Total[2]/phone$Total[3]
p.and

#Part 2
p.3iph <- phone$X3rd[1]/phone$Total[3]
p.3iph

#Part 3
p.1iph <- (phone$X1st[1]/phone$Total[3])/(phone$X1st[3]/phone$Total[3])
p2.1iph <- phone$X1st[1]/phone$X1st[3]
p.1iph
p2.1iph

#Part 4
and <- p.and * phone$Total[3]
and

#Part 6
chance <- 1/8
iphone.exp <- c(chance*phone$Total[3],chance*phone$Total[3],chance*phone$Total[3],chance*phone$Total[3],4*chance*phone$Total[3])
and.exp <- c(chance*phone$Total[3],chance*phone$Total[3],chance*phone$Total[3],chance*phone$Total[3],4*chance*phone$Total[3])
phone <- rbind(phone, iphone.exp, and.exp)
phone

iphone.res <- ((phone[1,] - phone[4,])**2)/iphone.exp
and.res <- ((phone[2,] - phone[5,])**2)/and.exp

chi <- sum(iphone.res, and.res)
chi

dof <- 3

1-pchisq(chi, dof)

#Question 4
iphone <- c(647,182,829)
android <- c(944,278,1222)

phone <- rbind(iphone,android)
colnames(phone) <- c("Aqua","Other", "Total")
phone <- data.frame(phone)
head(phone)
#When you look at the data, it looks as if the proportion of Aqua Android phonesis 
#about the same as the proportion of Android phones

iphone <- c(647,182)
android <- c(944,278)

phone <- rbind(iphone,android)
colnames(phone) <- c("Aqua","Other")
phone <- data.frame(phone)
head(phone)
#Chi-squared Test of Independence

chisq.test(phone)
#http://www.r-tutor.com/elementary-statistics/goodness-fit/chi-squared-test-independence

#Question 5




require(xlsx)

data <- read.xlsx("C:/Users/17574/Documents/MSBA/Statistics/M7/buad502a-m7-novice-dataset3-casino.xls", 1)
head(data)

#Part 1
model1 <- lm(data[,3]~data[,2],data=data)
summary(model1)
coef(model1)

#Part 2
day1200 <- 1200*coef(model1)[2]+coef(model1)[1]
day1200

#Part 3
se <- as.numeric(summary(model1)[6])
lower <- day1200 - (1.96*se)
upper <- day1200 + (1.96*se)
lower
upper

#Part 4
se

#Part 5
day25000 <- 25000*coef(model1)[2]+coef(model1)[1]
day25000

#Part 6
lower <- day25000 - (1.96*se)
upper <- day25000 + (1.96*se)
lower
upper



#18.2 Use the source function to input executable R commands
# file created in R and saved to folder on C: drive
source("C:/Users/17574/Documents/MSBA/R class files/18_2.R")

#18.3 Read file using scan function with data values separated by spaces
x = scan(file = "C:/Users/17574/Documents/MSBA/R class files/18_3.R", )
print(mean(x))

#19.1 Write a single R command that calculates the probability of
#     an Australian male being over 6 feet tall
#     x = 72, mean = 70, sd = 3
#     pnorm will provide the probability less than 72
#     to get the probability greater than 72 is 1 - pnorm
1 - pnorm(72, 70, 3)

#19.2 f(x) = P(X = 2) n = 3 and p = 2/3
dbinom(2, 3, 2/3)

#19.12
mean(runif(1000000))

#19.14
prod(rbinom(2, 3, .4))

#20.1 graph scatterplot and speeds and stopping distance
plot(cars$speed, cars$dist, 
     main = "Scatterplot of Speed and Stopping Distance",
     sub = "50 cars from the 1920's",
     xlab = "Speed (mph)", ylab = "Distance (ft)" )


#20.4 QQ plot for islands dataset for normality
#     Is data set well-modeled by a normal distribution
qqnorm(islands, main = "Normal Q-Q Plot of Islands Dataset")
qqline(islands, col = "red")

#21.2 Use R to plot the function y=cos x on 0<x<4pi
x = seq(0, 4 * pi, length = 100)
y = cos(x)
plot(x, y, type = "l", main = "Plot of Cosine of 0 < x < 4*pi",
     col = "red")

#21.6 Make a plot of rate vs. balance
balance = function(deposit = 1000, rate = .01, years = 20) {
  return(deposit * (1 + rate) ^ years)
}                          #Balance function chapter 8     
x = 1:9                    # vary x from 1 to 9
y = balance(rate = x/100)  #call function for x/100
plot(x, y, xlab = "Interest rate %", ylab = "Final Balance",
     col = "blue", type = "b", las = 1,
     xlim=c(0,10), ylim=c(0,6000),
     main = "Final Balance for Varying Interest Rates", 
     sub = "20 year term for initial deposit of $1000")

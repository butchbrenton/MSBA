## M7

# 25.2
# random throw of 6 6-sided die
# iterate nrep times
# use unique function to test x
# if length of unique values is 6 (all apear exactly once)
# then iterate the count
nrep = 100000 
count = 0
for (i in 1:nrep) {
  x = sample(1:6, 6, replace = TRUE)
  if (length(unique(x)) == 6) count = count + 1
}
print (count/nrep)

#25.3  
# create vector with random placement of 40 NA's and 10 1's
# iterate loop nrep times
# use the diff function, adjacent 1's will deliver 0 not NA
# sum number of NA's in result, if all NA's (49)
# then no two women in line are adjacent
# iterate the count when this occurs
nrep = 100000   
count = 0
for (i in 1:nrep) {
  x = sample(c(rep(NA, 40), rep(1, 10))) #women are 1
  if (sum(is.na(diff(x))) == 49 ) count = count + 1
}
print (count/nrep)

# 25.4
# random throw of 5 6-sided dice
# iterate nrep times
# three repeating ones will be in position 123, 234, or 345
# sum these positions, if equal to three they are all 1's
# then iterate the count
nrep = 500000
count = 0
for (i in 1:nrep) {
  x = sample(1:6, 5, replace = TRUE)
  if ( sum(x[1:3]) == 3 ) count = count + 1
  if ( sum(x[2:4]) == 3 ) count = count + 1
  if ( sum(x[3:5]) == 3 ) count = count + 1
}
print(count/nrep)

#25.10
nrep = 100000
count = 0
for (i in 1:nrep)  {
  x = sample(6, 5, replace = TRUE)
  if (min(x) >= 2) count = count + 1
  }
print(count/nrep)

#26.2 
n     = length(precip)           # n is the sample size
xbar  = mean(precip)             # sample mean
sdev  = sd(precip)               # sample standard deviation
alph  = 0.1                      # alpha = 0.1 for 90% conf interval
crit  = qt(1 - alph / 2, n - 1)  # quantile for the t-distribution
half  = crit * sdev / sqrt(n)    # interval halfwidth
xbar + c(-1, 1) * half           # 90% confidence interval
print(paste("Confidence Interval: (", round(xbar - half, 3),
            ", ", round(xbar + half, 3), ")", sep = ""))

#27.3 
A <- matrix(-1:4, nrow = 2, ncol = 3) 
AT <- t(A)
print (A %*% AT)


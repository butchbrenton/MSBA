#22.3  The Caitlin number
n = 0 #nonnegative integer  - test integer result = 132
#a
if (n == 0)
  Cna = 1 else
    Cna = ((1/(n+1)) * (factorial(2*n)/(factorial(n) * factorial(n))))
#b
  if (n == 0)
  Cnb = 1 else
    Cnb = factorial(2 * n)/(factorial( n + 1) * factorial(n))
#c
  if (n < 2)
  Cnc = 1 else
    Cnc = prod((n + 2:n)/2:n)
cat(Cna,Cnb,Cnc)

#23.4  #vectorize the sum of two poisson distributions
n = 1000000     #length of vector
x = numeric(n)  #initialize x with zeros
x = rpois(n, 3) + rpois(n, 5) #sum two poisson distribution vectors
print(x[1:100])        #print first hundred elements of x

#23.8
# utilized basic structure of command found on Stack Overflow
checkerboard  = function (color1, color2) {plot(1:9, 1:9, type = "n")
  for (i in 1:8) {col <- if (i %% 2) c(color1, color2) else c(color2, color1)
  rect(i, 1:9, i+1, 9, col = col, border = "black")}
  }

checkerboard ("red" , "black")  #traditional checkerboard
checkerboard ("darkgreen" , "gold") #Go Tribe! (better Tribe color)
checkerboard ("black" , "gold") #Go Boilers!
checkerboard ("darkblue" , "gray") #Go Monarchs! (FTW)

#24.2 Recursive Fibonacci number function
Fib = function(n) {
  Fib = rep(1, n)                   #initializes sequence
  if (n < 3)                        #can't calculate 1,2 recursively
        Fib[n] = 1 else # returns 1 for 1,2
          for (i in 3:n) Fib[i] = Fib[i - 1] + Fib[i - 2]  #recursive sequence
  Fib[n]                            #returns nth element of sequence
  }
Fib(1)
Fib(2)
Fib(3)
Fib(4)
Fib(5)
Fib(6)
Fib(7)
Fib(8)
Fib(9)
Fib(10)
Fib(20)
Fib(30)

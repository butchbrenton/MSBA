#10.1
(2 - 3i) + (3 + 4i)  

#11.1  11.1 Create a 2x3x2 array named abb of US State abbreviations.  
#Then use the substr function to create a second 2x3x2 array named abb1 
#  which contains just the first letters of each string in abb.  

abb = array(state.abb, c(2, 3, 2))
abb
abb1 = substr(abb, 1, 1)
abb1

#12.2  
xor (c(FALSE, FALSE, TRUE, TRUE), c(FALSE, TRUE, FALSE, TRUE))

#13.1
x = c(1:100)
ifelse(floor(sqrt(x)) - sqrt(x) == 0, 0 , x)

#13.2 
month.len = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
month.abb[month.len > 30]

#13.5
x = c(-10:10)
mean(x[ x > 0])

#13.6
x = c(-10, -1, 0, 2, 17)
which(x > 0)

#14.1
x = c("3", "6", "5")
Str2num = function(x)  {as.numeric(paste(x, sep = '', collapse = ''))}
Str2num(x)
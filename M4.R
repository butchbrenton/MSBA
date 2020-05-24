#15.2 
x = list("gnu", 3:7, month.abb[1:12]) #creates the list named x 
print(x)
x[[3]][4]  #extracts the fourth element of the third component 


#16.2
creatures = c("dog", "cat", "armadillo", "human")
friendly = c(TRUE, TRUE, FALSE, TRUE)
diet = c("cats", "mice", "termites", "Twinkies(tm)")
waking.hours = c(c(13.9, 11.5, 5.9, 16.0))
creature.data = data.frame(friendly, diet, waking.hours, row.names = creatures)
new.creature.data  <- creature.data[ order(row.names(creature.data)), ]
print(new.creature.data)


#16.5
#Create arbitrary data frame of positive integers and -1's
xData <- data.frame(A = c(1, -1, 2, 3), B = c(3, 2, -1, 4), C = c(-1, 2, 4, 5), row.names = c("Row1" , "Row2", "Row3" , "Row4"))
print(xData)
xData[xData == "-1" ] <- NA   # replace -1 values with NA
print(xData)                  # print to validate

#17.1
#compute the mean value for the second column (stopping distance) of the built in cars data set
mean(cars[,2])

#17.2
# display a subset of the cars data associated with even-valued speeds
#use the subset function with a test for even-values
subset(cars, speed %%2 == 0)



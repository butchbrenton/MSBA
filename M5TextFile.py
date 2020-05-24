# -*- coding: utf-8 -*-

""" Program you solution to this assignment below using 
    the variable names as defined below                 """

"""import the states.csv file"""
#open file (change backslashes to double backslashes in file path)
#save to data list
#close file

f_in=open('C:\\Users\\17574\\WMPython\\WMPython\\Module5\\states.csv','r')
data=f_in.readlines()
f_in.close()

"""input using different variable name, convert to assigned variable"""
statesData = data 
"""Initialize variables"""
sumPop = 0
sumEVotes = 0
sumHwyMiles = 0
sumSqrMiles = 0

"""Let's do some data wrangling"""
statesData=statesData[1:] #deletes column headers
for i in range(len(statesData)):  #get rid of white space and split into list of lists
    statesData[i]=statesData[i].strip().split(',') #chained commands: split and strip
#converts data to correct type and sums data
    for j in range(len(statesData[i])): #iterates through sublist
        if j==1:
            statesData[i][j]=int(statesData[i][j]) 
            sumPop=sumPop + statesData[i][j]
        if j==2:
            statesData[i][j]=int(statesData[i][j]) 
            sumEVotes=sumEVotes+statesData[i][j]
        if j==3:
            statesData[i][j]=float(statesData[i][j]) 
            sumHwyMiles=sumHwyMiles+statesData[i][j]
        if j==4:
            statesData[i][j]=float(statesData[i][j]) 
            sumSqrMiles=sumSqrMiles+statesData[i][j]

#(list comprehension) generate list of sums
sums=[sumPop, sumEVotes, sumHwyMiles, sumSqrMiles] 

""" printed output """
print (statesData)
print (sums)
print ("The sum of the States' population:      ",sumPop, 'people')
print ("The sum of the States' electoral votes: ",sumEVotes, 'delegates')
print ("the sum of the States' highways:   ",sumHwyMiles, 'miles')
print ("The sum of the States' land area:    ",sumSqrMiles, 'square miles')
#statesData = 


#sums = 
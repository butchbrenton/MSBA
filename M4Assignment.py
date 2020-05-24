# -*- coding: utf-8 -*-

#def select(keys,values):
#    return (studentPerf.keys(),studentPerf.values())
# Insert function code here
#def select(keys,values):
#   for key in studentPerf:  #iterates through keys in dictionary
#      FirstName = key [0]  
#        Gender = key [1]     
#        Class = key [2]      
#    if Class == 'sophomore':   #conditional for sophomores
#            #creates list of 
#        elif Class == "junior': 
#        else :
#    return StudentPerf.keys()

       
    
studentPerf = {('Jeffery','male','junior'):[0.81,0.75,0.74,0.8],
('Able','male','senior'):[0.87,0.79,0.81,0.81],
('Don','male','junior'):[0.82,0.77,0.8,0.8],
('Will','male','senior'):[0.86,0.78,0.77,0.78],
('John','male','junior'):[0.74,0.81,0.87,0.73],
('Patrick','male','senior'):[0.9,0.82,0.94,0.79],
('Iggie','male','senior'):[0.8,0.88,0.87,0.88],
('Harvey','male','sophomore'):[0.66,0.76,0.79,0.76],
('Ralph','male','senior'):[0.81,0.78,0.8,0.89],
('Edward','male','senior'):[0.81,0.87,0.88,0.84],
('Eric','male','junior'):[0.76,0.73,0.83,0.76],
('Wallace','male','sophomore'):[0.7,0.8,0.79,0.8],
('Ronald','male','senior'):[0.76,0.78,0.82,0.83],
('Perry','male','junior'):[0.83,0.87,0.77,0.75],
('Robert','male','senior'):[0.92,0.8,0.82,0.84],
('Thomas','male','junior'):[0.76,0.72,0.8,0.72],
('Mark','male','senior'):[0.87,0.79,0.81,0.83],
('Santiago','male','junior'):[0.77,0.81,0.74,0.75],
('Diego','male','senior'):[0.78,0.8,0.8,0.8],
('Samuel','male','senior'):[0.8,0.89,0.82,0.87],
('Alejandro','male','senior'):[0.86,0.79,0.87,0.8],
('Hector','male','junior'):[0.79,0.72,0.78,0.72],
('Jin','male','senior'):[0.83,0.79,0.9,0.8],
('Yu Yan','male','junior'):[0.75,0.72,0.8,0.81],
('Fei Hung','male','senior'):[0.86,0.91,0.84,0.9],
('Chen','male','senior'):[0.76,0.77,0.84,0.88],
('Wei','male','senior'):[0.84,0.86,0.78,0.88],
('Fang','male','senior'):[0.78,0.89,0.89,0.86],
('Anders','male','junior'):[0.8,0.73,0.78,0.8],
('Nils','male','junior'):[0.82,0.84,0.79,0.76],
('Arne','male','sophomore'):[0.68,0.73,0.81,0.72],
('Jochen','male','junior'):[0.82,0.86,0.8,0.73],
('Jurgen','male','junior'):[0.68,0.78,0.78,0.81],
('Carl','male','junior'):[0.72,0.82,0.79,0.76],
('Thor','male','senior'):[0.91,0.84,0.9,0.89],
('Harold','male','junior'):[0.76,0.78,0.8,0.79],
('Aditya','male','senior'):[0.83,0.87,0.82,0.83],
('Varun','male','senior'):[0.76,0.86,0.88,0.79],
('Shantanu','male','senior'):[0.79,0.81,0.78,0.78],
('Krishnan','male','sophomore'):[0.66,0.76,0.74,0.8],
('Manoj','male','junior'):[0.75,0.77,0.72,0.81],
('Rahul','male','sophomore'):[0.73,0.67,0.68,0.7],
('Rohit','male','sophomore'):[0.75,0.75,0.77,0.74],
('Vijay','male','senior'):[0.91,0.84,0.83,0.9],
('Sophie','female','senior'):[0.83,0.96,0.9,0.95],
('Lynn','female','senior'):[0.97,0.85,0.93,0.88],
('Bailey','female','sophomore'):[0.83,0.78,0.74,0.75],
('Karen','female','junior'):[0.79,0.88,0.9,0.84],
('Audrey','female','junior'):[0.79,0.85,0.86,0.81],
('Suzie','female','junior'):[0.81,0.82,0.93,0.88],
('Marissa','female','senior'):[1,0.92,0.95,0.91],
('Hong','female','senior'):[0.92,0.93,0.88,1],
('Susudio','female','sophomore'):[0.75,0.85,0.85,0.81],
('Clarissa','female','junior'):[0.79,0.84,0.82,0.83],
('Layla','female','junior'):[0.87,0.88,0.87,0.92],
('Alanis','female','sophomore'):[0.75,0.77,0.76,0.81],
('Erin','female','senior'):[0.88,0.93,0.97,0.91],
('Chantel','female','junior'):[0.85,0.84,0.85,0.79],
('Laura','female','junior'):[0.82,0.84,0.94,0.88],
('Laurie','female','senior'):[0.86,0.93,0.89,0.94],
('Elle','female','senior'):[0.98,0.92,0.98,0.99],
('Alisa','female','sophomore'):[0.75,0.86,0.89,0.83],
('Else','female','junior'):[0.91,0.8,0.84,0.88],
('Anna','female','senior'):[0.88,0.95,0.97,0.83],
('Dorothy','female','junior'):[0.94,0.94,0.89,0.84],
('Bridgette','female','junior'):[0.85,0.8,0.84,0.8],
('Sophia','female','senior'):[0.93,0.96,0.94,0.87],
('Bianca','female','junior'):[0.83,0.79,0.93,0.9],
('Mia','female','junior'):[0.89,0.85,0.89,0.91],
('Monika','female','junior'):[0.89,0.82,0.9,0.91],
('Emma','female','senior'):[0.96,0.85,0.88,0.97],
('Margaurite','female','junior'):[0.88,0.86,0.85,0.79],
('Helga','female','senior'):[0.9,0.85,0.84,0.89],
('Patsy','female','junior'):[0.84,0.88,0.9,0.86],
('Phoebe','female','senior'):[0.85,0.94,0.88,0.97],
('Vivian','female','junior'):[0.78,0.86,0.89,0.77],
('Breeanne','female','sophomore'):[0.74,0.82,0.84,0.85],
('Charlotte','female','junior'):[0.77,0.84,0.87,0.77],
('Amelia','female','senior'):[0.87,1,0.89,0.92],
('Olivia','female','junior'):[0.84,0.78,0.85,0.91],
('Isabella','female','sophomore'):[0.78,0.86,0.83,0.8],
('Evelyn','female','junior'):[0.85,0.88,0.91,0.88],
('Abagail','female','senior'):[0.93,0.94,0.87,0.84],
('Ella','female','sophomore'):[0.75,0.82,0.76,0.87],
('Ava','female','junior'):[0.88,0.83,0.9,0.81],
('Madison','female','senior'):[0.93,0.97,0.9,0.95],
('Chloe','female','junior'):[0.88,0.88,0.94,0.84],
('Grace','female','junior'):[0.9,0.85,0.88,0.83],
('Aubrey','female','junior'):[0.83,0.77,0.83,0.8],
('Mila','female','sophomore'):[0.82,0.85,0.79,0.83],
('Zoe','female','sophomore'):[0.82,0.81,0.83,0.81],
('Leah','female','sophomore'):[0.8,0.84,0.78,0.75],
('Stella','female','senior'):[0.93,0.9,0.96,0.92],
('Claire','female','sophomore'):[0.83,0.79,0.86,0.86],
('Aurora','female','senior'):[0.92,0.87,0.91,0.9],
('Lucy','female','senior'):[0.82,0.82,0.96,0.88],
('Samantha','female','senior'):[0.92,0.95,1,0.93],
('Tabitha','female','senior'):[0.97,0.87,0.89,0.88]}

#test lines
#print (len(studentPerf))
#print(studentPerf.items ())

#print(studentPerf.keys ())
#print(studentPerf.values())

#keyvalue=select(studentPerf)
#print (keyvalue)

#index=selectindex(studentPerf)
#print(index)


# Complete the main program code below using the listed variables 

#select(dict/keys,index)

"""Generates list of First Names only for Sophomores"""
sophNames = []           #creates empty list
for key in studentPerf:  #iterates through keys in dictionary
    FirstName = key [0]  
    Gender = key [1]     #left for consistency
    Class = key [2]      
    if Class == 'sophomore':   #conditional for sophpmores
        sophNames.append (FirstName)  # adds first name to list
print ('The sophomores are:', sophNames)  



#classes = select()
# calls function 'select' to return list of class names only
#for key of [2] will return a list of ['sophomore', 'junior', 'senior']v
classes = ['sophomore', 'junior', 'senior']

#genders = select()
#calls function 'select' to return a list of genders only
#for key of [1] to return a list of ['female', 'male']
genders = ['female','male']

#dictAvgGrade = 
"""Generates dictionary of student average grades"""
dictAvgGrade = dict()
for key in studentPerf:  #iterates through keys in dictionary
    AveGrade= sum(studentPerf[key])/4*100 #determines average grade
    dictAvgGrade[key] = [AveGrade] #Builds dictionary
print ('The average grades are :  ', dictAvgGrade)

"""Generates list average grades for sophomores"""
gradesSoph = []           #creates empty list
for key in dictAvgGrade:  #iterates through keys in dictionary
    FirstName = key [0]  #left for consistency
    Gender = key [1]     #left for consistency
    Class = key [2]      
    if Class == 'sophomore':   #conditional for sophpmores
        AveGrade = (dictAvgGrade[key]) #assigns variable 
        gradesSoph.append (AveGrade)  # adds grade to list
print ('The average grade for sophmores are:', gradesSoph)  

#dictDemClass = 
#dictDemClass = {key:classes:0} #creates dictionary with classes with values of 0
#print (dictDemClass)
    
    
#dictDemClass = 
#counts the number of students in each class and assigns the value to the corresponding key

#dictDemGender = 
#Initializes the gender dictionary


#dictDemGender = 
#Assigns value of counted studnets to each gender

#dictGradeClass = 
#dictGradeClass = () #builds empty dictionary
#dictGradeClass = {'sophmore':gradesSoph,'junior':gradesJun,'senior':gradesSen}
#assigns variables to Classes to create dictionary
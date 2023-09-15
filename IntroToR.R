#PROGRAM  HEADER ---------------------------------------------------------------
#SCRIPT TITLE           : IntroToR  
#PROJECT                : R Training
#TASK                   : Introduction to coding basics with Base R programming
#CREATED BY             : Jasper         
#DATE CREATED           : 02Apr2023                         
#DATE LAST MODIFIED     : 15Sept2023                                                                                                
#LAST MODIFIED BY       :                                                                                                     
#REASON for MODIFICATION:                                                                                            
#-------------------------------------------------------------------------------

# DATA TYPES

## Numeric : data in form of numbers eg 12, 3, 3.4 
numeric1 <- c(1, 2, 3, 4, 5)
numeric2 <- c(1L, 2L, 3L, 4L, 5L)
class(numeric1)
typeof(numeric2)

## Integer: type in form of whole numbers only
numeric3 <- c(1L, 3L,5L)
typeof(numeric3)

## Character: type in form of letters or sentences. Must always be in double quotations marks
char1 <- c("ann", "erick", "", "l")
class(char1)
typeof(char1)

## Factor : type of data that store ordinal/categorical data points
fact1 <- factor("Primary", "Secondary", "University")
class(fact1)
typeof(fact1)

##Assigning values to factor data types
?factor

fact2 <- as.factor(fact1, 
                   levels = c("Primary", "Secondary", "University"),
                   labels = c("Primary", "Secondary", "University"), 
                   values = c(1, 2, 3)) #has an error. to investigate cause of error

# Type conversion
data6 <- as.character(numeric1) #converting numeric to character
data7 <- as.numeric(data6) #converting character to numeric
data8 <- as.numeric(numeric3) #converting integer to numeric

class(data6)
typeof(data6)

class(data7)
typeof(data7)

class(data8)
typeof(data8)

# dates and times types
date1 <- Sys.time()
date1
class(date1)
typeof(date1)

date2 <- Sys.Date()
date2

class(date2)
typeof(date2)

#calculation using dates

date2
date2 - 35

# DATA STRUCTURES

## 1. Vectors : One dimensional collection of data types. placeholder for one type of data

vect1 <- c(1, 3, 5, 68, 0) #vector of specified numeric data
vect1

vect2 <- 1:10 #vector of sequential numbers 1 to 10 
vect2

vect3 <- c(1:10, 15, 17, 21) #vector of 1 to 10 then 15 17 21
vect3

vect4 <- 25:0 #vector of numbers decreasing from 25 to 0
vect4

vect5 <- 0:30 #vector of numbers increasing from 0 to 30
vect5

vect6 <- 2.5:7.5
vect6

#NOTE: the default constant by which vector increases or decreases is 1

# getting the element at a position in a vector

vect5[25] #element at position 25, use square bracket to indicate position
vect5[26] #element at position 25, use square bracket to indicate position

# Calculations using vectors

vect5 * 2 #each element in the vector is multiplied by 2

vect5 - 10 # 10 is subtracted from each of the elements

vect5 / 5 # each element of vect5 is divided by 5

vect5^5 #each element is raised to the power of 5

sqrt(vect5) # outputs square root of each element of vector5

sqrt(vect5[31]) #outputs sqrt of element at position 31 of vect5

#Forming sequential numbers

(vect7 <- seq(from = 1, 
             to = 25, 
             by = 2)) #creates sequence of numbers between 1 and 25 increasing by 2

(vect8 <- seq(from = 1, 
             to = 25, 
             length.out = 50)) # creates a vector of length 50 (50 elements) from 1 to 25

(vect9 <- 0:30)
(length(vect9)) # function length before a vector object outputs vector length 

class(vect3)

class(vect4)

data5 <- c(1, 2, 3, "Secondary", "University")
class(data5)

as.numeric(data5)

## 2. Data frames : multidimensional collection of different data types. 
                  #use function data.frame to assign object as a data frame
                  #columns must have equal number of rows

(df1 <- data.frame(col1 = c(1, 2, 3, 4, 5,7),
                  col2 = c("ann", "john", "erick", "james", "june", "Jasper")))

class(df1)
typeof(df1)

df1[2, 2] #outputs element at row2, column2 from data frame 1=john
df1[6, 2]  #outputs element at row6, column 2 =jasper

df1$col2 #outputs all elements of column2 , $reference the column 

df1$col2[5] #outputs the element at position 5 of column 2=june

(df1$col3 <- df1$col1*4) #multiplies each element in column 1 by 4 and assigns them to column 3 of the same data frame
df1

(df1$col4 <- df1$col1/3)

(df1$col5 <- round(df1$col4, 3)) #rounds off col4 to 4 d.p

(df1$col6 <- signif(df1$col4, 3))

df1[, "col4"] #display all elements in col4-col name
df1[, 4] #display all elements in col4-col number

df1[3, ] #display all elements in row3 -row number

df1

df1[df1$col3 < 12, ] #display rows with col3 less than 12
df1[df1$col5 == 1, ] #display rows with col5 equals 1
df1[df1$col2 == "john", ] #display rows with col2 containing john
df1[df1$col6 > 1, ] #display rows with col6 greater than 1


(df2 <- data.frame(
  var1 = 1:15,
  var2 = 16:30
))

(df3 <- data.frame(
  var3 = 1:15,
  var4 = 16:30
))


(df4 <- data.frame(
  var1 = 16:30,
  var2 = 31:45
))

(df5 <- cbind(df2, df3)) #join two data frames row wise. Must have equal number of rows
(df6 <- rbind(df2, df4)) #appends a data frame to another, col names must be similar

(df2 <- data.frame(
  var1 = 1:15,
  var2 = 16:30
))

(df2$var3 <- paste(df2$var1, df2$var2)) #concatenates var1 and var2 with space in bewtween
(df2$var4 <- paste0(df2$var1, "-", df2$var2)) #concatenates var1 and var2 with a hyphen and no space bewtween

df2

attach(df1)
detach(df1)

?attach

## 3. Lists 

(list1 <- list(
  var1 = 1:20,
  var2 = rep(c("A", "B"), 10)
))


list1$var2[6]

list1[1]

list1[[1]][6]
list1$var1[6]

(list2 <- list(
  list1 = list(var1 = 1:20,
               var2 = rep(c("A", "B"), 10)),
  var3 = rep(c("A", "B"), 10)
))

str(list2)  # look at the structure

list2$list1$var1[1]
list2[[1]][1]

list2$list1$var2[1]
list2[[2]][1]


(list_all <- list(
  df1 = list(var1 = 1:20,
             var2 = rep(c("A", "B"), 10)),
  df2 = list(var3 = 1:20,
             var4 = rep(c("A", "B"), 10))
))

# convert list to dataframe
(list_all1 <- do.call(list_all)) #investigate


# Accessing elements in objects

# Missing data
# Numeric Missing data
(df1 <- data.frame(
  var1 = c(1:12, NA, NA, NA),
  var2 = 16:30,
  var3 = c(rep("", 5), rep(c("A", "B"), 5))
))

# check if col has missing numeric values

is.na(df1$var1)

!is.na(df1$var1)

sum(is.na(df1$var1))

df1[is.na(df1$var1), ]

df1[!is.na(df1$var1), ]


mean(df1$var1, na.rm = TRUE)

# Character Missing data

df1[df1$var3 != "", ]

(df1$var3 <- ifelse(df1$var3 == "", "M", df1$var3))
(df1$var4 <- ifelse(df1$var3 == "", "M", df1$var3)) #creates a new variable, col4

df1[df1$var3 == "", ]

df1

## 4. Matrices

# Importing data


# END OF PROGRAM ---------------------------------------------------------------
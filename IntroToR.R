#PROGRAM  HEADER ---------------------------------------------------------------
#SCRIPT TITLE           : IntroToR  
#PROJECT                : R Training
#TASK                   : Practise coding basics for Base R programming
#CREATED BY             : Jasper         
#DATE CREATED           : 02Apr2023                         
#DATE LAST MODIFIED     :                                                                                                  
#LAST MODIFIED BY       :                                                                                                     
#REASON for MODIFICATION:                                                                                            
#-------------------------------------------------------------------------------
# Data types
## Numeric

data1 <- c(1, 2, 3, 4, 5)
data2 <- c(1L, 2L, 3L, 4L, 5L)

class(data1)
typeof(data2)

## Character

data3 <- c("ann", "erick", "", "l")
class(data3)
typeof(data3)

## Factor
data4 <- factor("Primary", "Secondary", "University")
class(data4)

# data5 <- as.factor(data4, labels = c("Primary", "Secondary", "University"),
#                 values = c(1, 2, 3))

# Type conversion
data6 <- as.character(data1)
data7 <- as.numeric(data6)
data8 <- as.numeric(data3)

# date types
data9 <- Sys.time()
data9

data10 <- Sys.Date()
data10

class(data10)
typeof(data10)

data10
data10 - 35


# Objects
## Vectors
data1 <- c(1, 3, 5, 68, 0)
data2 <- 1:10
data3 <- c(1:10, 15, 17, 21)
data4 <- 25:0
data5 <- 0:30
data5a <- 2.5:7.5

data5[25]
data5[26]

data5 * 2

data5 - 10

data5 / 10

data5^5

sqrt(data5)

sqrt(data5[31])

data6 <- seq(from = 1, 
             to = 25, 
             by = 2)
data6

data7 <- seq(from = 1, 
             to = 25, 
             length.out = 50)

data5 <- 0:30
length(data5)


data3 <- c(1:10, 15, 17, 21)
class(data3)

data4 <- c("Primary", "Secondary", "University")
class(data4)


data5 <- c(1, 2, 3, "Secondary", "University")
class(data5)

as.numeric(data5)

## Data frames
df1 <- data.frame(col1 = c(1, 2, 3, 4, 5),
                  col2 = c("ann", "john", "erick", "james", "june"))


class(df1)
typeof(df1)

df1[2, 2]
df1[4, 2]

df1$col2

df1$col2[5]

df1$col3 <- df1$col1*4
df1$col4 <- df1$col1/3
df1$col5 <- round(df1$col4, signif())
df1$col6 <- signif(df1$col4, 3)

df1[, "col4"]
df1[, 4]

df1[3, ]
df1[, 3]

df1[df1$col3 < 12, ]
df1[df1$col5 == 1, ]
df1[df1$col2 == "john", ]
df1[df1$col6 > 1, ]


df8 <- data.frame(
  var1 = 1:15,
  var2 = 16:30
)

df9 <- data.frame(
  var3 = 1:15,
  var4 = 16:30
)


df11 <- data.frame(
  var1 = 16:30,
  var2 = 31:45
)

df10 <- cbind(df8, df9)
df12 <- rbind(df8, df11)

df8 <- data.frame(
  var1 = 1:15,
  var2 = 16:30
)

df8$var3 <- paste(df8$var1, df8$var2)
df8$var4 <- paste0(df8$var1, "-", df8$var2)


attach(df1)
detach(df1)

# Lists 


list1 <- list(
  var1 = 1:20,
  var2 = rep(c("A", "B"), 10)
)


list1

list1$var2[6]

list1[1]

list1[[1]][6]

list2 <- list(
  list1 = list(var1 = 1:20,
               var2 = rep(c("A", "B"), 10)),
  var3 = rep(c("A", "B"), 10)
)

list2

str(list2)  # look at the structure

list2$list1$var2[1]

list2[[1]][1]
list2[[2]][1]


list_all <- list(
  df1 = list(var1 = 1:20,
             var2 = rep(c("A", "B"), 10)),
  df2 = list(var3 = 1:20,
             var4 = rep(c("A", "B"), 10))
)

# convert list to dataframe
list_all1 <- do.call(list_all)




# Accessing elements in objects

# Missing data
# Numeric Missing data
df1 <- data.frame(
  var1 = c(1:12, NA, NA, NA),
  var2 = 16:30,
  var3 = c(rep("", 5), rep(c("A", "B"), 5))
)

# check if col has missing numeric values
is.na(df1$var1)

!is.na(df1$var1)

sum(is.na(df1$var1))

df1[is.na(df1$var1), ]

df1[!is.na(df1$var1), ]


mean(df1$var1, na.rm = TRUE)

# Character Missing data
df1[df1$var3 != "", ]


df1$var3 <- ifelse(df1$var3 == "", "M", df1$var3)
df1$var4 <- ifelse(df1$var3 == "", "M", df1$var3)


df1[df1$var3 == "", ]

# df1[df1$var3 == "", ] <- "M"

df1$var3[df1$var3 == ""] <- "M"

# Importing data
## Modern R (Packages)













# END OF PROGRAM ---------------------------------------------------------------
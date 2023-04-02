#PROGRAM  HEADER ---------------------------------------------------------------
#SCRIPT TITLE           : Cleaning  
#PROJECT                : R Training
#TASK                   : Import, Clean and transform STI dataset for analysis
#CREATED BY             : Jasper         
#DATE CREATED           : 02Apr2023                         
#DATE LAST MODIFIED     :                                                                                                  
#LAST MODIFIED BY       :                                                                                                     
#REASON for MODIFICATION:                                                                                            
#-------------------------------------------------------------------------------

# Importing data

STIData <- read_excel("Data/STIData.xls")

# Check for duplicates
duplicated(STIData$IdNumber)

sum(duplicated(STIData$IdNumber))

# convert ID Number to numeric
STIData$IdNumber <- as.numeric(STIData$IdNumber)

# save duplicates as another dataset
dups <- STIData[duplicated(STIData$IdNumber), ]


# save records duplicated by ID Number
dups2 <- STIData[STIData$IdNumber == 51, ]

# Reassign ID number for subject 51, when age ==  23

STIData$IdNumber[STIData$IdNumber == 51 & STIData$A1Age == 23] <- 227

# check duplicates again
sum(duplicated(STIData$IdNumber))

# check Case status
table(STIData$CaseStatus)

case3 <- STIData[STIData$CaseStatus == 3, ]

STIData$CaseStatus[STIData$CaseStatus == 3 & STIData$IdNumber == 31] <- 1
STIData$CaseStatus[STIData$CaseStatus == 3 & STIData$IdNumber == 1] <- 2
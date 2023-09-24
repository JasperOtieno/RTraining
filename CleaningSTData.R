#PROGRAM  HEADER ---------------------------------------------------------------
#SCRIPT TITLE           : CleaningSTIData  
#PROJECT                : R Training
#TASK                   : Import, Clean and transform the STI dataset for analysis
#CREATED BY             : Jasper         
#DATE CREATED           : 02Apr2023                         
#DATE LAST MODIFIED     : 24 Sept 2023                                                                                                
#LAST MODIFIED BY       : Jasper                                                                                                   
#REASON for MODIFICATION:                                                                                            
#-------------------------------------------------------------------------------

# Load necessary package and Importing data
if(!require(pacman))install.packages("pacman")

pacman::p_load(
tidyverse,
readxl,
inspectdf,
plotly,
janitor,
esquisse,
here
)

STIData <- read_xls(here("Dataset/STIData.xls"))
names(STIData)
dim(STIData)

#Clean the sex variable: two vars with some different values

unique(STIData$Sex...35)
unique(STIData$Sex...47)

STIData <- STIData %>% 
#check if the two sex vars are exact match and correctly replace if possible
  
  dplyr::mutate (Sexdif= ifelse(STIData$Sex...35 == STIData$Sex...47, "TRUE", "FALSE"), #check if exact match
                 Sex= case_when(STIData$Sex...47==STIData$Sex...35 ~STIData$Sex...47,
                                STIData$Sex...35=="Female"& STIData$Sex...47=="Male" ~"Female", #require verification
                                STIData$Sex...35=="Male"& STIData$Sex...47=="Female" ~"Male", #require verification
                                STIData$Sex...47=="Male" & is.na(STIData$Sex...35) ~"Male",
                                STIData$Sex...47=="Female" & is.na(STIData$Sex...35) ~"Female",
                                is.na(STIData$Sex...47) & STIData$Sex...35=="Male" ~"Male",
                                TRUE ~"No sex" #reassign values
                             ))

(missex<-STIData[is.na(STIData$Sexdif),c(1,35,47:49)])

STIData<- STIData %>% 
  select(-c(Sex...35,Sex...47,Sexdif))


# Check for duplicates on subject identifier and rows and clean

dups <- janitor::get_dupes(STIData) %>% 
  select(IdNumber,dupe_count, Date, A1Age, A2Occupation, Weight, Height) # save duplicate rows

(dupID<-janitor::get_dupes(STIData,IdNumber) %>% 
  select(IdNumber,dupe_count, Date, A1Age, A2Occupation, Weight, Height)) #save duplicates as another dataset

#dups <- STIData[duplicated(STIData$IdNumber), ] analogous to line 58-59
#sum(duplicated(STIData$IdNumber)) #get total number of duplicates
#sort(unique(STIData$IdNumber))

#Reassign ID number for subject 51, when age ==  23

STIData <- STIData %>%
  dplyr::mutate(IdNumber2= case_when(STIData$IdNumber==51 & STIData$A1Age == 23 ~ 227,
                            TRUE~STIData$IdNumber))   
STIData <- STIData %>% 
  select(-c(IdNumber)) %>% 
  rename(IdNumber=IdNumber2) %>% 
  relocate(IdNumber, .before = CaseStatus)

#Check type of the Id number and convert to character if needed
class(STIData$IdNumber)
STIData$IdNumber <- as.character(STIData$IdNumber)

#Confirm no ID duplicates
(dupID<-janitor::get_dupes(STIData,IdNumber) %>% 
    select(IdNumber,dupe_count, Date, A1Age, A2Occupation, Weight, Height))


#Check Case status and clean
table(STIData$CaseStatus)

(case3 <- STIData[STIData$CaseStatus == 3, 1:5 ])

STIData <- STIData %>%
  dplyr::mutate(CaseStatus2= case_when(STIData$CaseStatus==3 & STIData$IdNumber == 31 ~ 1,
                                       STIData$CaseStatus==3 & STIData$IdNumber == 1 ~ 2,
                                       TRUE~STIData$CaseStatus)) #reassign values

STIData <- STIData %>% 
  select(-c(CaseStatus)) %>% 
  rename(CaseStatus=CaseStatus2) %>% 
  relocate(CaseStatus, .after = IdNumber)

#confirm cleaned status
table(STIData$CaseStatus)


###########################################################################################
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

# Reassign proper codes to CaseStatus
STIData1 <- 
  STIData %>% 
  dplyr::mutate(CaseStatus1 = dplyr::if_else(CaseStatus == 2, 0, CaseStatus)) %>% 
  dplyr::relocate(c(CaseStatus1, Sex...47), .after = CaseStatus)


STIData1$CaseStatus1 <- factor(STIData1$CaseStatus1,
                               levels = 0:1,
                               labels = c("Negative", "Positive"))


class(STIData1$CaseStatus1)

# Drop unnecessary variables 
STIData1 <- 
  STIData1 %>% 
  dplyr::select(-c(CaseStatus, C3StiYesno))


# Rename variables 
STIData2 <- 
  STIData1 %>% 
  dplyr::rename(CaseStatus = CaseStatus1,
                ID = IdNumber,
                Sex = Sex...47,
                Age = A1Age,
                Occupation = A2Occupation,
                Church = A3Church,
                `Level Of Education` = A4LevelOfEducation)

# Grouping numerical variable
STIData2 <- 
  STIData2 %>% 
  dplyr::mutate(
    AgeCat = case_when(
      Age < 18 ~ "Below 18",
      Age >= 18 & Age < 30 ~ "18 - 30",
      Age >= 30 & Age < 50 ~ "30 - 50",
      Age >= 50 ~ "Above 50"
    )
  ) %>% 
  dplyr::relocate(AgeCat, .after = Age)

table(STIData2$AgeCat, STIData2$Sex)

# Removing numbers in string values
STIData2 <- 
  STIData2 %>% 
  dplyr::mutate(Occupation = toupper(stringr::str_replace(Occupation, "\\d", "")),
                Church = stringr::str_replace(Church, "\\d", ""),
                `Level Of Education` = stringr::str_replace(`Level Of Education`, "\\d", ""),
                A5MaritalStatus = stringr::str_replace(A5MaritalStatus, "\\d", ""),)
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
                             )) %>% 
  relocate(Sex, .after=A1Age)

(missex<-STIData[is.na(STIData$Sexdif),c(1,35,47:49)]) #output those missing sex entry

STIData<- STIData %>% 
  select(-c(Sex...35,Sex...47,Sexdif)) #remove the sex vars after creating clean one


#Check for duplicates on subject identifier and rows and clean

dups <- janitor::get_dupes(STIData) %>% 
  select(IdNumber,dupe_count, Date, A1Age, A2Occupation, Weight, Height) # save duplicate rows

(dupID<-janitor::get_dupes(STIData,IdNumber) %>% 
  select(IdNumber,dupe_count, Date, A1Age, A2Occupation, Weight, Height)) #save rows with duplicate IDs as another dataset

#sum(duplicated(STIData$IdNumber)) #get total number of duplicates
#sort(unique(STIData$IdNumber))

#Reassign ID number for subject 51, when age ==  23

STIData <- STIData %>%
  dplyr::mutate(IdNumber2= case_when(STIData$IdNumber==51 & STIData$A1Age == 23 ~ 227,
                            TRUE~STIData$IdNumber)) %>% 
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

(case3 <- STIData[STIData$CaseStatus == 3, 1:5 ]) #select rows with case=3 and columns 1:5 only

STIData <- STIData %>%
  dplyr::mutate(CaseStatus2= case_when(STIData$CaseStatus==3 & STIData$IdNumber == 31 ~ 1,
                                       STIData$CaseStatus==3 & STIData$IdNumber == 1 ~ 2,
                                       TRUE~STIData$CaseStatus)) %>% #reassing values
          select(-c(CaseStatus)) %>% 
          rename(CaseStatus=CaseStatus2) %>% 
          relocate(CaseStatus, .after = IdNumber)
  

#Convert CaseStatus to a factor and label
class(STIData$CaseStatus)

STIData$CaseStatus <- factor(STIData$CaseStatus,
                               levels = 1:2,
                               labels = c("Positive", "Negative"))  #assign labels

#confirm cleaned status
table(STIData$CaseStatus)

#Clean Date
table(STIData$Date) #nothing to clean

#Clean A1Age
table(STIData$A1Age)

STIData<- STIData %>% 
  mutate(AgeCat= case_when(A1Age <18 ~ "Below 18 Years",
                           A1Age >=18 & A1Age <=35 ~ "18 to 35 Years",
                           A1Age>35 & A1Age<=50 ~ "36 to 50 Years",
                           A1Age>50 ~ "Above 50 Years",
                           TRUE~ "Not categorized")) %>% 
  relocate(AgeCat, .after=A1Age)

#Confirm age categories
table(STIData$AgeCat)


# Remove numbers and special characters preceding string values

STIData<- STIData %>% 
  dplyr::mutate(A2Occupation = tolower(str_replace(A2Occupation, "\\d", "")),
                A3Church = str_replace(A3Church, "\\d", ""),
                A4LevelOfEducation = str_replace(A4LevelOfEducation, "\\d", ""),
                A5MaritalStatus = str_replace(A5MaritalStatus, "\\d", ""),
                D2Group1 = str_replace(D2Group1, "\\d", ""),
                D2Group2 = str_replace(D2Group2, "\\d", ""),
                E8WhyhaveSTI = str_replace(E8WhyhaveSTI, "\\d", ""),
                N10givereceiveforsex = str_replace(N10givereceiveforsex, "\\d", ""),
                N11Usedcondom = str_replace(N11Usedcondom, "\\d", ""),
                N12UseCondom = str_replace(N12UseCondom, "\\d", ""),
                N13TakenAlcohol = str_replace(N13TakenAlcohol, "\\d", ""),
                Typeofsti = str_replace(Typeofsti, "\\d", ""),
                N9Relationship = str_replace(N9Relationship, "\\d", ""))


###########################################################################################

# Rename variables 
# STIData2 <- 
#   STIData1 %>% 
#   dplyr::rename(CaseStatus = CaseStatus1,
#                 ID = IdNumber,
#                 Sex = Sex...47,
#                 Age = A1Age,
#                 Occupation = A2Occupation,
#                 Church = A3Church,
#                 `Level Of Education` = A4LevelOfEducation)



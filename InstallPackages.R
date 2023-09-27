#PROGRAM  HEADER ---------------------------------------------------------------
#SCRIPT TITLE           : Install Packages  
#PROJECT                : Package Installation
#TASK                   : Import, Clean and transform the STI dataset for analysis
#CREATED BY             : Jasper         
#DATE CREATED           : 27Sept2023                         
#DATE LAST MODIFIED     :                                                                                                 
#LAST MODIFIED BY       : 
#REASON for MODIFICATION:                                                                                            
#-------------------------------------------------------------------------------

#Load packages needed for most R data tasks

if(!require(pacman)) install.packages("pacman")
pacman::p_load(

  tidyverse,  #for general data cleaning tasks
  readxl,     #for reading excel files
  writexl,    # for writing excel files
  janitor,    # for data manipulation
  plotly,     #for interactive graphs
  here,       #for reading file paths
  arsenal     #for comparing data sets
  
)
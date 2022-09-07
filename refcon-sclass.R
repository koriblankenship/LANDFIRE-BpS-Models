# title: SUCCESSION CLASS RESULTS
# author: Kori Blankenship
# date: 22 June 2019
# output: table with the percent of each sclass for every bps in the master LANDFIRE model library
#
#This script reads results from the LANDFIRE master bps model database and calculates the sclass reference conditions.
#The LANDFIRE master bps model database (.sim) uses a standard set of ouput options:
#1. stateclasses every 1 timestep with "include zero values" option selected
#2. transitions ever 500 timesteps with "calculate as interval means" option selected
#The script is desigend to take these ouputs.
#To calculate sclass reference percents the first 500 timesteps, when the model equiliberalates and 
#overcomes the infulence of the initial conditions, are eleminated. 

# *************************************************************
# Setup
# *************************************************************
library(rsyncrosim)
library(dplyr)
library(readr)

# *************************************************************
# Get L49 State Class results 
# *************************************************************
#sim setup
programFolder = "C:/Program Files/SyncroSim"
mywd = "C:/Users/kblankenship/Documents/aBpSreview/REVIEW/MODELS/ModelQC"
mylib = "LANDFIRE BpS Models 03 April 2019"
myprj = "BpS Models"
myscn = 8802
setwd(mywd)
Ses = session(programFolder)
Lib = ssimLibrary(name=mylib, model="stsim", session=Ses)
Prj = project(Lib, project=myprj)
Scn = scenario(Prj, myscn)

#get sclass results
OutStateSheet = "STSim_OutputStratumState"
SclassResults49 = datasheet(Scn, OutStateSheet)
#keep results for timesteps 500-1000
SclassResults49<-SclassResults49[SclassResults49$Timestep>500,]

#remove the set of unattributed columns (i.e. fields that LANDFIRE does not use, for example SecondaryStratumID)
SclassResults49 <- SclassResults49[ -c(4,5,9,10,11)]
#update transition group column name
names(SclassResults49) <- c("Iteration", "Timestep", "StratumID", "StateClassID", "Structure", "Cover", "Amount")


# *************************************************************
# Calculate Sclass %
# *************************************************************

#Create table summing total simulation cells (amount) for each bps
BpsTotalAmount<-SclassResults49 %>%
  group_by(StratumID)%>%
  summarize(BpsAmount=sum(Amount))

#Create table summing total simulation cells(amount) for each combination of bps& sclass and retain StateLabels
SclassTotalAmount<-SclassResults49 %>%
  group_by(StratumID,StateClassID,Structure,Cover)%>%
  summarize(ClassAmount=sum(Amount))

#Create a new table for the sclass percent
SclassPercent<-SclassTotalAmount
#Join in the bps total amount
SclassPercent <- merge(SclassPercent, BpsTotalAmount, by.x = "StratumID")
#Calculate Sclass percent
SclassPercent$ClassPercent<-((SclassPercent$ClassAmount/SclassPercent$BpsAmount)*100)

#ERROR CHECK: sum sclass percents for each bps to see if they equal 100
EcheckSclassPercent<-SclassPercent %>%
  group_by(StratumID) %>%
  summarize(SclassSum=sum(ClassPercent))


# *************************************************************
# Round the Sclass %
# *************************************************************
#remove unneeded proportion column ????????
TEST <- SclassPercent[ -c(3,4,5,6)]
#number sclasses 1-5 for each bps
TEST <- TEST %>% group_by(StratumID) %>% mutate(id = 1:n())

#spread
library(tidyr)
TEST2 <- TEST %>% spread(id, ClassPercent)


TEST2 %>% group_by(StratumID) %>% mutate(id = 1:n())
#library(dplyr)
df %>% group_by(personid) %>% mutate(id = row_number())
df %>% group_by(personid) %>% mutate(id = 1:n())
df %>% group_by(personid) %>% mutate(id = seq_len(n()))
df %>% group_by(personid) %>% mutate(id = seq_along(personid))



#spread the data
SclassPercent49 <- SclassPercent %>% spread(StateClassID, ClassPercent)

round the percents
make them sum to 100


# *************************************************************
# Export Results
# *************************************************************
setwd(myresults)

#Sclass percent
write.csv(SclassPercent, "SclassPercent.csv")



# tite: SUMMARIZE ST-SIM MODEL RESULTS
# author: Kori Blankenship
# date: 28 Febuary 2019
# output: 
#   1 csv summarizing frequency of disturbance by BpS 
#   1 csv summarizing average state class amount (%) by BpS.

# INSTRUCTIONS: 
# 1. Under "Setup" specify the programFolder where Syncrosim is installed (defalut directory is given). 
# 2. Set the working directory (mywd) to the location of the STSim library with results you want summarized.
# 3. Provide the name of the sim library (mylib), project (myprj), and scenario (myscn) for which you are summarizing results.
#    Specify the results scenario by number. Use scenario(<Project>) to get a list of scenarios in your project. 

# *************************************************************
# Setup
# *************************************************************
programFolder = "C:/Program Files/SyncroSim"
mywd = "C:/Users/kblankenship/Documents/aBpSreview/REVIEW/MODELS/ModelQC"
mylib = "LANDFIRE BpS Models 26 Feb 2019"
myprj = "BpS Models"
myscn = 8801

library(rsyncrosim)
library(dplyr)
Ses = session(programFolder)
setwd(mywd)

Lib = ssimLibrary(name=mylib, model="stsim", session=Ses)
Prj = project(Lib, project=myprj)
Scn = scenario(Prj, myscn)
#if needed, list scenarios w/in a project using command: scenario(Prj)

# *************************************************************
# Get Probabilistic Transition results
# *************************************************************
OutTransSheet = "STSim_OutputStratumTransition"
OutTransData = datasheet(Scn, OutTransSheet)

#Copy Transition results to a working results table
TransResults<-as.data.frame(OutTransData)

#keep results for timesteps 501-1000
TransResults<-TransResults[TransResults$Timestep>500,]

#add proportion field and calculate its value
TransResults$proportion<-(TransResults$Amount/1000)

#Calculate FRI as the inverse of the average proportion for unique StratumID and TransitionGroupID 
#mean is not used b/c I want to specify that I want the average of 10 iterations
#iterations with 0 values for transitions are not written to sim database
TransResults2<-TransResults %>%
  group_by(StratumID,TransitionGroupID)%>%
  summarize(FRI= 1/((sum(proportion))/10))

#export results and raw data to csv
write.csv(TransResults2, file=paste(mylib, "TransResults2.csv"))
write.csv(OutTransData, file=paste(mylib, "OutTransData.csv"))

# *************************************************************
# Get State Class results
# *************************************************************
OutStateSheet = "STSim_OutputStratumState"
OutStateData = datasheet(Scn, OutStateSheet)

#Copy state class results to a working results table
SclassResults<-as.data.frame(OutStateData)
#keep results for timesteps 500-1000
SclassResults<-SclassResults[SclassResults$Timestep>500,]
#Delete unused fields
SclassResults<-SclassResults[ -c(4,5,9,10,11)]

#Create table summing amount for each bps
BpsTotalAmount<-SclassResults %>%
  group_by(StratumID)%>%
  summarize(BpsAmount=sum(Amount))

#Create table summing amount for each bps/sclass
SclassTotalAmount<-SclassResults %>%
  group_by(StratumID,StateClassID)%>%
  summarize(ClassAmount=sum(Amount))

#Create a new table for the sclass percent
SclassPercent<-SclassTotalAmount
#Join in the bps total amount
SclassPercent <- merge(SclassPercent, BpsTotalAmount, by.x = "StratumID")
#Calculate Sclass percent
SclassPercent$ClassPercent<-((SclassPercent$ClassAmount/SclassPercent$BpsAmount)*100)

#export results for last 500 timesteps to csv
write.csv(SclassPercent, file=paste(mylib, "SclassPercent.csv"))


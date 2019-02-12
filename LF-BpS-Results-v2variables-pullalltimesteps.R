# tite: SUMMARIZE ST-SIM MODEL RESULTS
# author: Kori Blankenship
# date: 8 January 2019
# output: 1 csv summarizing frequency of disturbance by BpS & 1 csv summarizing average state class amount (%) by BpS.

# INSTRUCTIONS: 
# 1. Under "Setup" specify the programFolder where Syncrosim is installed (defalut directory is given). 
# 2. Set the working directory (mywd) to the location of the STSim library with results you want summarized.
# 3. Provide the name of the sim library (mylib), project (myprj), and scenario (myscn) for which you are summarizing results.
#    Specify the results scenario by number. Use scenario(<Project>) to get a list of scenarios in your project. 

# *************************************************************
# Setup
# *************************************************************
programFolder = "C:/Program Files/SyncroSim"
mywd = "C:/Users/kblankenship/Documents/aLANDFIRE/MODEL/STSM/Rsyncrosim/Dec19SimResultsCOPY"
mylib = "LANDFIRE BpS Models 19 Dec 2018"
myprj = "BpS Models"
#myscn = 8791
myscn = 8795
library(rsyncrosim)
library(dplyr)
Ses = session(programFolder)
setwd(mywd)
getwd(mywd)
Lib = ssimLibrary(name=mylib, model="stsim", session=Ses)
Prj = project(Lib, project=myprj)
Scn = scenario(Prj, myscn)
scenario(Prj)

# *************************************************************
# Get Probabilistic Transition results
# *************************************************************
OutTransSheet = "STSim_OutputStratumTransition"
OutTransData = datasheet(Scn, OutTransSheet)

#Copy Transition results to a working results table
TransResults<-as.data.frame(OutTransData)

#keep results for timesteps 500-1000
TransResults<-TransResults[TransResults$Timestep>499,]

#add proportion field and calculate its value
TransResults$proportion<-(TransResults$Amount/1000)

#Calculate FRI as (1/Average Proportion) for unique StratumID and TransitionGroupID 
TransResults<-TransResults %>%
  group_by(StratumID,TransitionGroupID)%>%
  summarize(FRI= 1/(mean(proportion)))

#export results to csv
write.csv(TransResults, file=paste(mylib, "xTransResults.csv"))


# *************************************************************
# Get State Class results
# *************************************************************
OutStateSheet = "STSim_OutputStratumState"
OutStateData = datasheet(Scn, OutStateSheet)

#Copy state class results to a working results table
ClassResults<-as.data.frame(OutStateData)

#keep results for timesteps 500-1000
ClassResults<-ClassResults[ClassResults$Timestep>499,]

#calcualte state class percent at each timestep
ClassResults$ClassProportion<-(ClassResults$Amount/1000)

#Calculate state class average percent for unique StratumID and TransitionGroupID for timesteps >500
ClassResults<-ClassResults %>%
  group_by(StratumID,StateClassID)%>%
  summarize(ClassAverage= 100*(mean(ClassProportion)))
#  summarize(ClassAverage= 1/(mean(ClassProportion)))

#export results to csv
write.csv(ClassResults, file=paste(mylib, "xClassResults.csv"))

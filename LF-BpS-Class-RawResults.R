# tite: PROVIDES ST-SIM MODEL CLASS RAW RESULTS FOR 1 BPS
# author: Kori Blankenship
# date: 28 January 2019
# output: 

# INSTRUCTIONS: 
# 1. Under "Setup" specify the programFolder where Syncrosim is installed (defalut directory is given). 
# 2. Set the working directory (mywd) to the location of the STSim library with results you want summarized.
# 3. Provide the name of the sim library (mylib), project (myprj), and scenario (myscn) for which you are summarizing results.
#    Specify the results scenario by number. Use scenario(<Project>) to get a list of scenarios in your project. 
# 4. my bps is the name of a StratumID you want raw results for

# *************************************************************
# Setup
# *************************************************************
programFolder = "C:/Program Files/SyncroSim"
mywd = "C:/Users/kblankenship/Documents/aBpSreview/REVIEW/MODELS/ModelQC"
mylib = "LANDFIRE BpS Models 25 Jan 2019"
myprj = "BpS Models"
myscn = 8798
mybps = "10660_22"

library(rsyncrosim)
library(dplyr)
Ses = session(programFolder)
setwd(mywd)

Lib = ssimLibrary(name=mylib, model="stsim", session=Ses)
Prj = project(Lib, project=myprj)
Scn = scenario(Prj, myscn)
#if needed, list scenarios w/in a project using command: scenario(Prj)


# *************************************************************
# Get State Class results
# *************************************************************
OutStateSheet = "STSim_OutputStratumState"
OutStateData = datasheet(Scn, OutStateSheet)

#Copy state class results to a working results table
ClassResults1<-as.data.frame(OutStateData)

#keep results for timesteps 500-1000
ClassResults1<-ClassResults1[ClassResults1$Timestep>500,]

#keep results for the selected BpS
ClassResultsBpS<-ClassResults1 %>%
  filter(StratumID == mybps) %>%
  select(Iteration, Timestep, StratumID, StateClassID, Amount)

#calcualte state class percent at each timestep
ClassResultsBpS$ClassProportion<-(ClassResultsBpS$Amount/1000)

#export results to csv
write.csv(ClassResultsBpS, file=paste(mybps, "ClassResultsBpS.csv"))



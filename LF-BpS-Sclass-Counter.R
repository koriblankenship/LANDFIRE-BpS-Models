# tite: Compares the number of sclasses between sim and the sclass mapping rules table
# author: Kori Blankenship
# date: 30 January 2019
# output: a csv comparing the count of sclasses from sim and the sclass mapping rules table; also lists unique sclass values from sclass mapping rules table

# INSTRUCTIONS: 
# 1. Under "Setup" specify the programFolder where Syncrosim is installed (defalut directory is given). 
# 2. Set the working directory (mywd) to the location of the STSim library with results you want summarized.;
#     Set the xclwd to the location of the xlsx sclass rules worksheet
# 3. Provide the name of the sim library (mylib), project (myprj), and scenario (myscn) for which you are summarizing results.
#    Specify the results scenario by number. Use scenario(<Project>) to get a list of scenarios in your project. 


# *************************************************************
# Setup
# *************************************************************
programFolder = "C:/Program Files/SyncroSim"
mywd = "C:/Users/kblankenship/Documents/aBpSreview/REVIEW/MODELS/ModelQC"
xclwd = "C:/Users/kblankenship/Documents/aBpSreview/SCLASS/RULES"
mylib = "LANDFIRE BpS Models 25 Jan 2019"
myprj = "BpS Models"
myscn = 8791

library(rsyncrosim)
library(dplyr)
Ses = session(programFolder)
setwd(mywd)

Lib = ssimLibrary(name=mylib, model="stsim", session=Ses)
Prj = project(Lib, project=myprj)
Scn = scenario(Prj, myscn)
#if needed, list scenarios w/in a project using command: scenario(Prj)


# *************************************************************
# Count classes in Deterministic Transitions table in .sim
# *************************************************************
DtransSheet = "STSim_DeterministicTransition"
DtransData = datasheet(Scn, DtransSheet)
#count number of classes per bps
DtransFreqTale<-as.data.frame(table(DtransData$StratumIDSource))
#change the ModelCode values to character
DtransFreqTale$ModelCode<-as.character(DtransFreqTale$Var1)


# *************************************************************
# Count classes in state class rules table
# *************************************************************
setwd(xclwd)

#import rules table
Classrules <- read.csv(file="LANDFIRESuccessionClassMappingRulesPARSER_12192018.csv", header=TRUE, sep=",",
                       colClasses="character")

#rename headers
names(Classrules) <- c("BpsCode", "ModelCode", "UpperLayerLifeform", "HeightClass", "c10", "c20", "c30", "c40", "c50", "c60", "c70", "c80", "c90", "c100", "AddRule")  

#count and list the unique classes for each BpS in the sclass rules table
#substr is used to list just the first character of the sclass name, this was done so that leafform qualifiers (con, hdw, brdlf) were not counted as unique
Classrules2<-Classrules %>%
  group_by(ModelCode) %>%
  summarise(count = length(unique(substr(union_all(c10,c20,c30,c40,c50,c60,c70,c80,c90,c100),1,1))),
            list = list(unique(substr(union_all(c10,c20,c30,c40,c50,c60,c70,c80,c90,c100),1,1))))


# *************************************************************
# Merge the tables with the sclass counts from sim and the sclass rules and compare sclass counts
# *************************************************************
test<-merge(Classrules2,DtransFreqTale,by="ModelCode")
test$diff<-test$count-test$Freq

#change list field to character and export results to csv
test$list<-as.character(test$list)
write.csv(test, file = "SclassCounter.csv")


#Classrules2$ModelCode<-as.character(Classrules2$ModelCode)

#test<-Classrules[Classrules$ModelCode=="10170_6_7_8_9_12_18",]

#export results to csv
#write.csv(ClassResultsBpS, file=paste(mybps, "ClassResultsBpS.csv"))



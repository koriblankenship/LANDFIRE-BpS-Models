# title: LANDFIRE Fire Refcon
# author: Kori Blankenship
# date: 22 June 2019
# output: A table with fire frequency and severity for every BpS model

#This script reads results from the LANDFIRE master bps model database and calculates the fire reference conditions.
#The LANDFIRE master bps model database (.sim) uses a standard set of ouput options:
#1. stateclasses every 1 timestep with "include zero values" option selected
#2. transitions ever 500 timesteps with "calculate as interval means" option selected
#This script is desigend to take these ouputs.
#To calculate fire reference conditions the first 500 timesteps, when the model equiliberalates and 
#overcomes the infulence of the initial conditions, are eleminated. Non-fire transitions are also removed.
#Results are calculated as the average of 10 iterations for the last 500 timesteps.


# *************************************************************
# Setup
# *************************************************************
library(rsyncrosim)
library(dplyr)

#Bring in FRG attribute tables
FRGold <- read_csv("C:/Users/kblankenship/Documents/aBpSreview/PUB/Rinputs/FRGold.csv")
FRGnew <- read_csv("C:/Users/kblankenship/Documents/aBpSreview/PUB/Rinputs/FRGnew.csv")

#Setup sim info
programFolder = "C:/Program Files/SyncroSim"
mywd = "C:/Users/kblankenship/Documents/aBpSreview/REVIEW/MODELS/ModelQC"
mylib = "LANDFIRE BpS Models 26 Aug 2019"
myprj = "BpS Models"
myscn = 8804

setwd(mywd)
Ses = session(programFolder)
Lib = ssimLibrary(name=mylib, model="stsim", session=Ses)
Prj = project(Lib, project=myprj)
Scn = scenario(Prj, myscn)

# *************************************************************
# Get Probabilistic Transition results
# *************************************************************
#Pull in the sim table w/ the transition results and process
OutTransSheet = "STSim_OutputStratumTransition"
TransResults = datasheet(Scn, OutTransSheet)
#keep results for timesteps 501-1000
#Transitions were output every 500 timesteps w/ calculate interval mean on so only timestep 1000 remains in the TransResults
TransResults<-TransResults[TransResults$Timestep>500,]
#write results for error checking
#write.csv(TransResults, file=paste(mylib, "TransResultsRaw.csv"))

#Subset the Fire results ####LINES 92-96#####
TransFire49 <- TransResults %>%
  filter(TransitionGroupID %in% c("All Fire", "Replacement Fire", "Mixed Fire", "Surface Fire"))                  

# *************************************************************
# Get state class data required to calculate the number of simulation cells/bps
# *************************************************************
#Pull in the sim table w/ the number of cells/bps/iteration and process it
#expect ~10 minutes to pull in OutStratStateSheet
OutStratStateSheet = "STSim_OutputStratumState"
StratStateResults = datasheet(Scn, OutStratStateSheet)
#remove first 500 timesteps (these are not used to calculate results)
StratStateResults<-StratStateResults[StratStateResults$Timestep>500,]
#Summarize the # of simulation cells (Amount) per Iteration/Timestep/BpS
StratStateResults<-StratStateResults %>%
  group_by(Iteration, Timestep, StratumID) %>%
  summarize(IterationAmount=sum(Amount)) 
#Delete unused (timestep) field
StratStateResults49<-StratStateResults[ -c(2)]
#Select unique values b/c the # of cells varies by iteration/bps (not by timestep)
#each iteration is run using a different number of cells for a BpS
StratStateResults49<-StratStateResults49 %>%
  distinct(Iteration, StratumID, .keep_all = TRUE)

####ERROR CHECK: count the number of iterations per bps, each bps should have 10 iterations
ECheckStratStateResults49<-StratStateResults49 %>%
  group_by(StratStateResults49$StratumID) %>%
  summarise(n = n())
####ERROR CHECK

# *************************************************************
# Join the fire reults w/ the state class data 
# *************************************************************
#Join TransFire49 and StratStateResults49 
#Rows are added to TransResults here from StratStateResults49 in cases where a BpS model has no transtions 
#and therefore not in TransResults. this includes: 11000_26, 11460_26, 18060_79
TransFire49 <- merge(TransFire49, StratStateResults49, all = TRUE)

# *************************************************************
# Calculate fire frequency
# *************************************************************
#CALC: the proportion of cells affected by each transition per iteration   
TransFire49$Proportion = (TransFire49$Amount/TransFire49$IterationAmount)
####ERROR CHECK: write out table for error checking
write.csv(TransFire49, "C:/Users/kblankenship/Documents/aBpSreview/PUB/Rresults/ERRORCHECKTransFire49.csv")
####ERROR CHECK

#CALC: average annual proportion of area burned by Bps 
#mean is not used b/c iterations with 0 values for transitions are not written to sim database; we want the average of 10 iterations
Fire49 <-TransFire49 %>%
  group_by(StratumID,TransitionGroupID)%>%
  summarize(AvgProportion=(sum(Proportion))/10)

#Calc: fire frequency
Fire49$FRI=(1/Fire49$AvgProportion)

####ERROR CHECK: write out table for error checking: check the FRI calcualtion and count the number of unique bps model codes
write.csv(Fire49, "C:/Users/kblankenship/Documents/aBpSreview/PUB/Rresults/ERRORCHECKFire49.csv")
####ERROR CHECK


# *************************************************************
# Format Refcon part 1: FRI
# *************************************************************
#remove unneeded proportion column 
FireFRI <- Fire49[ -c(3)]
#spread the data
FireFRI <- FireFRI %>% spread(TransitionGroupID, FRI)
#remove unneeded "NA" column 
FireFRI <- FireFRI[ -c(6)]
#Add column for the 5 digit bps codes and calculate it from StratumID
FireFRI$BpsCode = (substr(FireFRI$StratumID, 1, 5))
#Specify column order
FireFRI <- FireFRI[,c(6,1,2,5,3,4)]
#Reame variables w/o spaces
names(FireFRI) <- c("BpsCode", "StratumID", "AllFire", "SurfaceFire", "MixedFire", "ReplacementFire")

# *************************************************************
# Format Refcon part 2: Calc % Severity
# *************************************************************
#rm(FireRefcon)
#create a copy of the FireFRI table
FireRefcon <- FireFRI
#calculate percent by severity type
FireRefcon$PerLow <- ((FireRefcon$AllFire/FireRefcon$SurfaceFire)*100)
FireRefcon$PerMix <- ((FireRefcon$AllFire/FireRefcon$MixedFire)*100)
FireRefcon$PerRep <- ((FireRefcon$AllFire/FireRefcon$ReplacementFire)*100)

#if a severity type is not used, it gets a value of 0
FireRefcon <- FireRefcon %>%
  mutate(PerLow = ifelse(is.na(PerLow),0,PerLow)) %>%
  mutate(PerMix = ifelse(is.na(PerMix),0,PerMix)) %>%
  mutate(PerRep = ifelse(is.na(PerRep),0,PerRep)) 

#ERROR CHECK: severity should sum to 100 if fire is used or 0 if no fire is used in the model
#Remove all columns except % severity
ECheckPercentSeverity <- FireRefcon[,-c(1,2,3,4,5,6)]
#sum % severity
ECheckPercentSeverity$SumSeverity <- rowSums(ECheckPercentSeverity)

# *************************************************************
# Format Refcon part 3: Round Values 
# *************************************************************
#Rounding to nearest whole number
FireRefcon2<-FireRefcon 

FireRefcon2$OverallMFRI <- round(FireRefcon2$AllFire)
FireRefcon2$ReferenceLowSeverityAverageFireFrequency <- round(FireRefcon2$SurfaceFire)
FireRefcon2$ReferenceMixedSeverityAverageFireFrequency <- round(FireRefcon2$MixedFire)
FireRefcon2$ReferenceReplacementSeverityAverageFireFrequency <- round(FireRefcon2$ReplacementFire)
FireRefcon2$PercentLowSeverityFires <- round(FireRefcon2$PerLow)
FireRefcon2$PercentMixedSeverityFires <- round(FireRefcon2$PerMix)
FireRefcon2$PercentReplacementSeverityFires <- round(FireRefcon2$PerRep)

#ERROR CHECK: export FireRefcon2 and check calculation of percent severity and rounding of values
write.csv(FireRefcon2, "C:/Users/kblankenship/Documents/aBpSreview/PUB/Rresults/ERRORCHECKFireRefcon2.csv") 
#ERROR CHECK

#remove unneeded columns 
FireRefcon2 <- FireRefcon2[ -c(3,4,5,6,7,8,9)]

# *************************************************************
# Format Refcon part 4: make fire severity percents sum to 100
#Once the percents are rounded they may not sum to 100%
#This step ensure that the percent Low+Mixed+High equal 100
# *************************************************************

#Sum the percent low, mixed and replacement severity
FireRefcon2$SumPercent<-(FireRefcon2$PercentLowSeverityFires+FireRefcon2$PercentMixedSeverityFires+
                           FireRefcon2$PercentReplacementSeverityFires)

#Find the min fire severity value that does NOT = zero 
#have to remove 0s b/c they indicate no fire and should not be rounded to make all fire severities sum to 100%
#SevMin = "Inf" for models w/ no fire
FireRefcon2 <- FireRefcon2 %>%
  rowwise() %>% mutate(SevMin = min(PercentLowSeverityFires[PercentLowSeverityFires>0], 
                                    PercentMixedSeverityFires[PercentMixedSeverityFires>0], 
                                    PercentReplacementSeverityFires[PercentReplacementSeverityFires>0]))

#Find the max fire severity value
FireRefcon2 <- FireRefcon2 %>%
  rowwise() %>% mutate(SevMax = max(PercentLowSeverityFires[PercentLowSeverityFires>0], 
                                    PercentMixedSeverityFires[PercentMixedSeverityFires>0], 
                                    PercentReplacementSeverityFires[PercentReplacementSeverityFires>0]))

#Round the low severity percent if needed
FireRefcon3 <- FireRefcon2 %>% 
  mutate(PercentLow = ifelse((SumPercent<100 & SevMin==PercentLowSeverityFires), PercentLowSeverityFires+1,
                             ifelse((SumPercent>100 & SevMax==PercentLowSeverityFires), PercentLowSeverityFires-1, PercentLowSeverityFires)))
#Redo the sum percent adding the PercentLow computed above with the unaltered mixed and replacment values
FireRefcon3$SumPercent<-(FireRefcon3$PercentLow+FireRefcon3$PercentMixedSeverityFires+
                           FireRefcon3$PercentReplacementSeverityFires)

#Round the mixed severity percent if needed
FireRefcon3 <- FireRefcon3 %>% 
  mutate(PercentMixed = ifelse((SumPercent < 100 & SevMin == PercentMixedSeverityFires), PercentMixedSeverityFires+1,
                               ifelse((SumPercent > 100 & SevMax == PercentMixedSeverityFires), PercentMixedSeverityFires-1, PercentMixedSeverityFires)))
#Redo the sum percent adding the PercentLow & PercentMixed computed above with the unaltered replacment values
FireRefcon3$SumPercent<-(FireRefcon3$PercentLow+FireRefcon3$PercentMixed+FireRefcon3$PercentReplacementSeverityFires)

#Round the replacement severity percent if needed
FireRefcon3 <- FireRefcon3 %>% 
  mutate(PercentReplacement = ifelse((SumPercent < 100 & SevMin == PercentReplacementSeverityFires), PercentReplacementSeverityFires+1,
                                     ifelse((SumPercent > 100 & SevMax == PercentReplacementSeverityFires), PercentReplacementSeverityFires-1, PercentReplacementSeverityFires)))

#ERROR CHECK
#Write FireRefcon3 and check that the fire severity percents were adjusted corretly to make all severities sum to 100%
write.csv(FireRefcon3, "C:/Users/kblankenship/Documents/aBpSreview/PUB/Rresults/ERRORCHECKFireRefcon3.csv") 
#Remove all columns except % severity and sum % severity
#severity should sum to 100 if fire is used or 0 if no fire is used in the model
ECheckPercentSeverity2 <- FireRefcon3[,-c(1,2,3,4,5,6,7,8,9,10,11,12)]
ECheckPercentSeverity2$SumSeverity <- rowSums(ECheckPercentSeverity2)

# *************************************************************
# Format Refcon Part 5: Apply FRG Cassification
# *************************************************************
#remove columns w/ unadjusted fire severity data and other unneeded columns
FireRefconFin <- FireRefcon3[ -c(7,8,9,10,11,12)]

#Add OLD FRG classification
#FRG1: 0-35yr, <2/3 replacement
#FRG2: 0-35yr, >=2/3 replacement
#FRG3: 36-100yr, <80% replacement or 101-200, <2/3
#FRG4: 36-100yr, >=80% replacement or 101-200, >=2/3
#FRG5: >=201yr, any severity
FireRefconFin <- FireRefconFin %>% 
  mutate(FRGold = ifelse((OverallMFRI < 36 & PercentReplacement < 66.666667), 1,
                         ifelse((OverallMFRI <36 & PercentReplacement >= 66.666667), 2,
                                ifelse((OverallMFRI >= 36 & OverallMFRI < 101 & PercentReplacement < 80), 3,
                                       ifelse((OverallMFRI >= 101 & OverallMFRI < 201 & PercentReplacement < 66.666667), 3,
                                              ifelse((OverallMFRI >= 36 & OverallMFRI < 101 & PercentReplacement >= 80), 4,
                                                     ifelse((OverallMFRI >= 101 & OverallMFRI < 201 & PercentReplacement >=66.666667), 4, 5)))))))

#Add NEW FRG classification
#I-A, FRGold = 1 & FRI = 0-5yrs ~ I-B, FRGold = 1 & FRI = 6-15yrs ~ #I-C, FRGold = 1 & FRI = 16-35yrs
#II-A, FRGold = 2 & FRI = 0-5yrs ~ #II-B, FRGold = 2 & FRI = 6-15yrs ~ #II-C, FRGold = 2 & FRI = 16-35yrs
#III-A, FRGold = 3 & FRI = 36-100yrs ~ #III-B, FRGold = 3 & FRI = 101-200yrs
#IV-A, FRGold = 4 & FRI = 36-100yrs ~ #IV-B, FRGold = 4 & FRI = 101-200yrs
#V-A, FRGold = 5 & FRI = 201-500yrs ~ #V-B, FRGold = 5 & FRI = 501+yrs
FireRefconFin <- FireRefconFin %>% 
  mutate(FRGnew = ifelse((FRGold == 1 & OverallMFRI < 6), "I-A",
                         ifelse((FRGold == 1 & OverallMFRI >= 6 & OverallMFRI < 16), "I-B",
                                ifelse((FRGold == 1 & OverallMFRI >=16 & OverallMFRI < 36), "I-C",
                                       ifelse((FRGold == 2 & OverallMFRI < 6), "II-A",
                                              ifelse((FRGold == 2 & OverallMFRI >= 6 & OverallMFRI < 16), "II-B",
                                                     ifelse((FRGold == 2 & OverallMFRI >=16 & OverallMFRI < 36), "II-C",
                                                            ifelse((FRGold == 3 & OverallMFRI >= 36 & OverallMFRI < 101), "III-A",
                                                                   ifelse((FRGold == 3 & OverallMFRI >= 101 & OverallMFRI < 201), "III-B",
                                                                          ifelse((FRGold == 4 & OverallMFRI >= 36 & OverallMFRI < 101), "IV-A",
                                                                                 ifelse((FRGold == 4 & OverallMFRI >= 101 & OverallMFRI < 201), "IV-B",
                                                                                        ifelse((FRGold == 5 & OverallMFRI >= 201 & OverallMFRI < 501), "V-A", "V-B"))))))))))))
#if the frg-old is NA then frg-new is "V-B"
FireRefconFin$FRGnew[is.na(FireRefconFin$FRGnew)] <- "V-B" 


#Join FRG attributes
#old
FireRefconFin <- merge(FireRefconFin, FRGold, all = TRUE)
#new
FireRefconFin <- merge(FireRefconFin, FRGnew, all = TRUE)

#Reorder Columns and Finalize Names
FireRefconFin <- FireRefconFin[,c(3,4,5,6,7,8,9,10,11,2,12,13,1,14)]

# *************************************************************
# Write results to csv
# *************************************************************
write.csv(FireRefconFin, "C:/Users/kblankenship/Documents/aBpSreview/PUB/Rresults/FireRefconFin.csv")   




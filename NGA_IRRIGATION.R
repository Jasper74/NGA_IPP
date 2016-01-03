#######################################
############# IRRIGATION ##############
#######################################

#Conveyance method: 1 divert stream   2 bucket   3 handpump   4 treadlepump   5 motorpump   6 gravity   7 shadouf   8 sprinkler
#costs per hectare  $640 [1]                                  $259 [1]        $263 [1]      

# [1] Xie et al. (2014); costs given in dollars and therefore do not need to be converted using exchange rates 
#

#==========================================================================================
#THE FOLLOWING TWO LINES CAN BE EDITED TO INDICATE IRRIGATION COSTS OF CONVEYANCE METHODS
#==========================================================================================
irrigation_costs_1 <- data.frame(convey_method=c(1:8), irrigationcost=c(640, 0, 0, 259, 263, 0, 0, 0))
irrigation_costs_2 <- data.frame(convey_method2=c(1:8), irrigationcost2=c(640, 0, 0, 259, 263, 0, 0, 0))

dataPath <- "Nigeria/DATA" 
# dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA"
# Set the working directory to the appropriate folder for Niger
wdPath <- "Desktop/LEI - Irrigation Potential Project/"
setwd(wdPath)

library(haven)
library(stringr)
library(plyr)
library(dplyr)
library(foreign)

# Load data
nga_ir <- read_dta(file.path(dataPath, "Post Planting Wave 2/Agriculture/sect11b1_plantingw2.dta")) %>%
  dplyr::select(hhid, ea, plotid, zone, state, irrigated=s11b1q39, water_source=s11b1q40, convey_method=s11b1q41a, 
                convey_method2=s11b1q41b)
nga_ir <- data.frame(lapply(nga_ir, unclass))

#Remove observations that have NA for irrigation use 
nga_ir <- nga_ir[!is.na(nga_ir$irrigated),]
#If no irrigation use, use of irrigation sources and methods is 0
nga_ir[nga_ir$irrigated==2,c(5:9)] <- 0
#Removing observations that have 'other' filled in
nga_ir <- nga_ir[nga_ir$water_source!=6,]
nga_ir <- nga_ir[!nga_ir$convey_method==9,]
nga_ir <- dplyr::filter(nga_ir, irrigated==0 | irrigated==1)

#Merge with irrigation costs for irriation conveyance method
nga_ir <- left_join(nga_ir, irrigation_costs_1)
nga_ir <- left_join(nga_ir, irrigation_costs_2)
nga_ir[is.na(nga_ir)] <- 0

nga_ir$irrigation_cost <- nga_ir$irrigationcost + nga_ir$irrigationcost2
nga_ir <- dplyr::select(nga_ir, hhid, plotid, water_source, convey_method, convey_method2, irrigation_cost)

write.csv(nga_ir, "Nigeria/Output/irrigation_cost.csv")

#loading in EA for GPS 
nga_hhvars <- read_dta(file.path(dataPath, "Geodata Wave 2/NGA_HouseholdGeovars_Y2.dta")) %>%
  dplyr::select(hhid, ea, lat=LAT_DD_MOD, lon=LON_DD_MOD)
ar <- read.csv(paste(dataPath,'/nga_area.csv',sep='')) %>% dplyr::select(hhid, plotid, area)
nga_ir <- left_join(nga_ir, nga_hhvars)








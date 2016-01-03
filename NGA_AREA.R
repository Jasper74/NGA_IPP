#NGA AREAS
#Set the path for storage of output 
dataPath <- "Nigeria/DATA"
# dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA"
# Set the working directory to the appropriate folder for Niger
wdPath <- "Desktop/LEI - Irrigation Potential Project/"
setwd(wdPath)

# WDswitch
ar <- read_dta(file.path(dataPath, "Post Planting Wave 2/Agriculture/sect11a1_plantingw2.dta")) %>%
  dplyr::select(hhid, plotid, est=s11aq4a, est_unit=s11aq4b, area=s11aq4c) #gps
ar <- data.frame(lapply(ar, unclass))
ar[,c(3,5)] <- ar[,c(3,5)]/10000

#WINSORIZING
lim <- quantile(ar[,'area'], probs = 0.98, na.rm = TRUE)
ar[, 'area'][ar[, 'area'] > lim] <- lim 

#tot_area <- summarise(group_by(ar, hhid),
#                      tot_area = sum(area, na.rm=T))
#ar <- left_join(ar, tot_area)
#ar$area_proportion <- ar$area / ar$tot_area

ar <- dplyr::select(ar, hhid, plotid, area)

write.csv(ar, file=paste(dataPath,"/nga_area.csv", sep=""))

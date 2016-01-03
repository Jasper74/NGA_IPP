library(haven)
library(stringr)
library(plyr)
library(dplyr)
library(foreign)
library(reshape2)

dataPath <- "Nigeria/DATA"
wdPath <- "Desktop/LEI - Irrigation Potential Project/"
setwd(wdPath)

fr <- read_dta(file.path(dataPath, "Post Planting Wave 2/Agriculture/sect11d_plantingw2.dta")) %>%
  dplyr::select(hhid, plotid, ea, zone, fertilizer_use=s11dq1,
                lofert_use = s11dq2, lofert_type=s11dq3, lofert_kg=s11dq4, 
                freefert_use=s11dq6, freefert_type=s11dq7, freefert_kg=s11dq8, freefert_cost=s11dq10,
                comfert_use=s11dq12, comfert_type=s11dq15, comfert_kg=s11dq16, comfert_cost=s11dq17, comfert_val=s11dq19,
                comfert_use2=s11dq24, comfert_type2=s11dq27, comfert_kg2=s11dq28, comfert_cost2=s11dq31, comfert_val2=s11dq29)

#Unclass the data frame (to get read of weird variable types that mess with functions)
fr <- data.frame(lapply(fr, unclass))
#Remove NA values for overall fertilizer use dummy 
fr <- fr[!is.na(fr$fertilizer_use),]
#If no overall fertilizer use, then the dummies for specific sources of fert use are 0 oo
fr[fr$fertilizer_use==2, c("lofert_use","freefert_use","comfert_use","comfert_use2")] <- 2
fr <- fr[!is.na(fr$lofert_use) & !is.na(fr$freefert_use) & !is.na(fr$comfert_use) & !is.na(fr$comfert_use2),]

fr[fr$lofert_use==2, c("lofert_type","lofert_kg")] <- 0
fr[fr$freefert_use==2, c("freefert_type","freefert_kg", "freefert_cost")] <- 0
fr[fr$comfert_use==2, c("comfert_type","comfert_kg","comfert_val","comfert_cost")] <- 0
fr[fr$comfert_use2==2, c("comfert_type2","comfert_kg2","comfert_val2","comfert_cost2")] <- 0

#Total fert costs
fr <- within(fr, fert_price <- comfert_val / comfert_kg)
fr <- within(fr, fert_price2 <- comfert_val2 / comfert_kg2)

#1: NPK   2: UREA   3: COMPOSITE MIXTURE    4: OTHER
#ORGANIC MANURE (3) IS NOT AN INORGANIC FERTILIZER AND SHEAHAN ET AL (2014) DOES NOT INDICATE A NPK RATING
fr$lofert_urea <- ifelse((fr$lofert_type==2), fr$lofert_kg, 0)
fr$freefert_urea <- ifelse((fr$freefert_type==2), fr$freefert_kg, 0)
fr$comfert_urea <- ifelse((fr$comfert_type==2), fr$comfert_kg, 0)
fr$comfert_urea2 <- ifelse((fr$comfert_type2==2), fr$comfert_kg2, 0)
fr$urea <- fr$lofert_urea + fr$freefert_urea + fr$comfert_urea + fr$comfert_urea2 

fr$lofert_npk <- ifelse((fr$lofert_type==1), fr$lofert_kg, 0)
fr$freefert_npk <- ifelse((fr$freefert_type==1), fr$freefert_kg, 0)
fr$comfert_npk <- ifelse((fr$comfert_type==1), fr$comfert_kg, 0)
fr$comfert_npk2 <- ifelse((fr$comfert_type2==1), fr$comfert_kg2, 0)
fr$npk <- fr$lofert_npk + fr$freefert_npk + fr$comfert_npk + fr$comfert_npk2

#Convert to NPK using the conversions in Sheahan et al (2014)
#NPK USE PER PLOTID
fr$N <- 0.46*fr$urea + 0.27*fr$npk
fr$P <- 0.13*fr$npk
fr$K <- 0.13*fr$npk


f1<- dplyr::select(fr, hhid, plotid, ea, zone, comfert_type, comfert_kg, comfert_cost) %>%
  dplyr::filter(comfert_type %in% c(1,2))
f2<- dplyr::select(fr, hhid, plotid, ea, zone, comfert_type=comfert_type2, comfert_kg=comfert_kg2, comfert_cost=comfert_cost2) %>%
  dplyr::filter(comfert_type %in% c(1,2))

f <- rbind(f1, f2)
f$N <- ifelse((f$comfert_type==1), 0.46*f$comfert_kg, 0)
f$N <- ifelse((f$comfert_type==2), f$N+0.27*f$comfert_kg, f$N)
f$P <- ifelse((f$comfert_type==2), 0.13*f$comfert_kg, 0)
f$K <- ifelse((f$comfert_type==2), 0.13*f$comfert_kg, 0)

#SPLIT COSTS ACCORDING TO NPK PROPORTIONS
f$N_cost <- ifelse((f$comfert_type==1), 0.630137*f$comfert_cost, 0)
f$N_cost <- ifelse((f$comfert_type==2), f$N_cost+0.369863*f$comfert_cost, f$N_cost)
f$P_cost <- ifelse((f$comfert_type==2), f$comfert_cost, 0)
f$K_cost <- ifelse((f$comfert_type==2), f$comfert_cost, 0)

#CORRECTIONS
f <- dplyr::filter(f, zone %in% 1 & N<600 | zone %in% 3 & N<1000 |
                     zone %in% 4 & N<250  | zone %in% 6 & N<40   |
                     !zone %in% c(1,3,4,6) & N>=0 | is.na(N))

#MAKE TABLE BY EA OR REGION
t_nga_pfert_zone <- summarise(group_by(f, zone),
                            n = length(hhid),
                            p_N = sum(N_cost, na.rm=T) / sum(N),
                            p_P = sum(P_cost, na.rm=T) / sum(P),
                            p_K = sum(K_cost, na.rm=T) / sum(K))
                            
t_nga_pfert_zone <- summarise(group_by(f, zone),
                              n = length(hhid),
                              p_N = sum(N_cost, na.rm=T) / sum(N),
                              p_P = sum(P_cost, na.rm=T) / sum(P),
                              p_K = sum(K_cost, na.rm=T) / sum(K))

write.csv(t_nga_pfert_zone, "Nigeria/Output/NGA_fertprices_zone")

  
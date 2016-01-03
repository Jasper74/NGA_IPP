#######################################
########## NIGERIA 2012-13 ###########
#######################################

#Permalink for data files: http://go.worldbank.org/1O3UK5WO00

#Set the path for storage of output 
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

options(scipen=999)

#######################################
############### OUTPUT ################
#######################################

oput <- read_dta(file.path(dataPath, "Post Harvest Wave 2/Agriculture/secta3_harvestw2.dta")) %>%
  dplyr::select(hhid, state, lga, plotid, ea, zone, cropname, cropcode, cropid, 
         harvested=sa3q3,     harv_area_m2=sa3q5a,  area_unit=sa3q5b,   qty=sa3q6a1,    harv_unit=sa3q6a2,
         harv_sold=sa3q9,     sold_qty=sa3q11a,  sold_unit=sa3q11b,  sold_value=sa3q12,
         harv_sold2=sa3q14,   sold_qty2=sa3q16a, sold_unit2=sa3q16b, sold_value2=sa3q17)
oput <- data.frame(lapply(oput, unclass))

# loading conversion data and joining with oput to obtain accurate estimates of harvest weights
oput <- left_join(oput, read_dta("Nigeria/DATA/w2agnsconversion.dta") %>%  
                    dplyr::select(cropcode, harv_unit=nscode, conversion)) %>% 
                                  mutate(conversion = ifelse(is.na(conversion), harv_unit, conversion))

oput <- left_join(oput, conv_sold <- read_dta("Nigeria/DATA/w2agnsconversion.dta") %>%  
                    dplyr::select(cropcode, sold_unit=nscode, conversion_sold=conversion)) %>% 
                                  mutate(conversion_sold = ifelse(is.na(conversion_sold), sold_unit, conversion_sold))


oput <- left_join(oput, conv_sold2 <- read_dta("Nigeria/DATA/w2agnsconversion.dta") %>%  
                    dplyr::select(cropcode, sold_unit2=nscode, conversion_sold2=conversion)) %>% 
                                  mutate(conversion_sold2 = ifelse(is.na(conversion_sold2), sold_unit2, conversion_sold2))


#Harvested area conversion
areaconv <- read.csv("Nigeria/DATA/areaconv.csv") %>% 
  dplyr::select(zone, area_unit, area_conv=conv)
oput <- left_join(oput, areaconv)
oput$harv_area_ha <- oput$harv_area_m2 * oput$area_conv

oput$conversion[oput$conversion==2] <- 0.001
oput$conversion_sold[oput$conversion_sold==2] <- 0.001
oput$conversion_sold2[oput$conversion_sold2==2] <- 0.001

oput <- mutate(oput, 
harvest_kg = qty * conversion,
sold_volume_kg = sold_qty * conversion_sold,
sold_volume_kg2 = sold_qty2 * conversion_sold2
#crop_price1 = sold_valu / qty_sold,
#crop_price2 = sold_valu2 / qty_sold2
)

#oput <- oput[!oput$harvested==2,]
#oput <- oput[!is.na(oput$harvest) & oput$harvest>=0,]

#oput[oput$harv_sold %in% 2, c("sold_qty","sold_unit","sold_valu","harv_sold2")] <- 0
#oput[oput$harv_sold2 %in% c(0,2), c("sold_qty2","sold_unit2","sold_valu2")] <- 0

oput <- dplyr::select(oput, hhid, state, lga, ea, zone, plotid, cropid, cropname, cropcode, harvested, harv_area_ha, harvest_kg, sold_volume_kg, 
                      sold_value, sold_volume_kg2, sold_value2)

#######################################
############### IRRIGATION ############
#######################################
ir <- read.csv("Nigeria/Output/irrigation_cost.csv") %>%
  dplyr::select(hhid, plotid, water_source, convey_method, convey_method2)

#######################################
############# INPUT COSTS #############
#######################################

ic <- read_dta(file.path(dataPath, "Post Planting Wave 2/Agriculture/sect11c2_plantingw2.dta")) %>%
  dplyr::select(hhid, plotid,
  pest_use=s11c2q1, pest_cost=s11c2q4a, pest_cost2=s11c2q4b, pest_ik=s11c2q5a, pest_ik2=s11c2q5b,
  herb_use=s11c2q10, herb_cost=s11c2q13a, herb_cost2=s11c2q13b, herb_ik=s11c2q14a, herb_ik2=s11c2q14b,
  anim_use=s11c2q19, anim_rentdays=s11c2q21, anim_cost=s11c2q23a, anim_cost_unit=s11c2q23b,
  anim_ik=s11c2q24a, anim_ik_unit=s11c2q24b, anim_feed=s11c2q25,
  mach_use=s11c2q27, mach_cost=s11c2q29, mach_cost=s11c2q32, mach_ik=s11c2q32)

ic <- data.frame(lapply(ic, unclass))
ic <- ic[!is.na(ic$pest_use) & !is.na(ic$herb_use) & !is.na(ic$anim_use) & !is.na(ic$mach_use),]

ic[ic$pest_use==2, c("pest_use","pest_cost", "pest_cost2", "pest_ik", "pest_ik2")] <- 0
ic[ic$herb_use==2, c("herb_use","herb_cost","herb_cost2","herb_ik","herb_ik2")] <- 0
ic[ic$anim_use==2, c("anim_use","anim_rentdays", "anim_cost","anim_cost_unit","anim_ik","anim_ik_unit","anim_feed")] <- 0
ic[ic$mach_use==2, c("mach_use","mach_cost","mach_ik")] <- 0

#ic$anim_days[is.na(ic$anim_days)] <- 0
#Filter out observations with pesticide use but NA values
ic <- ic[!is.na(ic$pest_cost),]
#Similarly for herbicide, animal and machine use
ic <- ic[!is.na(ic$herb_cost),]
ic <- ic[!is.na(ic$anim_cost),]
ic <- ic[!is.na(ic$mach_cost),]
ic[is.na(ic)] <- 0

#Animal costs (try to make something of the weird units)
#1: hours 2: days 3: month 4: acre 5: hectare 6: planting period 7:other  HOURS ARE CONSIDERED DAYS (LOOK AT PLOTS)
ic <- left_join(ic, anim_conv <- data.frame(anim_cost_unit = c(0:7), conv=c(0,1,1,NA,1,1,1,NA)))
ic <- left_join(ic, anim_conv_ik <- data.frame(anim_ik_unit = c(0:7), conv_ik=c(0,1,1,NA,1,1,1,NA)))

#ADD AREAS 
ar <- read.csv(paste(dataPath,'/nga_area.csv',sep='')) %>% dplyr::select(hhid, plotid, area)
### ANIMAL RENT COSTS ###
ic_a <- dplyr::filter(ic, anim_cost_unit %in% c(1,2,3,4,6)) %>% dplyr::select(hhid, plotid, anim_rentdays, anim_cost, anim_cost_unit, conv) 
ic_a <- left_join(ic_a, ar)

ic_a$daily_cost <- ic_a$anim_cost * ic_a$conv
#Set days to 1 for non-time units (acre, hectare and 'planting period')
ic_a$anim_rentdays[ic_a$anim_cost_unit %in% c(4,5,6)] <- 1 
ic_a$animal_rent <- ic_a$anim_rentdays * ic_a$daily_cost 

#Convert acres to hectares for anim_cost_unit==4
ic_a$anim_cost[ic_a$anim_cost_unit==4] <- ic_a$anim_cost[ic_a$anim_cost_unit==4] * 2.47105381
ic_a$animal_rent[ic_a$anim_cost_unit %in% c(3,4)] <- ic_a$anim_cost[ic_a$anim_cost_unit %in% c(3,4)] * ic_a$area[ic_a$anim_cost_unit %in% c(3,4)]

#### IN KIND ####
ic_b <- dplyr::filter(ic, anim_ik_unit %in% c(1,2,3,4,6)) %>% dplyr::select(hhid, plotid, anim_rentdays, anim_ik, anim_ik_unit, conv_ik) 
ic_b <- left_join(ic_b, ar)

ic_b$daily_cost <- ic_b$anim_ik * ic_b$conv_ik
#Set days to 1 for non-time units (acre, hectare and 'planting period')
ic_b$anim_rentdays[ic_b$anim_ik_unit %in% c(3,4,6)] <- 1 
ic_b$animal_ik <- ic_b$anim_rentdays * ic_b$daily_cost 

#Convert hectares to acres for anim_cost_unit==4
ic_b$anim_ik[ic_b$anim_ik_unit==4] <- ic_b$anim_ik[ic_b$anim_ik_unit==4] * 2.47105381
ic_b$animal_ik[ic_b$anim_ik_unit %in% c(3,4)] <- ic_b$anim_ik[ic_b$anim_ik_unit %in% c(3,4)] * ic_b$area[ic_b$anim_ik_unit %in% c(3,4)]

ic_a <- dplyr::select(ic_a, hhid, plotid, animal_rent)
ic_b <- dplyr::select(ic_b, hhid, plotid, animal_ik)

ic <- left_join(ic, ic_a) 
ic <- left_join(ic, ic_b)
ic[is.na(ic)] <- 0

ic <- within(ic, animal_cost <- animal_rent + animal_ik + anim_feed)
rm(anim_conv, anim_conv_ik, ic_a, ic_b)

#Total input costs
ic <- within(ic, input_cost <- pest_cost+pest_cost2+pest_ik+pest_ik2+herb_cost+herb_cost2+herb_ik+herb_ik2+animal_cost + mach_cost+mach_ik)

ic <- dplyr::select(ic, hhid, plotid, plot_input_cost=input_cost)

#######################################
############### FERTILIZER ############
#######################################\

fr <- read_dta(file.path(dataPath, "Post Planting Wave 2/Agriculture/sect11d_plantingw2.dta")) %>%
  dplyr::select(hhid, plotid, fertilizer_use=s11dq1,
                lofert_use = s11dq2, lofert_type=s11dq3, lofert_kg=s11dq4, 
                freefert_use=s11dq6, freefert_type=s11dq7, freefert_kg=s11dq8, freefert_cost=s11dq10,
                comfert_use=s11dq12, comfert_type=s11dq15, comfert_kg=s11dq16, comfert_cost=s11dq17, comfert_val=s11dq19,
                comfert_use2=s11dq24, comfert_type2=s11dq27, comfert_kg2=s11dq28, comfert_cost2=s11dq31, comfert_val2=s11dq29
  )
#Remove rows with only NAs 
fr <- data.frame(lapply(fr, unclass))
fr <- fr[!is.na(fr$fertilizer_use),]
fr[fr$fertilizer_use==2, c("lofert_use","freefert_use","comfert_use","comfert_use2")] <- 2
fr <- fr[!is.na(fr$lofert_use) & !is.na(fr$freefert_use) & !is.na(fr$comfert_use) & !is.na(fr$comfert_use2),]

fr[fr$lofert_use==2, c("lofert_type","lofert_kg")] <- 0
fr[fr$freefert_use==2, c("freefert_type","freefert_kg", "freefert_cost")] <- 0
fr[fr$comfert_use==2, c("comfert_type","comfert_kg","comfert_val","comfert_cost")] <- 0
fr[fr$comfert_use2==2, c("comfert_type2","comfert_kg2","comfert_val2","comfert_cost2")] <- 0

#UREA AND NPK use(kg)
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

#UREA AND NPK COST/VALUE
fr$comfert_urea_val  <- ifelse((fr$comfert_type==2), fr$comfert_val, 0)
fr$comfert_urea2_val  <- ifelse((fr$comfert_type2==2), fr$comfert_val2, 0)
fr$urea_cost  <- fr$comfert_urea_val + fr$comfert_urea2_val

fr$comfert_npk_val  <- ifelse((fr$comfert_type==1), fr$comfert_val, 0)
fr$comfert_npk2_val  <- ifelse((fr$comfert_type2==1), fr$comfert_val2, 0)
fr$npk_cost  <- fr$comfert_npk_val + fr$comfert_npk2_val

#Convert to NPK using the conversions in Sheahan et al (2014)
#NPK USE PER PLOTID
fr$N <- 0.46*fr$urea + 0.27*fr$npk
fr$P <- 0.13*fr$npk
fr$K <- 0.13*fr$npk

fr$N_cost <- fr$urea_cost + fr$npk_cost * 0.509434
fr$P_cost <- fr$npk_cost * 0.245283
fr$K_cost <- fr$npk_cost * 0.245283

#Total fert costs
fr <- within(fr, fert_cost <- freefert_cost + comfert_cost + comfert_cost2)
fr <- dplyr::select(fr, hhid, plotid, plot_urea_kg=urea, plot_npk_kg=npk, plot_N_kg=N, plot_P_kg=P, plot_K_kg=K,
                    plot_urea_cost=urea_cost, plot_npk_cost=npk_cost, plot_N_cost=N_cost, plot_P_cost=P_cost, 
                    plot_K_cost=K_cost, plot_other_fert_cost=fert_cost)

#######################################
############### SEED ##################
#######################################

seed <- read_dta(file.path(dataPath, "Post Planting Wave 2/Agriculture/sect11e_plantingw2.dta")) %>%
  dplyr::select(hhid, plotid, cropname, cropid, allseed_use=s11eq3, seedtype=s11eq5, 
                freeseed_use=s11eq8, freeseed_cost=s11eq12,
                seed_use=s11eq14, seed_type=s11eq17, seed_transport=s11eq19, seed_cost=s11eq21, 
                seed_use2=s11eq26, seed_type2=s11eq29, seed_transport2=s11eq31, seed_cost2=s11eq33
                )
seed <- data.frame(lapply(seed, unclass))
seed <- seed[!is.na(seed$allseed_use),]

#If overall 
seed[seed$allseed_use==2, c("freeseed_use","seed_use","seed_use2")] <- 2
seed <- seed[!is.na(seed$freeseed_use),] 
seed[seed$freeseed_use==2, "freeseed_cost"] <- 0 
seed <- seed[!is.na(seed$seed_use),] 
#A lot of obs are dropped if NA's for seed_use2 are removed. If the first commercial seed is filled it; give seed_use2 a "no use"
seed[seed$seed_use==2, c("seed_use2")] <- 2 

seed[seed$seed_use==2, c("seed_type","seed_transport","seed_cost")] <- 0
seed <- seed[!is.na(seed$seed_use2),]
seed[seed$seed_use2==2, c("seed_type2","seed_transport2","seed_cost2")] <- 0
seed[is.na(seed)] <- 0

seed <- within(seed, seed_totalcost <- freeseed_cost + seed_transport + seed_cost + seed_transport2 + seed_cost2)
seed <- dplyr::select(seed, hhid, plotid, cropid, cropname, crop_seed_cost=seed_totalcost)

#######################################
############### labor ################
#######################################

# WDswitch
lab <- read_dta(file.path(dataPath, "Post Planting Wave 2/Agriculture/sect11c1_plantingw2.dta")) %>%
  dplyr::select( hhid, plotid, 
                 fam1_weeks=s11c1q1a2, fam1_daysweek=s11c1q1a3, fam1_hours=s11c1q1a4,
                 fam2_weeks=s11c1q1b2, fam2_daysweek=s11c1q1b3, fam2_hours=s11c1q1b4,
                 fam3_weeks=s11c1q1c2, fam3_daysweek=s11c1q1c3, fam3_hours=s11c1q1c4,
                 fam4_weeks=s11c1q1d2, fam4_daysweek=s11c1q1d3, fam4_hours=s11c1q1d4,
                 n_men=s11c1q2, men_days=s11c1q3, men_avgwage=s11c1q4,
                 n_women=s11c1q5, women_days=s11c1q6, women_avgwage=s11c1q7,
                 n_child=s11c1q8, child_days=s11c1q9, child_avgwage=s11c1q10)

#Remove rows with all NAs
lab <- lab[rowSums(is.na(lab))!=21, ]
lab[is.na(lab)] <- 0

lab <- within(lab, fam_labdays <- (fam1_weeks*fam1_daysweek) +
                (fam2_weeks*fam2_daysweek) +
                (fam3_weeks*fam3_daysweek) + 
                (fam4_weeks*fam4_daysweek))

lab <- within(lab, lab_hire_days <- (n_men*men_days) + (n_women*women_days) + (n_child*child_days))
lab <- within(lab, lab_hire_cost <- (n_men*men_days*men_avgwage) + (n_women*women_days*women_avgwage) + (n_child*child_days*child_avgwage))

#######################################
############### labor HARVEST ########
#######################################

# WDswitch
labh <- read_dta(file.path(dataPath, "Post Harvest Wave 2/Agriculture/secta2_harvestw2.dta")) %>%
  dplyr::select( hhid, plotid, 
                 fam1_weeks=sa2q1a2, fam1_daysweek=sa2q1a3, fam1_hours=sa2q1a4,
                 fam2_weeks=sa2q1b2, fam2_daysweek=sa2q1b3, fam2_hours=sa2q1b4,
                 fam3_weeks=sa2q1c2, fam3_daysweek=sa2q1c3, fam3_hours=sa2q1c4,
                 fam4_weeks=sa2q1d2, fam4_daysweek=sa2q1d3, fam4_hours=sa2q1d4,
                 n_men=sa2q2, men_days=sa2q3, men_avgwage=sa2q4,
                 n_women=sa2q5, women_days=sa2q6, women_avgwage=sa2q7,
                 n_child=sa2q8, child_days=sa2q9, child_avgwage=sa2q10)
labh <- as.data.frame(lapply(labh, unclass))

labh <- labh[rowSums(is.na(labh))!=21, ]
labh[is.na(labh)] <- 0

labh <- within(labh, fam_labhdays <- (fam1_weeks*fam1_daysweek) +
                (fam2_weeks*fam2_daysweek) +
                (fam3_weeks*fam3_daysweek) + 
                (fam4_weeks*fam4_daysweek))

labh <- within(labh, labh_hire_days <- (n_men*men_days) + (n_women*women_days) + (n_child*child_days))
labh <- within(labh, labh_hire_cost <- (n_men*men_days*men_avgwage) + (n_women*women_days*women_avgwage) + (n_child*child_days*child_avgwage))

#Total labor
lab <- dplyr::select(lab, hhid, plotid, fam_labdays, lab_hire_days, lab_hire_cost)
labh <- dplyr::select(labh, hhid, plotid, fam_labhdays, labh_hire_days, labh_hire_cost)

lab <- left_join(lab, labh)
lab$total_fam_days <- lab$fam_labdays + lab$fam_labhdays
lab$total_hire_days <- lab$lab_hire_days + lab$labh_hire_days 
lab$total_hire_cost <- lab$lab_hire_cost + lab$labh_hire_cost

lab$lab_cost <- lab$total_hire_cost

lab <- dplyr::select(lab, hhid, plotid, 
                     plot_familylabor_days=total_fam_days, 
                     plot_hiredlabor_days=total_hire_days, 
                     plot_hiredlabor_cost=total_hire_cost)

#######################################
############### AREAs #################
#######################################

# WDswitch
ar <- read.csv(paste(dataPath,'/nga_area.csv',sep='')) %>% dplyr::select(hhid, plotid, area)
plot <- read_dta(file.path(dataPath, "Post Planting Wave 2/Agriculture/sect11f_plantingw2.dta")) %>% 
  dplyr::select(hhid, plotid, method=s11fq2)
nga_hhvars <- read_dta(file.path(dataPath, "Geodata Wave 2/NGA_HouseholdGeovars_Y2.dta")) %>%
  dplyr::select(hhid, ea, lat=LAT_DD_MOD, lon=LON_DD_MOD)
nga_hhvars <- data.frame(lapply(nga_hhvars, unclass))
nga_hhvars$hhid <- as.numeric(nga_hhvars$hhid)

nga <- oput
nga <- merge(nga, lab)
nga <- left_join(nga, fr)
nga <- left_join(nga, ic)
nga <- left_join(nga, seed)
nga <- left_join(nga, ir)
nga <- left_join(nga, nga_hhvars)

nga$plot_input_cost <- nga$plot_input_cost + nga$plot_other_fert_cost
nga <- dplyr::select(nga, -plot_other_fert_cost)

#Convert NAIRA TO $
infl <- 1.085 * 1.081 
exc_rate <- 0.00502387 # 17-12-2015 http://www.xe.com/currencyconverter/convert/?Amount=1&From=USD&To=NGN
nga[,c("sold_value", "sold_value2", "plot_hiredlabor_cost","plot_urea_cost","plot_npk_cost",
       "plot_N_cost", "plot_P_cost", "plot_K_cost","plot_input_cost","crop_seed_cost")] <- 

   nga[,c("sold_value", "sold_value2", "plot_hiredlabor_cost","plot_urea_cost","plot_npk_cost",
         "plot_N_cost", "plot_P_cost", "plot_K_cost","plot_input_cost", "crop_seed_cost")] * infl * exc_rate

write.csv(nga, "Nigeria/Output/NGA_rawdata.csv")



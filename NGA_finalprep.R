dataPath <- "Nigeria/DATA"
wdPath <- "Desktop/LEI - Irrigation Potential Project/"
setwd(wdPath)

#Conveyance method: 1 divert stream   2 bucket   3 handpump   4 treadlepump   5 motorpump   6 gravity   7 shadouf   8 sprinkler
#costs per hectare  $640 [1]          $10[2]     $40          $259 [1]        $263 [1]      $50                     $50

# [1] Xie et al. (2014); costs given in dollars and therefore do not need to be converted using exchange rates 
# [2] own estimation

#==========================================================================================
#THE FOLLOWING TWO LINES CAN BE EDITED TO INDICATE IRRIGATION COSTS OF CONVEYANCE METHODS
#==========================================================================================
irrigation_cost_convey_method <- data.frame(convey_method=c(1:8), irrigationcost=c(640, 10, 40, 259, 263, 50, 0, 50))
irrigation_cost_convey_method2 <- data.frame(convey_method2=c(1:8), irrigationcost2=c(640, 10, 40, 259, 263, 50, 0, 50))

nga <- read.csv('Nigeria/Output/nga_v2') %>% dplyr::select(-X)
nga <- left_join(nga, irrigation_cost_convey_method)
nga <- left_join(nga, irrigation_cost_convey_method2)

nga$irrigationcost2[!is.na(nga$irrigationcost)] <- 0
nga$irrigation_cost <- (nga$irrigationcost + nga$irrigationcost2) * nga$harv_area_ha
nga <- dplyr::select(nga, -irrigationcost, -irrigationcost2)




#Corrections (only for final variables)
nga <- dplyr::filter(nga, harvest_kg>=0) 
nga <- dplyr::filter(nga, familylabor_days<400 | is.na(familylabor_days))
nga <- dplyr::filter(nga, hiredlabor_days<600 | is.na(hiredlabor_days))
nga <- dplyr::filter(nga, hiredlabor_cost<10000 | is.na(hiredlabor_cost))
nga <- dplyr::filter(nga, N_kg<500 | is.na(N_kg))
nga <- dplyr::filter(nga, P_kg<200 | is.na(P_kg))
nga <- dplyr::filter(nga, K_kg<200 | is.na(K_kg))

nga <- dplyr::filter(nga, N_cost<250 | is.na(N_cost))
nga <- dplyr::filter(nga, P_cost<150 | is.na(P_cost))
nga <- dplyr::filter(nga, K_cost<150 | is.na(K_cost))

nga <- dplyr::filter(nga, input_cost<600 | is.na(input_cost))
nga <- dplyr::filter(nga, seed_cost<500 | is.na(seed_cost))
#nga <- dplyr::filter(nga, total_cost< | is.na(total_cost))
nga <- dplyr::filter(nga, pricecost_ratio<1 | is.na(pricecost_ratio))

nga$unique_id <- paste(nga$hid, nga$plotid, nga$cropname,  sep="")
b  <- nga$unique_id[duplicated(nga$unique_id)]
b <- nga[nga$unique_id %in% b,]

t_nga <- summarise(group_by(nga, cropcode, irrigation_use),
                   n = length(hhid),
                   familylabor_days = mean(familylabor_days, na.rm=T),
                   hiredlabor_days = mean(hiredlabor_days, na.rm=T),
                   hiredlabor_cost = mean(hiredlabor_cost, na.rm=T),
                   N_kg = mean(N_kg, na.rm=T),
                   P_kg = mean(P_kg, na.rm=T),
                   K_kg = mean(K_kg, na.rm=T),
                   N_cost = mean(N_cost, na.rm=T),
                   P_cost = mean(P_cost, na.rm=T),
                   K_cost = mean(K_cost, na.rm=T),
                   irrigation_cost = mean(irrigation_cost, na.rm=T),
                   input_cost = mean(input_cost, na.rm=T),
                   seed_cost = mean(crop_seed_cost, na.rm=T),
                   total_cost = mean(total_cost, na.rm=T),
                   crop_cost_perkg = mean(crop_cost_perkg, na.rm=T),
                   pricecost_ratio = mean(pricecost_ratio, na.rm=T))
t_nga[,3:18] <- round(t_nga[,3:18], 3)

write.csv(t_nga, 'Nigeria/Output/nga_summary_percrop.csv')


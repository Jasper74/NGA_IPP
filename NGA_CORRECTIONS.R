#Prepping and corrections for Nigeria
#This code should be executed in order to avoid 

#Judgment is based on regional means and the influence of outliers

#############################################################################################################
####################################### CORRECTIONS #########################################################
#############################################################################################################
#Harvest
nga <- dplyr::filter(nga, zone %in% 1 & harvest<100000 | zone %in% 2 & harvest<5000 | zone %in% 3 & harvest<30000 |
                      zone %in% 4 & harvest<6000 | zone %in% 5 & harvest<30000 | zone %in% 6 & harvest<100000 | 
                      is.na(harvest))
nga <- dplyr::filter(nga, harvarea<23 | is.na(harvarea))

#Crop prices
nga <- dplyr::filter(nga, crop_prc1<10000 | is.na(crop_prc1))

#Labor and cost
nga <- dplyr::filter(nga, zone %in% 4 & total_hire_days<400 |
                          zone %in% 6 & total_hire_days<600 |
                         !zone %in% c(4,6) & total_hire_days>=0 | is.na(total_hire_days))

nga <- dplyr::filter(nga, zone %in% 1 & total_hire_cost < 600000 |
                          zone %in% 2 & total_hire_cost < 400000 | 
                          zone %in% 3 & total_hire_cost < 300000 | 
                          zone %in% 4 & total_hire_cost < 100000 | 
                          zone %in% 5 & total_hire_cost < 400000 | 
                          zone %in% 6 & total_hire_cost < 


                     #Urea and npk use 
nga <- dplyr::filter(nga, zone %in% c(2,3) & urea<1800 | !zone %in% c(2,3) & urea>=0 | is.na(urea))
nga <- dplyr::filter(nga, zone %in% 1 & npk<1600 | zone %in% 3 & npk<5000 | zone %in% 4 & npk<600 | 
                       zone %in% 6 & npk <300 | zone %in% c(2,5) & npk>=0 | is.na(npk))
#costs are OK 

#NPK use
nga <- dplyr::filter(nga, zone %in% 2 & N < 1000 | !zone %in% 2 & N>=0 | is.na(N))

#Input cost
nga <- dplyr::filter(nga, zone %in% 4 & input_cost<1000 | zone %in% 5 & input_cost<300 | zone %in% 6 & input_cost<1000 | 
                       zone %in% c(1,2,3) & input_cost>=0 | is.na(input_cost)) 

#############################################################################################################
####################################### DIVIDING COSTS OVER HARVESTED KG ####################################
#############################################################################################################
totharv <- summarise(group_by(nga, hhid),
                     totalharv = sum(harvest))

nga_2 <- left_join(nga, totharv)
nga_2$harvproportion <- nga_2$harvest / nga_2$totalharv

nga_2[c("lab","lab_cost","input_cost", "urea", "npk","urea_cost","npk_cost","N","P","K", "N_cost", "P_cost", "K_cost")] <-
  nga_2[c("lab","lab_cost","input_cost", "urea", "npk","urea_cost","npk_cost","N","P","K", "N_cost", "P_cost", "K_cost")] * nga_2$harvproportion


#############################################################################################################
####################################### ELEMINATING MULTIPLE CROPS PER PLOT #################################
#############################################################################################################
nga$plot_id <- paste(nga$hhid, nga$plotid, sep="")
multicrop <- nga[duplicated(nga$plot_id),]
bad <- multicrop$plot_id
nga_1 <- nga[!nga$plot_id %in% bad,]

write.csv(nga_1, file="Nigeria/Output/nga_1.csv")
write.csv(nga_2, file="Nigeria/Output/nga_2.csv")

#############################################################################################################
nga_national <- summarise(group_by(nga_1, cropname),
                          n = length(hhid),
                          avgarea = mean(area, na.rm=T),
                          harvest = mean(harvest, na.rm=T)/mean(area, na.rm=T),
                          qty_sold = mean(qty_sold, na.rm=T),
                          valu = mean(sold_valu, na.rm=T),
                          lab = mean(lab, na.rm=T)/mean(area, na.rm=T),
                          lab_cost = mean(lab_cost, na.rm=T)/mean(area, na.rm=T),
                          N = mean(N, na.rm=T)/mean(area, na.rm=T),
                          P = mean(P, na.rm=T)/mean(area, na.rm=T),
                          K = mean(K, na.rm=T)/mean(area, na.rm=T),
                          N_cost = mean(N_cost, na.rm=T)/mean(area, na.rm=T),
                          P_cost = mean(P_cost, na.rm=T)/mean(area, na.rm=T),
                          K_cost = mean(K_cost, na.rm=T)/mean(area, na.rm=T),
                          input_cost = mean(input_cost, na.rm=T)/mean(area, na.rm=T))
nga_national <- dplyr::arrange(nga_national, -n) 

nga_state <- summarise(group_by(nga_1, zone, cropname),
                       n = length(hhid),
                       avgarea = mean(area, na.rm=T),
                       harvest = mean(harvest, na.rm=T)/mean(area, na.rm=T),
                       qty_sold = mean(qty_sold, na.rm=T),
                       valu = mean(sold_valu, na.rm=T),
                       lab = mean(lab, na.rm=T)/mean(area, na.rm=T),
                       lab_cost = mean(lab_cost, na.rm=T)/mean(area, na.rm=T),
                       N = mean(N, na.rm=T)/mean(area, na.rm=T),
                       P = mean(P, na.rm=T)/mean(area, na.rm=T),
                       K = mean(K, na.rm=T)/mean(area, na.rm=T),
                       N_cost = mean(N_cost, na.rm=T)/mean(area, na.rm=T),
                       P_cost = mean(P_cost, na.rm=T)/mean(area, na.rm=T),
                       K_cost = mean(K_cost, na.rm=T)/mean(area, na.rm=T),
                       input_cost = mean(input_cost, na.rm=T)/mean(area, na.rm=T),
                       lat = mean(lat),
                       lon = mean(lon))
nga_state <- dplyr::arrange(nga_state, -n) 
nga_state[,3:18] <- round(nga_state[,3:18],3) 

nga_ea <- summarise(group_by(nga_1, ea, cropname),
                       n = length(hhid),
                       avgarea = mean(area, na.rm=T),
                       harvest = mean(harvest, na.rm=T)/mean(area, na.rm=T),
                       qty_sold = mean(qty_sold, na.rm=T),
                       valu = mean(sold_valu, na.rm=T),
                       lab = mean(lab, na.rm=T)/mean(area, na.rm=T),
                       lab_cost = mean(lab_cost, na.rm=T)/mean(area, na.rm=T),
                       N = mean(N, na.rm=T)/mean(area, na.rm=T),
                       P = mean(P, na.rm=T)/mean(area, na.rm=T),
                       K = mean(K, na.rm=T)/mean(area, na.rm=T),
                       N_cost = mean(N_cost, na.rm=T)/mean(area, na.rm=T),
                       P_cost = mean(P_cost, na.rm=T)/mean(area, na.rm=T),
                       K_cost = mean(K_cost, na.rm=T)/mean(area, na.rm=T),
                       input_cost = mean(input_cost, na.rm=T)/mean(area, na.rm=T),
                       lat = mean(lat),
                       lon = mean(lon))
nga_ea <- dplyr::arrange(nga_ea, -n) 
nga_ea[,3:18] <- round(nga_ea[,3:18],3)

write.csv(nga_national, file="Nigeria/Output/NGA_national.csv")
write.csv(nga_state,    file="Nigeria/Output/NGA_state.csv")
write.csv(nga_ea,       file="Nigeria/Output/NGA_ea.csv")




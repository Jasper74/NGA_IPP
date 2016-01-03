dataPath <- "Nigeria/DATA" 
# dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA"
# Set the working directory to the appropriate folder for Niger
wdPath <- "Desktop/LEI - Irrigation Potential Project/"
setwd(wdPath)

nga <- read.csv('Nigeria/Output/NGA_rawdata.csv')

#=========================================================================================================
#Corrections (to make sure that spreading costs is done accurately)
nga <- dplyr::filter(nga, harv_area_ha<60 | is.na(harv_area_ha))
nga <- dplyr::filter(nga, zone %in% 5 & harvest_kg<30000 | zone %in% 1 & harvest_kg<50000 |
                      zone %in% 3 & harvest_kg<20000 | !zone %in% c(1,3,5) & harvest_kg>0 | is.na(harvest_kg))
nga <- dplyr::filter(nga, sold_value<6000 | is.na(sold_value))
#=========================================================================================================
#labor price
nga$hiredlabor_price_perday <- nga$plot_hiredlabor_cost / nga$plot_hiredlabor_days
#Remove obervations that have 0 hired days with a cost
labor_prices <- dplyr::select(nga, hhid, zone, ea, lat, lon, hiredlabor_price_perday, plot_hiredlabor_days, plot_hiredlabor_cost) %>%
  dplyr::filter(plot_hiredlabor_days>0 & plot_hiredlabor_cost>0 & hiredlabor_price_perday<2000)
p_lab <- summarise(group_by(labor_prices, ea),
                   p_labor = mean(hiredlabor_price_perday, na.rm=T))
nga <- left_join(nga, p_lab) 
nga$p_labor <- ifelse((is.na(nga$p_labor)), mean(nga$p_labor, na.rm=T), nga$p_labor)

nga$plot_hiredlabor_cost <- ifelse((nga$plot_hiredlabor_cost==0 & nga$plot_hiredlabor_days>0), (nga$plot_hiredlabor_days * nga$p_labor), nga$plot_hiredlabor_cost)

write.csv(p_lab, 'Nigeria/Output/laborprices.csv')
#=========================================================================================================
#Obtain crop yield in kilogram per hectare
nga$yield_per_hectare <- nga$harvest_kg / (nga$harv_area_ha)
#Correcting outliers (removes 400 obs, take closer look if it is actually used since it may be worthwhile winsorizing these obs)
#nga <- dplyr::filter(nga, yield_per_hectare<10000 | is.na(yield_per_hectare))

#Calculate prices for each crop 
crop_prices <- dplyr::select(nga, ea, zone, cropname, sold_volume_kg, sold_value)
crop_prices2<- dplyr::select(nga, ea, zone, cropname, sold_volume_kg=sold_volume_kg2,
                             sold_value=sold_value2)
crop_prices <- rbind(crop_prices, crop_prices2)

crop_prices$price <- crop_prices$sold_value / crop_prices$sold_volume_kg 
crop_prices <- dplyr::filter(crop_prices, price<52)
crop_price_table <- summarise(group_by(crop_prices, cropname),
                              p_crop_perkg = mean(price, na.rm=T))

nga <- left_join(nga, crop_price_table)
#national_pcrop <- read.csv('Niger/Output/nga_cropprices_national.csv') %>%
#  dplyr::select(cropname, p_crop_perkg=crop_prc)
#nga <- left_join(nga, national_pcrop, by="cropname")

nga$crop_revenue <- nga$harvest_kg * nga$p_crop_perkg
nga$crop_revenue[is.na(nga$crop_revenue)] <- 0

#Calculate total rev per plot and per household 
plot_revenue <- summarise(group_by(nga, hhid, plotid),
                          total_plot_revenue = sum(crop_revenue, na.rm=T))
nga <- left_join(nga, plot_revenue)

#Share of crop revenue in total plot revenue
nga$cropshare_revenue<- nga$crop_revenue / nga$total_plot_revenue

#CONVERT PLOT COSTS TO COSTS PER CROP
nga$familylabor_days <- nga$plot_familylabor_days * nga$cropshare_revenue
nga$hiredlabor_days <- nga$plot_hiredlabor_days * nga$cropshare_revenue
nga$hiredlabor_cost <- nga$plot_hiredlabor_cost * nga$cropshare_revenue

nga$urea_kg <- nga$plot_urea_kg * nga$cropshare_revenue
nga$npk_kg <- nga$plot_npk_kg * nga$cropshare_revenue

nga$N_kg <- nga$plot_N_kg * nga$cropshare_revenue
nga$P_kg <- nga$plot_P_kg * nga$cropshare_revenue
nga$K_kg <- nga$plot_K_kg * nga$cropshare_revenue

nga$urea_cost <- nga$plot_urea_cost * nga$cropshare_revenue
nga$npk_cost <- nga$plot_npk_cost * nga$cropshare_revenue

nga$N_cost <- nga$plot_N_cost * nga$cropshare_revenue
nga$P_cost <- nga$plot_P_cost * nga$cropshare_revenue
nga$K_cost <- nga$plot_K_cost * nga$cropshare_revenue

nga$input_cost <- nga$plot_input_cost * nga$cropshare_revenue

nga$crop_seed_cost <- ifelse((is.na(nga$crop_seed_cost) & !is.na(nga$input_cost)), 0, nga$crop_seed_cost)
nga <- within(nga, total_cost <- N_cost + P_cost + K_cost + input_cost + crop_seed_cost)
nga$crop_cost_perkg <- nga$total_cost / nga$harvest_kg

nga$pricecost_ratio <- nga$crop_cost_perkg / nga$p_crop_perkg

nga_v2 <- dplyr::select(nga, hhid, ea, lat, lon, plotid, cropname, cropcode, harvest_kg,
                        harvest_kg, harv_area_ha, p_crop_perkg, crop_revenue, 
                        total_plot_revenue, cropshare_revenue,
                        familylabor_days, hiredlabor_days, hiredlabor_cost, 
                        N_kg, P_kg, K_kg, N_cost, P_cost, K_cost,
                        water_source, convey_method, convey_method2, 
                        input_cost, crop_seed_cost, total_cost, crop_cost_perkg, pricecost_ratio)
write.csv(nga_v2, 'Nigeria/Output/nga_v2')


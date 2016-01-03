nga_original <- nga

############
# OPTION 1 #
############

#Obtain crop yield per hectare
nga$yield_per_hectare <- nga$harvest / nga$harvarea

#Calculate prices for each crop 
national_pcrop <- read.csv('Nigeria/Output/nga_cropprices_national.csv') %>%
  dplyr::select(cropcode, p_crop = price)
nga <- left_join(nga, national_pcrop, by="cropcode")

#Calculate quantities sold 
nga$qty_sold[is.na(nga$qty_sold)] <- 0
nga$qty_sold2[is.na(nga$qty_sold2)] <- 0

nga$totalsold <- nga$harvest

nga$totalrev <- nga$totalsold * nga$p_crop

#Calculate total rev per plot 
table_rev <- summarise(group_by(nga, hhid, plotid),
                       total_plot_revenue = sum(totalrev, na.rm=T))
nga <- left_join(nga, table_rev)

nga$revenue_share <- nga$totalrev / nga$total_plot_revenue

#Example for overview
nga_option1 <- dplyr::select(nga, hhid, plotid, cropname, harvested, harvarea, harvest, qty_sold, sold_valu, qty_sold2, sold_valu2, 
                      total_fam_days, total_hire_days, total_hire_cost, area, yield_per_hectare, p_crop, totalsold, totalrev,
                      total_plot_revenue, revenue_share)

nga_option1$total_fam_days_revenueshare <- nga_option1$total_fam_days * nga_option1$revenue_share
nga_option1$total_hire_days_revenueshare <- nga_option1$total_hire_days * nga_option1$revenue_share
nga_option1$total_hire_cost_revenueshare <- nga_option1$total_hire_cost * nga_option1$revenue_share

check <- dplyr::select(nga_option1, hhid, plotid, total_fam_days, total_hire_days, total_hire_cost, total_fam_days_revenueshare,
                       total_hire_days_revenueshare, total_hire_cost_revenueshare, revenue_share)

############
# OPTION 2 #
############
#Remove all plot observations with multiple plots

#plot <- read_dta("Nigeria/DATA/Post Harvest Wave 2/Agriculture/secta1_harvestw2.dta") %>%
#  dplyr::select(hhid, plotid, monocrop=sa1q29)

nga$plot_id <- paste(nga$hhid, nga$plotid, sep="")

multicrop <- nga[duplicated(nga$plot_id),]
nga_option2 <- nga[!nga$plot_id %in% multicrop$plot_id,]



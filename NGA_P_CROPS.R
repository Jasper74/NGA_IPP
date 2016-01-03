dataPath <- "Nigeria/DATA"
wdPath <- "Desktop/LEI - Irrigation Potential Project/"
setwd(wdPath)

library(haven)
library(stringr)
library(plyr)
library(dplyr)
library(foreign)

options(scipen=999)

# WDswitch
harv <- read_dta(file.path(dataPath, "Post Harvest Wave 2/Agriculture/secta3_harvestw2.dta")) %>%
  dplyr::select(hhid, plotid, cropid, cropname, cropcode, 
                harv_area=sa3q5a,  area_unit=sa3q5b,  harv_qty=sa3q6a1, harv_unit=sa3q6a2,
                harv_sold=sa3q9,   sold_qty=sa3q11a,  sold_unit=sa3q11b, sold_val=sa3q12,
                harv_sold2=sa3q14, sold_qty2=sa3q16a, sold_unit2=sa3q16b, sold_val2=sa3q17) 

#Units in the dataset are denoted by labelled numbers (1= 'kilo', etc)
# loading conversion data to obtain accurate estimates of harvest weights
conv_harv <- read_dta("Nigeria/DATA/w2agnsconversion.dta") %>%  
  dplyr::select(cropcode, harv_unit=nscode, conversion_harv=conversion)
conv_sold <- read_dta("Nigeria/DATA/w2agnsconversion.dta") %>%  
  dplyr::select(cropcode, sold_unit=nscode, conversion_sold=conversion)
conv_sold2 <- read_dta("Nigeria/DATA/w2agnsconversion.dta") %>%  
  dplyr::select(cropcode, sold_unit2=nscode, conversion_sold2=conversion)

harv <- left_join(harv, conv_harv)
harv <- left_join(harv, conv_sold)
harv <- left_join(harv, conv_sold2)

harv$harv_unit[harv$harv_unit==2] <- 0.001
harv$harv_unit[harv$harv_unit==3] <- NA  #litres (1l = 1kg?)
harv$harv_unit[harv$harv_unit==4] <- NA  #centiliters (1l = 100cl?)

harv$sold_unit[harv$sold_unit==2] <- 0.001
harv$sold_unit[harv$sold_unit==3] <- NA  #litres (1l = 1kg?)
harv$sold_unit[harv$sold_unit==4] <- NA  #centiliters (1l = 100cl?)

harv$sold_unit2[harv$sold_unit2==2] <- 0.001
harv$sold_unit2[harv$sold_unit2==3] <- NA  #litres (1l = 1kg?)
harv$sold_unit2[harv$sold_unit2==4] <- NA  #centiliters (1l = 100cl?)

harv$conversion_harv <- ifelse(is.na(harv$conversion_harv), harv$harv_unit, harv$conversion_harv)
harv$conversion_sold <- ifelse(is.na(harv$conversion_sold), harv$sold_unit, harv$conversion_sold)
harv$conversion_sold2 <- ifelse(is.na(harv$conversion_sold2), harv$sold_unit2, harv$conversion_sold2)

harv$harv_kg <- harv$harv_qty * as.numeric(harv$conversion_harv)
harv$sold_kg <- harv$sold_qty * as.numeric(harv$conversion_sold)
harv$sold_kg2<- harv$sold_qty2 * as.numeric(harv$conversion_sold2)

#New data frame for calculating prices
sales <- dplyr::select(harv, hhid, plotid, cropid, cropname, cropcode, harv_kg, sold_kg, sold_val, sold_kg2, sold_val2)

sales$price1 <- sales$sold_val / sales$sold_kg
sales$price2 <- sales$sold_val2 / sales$sold_kg2

#Now we can start correcting things
sales <- dplyr::filter(sales, sold_kg<50000 & sold_val<1500000 & sold_kg>0)
sales <- dplyr::filter(sales, price1<20000)

sales_crop <- group_by(sales, cropcode)
t_crop <- summarise(sales_crop, 
                    n = length(is.na(price1)),
                    price = mean(price1, na.rm=TRUE))

write.csv(t_crop, file="Nigeria/Output/nga_cropprices_national.csv")




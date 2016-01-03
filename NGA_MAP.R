#----------------------------------------------------------------------------------------------------------
#http://stackoverflow.com/questions/17723822/administrative-regions-map-of-a-country-with-ggmap-and-ggplot2
#----------------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(rgdal)
library(maptools)
library(rgeos)
library(mapproj)
library(maps)
library(data.table)
library(xlsx)


#-------------------------
#The function needs two inputs:
#mapdata: a data frame containing the variable of interest and a state indicator
#var: the name the variable of interest in the mapdata

ngamap <- function(mapdata, variable){
  #Loading in the spatial polygons from GADM (these need to be downloaded)
  nga.adm2.spdf <- readRDS("Nigeria/DATA/NGA_adm1.rds")
  nga.adm2.df <- fortify(nga.adm2.spdf)

  mapdata$id <- mapdata$state
  mapdata$var <- mapdata[, variable]
  
  mapdata_bystate <- group_by(mapdata, id)
  t <- summarise(mapdata_bystate, 
               var = mean(var))
  
  id.df <- data.frame(id= as.integer(unique(nga.adm2.df[,'id'])))
  id.df <- left_join(id.df, t)
  id.df$id <- as.character(id.df$id)
  
  nga.adm2.df <- merge(nga.adm2.df, id.df, by = 'id', all.x = TRUE)
  
  nga.adm2.centroids.df <- data.frame(long = coordinates(nga.adm2.spdf)[, 1], 
                                           lat = coordinates(nga.adm2.spdf)[, 2])
  
  nga.adm2.centroids.df[, 'ID_1'] <- nga.adm2.spdf@data[,'ID_1']
  nga.adm2.centroids.df[, 'NAME_1'] <- nga.adm2.spdf@data[,'NAME_1']
  
  n <- ggplot(nga.adm2.df, aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = cut(var,9,)), colour="black") +
    #geom_text(data = ethiopia.adm2.centroids.df, aes(label = NAME_2, x = long, y = lat, group = NAME_2), size = 2) + 
    labs(x=" ", y=" ") + 
    theme_bw() + scale_fill_brewer(variable, palette  = 'RdYlGn', na.value="grey") + 
    coord_map() + 
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) #+ 
    theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + 
    theme(panel.border = element_blank())
  
  n
}

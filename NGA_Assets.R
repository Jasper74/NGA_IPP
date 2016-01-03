assets <- read_dta(file.path(dataPath, "Post Harvest Wave 2/Agriculture/secta42_harvestw2.dta")) 

sprinkler <- dplyr::filter(assets, item_cd==309)
waterpump <- dplyr::filter(assets, item_cd==308)
  
#Use assets for approximating price of sprinklers and water pumps?
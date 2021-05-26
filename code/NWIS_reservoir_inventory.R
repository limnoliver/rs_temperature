library(tidyverse)
library(dataRetrieval)

# Creating a function to retierve the NWIS inventory sites temperature data. 
inventory_nwis_lakes <- function(site_type, parameter_cd){
  
  hucs <- stringr::str_pad(1:21, width = 2, pad = "0")
  lake_temp_inventory <- data.frame()
  # Inventory of reservoir sites from NWIS for lake's temperature data.
  for(huc in hucs){
    state_lake_temp_inventory <- whatNWISdata(huc = huc,
                                              siteType = site_type,
                                              parameterCd = parameter_cd) %>%
      select(c("agency_cd", "site_no", "station_nm", "dec_lat_va", 
               "dec_long_va", "count_nu"))%>%
      mutate(huc_number = huc)
    
    lake_temp_inventory <- bind_rows(lake_temp_inventory, state_lake_temp_inventory)
    
  }
  return(lake_temp_inventory)
}

# Testing the inventory_nwis_lakes function
test_inventory_nwis_lakes <- inventory_nwis_lakes(
  site_type = "LK" ,parameter_cd = "00010")

unique(test_inventory_nwis_lakes$huc_number)

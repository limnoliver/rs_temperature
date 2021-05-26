# this script inventories the U.S. for 
# reservoir temperature data, and filters it down to 
# those sites in the DRB, ILRB, UCRB

library(tidyverse)
library(dataRetrieval)

# Creating a function to retierve the NWIS inventory sites temperature data. 
inventory_nwis_lakes <- function(site_type, parameter_cd){
  
  hucs <- stringr::str_pad(1:21, width = 2, pad = "0")
  out_inventory <- data.frame()
  # Inventory of reservoir sites from NWIS for lake's temperature data.
  
  for(huc in hucs){
    print(paste("finding all reservoir data in HUC number", huc))
    inventory <- whatNWISdata(huc = huc,
                                              siteType = site_type,
                                              parameterCd = parameter_cd) %>%
      #filter(stat_cd %in% '00003') %>%
      select(agency_cd, site_no, station_nm, dec_lat_va, dec_long_va, count_nu, data_type_cd, begin_date, end_date) %>%
      group_by(site_no) %>% 
      slice_max(count_nu, with_ties = FALSE) %>%
      mutate(huc_number = huc)
    
    out_inventory <- bind_rows(out_inventory, inventory)
    
  }
  return(out_inventory)
}

# Testing the inventory_nwis_lakes function
res_inventory <- inventory_nwis_lakes(site_type = "LK" ,parameter_cd = "00010")

# write raw inventory
saveRDS(res_inventory, 'data/out/national_reservoir_inventory.rds')


res_inventory <- res_inventory %>%
  filter(!is.na(dec_long_va)) %>%
  sf::st_as_sf(coords = c('dec_long_va', 'dec_lat_va'), crs = 4326, remove = FALSE)

# filter to DRB/UCRB/IRB
source('code/match_to_basins.R')

res_matched <- filter(res_inventory, data_type_cd %in% c('uv', 'dv') | count_nu > 10) %>%
  filter(end_date >= as.Date('2013-01-01')) %>%
  match_to_hucs() %>%
  select(site_no, station_nm, latitude = dec_lat_va, longitude = dec_long_va, 
         count_nu, data_type_cd, basin_name) %>% st_drop_geometry()

readr::write_csv(res_matched, 'data/out/potential_rs_temperature_reservoir_sites.csv')


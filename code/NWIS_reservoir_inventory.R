library(tidyverse)
library(dataRetrieval)

inventory_nwis_lakes <- function(nwis_pull_params, siteType, parameterCd){
  #states_cd <- stateCd$STATE# Reterieving the U.S. state_codes.

  hucs <- stringr::str_pad(1:21, width = 2, pad = "0")
  nwis_pull_params$siteType <- siteType
  nwis_pull_params$parameterCd <- parameterCd

  # Inventory of reservoir sites from NWIS for lake's temperature data.
  lake_temp_inventory <- data.frame()

  for(huc in hucs){

    nwis_pull_params$huc = huc
    state_lake_temp_inventory <- do.call(whatNWISdata, nwis_pull_params) %>%
      select(c("agency_cd", "site_no", "station_nm", "dec_lat_va", "dec_long_va", "count_nu"))

    lake_temp_inventory <- bind_rows(lake_temp_inventory, state_lake_temp_inventory)

  }

}

# Inventory of reservoir sites from NWIS for lake's temperature data.
hucs <- stringr::str_pad(1:21, width = 2, pad = "0")
lake_temp_inventory <- data.frame()

for(huc in hucs){
  state_lake_temp_inventory <- whatNWISdata(huc = huc,
                                            #stateCd = state_cd,
                                            siteType = "LK",
                                            parameterCd="00010") %>%
    select(c("agency_cd", "site_no", "station_nm", "dec_lat_va", "dec_long_va", "count_nu"))

  lake_temp_inventory <- bind_rows(lake_temp_inventory, state_lake_temp_inventory)

}
summary(lake_temp_inventory)

# Testing the inventory_nwis_lakes function
test_inventory_nwis_lakes <- inventory_nwis_lakes(
  siteType = "LK",parameterCd = "00010")

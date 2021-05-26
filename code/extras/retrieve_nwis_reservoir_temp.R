# Function to retrieve NWIS temperature data. 

library(operators)

# Retrieve the 10 sites with highest NWIS inventory volume 
sites_id <- test_inventory_nwis_lakes %>% 
  top_n(17, count_nu) %>% select(site_no) %>% unique()


# Using map_df instead of for loop to loop through the huc-codes.
retrieve_nwis_temp <- function(sites_id, parameter_cd){
  nwis_temp_dat <- purrr::map_df(sites_id, function(site_id){
    readNWISdv(siteNumber = site_id, 
               parameterCd = parameter_cd) %>%
      rename(temp = X_00010_00003, temp_cd = X_00010_00003_cd)})
  return(nwis_temp_dat)
}

test_retrieve_nwis_temp <- retrieve_nwis_temp(sites_id = sites_id, 
                                              parameter_cd = '00010')

length(unique(test_retrieve_nwis_temp$site_no))

nwis_sites_pulled <- unique(nwis_temp_elevation_dat$site_no)
missing_sites <- filter(sites_id, site_no %!in%nwis_sites_pulled)


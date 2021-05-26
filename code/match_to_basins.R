
match_to_hucs <- function(in_sites) {
  # HUC units for each basin
  library(dplyr)
  library(sf)
  
  basins <- tibble(basin_name = c('DRB', rep(c('ILRB', 'UCRB'), each = 2)),
                   basin_huc4 = c('0204', '0712', '0713', '1401', '1402'))
  
  # read in HUCs and subset
  # data came from here: 
  hucs <- sf::st_read('data/in/wbdhu4_a_us_september2020.gdb') %>%
    filter(huc4 %in% basins$basin_huc4) %>%
    left_join(basins, by = c('huc4' = 'basin_huc4'))
  
  drb <- readRDS('data/in/drb_boundary.rds') %>% 
    st_as_sf() %>%
    st_transform(st_crs(hucs)) %>%
    mutate(basin_name = 'DRB')
  
  # read in temperature data
  # data come from here: 
  temp_sites <- in_sites

  # find sites that intersect with hucs
  points_in_hucs <- st_join(temp_sites, hucs, join = st_within)
  points_in_hucs <- filter(points_in_hucs, !is.na(basin_name)) %>%
    filter(!basin_name %in% 'DRB')
  points_in_drb <- st_join(temp_sites, drb) %>%
    filter(!is.na(basin_name))
  
  return(bind_rows(points_in_hucs, points_in_drb))
}


# read in HUC file
# got HUC file from here:

# HUC units for each basin
library(dplyr)
basins <- tibble(basin_name = c('DRB', rep(c('ILRB', 'UCRB'), each = 2)),
            basin_huc4 = c('0204', '0712', '0713', '1401', '1402'))

# read in HUCs and subset
# data came from here: 
library(sf)
hucs <- sf::st_read('data/in/wbdhu4_a_us_september2020.gdb') %>%
  filter(huc4 %in% basins$basin_huc4) %>%
  left_join(basins, by = c('huc4' = 'basin_huc4'))

# read in temperature data
# data come from here: 
temps <- readRDS('data/in/daily_temperatures.rds')
temp_sites <- readRDS('data/in/stream_sites_us.rds')
drb <- readRDS('data/in/drb_boundary.rds') %>% 
  st_as_sf() %>%
  st_transform(st_crs(hucs)) %>%
  mutate(basin_name = 'DRB')

# find sites that intersect with hucs
points_in_hucs <- st_join(temp_sites, hucs, join = st_within)
points_in_hucs <- filter(points_in_hucs, !is.na(basin_name)) %>%
  filter(!basin_name %in% 'DRB')
points_in_drb <- st_join(temp_sites, drb) %>%
  filter(!is.na(basin_name))

sites <- bind_rows(points_in_hucs, points_in_drb)

# temp summary by site
summary <- mutate(temps, year = lubridate::year(date)) %>%
  filter(year >= 2013) %>%
  group_by(site_id, source) %>%
  summarize(n_days = n(),
            med_n_daily_obs = median(n_obs))

# filter sites to those with temp data after 2013
sites <- filter(sites, site_id %in% unique(summary$site_id))
# do our best to reconcile duplicate data

sites <- group_by(sites, latitude, longitude) %>%
  mutate(duplicates = n()) %>% ungroup()

sites2 <- mutate(sites, latitude2 = round(latitude, 4), longitude2 = round(longitude, 4)) %>%
  group_by(latitude2, longitude2) %>% mutate(duplicates4 = n())

sites3 <- ungroup(sites2) %>%
  left_join(summary, by = c('site_id', 'source')) %>%
  group_by(latitude2, longitude2) %>%
  slice_max(n_days)

# sites out
rs_sites <- ungroup(sites3) %>%
  select(site_id, source, longitude, latitude, n_days, basin_name) %>%
  st_drop_geometry() %>%
  arrange(basin_name, -n_days)

write.csv(rs_sites, 'potential_rs_temperature_stream_sites.csv', row.names = FALSE)

# now filter temp data to those sites
temps <- filter(temps, site_id %in% unique(sites$site_id))

# test going to uv for a dv site
dv <- filter(rs_sites, source %in% 'nwis_dv')
uv <- dataRetrieval::readNWISuv(siteNumbers = '05576195', parameterCd = '00010')
compare <- filter(temps, site_id %in% 'USGS-05576195')

dropped_nwisdv <- ungroup(sites2) %>%
  filter(source %in% 'nwis_dv') %>%
  filter(!site_id %in% unique(sites3$site_id[sites3$source %in% 'nwis_dv']))

View(filter(summary, site_id %in% dropped_nwisdv$site_id[1]))
# ecosheds data
# need to use better site identifier for EcoSHEDS 
# because it includes USGS data
sheds <- readRDS("data/in/ecosheds_sites.rds")
names(hucs)
head(hucs)
library(ggplot2)
ggplot() +
  geom_sf(data = filter(hucs, basin_name %in% 'DRB')) +
  geom_sf(data = filter(points_in_hucs, basin_name %in% 'DRB'), color = 'red') +
  geom_sf(data = points_in_drb, color = 'blue')

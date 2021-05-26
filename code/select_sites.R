# This script selects potential temperature sites for matching
# to RS temperature. This takes in data from the national stream temperature
# data pipeline (https://github.com/USGS-R/2wp-temp-observations)

library(dplyr)

# read in temperature data
temps <- readRDS('data/in/daily_temperatures.rds')
temp_sites <- readRDS('data/in/stream_sites_us.rds')

# find sites that intersect with hucs
source('code/match_to_basins.R')
sites <- match_to_hucs(in_sites = temp_sites)

# filter sites to those with temp data after 2013
# temp summary by site
summary <- mutate(temps, year = lubridate::year(date)) %>%
  filter(year >= 2013) %>%
  group_by(site_id, source) %>%
  summarize(n_days = n(),
            med_n_daily_obs = median(n_obs))

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

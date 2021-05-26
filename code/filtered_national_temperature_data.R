# This script finds the sites/dates that have
# both a satellite overpass (dates from Tyler)
# and a temperature observation
library(tidyverse)
library(scipiper)

# Read in the qaqc national temperature data from here https://github.com/USGS-R/2wp-temp-observations
dat <- readRDS('data/in/daily_temperatures_qaqc.rds') %>%
  mutate(date = as.Date(date))

# Reading site-dates of satellite overpasses
sat_dat <- readr::read_csv('data/in/potential_rs_temperature_stream_sites_resolvable_width_dates.csv') %>%
  mutate(date = as.Date(dates, format = '%m/%d/%Y %H:%M'))

# need to fix ecoshed site IDs because there was a change in 
# naming convention since I originally provided Tyler sites
eco_sites <- readRDS('data/in/ecosheds_sites.rds') %>%
  mutate(new_site_id = paste(agency_name, location_name, sep = '-'),
         location_id = as.character(location_id), 
         source = 'ecosheds')

# fix ecosheds site IDs
sat_dat_f <- left_join(sat_dat, select(eco_sites, site_id = location_id, new_site_id, source)) %>%
  mutate(site_id = ifelse(source %in% 'ecosheds', new_site_id, site_id)) %>%
  select(-new_site_id)


# Combining the nation's daily temperature data with the site_dates data. 
matched <- sat_dat_f %>%
  left_join(dat, by = c("site_id", "date", "source")) %>%
    filter(!is.na(mean_temp_degC))

# some data has a new source since last pull
# (e.g., sites moving from uv to dv)
# can we find the siteIDs elsewhere? that is, don't use source to join
matched_missing <- filter(sat_dat_f, !site_id %in% unique(matched$site_id)) %>%
  left_join(dat, by = c('site_id', 'date')) %>%
  filter(!is.na(mean_temp_degC)) %>%
  mutate(source = source.y) %>% select(-source.x, -source.y)

# combine
matched_all <- bind_rows(matched, matched_missing) %>%
  select(-min_temp_degC, -max_temp_degC, -n_obs, -flag, -X1)

matched_sites <- group_by(matched_all, site_id, source) %>%
  summarize(n_matches = n())

# are there any important missing sites from sat_dat 
# that should have a lot of data?
# can compare n_matches to n_days to answer this Q
# looks like we've recovered all sites with lots of data
sat_sites <- sat_dat_f %>%
  select(site_id, source, n_days) %>%
  distinct() %>%
  left_join(matched_sites, by = 'site_id') %>%
  mutate(source = ifelse(!is.na(source.y), source.y, source.x)) %>% 
  select(-source.y, -source.x) %>%
  arrange(-n_matches) %>%
  left_join(distinct(select(sat_dat_f, site_id, basin_name, latitude, longitude)))

# write data
readr::write_csv(matched_all, 'data/out/matched_site_dates_temperature.csv')
readr::write_csv(sat_sites, 'data/out/summary_matched_site_dates_temperature.csv')


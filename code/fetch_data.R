library(dplyr)
library(lubridate)
library(tidyr)

# fetch data for selected sites
# read in sites
sites <- readxl::read_xlsx('data/in/ls8_st_data_combined_filt_summarized.xlsx', sheet = 'Summary')

# merge summary so I know which source to look for each site
summary <- readr::read_csv('data/out/summary_matched_site_dates_temperature.csv')

# some are NA - those are reservoir sites from NWIS
sites_s <- left_join(sites, select(summary, site_id, source, n_days), by = c('sitename'='site_id'))

############## NWIS Data ##################
# fetch data from nwis sites
# pull takes a long time, so split up into 4 chunks
nwis_uv_sites <- filter(sites_s, grepl('nwis', source)) %>%
  arrange(n_days)
nwis_uv_sites <- gsub('USGS-', '', nwis_uv_sites$sitename)

# pull reservoirs separately in case there's something different about the data
nwis_reservoirs <- filter(sites_s, is.na(source)) %>%
  left_join(select(readRDS(file = 'data/out/national_reservoir_inventory.rds'), site_no, data_type_cd), by = c('sitename'='site_no')) %>%
  mutate(sitename = dataRetrieval::zeroPad(sitename, padTo = 8))

# get data from dataRetrieval
# breaking up to get reasonable fetch times
nwis_uv1 <- dataRetrieval::readNWISuv(siteNumbers = nwis_uv_sites[1:10], parameterCd = '00010')
nwis_uv2 <- dataRetrieval::readNWISuv(siteNumbers = nwis_uv_sites[11:18], parameterCd = '00010')
nwis_uv3 <- dataRetrieval::readNWISuv(siteNumbers = nwis_uv_sites[19:24], parameterCd = '00010')
nwis_uv4 <- dataRetrieval::readNWISuv(siteNumbers = nwis_uv_sites[25:28], parameterCd = '00010')

# turn data from wide to long
# this accomodates multiple sensors at each site
# sensor metadata stored in "location_info"
nwis_long <- function(in_dat) {
  out_dat <- dataRetrieval::renameNWISColumns(in_dat) %>%
    select(site_no, dateTime, contains('Wtemp_Inst'), tz_cd) %>%
    tidyr::pivot_longer(
                 cols = contains('Wtemp'),
                 names_to = c('location_info', '.value'),
                 names_pattern = '(.*Wtemp)_(.*)',
                 values_drop_na = TRUE) %>%
    rename(temp_degC = Inst, temp_cd = Inst_cd)
}

# make data long and bind
out_data <- bind_rows(nwis_long(nwis_uv1), nwis_long(nwis_uv2), nwis_long(nwis_uv3), nwis_long(nwis_uv4)) %>%
  filter(!is.na(temp_degC)) %>%
  rename(sitename = site_no) %>%
  mutate(date = format(dateTime, '%Y-%m-%d'), 
         time = format(dateTime, '%H:%M')) %>%
  select(-dateTime)

# this took a long time to run, so writing the data
readr::write_csv(out_data, 'data/out/nwis_selected_sites_data.csv')

# pull reservoir data separately
res <- dataRetrieval::readNWISuv(siteNumbers = nwis_reservoirs$sitename, parameterCd = '00010') 
res_out <- dataRetrieval::renameNWISColumns(res) %>%
  select(sitename = site_no, dateTime, contains('Wtemp_Inst'), tz_cd) %>%
  tidyr::pivot_longer(
    cols = contains('Wtemp'),
    names_to = c('location_info', '.value'),
    names_pattern = '(.*Wtemp)_(.*)',
    values_drop_na = TRUE) %>%
  rename(temp_degC = Inst, temp_cd = Inst_cd) %>%
  mutate(date = format(dateTime, '%Y-%m-%d'), 
         time = format(dateTime, '%H:%M')) %>%
  select(-dateTime)

# pull reservoir data from QW
res_qw <- dataRetrieval::readNWISqw(siteNumbers = nwis_reservoirs$sitename, 
                                    parameterCd = '00010') 
res_qw_out <- res_qw %>%
  select(sitename = site_no, date = sample_dt, time = sample_tm, tz_cd = sample_start_time_datum_cd_reported,
         temp_degC = result_va, temp_cd = remark_cd) %>%
  mutate(date = as.character(date))

  
############## EcoSHEDS Data ##############
# ecosheds sites
ecosheds_sites <- filter(sites_s, grepl('ecosheds', source))
# manually retrieved data from these 6 sites from http://db.ecosheds.org/viewer
ecosheds_dat <- readr::read_csv('data/in/ecosheds_sites/values.csv') %>%
  mutate(sitename = paste(agency_name, location_name, sep = '-'),
         location_info = NA) %>%
  select(sitename, dateTime = datetime, temp_degC = `temperature (degC)`, temp_cd = flags, tz_cd = timezone) %>%
  mutate(date = as.character(as.Date(dateTime, format = '%m/%d/%Y')),
         time = format(as.POSIXct(dateTime, format = '%m/%d/%Y %H:%M'), '%H:%M')) %>%
  select(-dateTime)

############## NorWest Data ###############
# norwest sites (4)
# sub-daily norwest data not available from website

############## WQP Data ###################
# WQP sites
wqp_sites <- filter(sites_s, grepl('wqp', source))
wqp_dat <- dataRetrieval::readWQPdata(siteid = wqp_sites$sitename,
                       characteristicName = c("Temperature","Temperature, sample","Temperature, water","Temperature, water, deg F"))

# sites with date/time combined
wqp_out <- wqp_dat %>%
  filter(!is.na(ActivityStartTime.Time)) %>%
  mutate(tz_cd = 'UTC') %>%
  select(sitename = MonitoringLocationIdentifier, dateTime = ActivityStartDateTime, temp_degC = ResultMeasureValue, 
         temp_cd = MeasureQualifierCode, tz_cd) %>%
  filter(!is.na(temp_degC)) %>%
  mutate(date = format(dateTime, '%Y-%m-%d'), 
         time = format(dateTime, '%H:%M')) %>%
  select(-dateTime)

# combine it all
out <- bind_rows(out_data, res_out, res_qw_out, ecosheds_dat, wqp_out) %>%
  select(sitename, date, time, tz_cd, temp_degC, temp_cd)

# how many sites did we capture?
length(unique(sites$sitename)) # 177 candidate sites
length(unique(out$sitename)) # 160 sites that returned data

# write it out
readr::write_csv(out, 'data/out/subdaily_temperature_select_sites.csv')

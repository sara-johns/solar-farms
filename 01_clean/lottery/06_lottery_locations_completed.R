# Plot locations of solar farms, merge to completed farms
# Sara Johns
# sjohns@berkeley.edu
# Date created: 7/10/2023
# Date last modified: 7/10/2023

# 0. Set up

# Clear workspace
rm(list = ls())

# Load packages
library(pacman)
p_load(tidyverse, readxl, Hmisc, glue, foreign, stringr, data.table, sf,
       lubridate, ggpubr, qs, rgdal, zoo, dplyr, tidyr, measurements)

# Directory 
directory <- "/Users/sarajohns/Google Drive/My Drive/Farmers_Solar/data/"

# Set spatial defaults
projdeg <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
projaea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

#--------------------------------------------------------------------------------------------#
# 1. Get data
#--------------------------------------------------------------------------------------------#

# get all lottery projects
lottery <- fread(paste0(directory, 'ipa_lottery/processed/final_lottery_locations_with_co_town_zip.csv'))
lottery[, V1 := NULL]
lottery <- unique(lottery)
colnames(lottery) <- c("app_id", "project_name", "vendor", "group", "lat", "lon", "size_kw_ac", "town", "zip",
                       "county_name", "address", "ordinal", "lottery_status", "block", "queue_number", "small_subscribers",
                       "formatted_address", "accuracy", "type", "clean_add", "man_update", "lat_deg_dec_min", "lon_deg_dec_min")
lottery[, c("queue_number", "small_subscribers", "accuracy", "type", "clean_add", "lat_deg_dec_min", "lon_deg_dec_min") := NULL]

# get wide version of counties (multiple matches to zip code in some cases)
app_county <- unique(lottery[, c("app_id", "county_name")])
app_county[, count := sequence(.N), by = c('app_id')]
app_county <- dcast(app_county,  app_id ~ count, value.var = "county_name")
colnames(app_county) <- c("app_id", "county1", "county2", "county3", "county4")

lottery[, county_name := NULL]
lottery <- unique(lottery)

lottery <- merge(lottery, app_county, by = "app_id")

# get completed projects
completed <- data.table(read_excel(paste0(directory, "ipa_lottery/raw/PartII_Application_Completed_Projects.xlsx")))
# only want community solar
completed <- completed[Category=="CS",]
colnames(completed) <- c("app_id", "vendor_completed", "installer", "part2_app_date", "group_completed", "category",
                         "ceja_category", "block_completed", "size_kw_ac_completed", "size_kw_dc", "address_completed",
                         "suite_apt", "city", "state", "zip_completed", "county_completed", "icc_approval_date", "contract_effective_date",
                         "part2_app_verif", "online_date", "counterparty_util", "rec_system", "rec_id", "q_rec_contracted", "rec_price",
                         "rec_contract_value", "prev_wage_req", "financing_structure", "capacity_factor")
completed[, c("category", "ceja_category", "suite_apt", "prev_wage_req", "financing_structure", "group_completed") := NULL]
completed[, complete := 1]

# merge lottery and completed
lottery_completed <- merge(lottery, completed, by="app_id", all.x=T)
lottery_completed[is.na(complete), complete := 0]

# three completed projects have different zip codes/towns than reported on completed, all very close, use completed
lottery_completed[app_id %in% c(1027, 1500, 4846), zip := zip_completed]
lottery_completed[app_id %in% c(1027, 1500, 4846), town := city]


#--------------------------------------------------------------------------------------------#
# 2. Map to zip code
#--------------------------------------------------------------------------------------------#

# Zip code shapefile
# Source: https://cartographyvectors.com/map/1584-illinois-zip-codes

il_zips <- st_read(paste0(directory, "shapefile/illinois-zip-codes-_1584.geojson"),
                   stringsAsFactors = FALSE)
# Don't project because it makes the map plot tilted
#il_zips <- st_transform(il_zips, projaea)

il_zips$ZCTA5CE10 <- as.numeric(il_zips$ZCTA5CE10)

# Get project counts by zip code
proj_zip <- lottery_completed[,. (count = .N,
                                  completed = sum(complete)), 
                              by = "zip"]
proj_zip[, diff := count - completed]

# Merge to spatial data
il_zip_proj <- merge(il_zips, proj_zip, by.x="ZCTA5CE10", by.y="zip", all.x=T)
il_zip_proj[is.na(il_zip_proj$count), "count"] <- 0
il_zip_proj[is.na(il_zip_proj$complete), "completed"] <- 0

ggplot() +
  geom_sf(data = il_zip_proj, aes(fill=count), color ="gray50", size= 0.05) + 
  scale_fill_viridis_c(option="magma", direction=-1) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title=element_text(size=11, face = "bold"),
        legend.text=element_text(size=10, face = "bold"))

ggplot() +
  geom_sf(data = il_zip_proj, aes(fill=completed), color ="gray50", size= 0.05) + 
  scale_fill_viridis_c(option="magma", direction=-1) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title=element_text(size=11, face = "bold"),
        legend.text=element_text(size=10, face = "bold"))


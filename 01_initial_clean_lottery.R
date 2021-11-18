# Initial cleaning of lottery excel sheet 
# Sara Johns
# sjohns@berkeley.edu
# 9/8/2021

# 0. Set up

# Clear workspace
rm(list = ls())

# Load packages
library(pacman)
p_load(tidyverse, readxl, Hmisc, glue, foreign, stringr, data.table, 
       lubridate, ggpubr, qs, rgdal, zoo, dplyr, tidyr, measurements)

# Directory 
directory <- "/Users/sarajohns/Google Drive/My Drive/Farmers_Solar/data/ipa_lottery/"

#--------------------------------------------------------------------------------------------#
# 1. Get data
#--------------------------------------------------------------------------------------------#

group_a <- data.table(read_excel(paste0(directory, "raw/Lottery-Results-CS-A-Report-Corrected-4.12.19-1.xlsx")))
group_b <- data.table(read_excel(paste0(directory, "raw/Lottery-Results-CS-B-Report.xlsx")))

# separate locations that are already lat/lon vs those that are addresses to be geocoded
group_a_coord <- group_a[(grepl("°", Address) | !(is.na(str_match(group_a$Address, "([0-9]{2}\\.[0-9]{5,6})")[,1]))),]
group_a_add <- group_a[!(grepl("°", Address)) & (is.na(str_match(group_a$Address, "([0-9]{2}\\.[0-9]{5,6})")[,1])),]

group_b_coord <- group_b[(grepl("°", Address) | !(is.na(str_match(group_b$Address, "([0-9]{2}\\.[0-9]{5,6})")[,1]))),]
group_b_add <- group_b[!(grepl("°", Address)) & (is.na(str_match(group_b$Address, "([0-9]{2}\\.[0-9]{5,6})")[,1])),]

#--------------------------------------------------------------------------------------------#
# 2. Clean coordinates
#--------------------------------------------------------------------------------------------#

clean_coord <- function(dt) {
  
  # split lat lon
  dt[, Address := str_replace(Address, ";", ",")]
  dt[, Address := str_replace(Address, "N ", "N,")]
  dt <- dt %>% separate(Address, c("lat", "lon", "town"), ",", extra = "merge", remove = F)
  dt[is.na(town), town := lon][lon==town, lon := NA]
  
  # mark coordinates already in decimal degrees
  dt[!(is.na(str_match(lat, "([0-9]{2}\\.[0-9]{4,6})")[,1])), lat_in_dec := 1]
  dt[!(is.na(str_match(lon, "([0-9]{2}\\.[0-9]{4,6})")[,1])), lon_in_dec := 1]
  
  # mark if in degree dec min
  dt[is.na(lat_in_dec) & !(grepl('"', lat) | grepl("''", lat)), lat_deg_dec_min := 1]
  dt[is.na(lon_in_dec) & !(grepl('"', lon) | grepl("''", lon)), lon_deg_dec_min := 1]
  
  # convert the others to decimal degrees
  # get in format for conversion
  dt[is.na(lat_in_dec), lat := gsub("°", " ", lat)]
  dt[is.na(lat_in_dec), lat := gsub("'", " ", lat)]
  dt[is.na(lat_in_dec), lat := gsub("N", "", lat)]
  dt[is.na(lat_in_dec), lat := gsub('"', "", lat)]
  dt[is.na(lat_in_dec), lat := gsub("  ", " ", lat)]
  # convert
  dt[is.na(lat_in_dec) & is.na(lat_deg_dec_min), lat_dec := as.numeric(conv_unit(lat, from = "deg_min_sec", to = "dec_deg"))]
  dt[is.na(lat_in_dec) & lat_deg_dec_min==1, lat_dec := as.numeric(conv_unit(lat, from = "deg_dec_min", to = "dec_deg"))]
  
  # put coordinates already in decimal degrees in same column 
  dt[lat_in_dec == 1, lat_dec := as.numeric(gsub("[^0-9.-]", "", lat))]
  
  # same for longitude
  dt[is.na(lon_in_dec), lon := gsub("°", " ", lon)]
  dt[is.na(lon_in_dec), lon := gsub("'", " ", lon)]
  dt[is.na(lon_in_dec), lon := gsub("W", "", lon)]
  dt[is.na(lon_in_dec), lon := gsub('"', "", lon)]
  dt[is.na(lon_in_dec), lon := gsub('-', "", lon)]
  dt[is.na(lon_in_dec), lon := gsub("  ", " ", lon)]
  dt[is.na(lon_in_dec), lon := trimws(lon, which = "both")]
  dt[is.na(lon_in_dec) & is.na(lon_deg_dec_min), lon_dec := as.numeric(conv_unit(lon, from = "deg_min_sec", to = "dec_deg"))]
  dt[is.na(lon_in_dec) & lon_deg_dec_min==1, lon_dec := as.numeric(conv_unit(lon, from = "deg_dec_min", to = "dec_deg"))]
  dt[lon_in_dec == 1, lon_dec := as.numeric(gsub("[^0-9.-]", "", lon))]
  # make negative since in western hemisphere
  dt[lon_dec > 0, lon_dec := lon_dec * -1]
  
  # clean up columns
  dt[, lat := lat_dec][,lon := lon_dec]
  dt[, c("lat_in_dec", "lon_in_dec", "lat_dec", "lon_dec") := NULL]
  # weirdness with lat only columns so remove those
  dt[is.na(lon), lat := NA]
  
  return(dt)
}

group_a_coord <- clean_coord(group_a_coord)
group_b_coord <- clean_coord(group_b_coord)

#--------------------------------------------------------------------------------------------#
# 3. Clean addresses for best chance at geocoding
#--------------------------------------------------------------------------------------------#

clean_add <- function(dt) {
  
  # add IL to addresses
  dt[, Address := trimws(Address, which = "both")]
  dt[, Address := gsub("(.*)(.{6})$", "\\1, IL \\2", Address)]
  
  # remove common extraneous words
  dt[, clean_add := Address]
  dt[, clean_add := gsub("Address TBD, ", "", clean_add)]
  dt[, clean_add := gsub("address TBD, ", "", clean_add)]
  dt[, clean_add := gsub("Off ", "", clean_add)]
  dt[, clean_add := gsub("Approx. ", "", clean_add)]
  dt[, clean_add := gsub("Near ", "", clean_add)]
  dt[, clean_add := gsub("Across from ", "", clean_add)]
  dt[, clean_add := gsub("Corner of ", "", clean_add)]
  
  return(dt)
}

group_a_add <- clean_add(group_a_add)
group_b_add <- clean_add(group_b_add)

#--------------------------------------------------------------------------------------------#
# 4. Save datasets
#--------------------------------------------------------------------------------------------#

write_csv(group_a_add, paste0(directory, "processed/group_a_addresses.csv"))
write_csv(group_b_add, paste0(directory, "processed/group_b_addresses.csv"))

write_csv(group_a_coord, paste0(directory, "processed/group_a_coordinates.csv"))
write_csv(group_b_coord, paste0(directory, "processed/group_b_coordinates.csv"))

#--------------------------------------------------------------------------------------------#
# 5. Get list of developers and number of projects and save
#--------------------------------------------------------------------------------------------#

# combine groups
group_a[, group := "A"]
group_b[, group := "B"]
all_projects <- rbind(group_a, group_b)

# indicator for winning project
all_projects[, wins := ifelse(`Lottery Status` == "Accepted", 1, 0)]

# sum by developer (across groups)
developer_list <- all_projects[,. (winning_projects = sum(wins),
                                   total_projects = .N),
                               by = c("Name of Approved Vendor")]
developer_list <- developer_list[order(-total_projects)]

write_csv(developer_list, paste0(directory, "processed/list_of_developers.csv"))

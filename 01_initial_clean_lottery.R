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
       lubridate, ggpubr, qs, rgdal, zoo, dplyr, tidyr)

# Directory 
directory <- "/Users/sarajohns/Google Drive/My Drive/Farmers_Solar/data/ipa_lottery/"

#--------------------------------------------------------------------------------------------#
# 1. Get data
#--------------------------------------------------------------------------------------------#

group_a <- data.table(read_excel(paste0(directory, "Lottery-Results-CS-A-Report-Corrected-4.12.19-1.xlsx")))
group_b <- data.table(read_excel(paste0(directory, "Lottery-Results-CS-B-Report.xlsx")))

# separate locations that are already lat/lon

group_a_coord <- group_a[(grepl("°", Address) | !(is.na(str_match(group_a$Address, "([0-9]{2}\\.[0-9]{6})")[,1]))),]
group_a_add <- group_a[!(grepl("°", Address)) & (is.na(str_match(group_a$Address, "([0-9]{2}\\.[0-9]{6})")[,1])),]

group_b_coord <- group_b[(grepl("°", Address) | !(is.na(str_match(group_b$Address, "([0-9]{2}\\.[0-9]{6})")[,1]))),]
group_b_add <- group_b[!(grepl("°", Address)) & (is.na(str_match(group_b$Address, "([0-9]{2}\\.[0-9]{6})")[,1])),]

#--------------------------------------------------------------------------------------------#
# 2. Clean coordinates
#--------------------------------------------------------------------------------------------#

clean_coord <- function(dt) {
  dt[, Address := str_replace(Address, ";", ",")]
  dt[, Address := str_replace(Address, "N ", "N,")]
  dt <- dt %>% separate(Address, c("lat", "lon", "town"), ",", extra = "merge", remove = F)
  dt[is.na(town), town := lon][lon==town, lon := NA]
  return(dt)
}

group_a_coord <- clean_coord(group_a_coord)
group_b_coord <- clean_coord(group_b_coord)
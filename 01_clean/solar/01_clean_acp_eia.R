# Cleaning of ACP/EIA solar data
# Sara Johns
# sjohns@berkeley.edu
# Created: 08/29/2022
# Last modified: 08/30/2022

# 0. Set up

# Clear workspace
rm(list = ls())

# Load packages
library(pacman)
p_load(tidyverse, readxl, Hmisc, glue, foreign, stringr, data.table, 
       lubridate, zoo, dplyr, tidyr)

# Directory 
directory <- "/Users/sarajohns/Google Drive/My Drive/Farmers_Solar/data/"

#--------------------------------------------------------------------------------------------#
# 1. Get data
#--------------------------------------------------------------------------------------------#

# American Clean Power data
acp <- fread(paste0(directory, "ACP/ACP_IL_Solar_20220803.csv"))

# EIA production data
eia_2020 <- data.table(read_excel(paste0(directory, "EIA/f923_2020/EIA923_Schedules_2_3_4_5_M_12_2020_Final_Revision.xlsx")))
eia_2020[, year := 2020]

eia_2021 <- data.table(read_excel(paste0(directory, "EIA/f923_2021er/EIA923_Schedules_2_3_4_5_M_12_2021_Early_Release.xlsx"), skip = 6))
eia_2021[, year := 2021]

# Clean data, reshape to plant-month observations
clean_dts <- function(i) {
  
  dt <- i
  dt <- dt[, c(1,4:9,11:16,80:91,98)] # cols we want
  colnames(dt) <- c("plant_id_eia", "plantname", "operator", "operatorid", "state", "censusregion", "nerc", "naics", "eia_sec",
                    "sec_name", "prime_mover", "fueltype", "aer_fueltype", "jan_gen", "feb_gen", "mar_gen", "apr_gen", 
                    "may_gen", "jun_gen", "jul_gen", "aug_gen", "sep_gen", "oct_gen", "nov_gen", "dec_gen", "year") # label
  dt <- dt[!is.na(as.numeric(as.character(dt$plant_id_eia))),] # get rid of excess rows
  # keep IL solar
  dt <- dt[state=="IL" & fueltype=="SUN",]
  
  # make numeric
  num_cols <- c("plant_id_eia", "operatorid","jan_gen", "feb_gen", "mar_gen", "apr_gen", 
                "may_gen", "jun_gen", "jul_gen", "aug_gen", "sep_gen", 
                "oct_gen", "nov_gen", "dec_gen", "year")
  dt <- dt[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]
  dt[, year := round(year, 0)]
  
  return(dt)
  
}

eia_list <- lapply(list(eia_2020, eia_2021), clean_dts)
eia_il_solar <- rbindlist(eia_list)

eia_il_solar[, annual_gen := rowSums(eia_il_solar[,jan_gen:dec_gen], na.rm=T)]
# get monthly
eia_il_solar <- melt(eia_il_solar, id.vars = c("plant_id_eia", "plantname", "operator", "operatorid", "state",
                                             "censusregion", "nerc", "naics", "eia_sec", "sec_name", "prime_mover",
                                             "fueltype", "aer_fueltype", "year", "annual_gen"),
                        measure.vars = c("jan_gen", "feb_gen", "mar_gen", "apr_gen", 
                                         "may_gen", "jun_gen", "jul_gen", "aug_gen", "sep_gen", 
                                         "oct_gen", "nov_gen", "dec_gen"))

months <- data.table(month = c("jan", "feb", "mar", "apr", "may", "jun",
                               "jul", "aug", "sep", "oct", "nov", "dec"),
                     num_month = c(1:12))
for (i in 1:nrow(months)) {
  eia_il_solar[grep(months$month[i], variable), month := months$num_month[i]]
}
eia_il_solar[,gen_mwh := value]
eia_il_solar[, c("variable", "value") := NULL]

# EIA generator data
eia_gen <- data.table(read_excel(paste0(directory, "EIA/eia8602021ER/3_3_Solar_Y2021_Early_Release.xlsx"), skip = 2))
eia_il_gen <- eia_gen[State=="IL",]

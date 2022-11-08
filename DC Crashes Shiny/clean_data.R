###############
#### setup ####
###############

pacman::p_load(data.table, lubridate, ggplot2, sf)

# data source and information: 
# https://ddotwiki.atlassian.net/wiki/spaces/GIS0225/pages/2053603429/Crash+Data
# https://www.arcgis.com/sharing/rest/content/items/70392a096a8e431381f1f692aaa06afd/info/metadata/metadata.xml?format=default&output=html

setwd("/Users/katie/Documents/R Exploration/DC Car Crashes/")
crashes <- fread("Data/Crashes_in_DC.csv")

crashes.sub <- crashes[1:10000]

# create a couple of variables
crashes[,TOTAL_FATALITIES := FATAL_BICYCLIST + FATAL_DRIVER + FATALPASSENGER + FATAL_PEDESTRIAN]
crashes[,TOTAL_MINOR_INJURIES := MINORINJURIES_BICYCLIST + MINORINJURIES_DRIVER + MINORINJURIES_PEDESTRIAN + MINORINJURIESPASSENGER]
crashes[,TOTAL_MAJOR_INJURIES := MAJORINJURIES_BICYCLIST + MAJORINJURIES_DRIVER + MAJORINJURIES_PEDESTRIAN + MAJORINJURIESPASSENGER]

dt <- crashes[,c("LATITUDE", "LONGITUDE", "WARD", "REPORTDATE", "SPEEDING_INVOLVED",
                 "TOTAL_BICYCLES", "TOTAL_VEHICLES", "TOTAL_PEDESTRIANS",
                 "TOTAL_MINOR_INJURIES","TOTAL_MAJOR_INJURIES","TOTAL_FATALITIES")]

#######################
#### Data Cleaning ####
#######################

dt <- dt[REPORTDATE != ""] # no blank report dates

# subset lat/lon to metadata specifications
# one point had lon out of range
dt <- dt[LONGITUDE < -76.812535 & LONGITUDE > -77.225759
         ][LATITUDE < 39.000862 & LATITUDE > 38.786659]

# remove cases where no bikes, vehicles, pedestrians involved
explore <- crashes[TOTAL_BICYCLES == 0 & TOTAL_VEHICLES == 0 & TOTAL_PEDESTRIANS == 0]
dt <- dt[(TOTAL_BICYCLES > 0 | TOTAL_VEHICLES > 0 | TOTAL_PEDESTRIANS > 0)]

# make REPORTDATE a date type, create Year, Month fields
# +00 is a TZ reference
dt[,CLEANDATE := ymd_hms(gsub("\\+00", "", REPORTDATE))]
dt[, `:=` (YEAR = year(CLEANDATE),
           MONTH = month(CLEANDATE))]

# remove dates that make no sense
# date cannot be greater than date of data pull
# data collection  in pre-2009 years looks very bad so YEAR >= 2009 ?
dt <- dt[CLEANDATE <= ymd('22/11/03') & YEAR >= 2009] 

setorder(dt, CLEANDATE)

# metadata does not indicate what non-binary values of SPEEDING_INVOLVED indicate
# so they are dropped
dt <- dt[SPEEDING_INVOLVED == 0 | SPEEDING_INVOLVED == 1]

# create indicator for bike/pedestrian/car involved
cols <- paste0(c("VEHICLE","BICYCLE","PEDESTRIAN"),"_INVOLVED")
dt[,(cols) := lapply(.SD, function(x) {ifelse(x > 0, 1, 0)}), 
   .SDcols = c("TOTAL_VEHICLES","TOTAL_BICYCLES","TOTAL_PEDESTRIANS")]

###########################
#### Descriptive Stats ####
###########################

# there are a minority of cases where no vehicle is involved; makes sense
no.cars <- dt[TOTAL_VEHICLES == 0, .N, by = c("PEDESTRIAN_INVOLVED","BICYCLE_INVOLVED")]

# look at total crashes/year
# 2022 seems very incomplete despite data update in Nov 22
annual.crashes <- dt[VEHICLE_INVOLVED==1, .(TOTAL_CRASHES = .N), by = c("YEAR")]

###############################
#### Prepare for Shiny App ####
###############################

# drop to just car accidents involving bicycle or pedestrian - manageable size to plot + explore
dt <- dt[VEHICLE_INVOLVED & (PEDESTRIAN_INVOLVED == 1 | BICYCLE_INVOLVED == 1)]

# make SPEEDING INVOLVED discrete w/ yes/no
dt[,SPEEDING_INVOLVED := as.character(SPEEDING_INVOLVED)]
dt[SPEEDING_INVOLVED == "1", SPEEDING_INVOLVED := "Speeding Involved"]
dt[SPEEDING_INVOLVED == "0", SPEEDING_INVOLVED := "No Speeding Involved"]

# create groups covered variable
dt[BICYCLE_INVOLVED == 1 & PEDESTRIAN_INVOLVED == 0, GROUP_INVOLVED := "Cyclists Only"]
dt[BICYCLE_INVOLVED == 0 & PEDESTRIAN_INVOLVED == 1, GROUP_INVOLVED := "Pedestrians Only"]
dt[is.na(GROUP_INVOLVED), GROUP_INVOLVED := "Both Pedestrians and Cyclists"]

# save
fwrite(dt, "Data/Clean_Crashes.csv")

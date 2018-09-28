#Data Wrangling and Analysis
#Script to import data and clean it up

library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)
library(ggplot2)

#read in the data
allcatch <- read.csv("allcatch2001-2018.csv", header = TRUE) %>%
  mutate(Station = factor(Station, levels = c("220", "218", "214", "230", "231")))
temp_salin <- read.csv("tempsalin2001-2018.csv", header = TRUE) %>%
  mutate(Station = factor(Station, levels = c("220", "218", "214", "230", "231")))
str(allcatch)
#fix dates
allcatch$EndDate <- ymd(as.POSIXct(allcatch$EndDate, format = "%m/%d/%Y"))
temp_salin$Date <- ymd(as.POSIXct(temp_salin$Date, format = "%m/%d/%Y"))

allcatch <- allcatch %>% add_column(Month = month(allcatch$EndDate), .after = 2)
temp_salin <- temp_salin %>% add_column(Month = month(temp_salin$Date), .after = 2)


# Separate out the temp and salinity. Drop the columns that are irrelevant
# Put data into 'long' format. NOTE: They used a slightly different definition 
# of the bottom in early years ('bottom 1.5'). This is analogous to 'bottom'
watertemps <- temp_salin %>% dplyr::select(-c(Salin_Top, Salin_Mid, Salin_Bot, Salin_Bot_1.5)) 
watersalin <- temp_salin %>% dplyr::select(-c(Temp_Top, Temp_Mid, Temp_Bot, Temp_Bot_1.5)) 
  
# if needed for later: watersalin %>% gather(depth, salin_ppt, -c(Year, Date, Month, Station))

rm(temp_salin) #optional but cleans up environment


##### WIND #####
# wind and air temp data from NOAA NCEI
# https://www.ncdc.noaa.gov/cdo-web/datasets/LCD/stations/WBAN:27406/detail
# https://www1.ncdc.noaa.gov/pub/data/cdo/documentation/LCD_documentation.pdf
# Station WBAN:27406
# These data were downloaded in 10 year increments (LCD CSV). 
# In appendix (at end of this doc, I summarized hourly data into a daily summary)


deadhorsewind <- read.csv("deadhorsewind_2001-2018_daily.csv", header = TRUE, 
                          stringsAsFactors = FALSE) %>% 
  mutate(Date = ymd(as.POSIXct(Date, format = "%m/%d/%Y")),
         month = month(Date),
         dailymeanspeed = dailymeanspeed * 1.60934) %>%
  rename(dailymeanspeed_kph = dailymeanspeed) %>%
  filter(month == 7 | month == 8) %>%
  dplyr::select(Date, month, everything()) # reorder month column




##### DISCHARGE #####
# Data from USGS https://waterdata.usgs.gov/nwis/uv/?site_no=15908000
# https://nwis.waterdata.usgs.gov/usa/nwis/uv/?cb_00060=on&format=rdb&site_no=15908000&period=&begin_date=2001-01-01&end_date=2018-09-01
sagdisch <- read.csv("SagDischargeDaily_2001-2018.csv", header = TRUE) %>%
  mutate(datetime = as.POSIXct(paste0(date, " ", time), format = "%m/%d/%Y %H:%M"),
         Date = as_date(datetime),
         hour = hour(datetime) ) %>%
  dplyr::select(Date, hour, disch_cfs) %>%
  group_by(Date) %>% summarize(meandisch_cfs = mean(disch_cfs, na.rm = TRUE))




catchenviron <- left_join(allcatch, watersalin %>% dplyr::select(-c(Year, Month)), 
                          by = c("EndDate" = "Date", "Station" = "Station")) %>%
  left_join(watertemps %>% dplyr::select(-c(Year, Month)), 
            by = c("EndDate" = "Date", "Station" = "Station"))


##########################################
#APPENDIX
# The daily wind data was created using the following code:
# windhourly <- read.csv("../../Slope Project/Data/deadhorsewind_2001-2018_hourly.csv", header = TRUE, 
#          stringsAsFactors = FALSE) %>%
#   select(DATE, HOURLYWindSpeed, HOURLYWindDirection) %>% #remove extraneous data
#   rename(Date = DATE,
#          windhrly_mph = HOURLYWindSpeed,
#          windhrly_dir = HOURLYWindDirection) %>% 
#   mutate(Date = ymd(as.POSIXct(Date, format = "%m/%d/%Y")),
#          month = month(Date)) %>%
#   mutate_if(is.character, as.numeric) %>% select(Date, month, everything())
# 
# library(CircStats)
# winddaily <- windhourly %>% mutate(Year = year(Date)) %>% group_by(Date) %>% 
#   summarise(dailymeanspeed = mean(windhrly_mph, na.rm = TRUE),
#             dailymeandir = ((circ.mean(2*pi*na.omit(windhrly_dir)/360))*(360 / (2*pi))) %%360 )
# 
# write.csv(winddaily, file="deadhorsewind_2001-2018.csv")

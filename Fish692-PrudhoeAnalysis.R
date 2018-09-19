#Data Wrangling and Analysis

library(dplyr)
library(tidyr)

#read in the data
allcatch <- read.csv("allcatch2001-2018.csv", header = TRUE)
temp_salin <- read.csv("environ2001-2018.csv", header = TRUE)

#Separate out the temp and salinity. Drop the columns that are irrelevant
watertemps <- temp_salin %>% select(-c(Salin_Top, Salin_Mid, Salin_Bot, Salin_Bot_1.5))
watersalin <- temp_salin %>% select(-c(Temp_Top, Temp_Mid, Temp_Bot, Temp_Bot_1.5))

rm(temp_salin) #optional but cleans up environment


# wind and air temp data from NOAA NCEI
# https://www.ncdc.noaa.gov/cdo-web/datasets/LCD/stations/WBAN:27406/detail
# Station WBAN:27406
# These data were downloaded in 10 year increments (LCD CSV). 
# Then in Excel, I removed the hourly rows and left just the daily summary
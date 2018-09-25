#Data Wrangling and Analysis

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
watertemps <- temp_salin %>% select(-c(Salin_Top, Salin_Mid, Salin_Bot, Salin_Bot_1.5)) 
watersalin <- temp_salin %>% select(-c(Temp_Top, Temp_Mid, Temp_Bot, Temp_Bot_1.5)) 
  
# if needed for later: watersalin %>% gather(depth, salin_ppt, -c(Year, Date, Month, Station))

rm(temp_salin) #optional but cleans up environment


##### WIND #####
# wind and air temp data from NOAA NCEI
# https://www.ncdc.noaa.gov/cdo-web/datasets/LCD/stations/WBAN:27406/detail
# https://www1.ncdc.noaa.gov/pub/data/cdo/documentation/LCD_documentation.pdf
# Station WBAN:27406
# These data were downloaded in 10 year increments (LCD CSV). 
# Then in Excel, I removed the hourly rows/columns and left just the daily summary


deadhorsewind <- read.csv("deadhorsewind_2001-2018_daily.csv", header = TRUE, 
                          stringsAsFactors = FALSE) %>%
  select(DATE, DAILYAverageWindSpeed, DAILYPeakWindSpeed, PeakWindDirection, 
         DAILYSustainedWindSpeed, DAILYSustainedWindDirection) %>% #remove extraneous data
  rename(Date = DATE,
         meanwindmph = DAILYAverageWindSpeed,
         peakwindmph = DAILYPeakWindSpeed,
         peakwinddir = PeakWindDirection,
         sustwindmph = DAILYSustainedWindSpeed,
         sustwinddir = DAILYSustainedWindDirection) %>% 
  mutate(Date = ymd(as.POSIXct(Date, format = "%m/%d/%Y")),
         month = month(Date)) %>%
  mutate_if(is.character, as.numeric) %>% select(Date, month, everything()) # reorder month column




##### DISCHARGE #####
# Data from USGS https://waterdata.usgs.gov/nwis/uv/?site_no=15908000
# https://nwis.waterdata.usgs.gov/usa/nwis/uv/?cb_00060=on&format=rdb&site_no=15908000&period=&begin_date=2001-01-01&end_date=2018-09-01
sagdisch <- read.csv("SagDischargeDaily_2001-2018.csv", header = TRUE) %>%
  mutate(datetime = as.POSIXct(paste0(date, " ", time), format = "%m/%d/%Y %H:%M"),
         Date = as_date(datetime),
         hour = hour(datetime) ) %>%
  select(Date, hour, disch_cfs) %>%
  group_by(Date) %>% summarize(meandisch_cfs = mean(disch_cfs, na.rm = TRUE))







catchenviron <- left_join(allcatch, watersalin %>% select(-c(Year, Month)), 
                          by = c("EndDate" = "Date", "Station" = "Station")) %>%
  left_join(watertemps %>% select(-c(Year, Month)), 
            by = c("EndDate" = "Date", "Station" = "Station"))



###########################
### Explore Response Variables ####
ggplot(allcatch %>% filter(Species=="ARCS" & totcount>0), aes(x=(totcount))) + geom_histogram(bins = 30)
ggplot(allcatch %>% filter(Species=="ARCD" & totcount>0), aes(x=totcount)) + geom_histogram(bins = 30)
ggplot(allcatch %>% filter(Species=="BDWF" & totcount>0), aes(x=totcount)) + geom_histogram(bins = 30)
ggplot(allcatch %>% filter(Species=="BDWF" & totcount>0), aes(x=log(totcount))) + geom_histogram(bins = 30)
# note the slightly better distribution with the log transform
ggplot(allcatch %>% filter(Species=="LSCS" ), aes(x=(day.of.year), y=totcount)) + geom_point() +
  facet_wrap(~Year, scales = "free")
# summary: most catches are of zeros or very low catch abundance, very right skewed


###########################
### Explore Explanatory Variables ####

ggplot(watersalin %>% group_by(Year) %>% 
         summarise(annsal=mean(c(Salin_Top, Salin_Mid, Salin_Bot), na.rm = TRUE)),
       aes(x=Year, y=annsal)) + geom_line()

ggplot(watertemps %>% group_by(Year) %>% 
        summarise(anntemp=mean(c(Temp_Top, Temp_Mid, Temp_Bot), na.rm = TRUE)),
      aes(x=Year, y=anntemp)) + geom_line()

#show how salinity has been changing
plottext_salin <- data.frame(label = c(
    paste0("annual change: ", signif(coef(summary(lm(
      Salin_Mid ~ Year, data=watersalin %>% filter(Station == 214))))[2,1], 2)),
    paste0("annual change: ", signif(coef(summary(lm(
      Salin_Mid ~ Year, data=watersalin %>% filter(Station == 218))))[2,1], 2)),
    paste0("annual change: ", signif(coef(summary(lm(
      Salin_Mid ~ Year, data=watersalin %>% filter(Station == 220))))[2,1], 2)),
    paste0("annual change: ", signif(coef(summary(lm(
      Salin_Mid ~ Year, data=watersalin %>% filter(Station == 230))))[2,1], 2)) ),
  Station   = c(214, 218, 220, 230) )

ggplot(watersalin %>% filter(Station != 231), aes(as.factor(Year), Salin_Mid)) + 
  geom_boxplot() + scale_y_continuous(limits = c(0,35)) +
  facet_wrap(~Station, nrow=2) +
  geom_smooth(method="lm", aes(group=1)) +
  geom_text(data = plottext_salin, 
            mapping = aes(x=-Inf, y=-Inf, label = label),
            hjust = -0.2, vjust = -15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# salinity has significantly dropped for the eastern sites (230&214) 
# and increased at the western sites (220&218)


#show how temps have been increasing:
plottext_temp <- data.frame(label = c(
  paste0("annual change: ", signif(coef(summary(lm(
    Temp_Mid ~ Year, data=watertemps %>% filter(Station == 214))))[2,1], 2)),
  paste0("annual change: ", signif(coef(summary(lm(
    Temp_Mid ~ Year, data=watertemps %>% filter(Station == 218))))[2,1], 2)),
  paste0("annual change: ", signif(coef(summary(lm(
    Temp_Mid ~ Year, data=watertemps %>% filter(Station == 220))))[2,1], 2)),
  paste0("annual change: ", signif(coef(summary(lm(
    Temp_Mid ~ Year, data=watertemps %>% filter(Station == 230))))[2,1], 2)) ),
  Station   = c(214, 218, 220, 230) )

ggplot(watertemps %>% filter(Station != 231), aes(as.factor(Year), Temp_Mid)) + 
  geom_boxplot() + scale_y_continuous(limits = c(0,16)) +
  facet_wrap(~Station, nrow=2) +
  geom_smooth(method="lm", aes(group=1)) +
  geom_text(data = plottext_temp, 
            mapping = aes(x=-Inf, y=-Inf, label = label),
            hjust = -0.2, vjust = -15) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#all 4 sites have had a significant increase in temperatures



#Wind 
ggplot(deadhorsewind %>% filter(month==7 | month==8), aes(x=sustwinddir)) + 
  geom_histogram(col="black", bins = 40) + coord_polar()




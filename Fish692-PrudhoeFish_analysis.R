#Data Wrangling and Analysis
#Script to run models and analysis

library(dplyr)
library(tidyr)
library(vegan)

source('Fish692-PrudhoeFish_dataimport.R')

# turn data into a 'wide' catch matrix (Species by Year/Station)
catchmatrix <- catchenviron %>% group_by(Year, Station, Species) %>% summarise(anncount = sum(totcount)) %>%
  spread(Species, value = anncount) %>% replace(., is.na(.), 0) %>% ungroup()
catchmatrix$Station[catchmatrix$Station == 231] <- 214 # Treat the 231 Station as the precursor to 214




catchmatrix$Station <- as.numeric(catchmatrix$Station) #cheat to include it in shortcut 
catchmatrix <- catchmatrix[, which(colSums(catchmatrix) > 100)] # set this to zero to include all
# right now analysis is for only species >100 fish, all years combined



# standardize catches 0 to 1 (1 is max catch in a given year/station)
catchmatrix.std <- catchmatrix 
for (i in 3:ncol(catchmatrix.std)){
  catchmatrix.std[i] <- catchmatrix[i]/max(catchmatrix[i])}



### PERMANOVA ###
# create two grouping factors: one to cover 4 sites for 9 years, and again for a second time period
earlylateyrs <- gl(2,36) # Creates two groups of 36. 72 is b/c 4 sites for 18 years. 
stationdiffs <- gl(4,1,72)
stationdiffs[3] <- 4  #fix the ordering issue in 2001 b/c we had 231 this year
stationdiffs[4] <- 3
allyrs <- gl(18,4,72)

adonis2(catchmatrix.std %>% select(-c(Year, Station)) ~ stationdiffs + allyrs, perm = 9999)



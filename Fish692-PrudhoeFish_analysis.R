# !diagnostics off
#Data Wrangling and Analysis
#Script to run models and analysis
# !diagnostics off

library(dplyr)
library(tidyr)
library(vegan)
library(CircStats)

source('Fish692-PrudhoeFish_dataimport.R')

# turn data into a 'wide' catch matrix (Species by Year/Station)
catchmatrix <- catchenviron %>% group_by(Year, Station, Species) %>% summarise(anncount = sum(totcount)) %>%
  spread(Species, value = anncount) %>% replace(., is.na(.), 0) %>% ungroup()
catchmatrix$Station[catchmatrix$Station == 231] <- 214 # Treat the 231 Station as the precursor to 214

catchmatrix <- catchmatrix %>% arrange(Year, Station) #was out of order in 2001 after station name change


# Exclude rare species:
# catchmatrix$Station <- as.numeric(catchmatrix$Station) #cheat to include it in shortcut
# catchmatrix <- catchmatrix[, which(colSums(catchmatrix) > 100)] 

# Right now analysis is for only species >100 fish, all years combined. Change 100 to 0 to inc all spp
# the following code makes a list of the species to keep which have a threshold of 100 currently,
# then filters based on this list, then turns back into wide format
keepspp <- (catchmatrix %>% gather(Species, counts, -Year, -Station) %>%
  group_by(Species) %>% summarize(counts = sum(counts)) %>% filter(counts > 100))$Species
catchmatrix <- catchmatrix %>% gather(Species, counts, -Year, -Station) %>% filter(Species %in% keepspp) %>%
  spread(Species, value = counts)

rownames(catchmatrix) <- paste0(catchmatrix$Year, catchmatrix$Station)

pru.env.ann <- catchmatrix %>% dplyr::select(Year, Station)
catchmatrix <- catchmatrix %>% dplyr::select(-Year, -Station)

# standardize catches 0 to 1 (1 is max catch in a given year/station)
#note that the order corresponds to the now deleted Year/station combo. DON'T CHANGE ORDER
catchmatrix.std <- catchmatrix 
for (i in 3:ncol(catchmatrix.std)){ #starts at 3 to exclude Year and station cols
  catchmatrix.std[i] <- catchmatrix[i]/max(catchmatrix[i])}


#now set up environ dataframe to correspond to catch dataframe
pru.env.ann <- pru.env.ann %>% 
  left_join(deadhorsewind %>% mutate(Year = year(Date)) %>% group_by(Year) %>% 
              summarise(annwindspeed_kph = mean(dailymeanspeed_kph, na.rm = TRUE),
                        annwinddir = ((circ.mean(2*pi*na.omit(dailymeandir)/360))*(360 / (2*pi))) %%360 ),
                          by = c("Year" = "Year")) %>% 
  left_join(sagdisch %>% mutate(Year = year(Date), month = month(Date)) %>% 
              group_by(Year) %>% filter(month == 7 | month == 8) %>%
              summarise(anndisch_cfs = mean(meandisch_cfs, na.rm = TRUE)),by = c("Year" = "Year") ) %>% 
  left_join(watersalin %>% group_by(Year, Station) %>% summarise(annsal_ppt = mean(Salin_Mid, na.rm = TRUE)), 
            by = c("Year" = "Year", "Station" = "Station")) %>%
  left_join(watertemps %>% group_by(Year, Station) %>% summarise(anntemp_c = mean(Temp_Mid, na.rm = TRUE)), 
            by = c("Year" = "Year", "Station" = "Station"))



#################
### PERMANOVA ###
#################

# example from vegan tutorial, page 33
# http://cc.oulu.fi/~jarioksa/opetus/metodi/vegantutor.pdf
data(dune)
data(dune.env)
betadun <- betadiver(dune, "z")
adonis(betadun ~ Management, dune.env, perm=200)
str(betadun)
str(dune.env)
str(dune)

#now on my own. only works with a factor (station so far)
betad <- betadiver(catchmatrix.std, "z")
str(betad)
adonis(betad ~ anntemp_c*Station, pru.env.ann, perm=200) #won't run
str(catchmatrix.std)
str(pru.env.ann)

adonis(catchmatrix.std ~ anntemp_c , data=pru.env.ann, perm=200)



# create two grouping factors: one to cover 4 sites for 9 years, and again for a second time period
earlylateyrs <- gl(2,36) # Creates two groups of 36. 72 is b/c 4 sites for 18 years. 2001-09 vs 2010-18
stationdiffs <- gl(4,1,72) # levels are the stations
allyrs <- gl(18,4,72)

adonis(catchmatrix.std) ~ stationdiffs + allyrs, perm = 9999)



### BRAY-CURTIS DISTANCE
braydist <- vegdist(catchmatrix.std, method="bray")
# library(gplots)
# heatmap.2(as.matrix(braydist))
adonis(braydist ~ Year + Station, data = pru.env.ann, perm = 9999)

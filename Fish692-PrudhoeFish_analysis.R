# !diagnostics off
#Data Wrangling and Analysis
#Script to run models and analysis


library(dplyr)
library(tidyr)
library(vegan)

source('Fish692-PrudhoeFish_dataimport.R')
#this pulls in the following (relevant) dataframes:
head(catchenviron) # all catch data, left joined with environ data
head(pru.env.ann) #annual summary of environmental data


# now turn catch data into a 'wide' catch matrix (Species by Year/Station)
# drop enivron data too, just focusing on catch here
catchmatrix.all <- catchenviron %>% group_by(Year, Station, Species) %>% summarise(anncount = sum(totcount)) %>%
  spread(Species, value = anncount) %>% replace(., is.na(.), 0) %>% ungroup()
catchmatrix.all$Station[catchmatrix.all$Station == 231] <- 214 # Treat the 231 Station as the precursor to 214
catchmatrix.all <- catchmatrix.all %>% arrange(Year, Station) #was out of order in 2001 after station name change


# Exclude rare species:
# catchmatrix$Station <- as.numeric(catchmatrix$Station) #cheat to include it in shortcut
# catchmatrix <- catchmatrix[, which(colSums(catchmatrix) > 100)] 

# Right now analysis is for only species >100 fish, all years combined. Change 100 to 0 to inc all spp
# the following code makes a list of the species to keep which have a threshold of 100 currently,
# then filters based on this list, then turns back into wide format
keepspp <- (catchmatrix.all %>% gather(Species, counts, -Year, -Station) %>%
  group_by(Species) %>% summarize(counts = sum(counts)) %>% filter(counts > 100))$Species
catchmatrix <- catchmatrix.all %>% gather(Species, counts, -Year, -Station) %>% filter(Species %in% keepspp) %>%
  spread(Species, value = counts)

rownames(catchmatrix) <- paste0(catchmatrix$Year, catchmatrix$Station)

#pru.env.ann <- catchmatrix %>% dplyr::select(Year, Station)
catchmatrix <- catchmatrix %>% dplyr::select(-Year, -Station)
catchmatrix.all <- catchmatrix.all %>% dplyr::select(-Year, -Station)

# standardize catches 0 to 1 (1 is max catch in a given year/station)
#note that the order corresponds to the now deleted Year/station combo. DON'T CHANGE ORDER
catchmatrix.std <- catchmatrix 
for (i in 3:ncol(catchmatrix.std)){ #starts at 3 to exclude Year and station cols
  catchmatrix.std[i] <- catchmatrix[i]/max(catchmatrix[i])}

# make sure that 'catchmatrix' and catchmatrix.std are both set up in same order as pru.env.ann





#################
### PERMANOVA ###
#################

# following example from vegan tutorial, page 33
# http://cc.oulu.fi/~jarioksa/opetus/metodi/vegantutor.pdf

betad <- betadiver(catchmatrix.std, "z")   # using Arrhenius z measure of beta diversity 
adonis(betad ~ Year, pru.env.ann, perm=999) 
adonis(betad ~ Station, pru.env.ann, perm=999) 
adonis(betad ~ anntemp_c, pru.env.ann, perm=999) 
adonis(betad ~ annsal_ppt, pru.env.ann, perm=999) 
adonis(betad ~ anndisch_cfs, pru.env.ann, perm=999)     # not significant
adonis(betad ~ annwinddir_ew, pru.env.ann, perm=999)    # not significant
adonis(betad ~ annwindspeed_kph, pru.env.ann, perm=999) # not significant

adonis(betad ~ annsal_ppt + anntemp_c +  Station + Year, pru.env.ann, perm=999) 
#note that terms are sequential so order matters!

boxplot(betadisper(betad, pru.env.ann$Station ))
#note that the western and eastern sites look similar


adonis(catchmatrix.std ~ annsal_ppt + annwinddir_ew + annwindspeed_kph + 
         anndisch_cfs + anntemp_c, data=pru.env.ann, perm=999)



### BRAY-CURTIS DISTANCE
braydist <- vegdist(catchmatrix.std, method="bray")
boxplot(betadisper(betad, pru.env.ann$Year ))

# library(gplots)
# heatmap.2(as.matrix(braydist))
adonis(braydist ~ Year + Station, data = pru.env.ann, perm = 9999)




# explore remane diagram exploring how salinity affects diversity


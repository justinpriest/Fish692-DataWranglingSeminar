# !diagnostics off
#Data Wrangling and Analysis
#Script to run models and analysis


library(dplyr)
library(tidyr)
library(vegan)

source('Fish692-PrudhoeFish_dataimport.R')
#this pulls in the following (relevant) dataframes:
head(catchenviron) # all catch data, left joined with environ data
head(pru.env.day)  # environmental data by day
head(pru.env.ann)  # annual summary of environmental data


head(catchmatrix)     # each row is a year/station, cols are species
head(catchmatrix.std) # standardize the above 0-1 (percent of max catch)
head(catchmatrix.day) # each row is a day/station, cols are species
head(catchmatrix.day.std)


#################
### OBJECTIVE 1: Test for changes in community assemblage structure
#################


###############
## PERMANOVA ##

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


###############
### BRAY-CURTIS DISTANCE
braydist <- vegdist(catchmatrix.std, method="bray")
boxplot(betadisper(betad, pru.env.ann$Year ))

# library(gplots)
# heatmap.2(as.matrix(braydist))
adonis(braydist ~ Year + Station, data = pru.env.ann, perm = 9999)




#################
### OBJECTIVE 2: Quantify if / how changes are related to environmental variability
#################








#################
### OBJECTIVE 3: Assess nature of observed changes: linear trend, nonlinear trend, struc break
#################



# explore remane diagram exploring how salinity affects diversity?


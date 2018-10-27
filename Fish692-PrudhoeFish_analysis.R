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

################################
## ORDINATION & DISSIMILARITY ##



### BRAY-CURTIS DISTANCE
braydist <- vegdist(catchmatrix.std, method="bray") # This is a dissimilarity matrix

# library(gplots)
# heatmap.2(as.matrix(braydist))

totalNMDS <- metaMDS(braydist, k=3) # had to choose k=3 to get below 0.20

#Plot, put years by color, add environ vectors
plot(totalNMDS, display = "sites", type = "n", main = "Annual Results - colored by year grp")
text(totalNMDS, select=which(pru.env.ann$Year<2010), col="red")
text(totalNMDS, select=which(pru.env.ann$Year>2009), col="blue")
#unclear if any patterns

plot(totalNMDS, display = "sites", type = "n", main = "Annual Results - colored by station")
text(totalNMDS, select=which(pru.env.ann$Station==214), col="red")
text(totalNMDS, select=which(pru.env.ann$Station==218), col="blue")
text(totalNMDS, select=which(pru.env.ann$Station==220), col="black")
text(totalNMDS, select=which(pru.env.ann$Station==230), col="green")
#pretty distinct groupings by site




nmdspoints <- as.data.frame(totalNMDS$points[1:72,]) # 72 is the number of year/stn combos
#row.names(nmdspoints) <- 2001:2017
nmdspoints$YearStn <- rownames(totalNMDS$points)
nmdspoints$Year <- as.numeric(substr(nmdspoints$YearStn, 1, 4))
nmdspoints$Station <- factor(substr(nmdspoints$YearStn, 5, 7))
nmdspoints$earlymidlate <- ifelse(nmdspoints$Year < 2007, "early", 
                                                  ifelse(nmdspoints$Year >= 2013, "late", "mid"))
#this arbitrarily divides study in thirds, not the correct approach but good as EDA

ggplot(nmdspoints, aes(x=MDS1, y=MDS2)) + geom_point() +
  geom_text(aes(label=YearStn, color=Station),hjust=.35, vjust=-.7, size=3)+
  theme_bw() + theme(panel.grid.minor = element_blank()) 

ggplot(nmdspoints, aes(x=MDS1, y=MDS2)) + geom_point() +
  geom_text(aes(label=YearStn, color=Year),hjust=.35, vjust=-.7, size=3)+
  theme_bw() + theme(panel.grid.minor = element_blank()) 
# I used earlymidlate as a color too, but pattern was too slight


library(RColorBrewer)
mypal  <- colorRampPalette(brewer.pal(6, "Greens"))
mypal2 <- colorRampPalette(brewer.pal(6, "Greys"))
mypal3 <- colorRampPalette(brewer.pal(6, "Blues"))
mypal4 <- colorRampPalette(brewer.pal(6, "Reds"))

ggplot(nmdspoints, aes(x=MDS1, y=MDS2)) + geom_point() + 
  geom_text(aes(label=YearStn, color=interaction(Year, Station))) + 
  #scale_color_continuous(low = "#3fdeff", high = "#144f5b") +
  scale_colour_manual(values = c(mypal(18), mypal2(18), mypal3(18), mypal4(18))) +
  theme(legend.position = "none")


##########################
## nMDS axis regression ##

plot(MDS1 ~ Year, data = nmdspoints)
summary(lm(MDS1 ~ Year , data = nmdspoints)) # overall not significant
summary(lm(MDS2 ~ Year , data = nmdspoints)) # marginally significant

#now let's break it down by Station
library(broom)
nmdspoints %>% group_by(Station) %>% do(model = lm(MDS1 ~ Year, data = .)) %>% 
  tidy(model) # significant trend at Station 230, marginal at 220 
nmdspoints %>% group_by(Station) %>% do(model = lm(MDS2 ~ Year, data = .)) %>% 
  tidy(model) # none significant

# visualize this for MDS1
ggplot(nmdspoints, aes(x=Year, y =MDS1, color = Station)) + 
  geom_point() + geom_smooth(method = "lm", se=FALSE)


######### lets try with biweekly
braydist.biwk <- vegdist(catchmatrix.biwk.stdtrans, method="bray")
totalNMDS.biwk <- metaMDS(braydist.biwk, k=3) #not convergent with k=2

nmdspoints.biwk <- as.data.frame(totalNMDS.biwk$points[1:284,]) # 284 is the number of year/biwk/stn combos
nmdspoints.biwk$YearStn <- rownames(nmdspoints.biwk)
nmdspoints.biwk$Year <- as.numeric(substr(nmdspoints.biwk$YearStn, 1, 4))
nmdspoints.biwk$biweekly <- factor(substr(nmdspoints.biwk$YearStn, 5, 5))
nmdspoints.biwk$Station <- factor(substr(nmdspoints.biwk$YearStn, 6, 8))

ggplot(nmdspoints.biwk, aes(x=MDS1, y=MDS2)) + geom_point() +
  geom_text(aes(label=YearStn, color=Year),hjust=.35, vjust=-.7, size=3)+
  theme_bw() + theme(panel.grid.minor = element_blank()) 
# this is just a cluster, but maybe it's significant. Let's check in the next section


#################
### OBJECTIVE 2: Quantify if / how changes are related to environmental variability
#################

## Bioenv ##
# Bioenv is a mantel type test: which combination of environmental var explain it best
bioenv(braydist ~ annwindspeed_kph + annwinddir + anndisch_cfs + 
         annsal_ppt + anntemp_c, pru.env.ann)
#temp and salinity are the best predictors

bioenv(braydist.biwk ~ biwkmeanspeed_kph + biwkmeandir + meandisch_cfs + 
         Salin_Top + Temp_Top + winddir_ew, pru.env.biwk)
# Temp, salinity, and wind direction are best subset of env variables


## EnvFit ##

env.vectors.ann <- envfit(totalNMDS, pru.env.ann, permutations = 999)
env.vectors.ann#Year and Station centroids are significant
# also signif are temp, salin, and marginally wind direction

env.vectors.biwk <- envfit(totalNMDS.biwk, pru.env.biwk %>% 
                           dplyr::select(-winddir_ew, -Salin_Mid, -Temp_Mid), 
                           na.rm = TRUE, permutations = 999)
env.vectors.biwk # salin, wind, and year are signif





plot(totalNMDS) 
plot(env.vectors.ann, p.max = 0.02) # >pvals make plot too busy 
#way to busy to repeat for biweekly scale

ggplot(nmdspoints, aes(x=MDS1, y =MDS2)) + geom_point() +
  scale_x_continuous(limits = c(-1, 1)) + scale_y_continuous(limits = c(-1, 1)) +
  geom_segment(data = data.frame(env.vectors.ann$vectors$arrows), aes(x=0, xend=NMDS1, y=0, yend=NMDS2))
#these segments don't match the arrows from before because of automatic scaling. I think. not sure


ggplot(nmdspoints, aes(x=MDS1, y =MDS2)) + geom_point() +
  scale_x_continuous(limits = c(-0.43, 0.4)) + scale_y_continuous(limits = c(-0.4, 0.4)) +
  geom_segment(data = data.frame(env.vectors.ann$vectors$arrows) %>% 
                 cbind(r2=env.vectors.ann$vectors$r, pval =env.vectors.ann$vectors$pvals), 
               aes(x=0, xend=NMDS1 * r2, y=0, yend=NMDS2*r2))



ggplot(nmdspoints.biwk, aes(x=MDS1, y =MDS2)) + geom_point() +
  scale_x_continuous(limits = c(-0.3, 0.31)) + scale_y_continuous(limits = c(-0.22, 0.3)) +
  geom_segment(data = data.frame(env.vectors.biwk$vectors$arrows) %>% 
                 cbind(r2=env.vectors.biwk$vectors$r, pval =env.vectors.biwk$vectors$pvals), 
               aes(x=0, xend=NMDS1 * r2 , y=0, yend=NMDS2 * r2)) #need to fix scale (mult by 5?)


###############
## PERMANOVA ##

# following example from vegan tutorial, page 33
# http://cc.oulu.fi/~jarioksa/opetus/metodi/vegantutor.pdf

betad <- betadiver(catchmatrix.std, "z")   # using Arrhenius z measure of beta diversity
boxplot(betadisper(betad, pru.env.ann$Year ))

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



adonis(braydist ~ Year + Station, data = pru.env.ann, perm = 9999)


betad.biwk <- betadiver(catchmatrix.biwk.stdtrans , "z") 
adonis(betad.biwk ~ Salin_Top + Temp_Top +Station + Year + biwkmeanspeed_kph, pru.env.biwk, perm=999) 
#not working yet


#simper

# use evenness as response variable
# how to standardize
# scale - year vs week vs 
# how to interpret PERMANOVA results



summary(simper(catchmatrix, pru.env.ann$Station))
# ARCD, ARCS, BDWF, and LSCS account for most of the differences
summary(simper(catchmatrix.std, pru.env.ann$Station))
# HBWF, ARFL, DLVN, RDWF explain most of the standardized diffs




#################
### OBJECTIVE 3: Assess nature of observed changes: linear trend, nonlinear trend, struc break
#################



# explore remane diagram exploring how salinity affects diversity?


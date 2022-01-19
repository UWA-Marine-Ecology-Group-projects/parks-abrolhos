###
# Project: Parks - Abrolhos
# Data:    BOSS fish, habitat
# Task:    Modelling fish abundance w/ FSSGAM
# author:  Claude, Brooke, Kingsley
# date:    Nov-Dec 2021
##

rm(list=ls())

# libraries----
detach("package:plyr", unload=TRUE)#will error - don't worry
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
library(gplots)
library(RColorBrewer)
# library(doParallel) #this can removed?
library(doSNOW)
library(gamm4)
library(RCurl) #needed to download data from GitHub
library(FSSgam)
library(GlobalArchive)
library(ggplot2)

## Setup ----
# set your working directory (manually, once for the whole R project)
# use the 'files' tab to set wd in '~/parks-abrolhos' manually (your relative path) then run this line (if we need it?)
working.dir <- getwd()
setwd(working.dir)
name <- "2021-05_Abrolhos_BOSS"  # set study name

# load and wrangle data-
maxn   <- read.csv("data/Tidy/2021-05_Abrolhos_BOSS.complete.maxn.csv") # let kingsley know if you've done ^^ and this doesn't work
length <- read.csv("data/Tidy/2021-05_Abrolhos_BOSS.complete.length.csv")
allhab <- readRDS("data/Tidy/merged_habitat.rds")%>%
  ga.clean.names()%>%
  glimpse()

#allhab <- allhab[ , !colnames(allhab) %in% colnames(allhab)[9:38]]

allhab <- allhab %>%
  dplyr::filter(method%in%c('BOSS'))%>%
  transform(kelps = kelps / totalpts) %>%
  transform(macroalgae = macroalgae / totalpts) %>%
  transform(sand = sand / totalpts) %>%
  transform(rock = rock / totalpts) %>%
  transform(biog = biog / totalpts) %>%
  glimpse()

names(maxn)

metadata <- maxn %>%
  distinct(sample, latitude, longitude, date, time, location, status, site, 
           depth, observer, successful.count, successful.length)

# look at top species ----
maxn.sum <- maxn %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  group_by(scientific) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  ungroup()

## Total frequency of occurrence
ggplot(maxn.sum, aes(x = reorder(scientific, maxn), y = maxn)) +   
  geom_bar(stat="identity",position = position_dodge()) +
  coord_flip() +
  xlab("Species") +
  ylab(expression(Overall ~ abundance ~ (Sigma ~ MaxN))) +
  #Theme1+
  theme(axis.text.y = element_text(face = "italic"))+
  #theme_collapse+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))#+

# Create total abundance and species richness ----
ta.sr <- maxn %>%
  dplyr::ungroup() %>%
  dplyr::group_by(scientific,sample) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  tidyr::spread(scientific, maxn, fill = 0) %>%
  dplyr::mutate(total.abundance = rowSums(.[, 2:(ncol(.))], na.rm = TRUE )) %>% #Add in Totals
  dplyr::mutate(species.richness = rowSums(.[, 2:(ncol(.))] > 0)) %>% # double check these
  dplyr::select(sample, total.abundance, species.richness) %>%
  tidyr::gather(., "scientific", "maxn", 2:3) %>%
  dplyr::glimpse()

# Create abundance of all recreational fished species ----
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master <- googlesheets4::read_sheet(url) %>%
  ga.clean.names() %>%
  filter(grepl('Australia', global.region)) %>% # Change country here
  dplyr::select(family, genus, species, fishing.type, australian.common.name) %>%
  distinct() %>%
  glimpse()

unique(master$fishing.type)

fished.species <- maxn %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(
    scientific %in%c("Serranidae Plectropomus spp"),"R", fishing.type)) %>%
  dplyr::filter(fishing.type %in% c("B/R", "B/C/R", "R", "C/R","C")) %>%
  dplyr::filter(!family%in%c("Monacanthidae", "Scorpididae", "Mullidae"))       # Brooke removed leatherjackets, sea sweeps and goat fish

unique(fished.species$scientific)

# Come back to maybe getting rid of some of these, but for now we continue on
fished.maxn <- fished.species %>%
  dplyr::ungroup() %>%
  dplyr::group_by(scientific,sample) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  spread(scientific,maxn, fill = 0) %>%
  dplyr::mutate(targeted.abundance=rowSums(.[, 2:(ncol(.))], na.rm = TRUE )) %>% #Add in Totals
  dplyr::select(sample,targeted.abundance) %>%
  gather(.,"scientific","maxn",2:2) %>%
  dplyr::glimpse()

# Pick top species
species.maxn <- maxn %>%
  dplyr::filter(scientific %in% c("Pomacentridae Chromis westaustralis",
                                  "Labridae Coris auricularis",
                                  "Chaetodontidae Chaetodon assarius",
                                  "Lethrinidae Lethrinus miniatus"
  ))%>%
  dplyr::select(sample,scientific,maxn) %>%
  distinct()

dat <- bind_rows(fished.maxn, species.maxn, ta.sr) %>%
  left_join(allhab) %>%
  left_join(metadata) %>%
  dplyr::filter(!scientific%in%c("targeted.abundance"))
  distinct()

# Set predictor variables---
names(maxn)
names(allhab)

pred.vars = c("depth", 
              "macroalgae", 
              "sand", 
              "biog", 
              "relief",
              "tpi",
              "slope",
              "detrended") 

# predictor variables Removed at first pass---
# broad.Sponges and broad.Octocoral.Black and broad.Consolidated 

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(dat[,pred.vars]), 2)
# nothing is highly correlated 

# Plot of likely transformations - thanks to Anna Cresswell for this loop!
par(mfrow = c(3, 2))
for (i in pred.vars) {
  x <- dat[ , i]
  x = as.numeric(unlist(x)) 
  hist((x)) #Looks best
  plot((x), main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x + 1))
  plot(log(x + 1))
}

# Review of individual predictors - we have to make sure they have an even distribution---
#If the data are skewed to low numbers try sqrt>log or if skewed to high numbers try ^2 of ^3

# # Re-set the predictors for modeling----
pred.vars <- c("depth", "macroalgae",  "sand", 
               "biog", "relief","tpi","slope","detrended") 

# Check to make sure Response vector has not more than 80% zeros----
unique.vars <- unique(as.character(dat$scientific))

resp.vars <- character()
for(i in 1:length(unique.vars)){
  temp.dat <- dat[which(dat$scientific == unique.vars[i]), ]
  if(length(which(temp.dat$maxn == 0)) / nrow(temp.dat) < 0.9){
    resp.vars <- c(resp.vars, unique.vars[i])}
}
resp.vars   

# butterfly fish removed becuase of too many zeros - I changed cutoff to 90%

# Run the full subset model selection----
savedir <- "output/fssgam - fish"
use.dat <- as.data.frame(dat) 
str(use.dat)

factor.vars <- c("location")# Status as a Factor with two levels
out.all     <- list()
var.imp     <- list()

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  use.dat <- as.data.frame(dat[which(dat$scientific == resp.vars[i]), ])
  
  Model1  <- gam(maxn ~ s(depth, k = 3, bs='cr'),
                 family = tw(),  data = use.dat)
  
  model.set <- generate.model.set(use.dat = use.dat,
                                  test.fit = Model1,
                                  pred.vars.cont = pred.vars,
                                  pred.vars.fact = factor.vars,
                                  linear.vars = "depth",
                                  k = 3,
                                 # smooth.smooth.interactions = c("depth","biog")#,
                                  #null.terms="s(Location,Site,bs='re')"
  )
  out.list <- fit.model.set(model.set,
                            max.models = 600,
                            parallel = T)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table <- out.list$mod.data.out  # look at the model selection table
  mod.table <- mod.table[order(mod.table$AICc), ]
  mod.table$cumsum.wi <- cumsum(mod.table$wi.AICc)
  out.i   <- mod.table[which(mod.table$delta.AICc <= 2), ]
  out.all <- c(out.all,list(out.i))
  # var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Either raw importance score
  var.imp <- c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Or importance score weighted by r2
  
  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name = as.character(out.i$modname[m])
    png(file = paste(savedir, paste(name, m, resp.vars[i], "mod_fits.png", sep = "_"), sep = "/"))
    if(best.model.name != "null"){
      par(mfrow = c(3, 1), mar = c(9, 4, 3, 1))
      best.model = out.list$success.models[[best.model.name]]
      plot(best.model,all.terms = T, pages = 1, residuals = T, pch = 16)
      mtext(side = 2, text = resp.vars[i], outer = F)}  
    dev.off()
  }
}

# Save model fits, data, and importance scores---
names(out.all) <- resp.vars
names(var.imp) <- resp.vars
all.mod.fits   <- do.call("rbind",out.all)
all.var.imp    <- do.call("rbind",var.imp)
write.csv(all.mod.fits[ , -2], file = paste(savedir, paste(name, "all.mod.fits.csv", sep = "_"), sep = "/"))
write.csv(all.var.imp, file = paste(savedir, paste(name, "all.var.imp.csv", sep = "_"), sep = "/"))
saveRDS(dat, "output/fish_abundance_fssgamdat.rds")

# Generic importance plots-
heatmap.2(all.var.imp, notecex = 0.4, dendrogram = "none",
          col = colorRampPalette(c("white", "yellow", "red"))(10),
          trace = "none", key.title = "", keysize = 2,
          notecol = "black", key = T,
          sepcolor = "black", margins=c(12, 16), lhei = c(4, 15), Rowv = FALSE, Colv = FALSE)



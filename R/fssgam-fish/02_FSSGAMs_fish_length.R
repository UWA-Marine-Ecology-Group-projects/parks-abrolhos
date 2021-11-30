# Part 1-FSS modeling----
rm(list=ls())

## librarys----
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
library(doParallel) #this can removed?
library(doSNOW)
library(gamm4)
library(RCurl) #needed to download data from GitHub
library(FSSgam)
library(GlobalArchive)
library(ggplot2)

## set study name
study <- "2021-05_Abrolhos_BOSS" 
name <- study

## Set your working directory ----
#working.dir<-getwd()
working.dir <- 'H:/GitHub/parks-abrolhos'

## Save these directory names to use later----
tidy.dir<-paste(working.dir,"data/Tidy",sep="/")

## Load the data sets -
setwd(tidy.dir)
dir()

length <- read.csv("2021-05_Abrolhos_BOSS.complete.length.csv")%>%
  mutate(scientific=paste(family,genus,species))

allhab <- readRDS("merged_habitat.rds")
allhab <- allhab[ , !colnames(allhab) %in% colnames(allhab)[9:38]]
allhab <- allhab %>%
  transform(kelps = kelps/totalpts)%>%
  transform(macroalgae = macroalgae/totalpts)%>%
  transform(sponge = sponge/totalpts)%>%
  transform(sand = sand/totalpts)%>%
  transform(rock = rock/totalpts)%>%
  transform(biog = biog/totalpts)%>%
  glimpse()

metadata <- length %>%
  distinct(sample,latitude,longitude,date,time,location,status,site,depth,observer,successful.count,successful.length)

names(allhab)

# Create abundance of all recreational fished species ----
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master<-googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>% # Change country here
  dplyr::select(family,genus,species,fishing.type,australian.common.name,minlegal.wa)%>%
  distinct()%>%
  glimpse()

unique(master$fishing.type)

fished.species <- length %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in%c("Serranidae Plectropomus spp")
                                      ,"R",fishing.type))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Serranidae Plectropomus spp"), "450", minlegal.wa))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R"))%>%
  dplyr::filter(!family%in%c("Monacanthidae", "Scorpididae", "Mullidae")) # Brooke removed leatherjackets, sea sweeps and goat fish

unique(fished.species$scientific)

# Come back to maybe getting rid of some of these, but for now we continue on
# fished.maxn <- fished.species %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(scientific,sample) %>%
#   dplyr::summarise(maxn = sum(maxn)) %>%
#   spread(scientific,maxn, fill = 0) %>%
#   dplyr::mutate(targeted.abundance=rowSums(.[,2:(ncol(.))],na.rm = TRUE )) %>% #Add in Totals
#   dplyr::select(sample,targeted.abundance) %>%
#   gather(.,"scientific","maxn",2:2) %>%
#   dplyr::glimpse()

without.min.length <- fished.species %>%
  filter(is.na(minlegal.wa))%>%
  distinct(scientific) # only charlies don't have one
unique(without.min.length$scientific)

legal <- fished.species %>%
  tidyr::replace_na(list(minlegal.wa=0)) %>%
  dplyr::filter(length>minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "greater than legal size") %>%
  dplyr::glimpse()

sublegal <- fished.species %>%
  dplyr::filter(length<minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "smaller than legal size") %>%
  dplyr::glimpse()

miniatus.legal <- fished.species %>%
  dplyr::filter(species%in%c("miniatus")) %>%
  dplyr::filter(length>minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "legal size red throat") %>%
  dplyr::glimpse()

miniatus.sublegal <- fished.species %>%
  dplyr::filter(species%in%c("miniatus")) %>%
  dplyr::filter(length<minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "sublegal size red throat") %>%
  dplyr::glimpse() # only two of these


combined.length <- bind_rows(legal, sublegal, miniatus.legal, miniatus.sublegal) # add pink snapper and other indicator species

unique(combined.length$scientific)

complete.length <- combined.length %>%
  dplyr::right_join(metadata, by = c("sample")) %>% # add in all samples
  dplyr::select(sample,scientific,number) %>%
  tidyr::complete(nesting(sample), scientific) %>%
  replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(scientific)) %>% # this should not do anything
  dplyr::left_join(.,metadata) %>%
  dplyr::left_join(.,allhab) %>%
  dplyr::filter(successful.length%in%c("Y")) %>%
  dplyr::mutate(scientific=as.character(scientific)) %>%
  dplyr::glimpse()

# Set predictor variables---
names(complete.length)
names(allhab)

pred.vars=c("depth", 
            "kelps", 
            "macroalgae", 
            "sponge", 
            "sand", 
            "rock", 
            "biog", 
            "relief",
            "tpi",
            "slope",
            "detrended") 

# predictor variables Removed at first pass---
# broad.Sponges and broad.Octocoral.Black and broad.Consolidated , "InPreds","BioTurb" are too rare

dat <- complete.length

# Check for correalation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(dat[,pred.vars]),2)
# nothing is highly correlated 

# Plot of likely transformations - thanks to Anna Cresswell for this loop!
par(mfrow=c(3,2))
for (i in pred.vars) {
  x<-dat[ ,i]
  x = as.numeric(unlist(x))
  hist((x))#Looks best
  plot((x),main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x+1))
  plot(log(x+1))
}

# Review of individual predictors - we have to make sure they have an even distribution---
#If the data are squewed to low numbers try sqrt>log or if squewed to high numbers try ^2 of ^3
# sponges very low
# octocoral, very very low
# macroalgae, very very low
# invert complex very very low
# Hydroids very very low
# consolidated ok
# bryzoa very very low
# ascidians very very low


# # Re-set the predictors for modeling----
pred.vars=c("depth", 
            "kelps", 
            "macroalgae", 
            "sponge", 
            "sand", 
            "rock", 
            "biog", 
            "relief",
            "tpi",
            "slope",
            "detrended")

# Check to make sure Response vector has not more than 80% zeros----
unique.vars=unique(as.character(dat$scientific))

unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$scientific==unique.vars[i]),]
  if(length(which(temp.dat$number==0))/nrow(temp.dat)<0.9){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}

unique.vars.use   

# changed to 90% - smaller than legal size included


# Run the full subset model selection----
#setwd("C:/GitHub/parks-abrolhos/output/fssgam - fish") #Brooke directory
setwd("H:/GitHub/parks-abrolhos/output/fssgam - fish") #Claude directory

resp.vars=unique.vars.use
use.dat=as.data.frame(dat)
str(use.dat)

name<- paste(study,"length",sep="_")

factor.vars=c("status","location")# Status as a Factor with two levels
out.all=list()
var.imp=list()

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  use.dat=as.data.frame(dat[which(dat$scientific==resp.vars[i]),])
  
  Model1=gam(number~s(depth,k=3,bs='cr')#+ s(location,Site,bs="re")
             ,
             family=tw(),  data=use.dat)
  
  model.set=generate.model.set(use.dat=use.dat,
                               test.fit=Model1,
                               pred.vars.cont=pred.vars,
                               pred.vars.fact=factor.vars,
                               factor.smooth.interactions = NA,
                               smooth.smooth.interactions = c("depth", "biog"),
                               k=3#,
                               #null.terms="s(Location,Site,bs='re')"
                               )
  out.list=fit.model.set(model.set,
                         max.models=600,
                         parallel=T)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table=out.list$mod.data.out  # look at the model selection table
  mod.table=mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
  out.i=mod.table[which(mod.table$delta.AICc<=2),]
  out.all=c(out.all,list(out.i))
  # var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Either raw importance score
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Or importance score weighted by r2
  
  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name=as.character(out.i$modname[m])
    
    png(file=paste(name,m,resp.vars[i],"mod_fits.png",sep="_"))
    if(best.model.name!="null"){
      par(mfrow=c(3,1),mar=c(9,4,3,1))
      best.model=out.list$success.models[[best.model.name]]
      plot(best.model,all.terms=T,pages=1,residuals=T,pch=16)
      mtext(side=2,text=resp.vars[i],outer=F)}  
    dev.off()
  }
}

# Model fits and importance---
names(out.all)=resp.vars
names(var.imp)=resp.vars
all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
write.csv(all.mod.fits[,-2],file=paste(name,"all.mod.fits.csv",sep="_"))
write.csv(all.var.imp,file=paste(name,"all.var.imp.csv",sep="_"))

# Generic importance plots-
heatmap.2(all.var.imp,notecex=0.4,  dendrogram ="none",
          col=colorRampPalette(c("white","yellow","red"))(10),
          trace="none",key.title = "",keysize=2,
          notecol="black",key=T,
          sepcolor = "black",margins=c(12,16), lhei=c(4,15),Rowv=FALSE,Colv=FALSE)

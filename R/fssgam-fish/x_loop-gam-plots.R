rm(list=ls())

library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)
library(GlobalArchive)
library(stringr)
library(ggplot2)
library(gamm4)
library(ggmap)
library(rgdal)
library(raster)
library(png)
library(cowplot)
library(tibble)
library(purrr)

# set theme
# Theme-
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.background = element_rect(fill="white"),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=10),
    legend.title = element_blank(),
    legend.position = c(0.2, 0.8),
    text=element_text(size=10),
    strip.text.y = element_text(size = 10,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=10),
    axis.title.y=element_text(vjust=0.6, angle=90, size=10),
    axis.text.x=element_text(size=10),
    axis.text.y=element_text(size=10),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())

## Set working directory----
working.dir <- getwd()
setwd(working.dir)
#OR Set manually once

# Bring in and format the  data----
dat.maxn <- readRDS("data/Tidy/dat.maxn.rds")%>%
  dplyr::filter(location%in%"NPZ6")%>%
  dplyr::rename(number = maxn)%>%
  glimpse()

dat.length <- readRDS("data/Tidy/dat.length.rds")%>%
  dplyr::filter(location%in%"NPZ6")%>%
  glimpse()

dat <- bind_rows(dat.maxn,dat.length)

topmodels <- readRDS("data/Tidy/all.mod.fits.RDS") %>%
  dplyr::mutate(rownames = row.names(.)) %>% # they cooked - do manually
  dplyr::mutate(taxa = c("total.abundance", "total.abundance",
                         "total.abundance", "species.richness")) %>% # Need to fix this in the original exports - row names not correct
  tidyr::separate(col = modname, into = c("term1", "term2", "term3"), sep = "\\+", remove = F) %>%
  dplyr::mutate(no.terms = rowSums(!is.na(select(., starts_with("term"))))) %>%
  dplyr::group_by(taxa) %>%
  dplyr::arrange(AICc, no.terms) %>%
  slice(1) %>%
  ungroup() %>%
  glimpse()
  
topmodels$formula <- gsub("\"","\'", topmodels$formula)
topmodels$formula <- substr(topmodels$formula, 1, nchar(topmodels$formula) - 9)

test <- topmodels %>%
  pivot_longer(cols = starts_with("term"), names_to = "NA", values_to = "terms") %>%
  dplyr::select(taxa, terms) %>%
  dplyr::filter(!is.na(terms)) %>%
  group_by(taxa) %>%
  dplyr::mutate(term.no = row_number()) %>%
  ungroup() %>%
  glimpse()

resp.vars <- c("total.abundance","species.richness")

# use.dat <- dat %>% dplyr::filter(scientific == "species.richness")
# 
# mod <- gam(number ~ s(biog, k = 3, bs = 'cr') + s(tpi, k = 3, bs = 'cr') + s(depth, k = 3, bs = 'cr'),
#            family = tw, data = use.dat)
# i <- 1

unique(test$terms)
colnames <- unique(test$terms)
predicts <- data.frame(biog = mean(dat$biog),
                       depth = mean(dat$depth),
                       tpi = mean(dat$tpi),
                       mean.relief = mean(dat$mean.relief))

# for (i in 1:length(unique(dat$scientific))) {
  use.dat <- as.data.frame(dat[which(dat$scientific == resp.vars[i]), ])
  mod = gam(eval(parse(text=(paste0("number~", topmodels$formula[i])))), family = "tw",data = use.dat)
  
  for (i in 1:nrow(test)) {
    
    # First term in the model
    colnames <- c(test$terms[i], test$terms[i + 1], name3 <- test$terms[i + 2])
    
    testdata <- expand.grid(name1 = seq(min(select(use.dat, c(test$terms[i]))),
                                        max(select(use.dat, c(test$terms[i]))),length.out = 20), # Term 1
                            name2 = sum(select(use.dat, c(test$terms[i + 1])))/nrow(select(use.dat, c(test$terms[i + 1]))), # Term 2
                            name3 = sum(select(use.dat, c(test$terms[i + 2])))/nrow(select(use.dat, c(test$terms[i + 2])))) %>% # Term 3
      distinct() %>%
      glimpse()
    
    names(testdata) <- colnames
    
    fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
    
    tempdat <- testdata %>% 
      data.frame(fits) %>%
      group_by(biog)%>% #only change here test$terms[1]
      summarise(number=mean(fit),se.fit=mean(se.fit))%>%
      ungroup()
    assign(as.character(paste(paste("predicts", test$taxa[i], sep = "."), test$terms[i], sep = ".")), tempdat)
    
    # Second term in the model
    colnames <- c(test$terms[i + 1], test$terms[i], name3 <- test$terms[i + 2])
    
    testdata <- expand.grid(name2 = seq(min(select(use.dat, c(test$terms[i + 1]))),
                                        max(select(use.dat, c(test$terms[i + 1]))),length.out = 20), # Term 1
                            name1 = sum(select(use.dat, c(test$terms[i])))/nrow(select(use.dat, c(test$terms[i]))), # Term 2
                            name3 = sum(select(use.dat, c(test$terms[i + 2])))/nrow(select(use.dat, c(test$terms[i + 2])))) %>% # Term 3
      distinct() %>%
      glimpse()
    
    names(testdata) <- colnames
    
    fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
    
    tempdat <- testdata %>% 
      data.frame(fits) %>%
      group_by(depth)%>% #only change here
      summarise(number=mean(fit),se.fit=mean(se.fit))%>%
      ungroup()
    assign(as.character(paste(paste("predicts", test$taxa[i + 1], sep = "."), test$terms[i + 1], sep = ".")), tempdat)
    
    # Third term in the model
    colnames <- c(test$terms[i + 2], test$terms[i], name3 <- test$terms[i + 1])
    
    testdata <- expand.grid(name3 = seq(min(select(use.dat, c(test$terms[i+2]))),
                                        max(select(use.dat, c(test$terms[i+2]))),length.out = 20),
                            name1 = sum(select(use.dat, c(test$terms[i])))/nrow(select(use.dat, c(test$terms[i]))),
                            name2 = sum(select(use.dat, c(test$terms[i + 1])))/nrow(select(use.dat, c(test$terms[i + 1])))) %>%
      distinct() %>%
      glimpse()
    
    names(testdata) <- colnames
    
    fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
    
    tempdat <- testdata %>% 
      data.frame(fits) %>%
      group_by(tpi)%>% #only change here
      summarise(number=mean(fit),se.fit=mean(se.fit))%>%
      ungroup()
    assign(as.character(paste(paste("predicts", test$taxa[i + 2], sep = "."), test$terms[i + 2], sep = ".")), tempdat)
    
  }  

# Subset the dataframe to the model
spec <- test %>%
  dplyr::filter(taxa %in% "species.richness") %>%
  glimpse()

covars <- unique(spec$terms)

specdat <- dat %>% 
  dplyr::select(c(all_of(covars)))

n.vars <- ncol(specdat)
var.names <- colnames(specdat)

# Sequence the data
for (i in 1:n.vars) {
  temp <- data.frame(sequ = seq(min(specdat[,i]), max(specdat[,i]), length = 20),
                     var = var.names[i])
  if (i == 1) {
    sequence <- temp
  }
  else {
    sequence <- rbind(sequence, temp)
  }
}

# Make the dataframes to predict data
predict_gam <- function(data) {
  
  for (i in 1:n.vars) {
    temp <- data.frame(sequ = seq(min(data[i]), max(data[i]), length = 20),
                      var = var.names[i])
    if (i == 1) {
      sequence <- temp
    }
    else {
      sequence <- rbind(sequence, temp)
    }}
    
    yeet <- setNames(split(sequence, sequence$var), paste0("testdata", unique(sequence$var)))
    means <- tibble::rownames_to_column(data.frame(means = colMeans(data)), "var") %>%
      spread(var, means)
    means <- do.call("rbind", replicate(20, means, simplify = FALSE))
    mod <- gam(number~s(biog,k=3,bs='cr') + s(depth,k=3,bs='cr')  + s(tpi,k=3,bs='cr'), 
               family = tw, data = dat[dat$scientific == "species.richness"])
    
    for (i in 1:n.vars) {
    yeet[[i]] <- yeet[[i]] %>% 
      cbind(means) %>%
      dplyr::select(-all_of(as.character(unique(yeet[[i]][2]))))
    
    names(yeet[[i]])[names(yeet[[i]]) == names(yeet[[i]][1])] <- as.character(unique(yeet[[i]][2]))
    
    fits <- predict.gam(mod, newdata = yeet[[i]], type='response', se.fit=T)
    
    temp2 = yeet[[i]] %>% 
      data.frame (fits)%>%
      group_by_at(1)%>% # Only change here
      dplyr::summarise(number=mean(fit),se.fit=mean(se.fit))%>%
      ungroup()
    assign(as.character(paste("predicts", names(yeet[[i]][1]), sep = ".")), temp2)
    
    }
  }

predict_gam(specdat)


# MODEL Total abundance (relief) ----
dat.total <- dat %>% filter(scientific=="total.abundance")

mod=gam(number~s(mean.relief,k=3,bs='cr'), family=tw,data=dat.total)
# predict - relief ----
testdata <- expand.grid(mean.relief=seq(min(dat$mean.relief),max(dat$mean.relief),length.out = 20)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.total.relief = testdata%>%data.frame(fits)%>%
  group_by(mean.relief)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Total abundance ----
# detrended bathy ----
ggmod.total.relief<- ggplot() +
  ylab("")+
  xlab("Relief")+
  geom_point(data=dat.total,aes(x=mean.relief,y=number),  alpha=0.2, size=1,show.legend=F)+
  geom_line(data=predicts.total.relief,aes(x=mean.relief,y=number),alpha=0.5)+
  geom_line(data=predicts.total.relief,aes(x=mean.relief,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.total.relief,aes(x=mean.relief,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Total abundance") +
  theme(plot.title = element_text(hjust = 0))
ggmod.total.relief
### Load files and tidy to merge together ====

### Libraries ----
library(data.table)

### Input data ----
# Homerange Area
hr.area <-
  readRDS('data/derived-data/vertices-dt.Rds')[, c('ID', 'id') := .(id, NULL)]

# Observed social networks
obs.nets <- readRDS('data/derived-data/social-networks.Rds')
obs.nets[, ID := paste(ID, season, Year, HERD, sep = "_")]
obs.nets[, c('season', 'Year', 'HERD') := NULL]

# Mean of 1000 randomly generated Social Networks
rdm.nets <- readRDS('data/derived-data/rdmNets-1000.Rds')
rdm.nets[, ID := paste(ID, season, Year, HERD, sep = "_")]
rdm.nets[, c('season', 'Year', 'HERD') := NULL]
rdm.nets <- rdm.nets[,  mean(strength_soc), by = .(ID)]
colnames(rdm.nets)[2] <- "meanRdmSoc"

# Homerange Overlap Networks
hr.nets <-
  readRDS('data/derived-data/homerange-networks.Rds')[, .(ID, strengthHR = strength)]

### Merge all ----
DT <- Reduce(function(x, y)
  x[y, on = 'ID'],
  list(hr.nets, obs.nets, rdm.nets, hr.area))


### Prep for 7 ----
DT[, ID := as.factor(ID)]
DT[, Year := as.factor(Year)]
DT[, logSoc := log(strength_soc + 0.0125)]
DT[, logHr := log(strengthHR + 0.0125)]
DT[, logArea := log(area)]
DT[, seasonByHerd := paste(season, HERD, sep = '_')]


### Output ----
saveRDS(DT, 'data/derived-data/final-dt.Rds')

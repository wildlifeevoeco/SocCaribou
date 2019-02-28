#### Calculate Social Networks Based On Season, Year, Herd ====

### Packages ----
libs <- c('data.table', 'spatsoc',
          'SocCaribou')
lapply(libs, require, character.only = TRUE)

### Input data ----
locs <- readRDS('data/derived-data/cleaned-locs.Rds')

locs[, c('stepLength') := NULL]

utm21N <- '+proj=utm +zone=21 ellps=WGS84'

### Proximity Based Social Networks ----
# Need to allocate columns since reading from .Rds
if (truelength(locs) == 0) alloc.col(locs)

# Temporal grouping 
group_times(locs, datetime = 'datetime', threshold = '5 minutes')

group_pts(
  locs,
  threshold = 50,
  splitBy = c('season', 'HERD', 'Year'),
  timegroup = 'timegroup',
  id = 'ANIMAL_ID',
  coords = c('EASTING', 'NORTHING')
)

### Generate descriptive stats for network data ----
# number of unique groups per HerdYr combo
descriptive <-
  data.table(as.numeric(locs[, .N, by = c('HERD', 'Year', 'season')]$N) -
               as.numeric(locs[, uniqueN(group), by = c('HERD', 'Year', 'season')]$V1))

descriptive2 <-
  cbind(locs[, uniqueN(group), by = c('HERD', 'Year', 'season')], descriptive)

colnames(descriptive2) <-
  c('HERD', 'Year', 'season', 'allGroups', 'groupsOfTwoOrMore')

# total number of groups obsered
sum(descriptive2$allGroups)
# total number of social groups
sum(descriptive2$groupsOfTwoOrMore)

# groups by season
descriptive2[, mean(groupsOfTwoOrMore), by = c('season')]
descriptive2[, sd(groupsOfTwoOrMore), by = c('season')]

### Dynamic Networks ----
nets <- dynamic_network(locs, id = 'ANIMAL_ID', by = c('season', 'HERD', 'Year'))

nets[, .N, by = .(season, HERD, Year)]

### Output ----
saveRDS(nets, 'data/derived-data/social-networks.Rds')

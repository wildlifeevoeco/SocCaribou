### Site Fidelity Analyses ====

### Packages ----
libs <- c('data.table', 
          'SocCaribou')
lapply(libs, require, character.only = TRUE)

### Input data ----
locs <- readRDS('data/derived-data/cleaned-locs.Rds')

# UTM zone
utm21N <- '+proj=utm +zone=21 ellps=WGS84'

### Within Season Home range Overlap - Site fidelity ----
site.fid <-
  hr_network(locs, 
             id = 'ID', utm = utm21N,
             by = c('season', 'ANIMAL_ID', 'HERD'),
             returns = 'overlap')

# Drop all 999s and where the right year is not the next year in the sequence
site.fid <- site.fid[leftYear != 999][rightYear ==  leftYear + 1]

# Add log site fidelity
site.fid[, logSiteFid := log(value + 0.0125)]

# And year combination
site.fid[, yearcombo := paste(leftYear, rightYear, sep = '_')]

### Output ----
saveRDS(site.fid, 'data/derived-data/site-fidelity-All.Rds')

### Home Range Analyses ====

### Packages ----
libs <- c('data.table', 'sp', 'adehabitatHR',
          'SocCaribou')
lapply(libs, require, character.only = TRUE)

### Input data ----
locs <- readRDS('derived-data/cleaned-locs.Rds')

### Calculate Home range area for each individual ----
utm21N <- '+proj=utm +zone=21 ellps=WGS84'

coords <- c('EASTING', 'NORTHING')
pts <- SpatialPointsDataFrame(locs[, ..coords],
                              proj4string = CRS(utm21N),
                              data = locs[, .(ID)])

ud <- kernelUD(pts, grid = 700, extent = 3)
vertices <- getverticeshr(ud, 95)
vert.dt <- as.data.table(vertices)

# Split up paste ID
vert.dt[, c('ANIMAL_ID', 'season', 'Year', 'HERD') := tstrsplit(id, '_')]

# convert from ha to km2
vert.dt[, areaKM2 := area/100]

### Home Range Overlap Networks ----
# Generate all homerange overlap networks
hr.nets <- hr_network(locs, 
                      id = 'ANIMAL_ID', utm = utm21N, 
                      by = c('season', 'HERD', 'Year'),
                      returns = 'network-stats')
 
# Restructure IDs for consistency
hr.nets[, ANIMAL_ID := ID]
hr.nets[, ID := paste(ANIMAL_ID, season, Year, HERD, sep = '_')]

### Output ----
saveRDS(vert.dt, 'data/derived-data/vertices-dt.Rds')
saveRDS(hr.nets, 'data/derived-data/homerange-networks.Rds')

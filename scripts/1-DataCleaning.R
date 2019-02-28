### Data cleaning ====

### Packages ----
libs <- c('data.table', 'rgdal')
lapply(libs, require, character.only = TRUE)

### Input data ----
in.path <- 'input/AllCaribouDataRaw.csv'

caribou <- fread(in.path, 
                 drop = c('V1', 'SPECIES', 'EPSG_CODE', 'Map_Quality', 'Fix_Time_Delta',
                          'COLLAR_FILE_ID', 'EXCLUDE', 'VENDOR_CL', 'DOP',
                          'NAV', 'VALIDATED', 'LOCQUAL'))

fogo <- fread('data/raw-data/FogoCaribou.csv', drop = 'datetime')

### Preprocessing ----
# Prep Fogo data to merge 
fogo[, HERD := as.factor('FOGO')]
fogo[, SEX := as.factor('F')]
fogo[, COLLAR_TYPE_CL := as.factor('GPS')]

# Only female GPS from Middle Ridge, Topsails or Fogo
caribou <- caribou[SEX == 'F' & COLLAR_TYPE_CL == 'GPS' &
                     (HERD == 'MIDRIDGE' | HERD == 'TOPSAILS')]


DT <- rbindlist(list(caribou, fogo), fill = TRUE, use.names = TRUE)

# Date fields
DT[, idate := as.IDate(FIX_DATE)]
DT[, itime := as.ITime(FIX_TIME)]
DT[, datetime := as.POSIXct(paste(idate, itime), format = "%Y-%m-%d %H:%M:%S" )]
DT[, Year := year(datetime)]
DT[, JDate := yday(datetime)]

# Set seasons
DT[JDate > 15 & JDate < 63, season := 'winter']
DT[JDate > 196 & JDate < 244, season := 'summer']

DT <- DT[!(is.na(season))]

# Loc fields
utm21N <- '+proj=utm +zone=21 ellps=WGS84'
DT[, c('EASTING', 'NORTHING') := 
        as.data.table(project(cbind(X_COORD, Y_COORD), utm21N))]

# ID fields
DT[, ID := paste(ANIMAL_ID, season, Year, HERD, sep = '_')]

# Add unique season, year, herd codes
DT[, SeasonYearHerd := paste(season, Year, HERD, sep = '_')]

### Subset ----
# Only data after 2002
DT <- DT[Year >= '2002']

# Sub by bounding box
DT <- DT[NORTHING > 5200000 & NORTHING < 6000000 &
                 EASTING > 0 & EASTING < 800000]

# Check number of points her ID*season*Year*Herd
DT[, Nunique := .N, by = ID]
DT <- DT[Nunique > 50]

DT[, .N, by = .(HERD, Year, season)]

# Drop some IDs (huge HRs, ...)
dropID <- c('FO2017014_summer_2018_FOGO', 'mr2012a03_summer_2013_MIDRIDGE',
            'sc2007096_summer_2008_TOPSAILS', 'mr2012a05_summer_2013_MIDRIDGE')
DT <- DT[!(ID %in% dropID)]

# Drop some season/years from Topsails
dropSeasYrHerd <- c('summer_2006_TOPSAILS',
                    'winter_2012_TOPSAILS', 'summer_2012_TOPSAILS',
                    'summer_2011_TOPSAILS')

DT <- DT[!(SeasonYearHerd %in% dropSeasYrHerd)]

# Check number of fixes that occur  +/- 5 minutes from  the hour
DT <- DT[!(minute(datetime) > 5 & minute(datetime) < 55)]

# How many fixes do not occur on the hour
DT[minute(datetime) != 0] 

### Step Length ----
# Set columns
time.col <- 'datetime'
coord.cols <- c('EASTING', 'NORTHING')

# Calculate step length and move rate
step_length(clean, coords = coord.cols, time = time.col, 
            splitBy = c('id', 'season', 'Year'), moverate = TRUE)

# Drop more than 30km/hr movements
DT <- DT[moveRate < 30000]

# Check how much data per year*herd
DT[, uniqueN(ANIMAL_ID), by = .(HERD,Year, season)] 

DT[, .N, by = .(HERD, season, Year, ID)][order(N)]

### Output ----
saveRDS(
  DT[, .(ANIMAL_ID, HERD, datetime, Year, JDate, season,
            EASTING, NORTHING, ID, SeasonYearHerd, stepLength, moveRate)],
  'data/derived-data/cleaned-locs.Rds')

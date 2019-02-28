### Randomizations ====

### Packages ----
libs <- c('data.table', 'ggplot2', 'gridExtra',
          'lme4', 'spatsoc',
          'SocCaribou')
lapply(libs, require, character.only = TRUE)

### Input data ----
locs <- readRDS('data/derived-data/cleaned-locs.Rds')

locs[, c('stepLength', 'moveRate', 'SeasonYearHerd') := NULL]

### Variables ----
utm21N <- '+proj=utm +zone=21 ellps=WGS84'

### Proximity Based Social Networks ----
# Need to allocate columns since reading from .Rds
if (truelength(locs) == 0) alloc.col(locs)

group_times(locs, datetime = 'datetime', threshold = '5 minutes')

group_pts(
  locs,
  threshold = 50,
  splitBy = c('season', 'HERD', 'Year'),
  timegroup = 'timegroup',
  id = 'ANIMAL_ID',
  coords = c('EASTING', 'NORTHING')
)

### Randomizations ----
# Number of iterations
N <-  1000

lsDynNets <- lapply(1:N, FUN = function(i) {
    locsRandom <-
      randomizations(
        locs,
        id = 'ANIMAL_ID',
        type = 'trajectory',
        splitBy = c('season', 'HERD', 'Year'),
        group = 'group',
        datetime = 'datetime',
        iterations = 1,
        coords = c('EASTING', 'NORTHING')
      )[!(observed)]
    
    group_times(locsRandom, datetime = 'randomdatetime', threshold = '5 minutes')
    
    group_pts(
      locsRandom,
      threshold = 50,
      splitBy = c('season', 'HERD', 'Year'),
      timegroup = 'timegroup',
      id = 'ANIMAL_ID',
      coords = c('EASTING', 'NORTHING')
    )
    
    print(i)
    
    return(dynamic_network(locsRandom, id = 'ANIMAL_ID',
                           c('season', 'HERD', 'Year'))[, iteration := i])
  }
)

dynNets <- rbindlist(lsDynNets)

### Output ----
saveRDS(dynNets, 'data/derived-data/rdmNets-1000.RDS')


### Run randomization models and extract coefficients ----
dynNets <- readRDS('data/derived-data/rdmNets-1000.RDS')

dynNets[, ANIMAL_ID := ID]

# log-transform graph strength
dynNets[, logSoc := log(strength_soc + 0.0125)]

rdmCoefs <- dynNets[, {
  mod1 = lmer(logSoc ~ season +
                (1 | HERD / ID) + (1 | Year), data = .SD)
  as.list(coef(summary(mod1))[,"Estimate"])
  }, 
  by = 'iteration']
setnames(rdmCoefs, c('iter', 'Intercept', 'season'))


## load observed data
obsNets <- readRDS('data/derived-data/final-dt.Rds')

obsMod <- lmer(log(strength_soc + 0.0125) ~  season +
                 (1 | HERD/ANIMAL_ID) + (1 | Year), data = obsNets)
obsCoef <- data.table(season = coef(summary(obsMod))[,'Estimate'][2])


### Figures ----
# Figure 5 
png('graphics/color_figs/Fig5_SNA-randomizations.png', units = 'px', 
    width = 3000, height = 3000, res = 600)
ggplot(rdmCoefs) +
  geom_histogram(aes(season), bins = 250) +
  geom_vline(color = 'red',
             xintercept = obsCoef$season,
             lwd = 2) +
  geom_vline(
    color = 'black',
    lty = 2,
    lwd = 1,
    xintercept = quantile(rdmCoefs$season, 0.025)
  ) +
  geom_vline(
    color = 'black',
    lty = 2,
    lwd = 1,
    xintercept = quantile(rdmCoefs$season, 0.975)
  ) +
  xlab('Coefficient value') +
  ylab('Frequency') +
  theme(
    axis.title = element_text(size = 12, color = 'black'),
    axis.text = element_text(size = 10, color = 'black'),
    plot.title = element_text(size = 10, hjust = 0, face = 'bold'),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = 'black',
      fill = NA,
      size = 1
    )
  )
dev.off()


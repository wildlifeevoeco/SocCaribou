### Data Analysis and Figures

### Packages ----
libs <- c('data.table', 'ggplot2', 'lme4','gridExtra', 'MCMCglmm')
lapply(libs, require, character.only = TRUE)

### Load Data ---- 
DT <- readRDS('data/derived-data/final-dt.Rds')

locs <- readRDS('data/derived-data/cleaned-locs.Rds')

sitefid <- readRDS('data/derived-data/site-fidelity-All.Rds')

### Summary data  ----
# Number of unique caribou per herd
DT[, .(uniqueN(ANIMAL_ID)), by = .(HERD, Year)]
DT[, .(uniqueN(ANIMAL_ID)), by = .(HERD)]
DT[, .(uniqueN(ID)), by = .(season)]

# number of individuals (Table 2)
counts <- DT[, .(count = uniqueN(ID)), by = .(season, Year, HERD)]
counts[, .(meanCount = mean(count), sdCount = sd(count)), by = .(HERD, season)]
counts[, .(meanCount = sum(count)), by = .(HERD, season)]

# mean social strength, spatial strength, and home range area (Table 2) 
DT[, .(
  meanStrengthHR = mean(strengthHR),
  meanStrengthSoc = mean(strength_soc),
  sdStrengthHR = sd(strengthHR),
  sdStrengthSoc = sd(strength_soc),
  meanAreaKM2 = mean(areaKM2),
  sdAreaKM2 = sd(areaKM2)
),
by = .(HERD, season)]


# Summary GPS data
count_loc <-
  locs[, .(count = uniqueN(EASTING)), by = .(ANIMAL_ID, season, Year, HERD)]

count_loc[, .(meanFix = mean(count), sdCount = sd(count)), by = .(season, HERD)]

# Summary Within Season/Site Fidelity

sitefid[, .N, by = season]
sitefid[, uniqueN(ANIMAL_ID), by = season]

### Predictions ----
# Prediction 1: home-range size larger in winter (from Hypothesis 1)

options(scipen = 999) 
m1 = lmer(logArea ~ logSoc * season + (1 | HERD / ANIMAL_ID) + (1 | Year), 
          data = DT)
Vcov <- vcov(m1, useScale = FALSE)
betas <- fixef(m1)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
round(cbind(betas, se, zval, pval), digits = 5)

summary(m1)

# Prediction 2: home-range overlap higher in winter (from Hypothesis 1)
m2 = lmer(logHr ~ season + (1 | HERD/ANIMAL_ID) + (1 | Year), 
          data = DT)
Vcov <- vcov(m2, useScale = FALSE)
betas <- fixef(m2)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
round(cbind(betas, se, zval, pval), digits = 5)

summary(m2)

# Prediction 3 (H1), and Prediction 5 (H2):
#   social strength will be low in both seasons and not differ from random (RDH) 
#   OR social sterngth will be higher than random in both seasons (CAH)

cols <- c("season", "Year", "ANIMAL_ID", "strength_soc", "HERD")
colsRdm <- c("season", "Year", "ANIMAL_ID", "meanRdmSoc", "HERD")
DTobs <- DT[, cols, with = FALSE]
DTrdm <- DT[, colsRdm, with = FALSE]
colnames(DTobs)[4] <- "strength"
colnames(DTrdm)[4] <- "strength"
DTobs$type <- "obs"
DTrdm$type <- "rdm"
DTcomp <- rbind(DTobs, DTrdm)
DTcomp[, logSoc := log(strength + 0.0125)]


m3 = lmer(logSoc ~ type * season +
            (1 | HERD / ANIMAL_ID) + (1 | Year), data = DTcomp)
Vcov <- vcov(m3, useScale = FALSE)
betas <- fixef(m3)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
round(cbind(betas, se, zval, pval), digits = 5)

summary(m3)

# Prediction 4: Site-fidelity lower in winter (from Hypothesis 2)
hist(sitefid$value)
m4 = lmer(logSiteFid ~ season + (1 | HERD / ANIMAL_ID) + (1 | yearcombo), 
          data = sitefid)
Vcov <- vcov(m4, useScale = FALSE)
betas <- fixef(m4)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
round(cbind(betas, se, zval, pval), digits = 5)

summary(m4)

### Figures ----
color <- c('orange', 'dodgerblue2')

# Figure 1: site-fidelity
png('graphics/color_figs/Fig1_UDOI_Colour.png', units = 'px', width = 3000,
    height = 3000, res = 600)
ggplot(sitefid, aes(season, value, fill = factor(season))) +
  geom_boxplot(
    aes(fill = season),
    notch = TRUE,
    outlier.color = NA,
    lwd = 0.6,
    alpha = 0.25
  ) +
  geom_jitter(
    aes(color = season),
    shape = 16,
    position = position_jitter(0.2),
    size = 2,
    alpha = 0.6
  ) +
  theme(
    legend.position = 'none',
    axis.title.y = element_text(size = 12, color = 'black'),
    axis.text = element_text(size = 10, color = 'black'),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = 'black',
      fill = NA,
      size = 1
    )
  ) +
  xlab('') +
  ylab('Utilization distribution overlap index') +
  scale_fill_manual(values = color) +
  scale_color_manual(values = color)
dev.off()

# Figure 2: spatial strength
png('graphics/color_figs/Fig2_HRO_Colour.png', units = 'px', 
    width = 3000, height = 3000, res = 600)

ggplot(DT,aes(season,strengthHR)) +
  geom_boxplot(aes(fill = season), notch=TRUE,outlier.color = NA,lwd = 0.6, alpha = 0.25) +
  geom_jitter(aes(color = season), shape=16, position=position_jitter(0.2), size = 2, alpha = 0.6) +
  theme(axis.title.x = element_blank(),
        legend.position='none',
        axis.title.y = element_text(size=12,color = 'black'),
        axis.text=element_text(size=10,color = 'black'),
        plot.title=element_text(size = 10,hjust=0, face = 'bold'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = 'black', fill=NA, size=1)) +
  ylab('Spatial graph strength') +
  scale_fill_manual(values=color) +
  scale_color_manual(values = color) 
dev.off()


# Figure 3: Social strength: observed vs random
DTcomp[, seasonByNet := paste(season, type, sep = "_")]
DTcomp[seasonByNet == 'summer_obs', seasonByNet := 'summer (obs)']
DTcomp[seasonByNet == 'winter_obs', seasonByNet := 'winter (obs)']
DTcomp[seasonByNet == 'summer_rdm', seasonByNet := 'summer (rdm)']
DTcomp[seasonByNet == 'winter_rdm', seasonByNet := 'winter (rdm)']

color <- c('orange',  'dodgerblue2', 
           'orange', 'dodgerblue2', 
           'orange', 'dodgerblue2')

png('graphics/color_figs/Fig3_SNA_Colour.png', units = 'px', width = 4000,
    height = 3000, res = 600)
ggplot(DTcomp, aes(seasonByNet, logSoc, fill = factor(seasonByNet))) +
  geom_boxplot(
    aes(fill = season),
    notch = TRUE,
    outlier.color = NA,
    lwd = 0.6,
    alpha = 0.25
  ) +
  geom_jitter(
    aes(color = season),
    shape = 16,
    position = position_jitter(0.2),
    size = 2,
    alpha = 0.6
  ) +
  theme(
    legend.position = 'none',
    axis.title.y = element_text(size = 12, color = 'black'),
    axis.text = element_text(size = 10, color = 'black'),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = 'black',
      fill = NA,
      size = 1
    )
  ) +
  xlab('') +
  ylab('log(social graph strength)') +
  scale_fill_manual(values = color) +
  scale_color_manual(values = color)
dev.off()

# Figure 4
png('graphics/color_figs/Fig4_HRA_Colour.png', units = 'px', width = 3000,
    height = 3000, res = 600)
ggplot(DT, aes(season, areaKM2)) +
  geom_boxplot(
    aes(fill = season),
    notch = TRUE,
    outlier.color = NA,
    lwd = 0.6,
    alpha = 0.25
  ) +
  geom_jitter(
    aes(color = season),
    shape = 16,
    position = position_jitter(0.2),
    size = 2,
    alpha = 0.6
  ) +
  theme(
    axis.title.x = element_blank(),
    legend.position = 'none',
    axis.title.y = element_text(size = 12, color = 'black'),
    axis.text = element_text(size = 10, color = 'black'),
    plot.title = element_text(size = 10, hjust = 0, face = 'bold'),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = 'black',
      fill = NA,
      size = 1
    )
  ) +
  labs(y = expression(Home ~ range ~ area ~ '' ~ (km ^ {
    2
  }))) +
  scale_fill_manual(values = color) +
  scale_color_manual(values = color)
dev.off()


### Export data for Dryad
out <- DT[, c("areaKM2", "ID", "logSoc", 
              "logHr", "logArea", "seasonByHerd") := NULL]
colnames(out)[2] <- "strengthSoc"

fwrite(out, "data/derived-data/final-dt-dyrad.Rds")

sitefid2 <- sitefid[,c("leftYear", "rightYear", "logSiteFid") := NULL]
colnames(sitefid2)[4] <- "siteFidelity"

fwrite(sitefid2, "data/dryad-data/final-dt-site-fidelity-dyrad.Rds")

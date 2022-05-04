# EDA for interval analysis
library(tangled)
library(tidyverse)
library(ggplot2)
library(gghighlight)
library(lme4)
library(lattice)
library(forcats)
library(stringr)
library(SemiMarkov)

# read in the data
pregnant <- read_csv(here::here('data-raw', '2021-02-25_years_since_pregnancy_1980-2013.csv'))%>% 
  dplyr::select(!starts_with('X')) %>% 
  dplyr::select(!c(`Last year sighted`, `F = first pregnancy detected`)) %>% 
  dplyr::filter(Year >=1980 & Year <= 2013)  %>% 
  drop_na(elapsed)
  
pregnant$Pregnant[is.na(pregnant$Pregnant)] <- 0 # so it can be either pregnant or not-pregnant

pregnant$Pregnant[pregnant$Pregnant == 'F'] <- '1'
pregnant$Pregnant <- as.numeric(pregnant$Pregnant)

# # quick tally and visual
# preg_interval <- pregnant %>% 
#   filter(Pregnant == 1) %>% 
#   select(elapsed, Year)
# summary(preg_interval$elapsed)
# 
# hist(preg_interval$elapsed, breaks = 25)
# plot(preg_interval$Year, preg_interval$elapsed)
# boxplot(preg_interval$elapsed ~ preg_interval$Year)
# ggplot(preg_interval, aes(Year, elapsed))+
#   geom_point()+
#   geom_smooth()

# Preserve only the pregnancy years since we're looking at the interval
# ditto the NAs, since that means (I think) it's the first observed pregnancy
# pregnant <- pregnant %>% 
#   filter(Pregnant == 1) %>% 
#   drop_na(elapsed)

# Build up the covariates to test the model
# Health data
healthmean <- sumh / ng
pregnant$health <- 0
pregnant$health_min <- 0
pregnant$mon_below_scl <- 0
pregnant$mon_below <- 0
pregnant$year_start <- 1971 # this will help me establish the window of the interval

for(i in 1:nrow(pregnant)){
  
  rowidx <- match(pregnant$EGNo[i], ID)
  s <- match(paste0("1-", pregnant$Year[i] - pregnant$elapsed[i]), myName) # note how this changes - it's over the whole interval now
  pregnant$year_start[i] <- pregnant$Year[i] - pregnant$elapsed[i]
  e <- match(paste0("12-", pregnant$Year[i]), myName)
  colidx <- s:e  
  colidx <- colidx[colidx > firstSight[ID == pregnant$EGNo[i]]] #remove months before firstSight
  pregnant$health[i] <- mean(healthmean[rowidx, colidx], na.rm = TRUE) # mean health over the interval
  pregnant$health_min[i] <- min(healthmean[rowidx, colidx], na.rm = TRUE) # min health over the interval
  pregnant$mon_below[i] <- length(which(healthmean[rowidx, colidx] < 67)) # number of months below the Rolland et al threshold
  pregnant$mon_below_scl[i] <- pregnant$mon_below[i] / length(healthmean[rowidx, colidx]) # scaled number of months below the Rolland et al threshold
}

# decadal covariate
pregnant <- pregnant %>% 
  mutate(decade = case_when(Year >= 1980 & Year <= 1989 ~ 1980,
                            Year >= 1990 & Year <= 1999 ~ 1990,
                            Year >= 2000 & Year <= 2009 ~ 2000,
                            Year >= 2010 & Year <= 2019 ~ 2010))

# bring in entanglement data
ent_tidy <- read_csv(here::here('data-raw','2020-09-29-entanglement_tidy.csv')) %>% 
  dplyr::select(!starts_with('X')) %>% 
  dplyr::select(!starts_with('Created')) %>% 
  dplyr::select(!starts_with('Edited')) %>% 
  dplyr::select(!starts_with('Change')) %>% 
  mutate(year = lubridate::year(lubridate::dmy(EndDate)),
         gear = str_detect(EntanglementComment, 'GEAR'),
         gear_vec = if_else(gear, 1, 0))

ent_tidy <- ent_tidy %>% 
  mutate(entvec = case_when(EntanglementStatus == 'minor' & gear_vec == 0 ~ 6,
                            EntanglementStatus == 'moderate' & gear_vec == 0 ~ 5,
                            EntanglementStatus == 'minor' & gear_vec == 1 ~ 4,
                            EntanglementStatus == 'severe' & gear_vec == 0 ~ 3,
                            EntanglementStatus == 'moderate' & gear_vec == 1 ~ 2,
                            EntanglementStatus == 'severe' & gear_vec == 1 ~ 1))

# pair up entanglement data with pregnancy data
pregnant$severity <- 0
pregnant$num_events <- 0

# now how figure out the entanglement during the interval
for (i in 1:nrow(pregnant)){
  # Get the Animal out and the year out for looking at the entanglement data
  egno_p <- pregnant$EGNo[i]
  year_end_int <- pregnant$Year[i]
  year_start_int <- pregnant$year_start[i]
  
  # subset the entanglement data to just pull out the 
  dsub <- ent_tidy %>% 
    filter(EGNo == egno_p & year %in% year_start_int:year_end_int)
  
  if(nrow(dsub) == 0) next()
  if(nrow(dsub) > 1) print(i) 
  pregnant$num_events[i] <- nrow(dsub)
  pregnant$severity[i] <- min(dsub$entvec, na.rm = TRUE) # keep the worst one
  
}

# # Establish severity & get unimpacted
pregnant <- pregnant %>%
  mutate(sev_num = case_when(severity == 0 ~ 0,
                             severity %in% c(4, 6) ~ 1,
                             severity %in% c(2, 5) ~ 2,
                             severity %in% c(3, 1)  ~ 3))
# 
# pregnant <- pregnant %>% 
#   group_by(EGNo) %>% 
#   mutate(ent_group = cumsum(sev_num)) 
# pregnant$severity[pregnant$ent_group == 0] <- 'unimpacted'
# pregnant$severity <- factor(pregnant$severity, levels = c('unimpacted', 'unentangled', 'minor', 'moderate', 'severe'))

mn_year <- mean(pregnant$Year)
sd_year <- sd(pregnant$Year)
pregnant$std_year <- (pregnant$Year - mn_year) / sd_year

pregnant$elapsed <- as.numeric(pregnant$elapsed)
pregnant$fctr_yr <- factor(pregnant$Year)
pregnant$EGNo <- factor(pregnant$EGNo)
pregnant$health_scl <- (pregnant$health - mean(pregnant$health, na.rm = TRUE)) / sd(pregnant$health, na.rm = TRUE)
pregnant$health_min_scl <- (pregnant$health_min - mean(pregnant$health_min, na.rm = TRUE)) / sd(pregnant$health_min, na.rm = TRUE)

# # what do these relationships look like
# pairs(~ elapsed + health + mon_below_scl + decade + Year + num_events + sev_num, data = pregnant, row1attop=FALSE,
#       main = "Elapsed Time Between Pregnancies")

# # First regression
# fit0 <- glm(elapsed ~ health_scl, data = pregnant)
# summary(fit0)
# 
# fit1 <- glm(elapsed ~ Year + health_scl + severity + num_events, data = pregnant)
# summary(fit1)
# 
# fit2 <- glm(elapsed ~ Year + mon_below_scl + severity + num_events, data = pregnant)
# summary(fit2)
# 
# fit3 <- glm(elapsed ~ sev_num, data = pregnant)
# summary(fit3)
# 
# fit3 <- glm(elapsed ~ num_events, data = pregnant)
# summary(fit3)
# 
# fit4 <- glm(elapsed ~ factor(decade), data = pregnant)
# summary(fit4)
# 
# fit5 <- glm(elapsed ~ mon_below_scl + factor(decade), data = pregnant)
# summary(fit5)
# 
# fit6 <- glm(elapsed ~ mon_below_scl:factor(decade), data = pregnant) # something popping out here
# summary(fit6)
# 
# fit7 <- glm(elapsed ~ mon_below_scl:factor(decade) + severity, data = pregnant) # something popping out here
# summary(fit7)
# 
# fit7 <- glm(elapsed ~ sev_num:factor(decade), data = pregnant)
# summary(fit7)
# 
# fit8 <- glm(elapsed ~ sev_num:mon_below_scl, data = pregnant)
# summary(fit8)
# 
# fit9 <- glm(elapsed ~ mon_below_scl:factor(decade) + factor(sev_num), data = pregnant) # something popping out here
# summary(fit9)
# 
# 
# fit10 <- glm(elapsed ~ mon_below_scl:factor(decade) + factor(sev_num):factor(decade), data = pregnant) # something popping out here
# summary(fit10)


# Assemble the data for semi-markov model(s)
# we want the transitions from not-pregnant to pregnant (this is the one we care about) & 
# from pregnant to non-pregnant
# trying to index this to find the year of pregnancy and the immediate next year for each animal
# h = not pregnant
# j = pregnant
hj_idx <- which(pregnant$Pregnant == 1)
jh_idx <- hj_idx + 1
preg_sub_hj <- pregnant[hj_idx, ]
preg_sub_jh <- pregnant[jh_idx, ]

# States and Transitions
semi_dat_hj <- preg_sub_hj %>% 
  dplyr::select(id = EGNo, year = Year, Pregnant = Pregnant, 
                time = elapsed, severity = sev_num, decade = decade, 
                health = health_scl) %>% 
  mutate(state.h = 1, state.j = Pregnant + 1)

semi_dat_jh <- preg_sub_jh %>% 
  dplyr::select(id = EGNo, year = Year, Pregnant = Pregnant, 
                time = elapsed, severity = sev_num, decade = decade, 
                health = health_scl) %>% 
  mutate(state.h = 2, state.j = Pregnant + 1)

semi_dat <- bind_rows(semi_dat_hj, semi_dat_jh) %>% 
  arrange(id, year)

ent_sev <- data.frame(ent_severity = semi_dat$severity, 
                      ent_sev_bin = semi_dat$severity, 
                      decade = semi_dat$decade, 
                      health = semi_dat$health)

ent_sev$ent_sev_bin <- ifelse(ent_sev$ent_severity > 0, 1, 0)

semi_df <- data.frame(semi_dat$id, semi_dat$state.h, semi_dat$state.j, semi_dat$time)

# Weibull test
# Transition set up
states_1 <- c("1","2")
mtrans_1 <- matrix(FALSE, nrow = 2, ncol = 2)
mtrans_1[1, 2] <- "W"
mtrans_1[2, 1] <- "W"

# Model fit
## semi-Markov model without covariates
fit0 <- semiMarkov(data = semi_df, states = states_1, mtrans = mtrans_1)
print(fit0)
# End Weibull test

# Transition set up
states_1 <- c("1","2")
mtrans_1 <- matrix(FALSE, nrow = 2, ncol = 2)
mtrans_1[1, 2] <- "E"
mtrans_1[2, 1] <- "E"

# Model fit
## semi-Markov model without covariates
fit1 <- semiMarkov(data = semi_df, states = states_1, mtrans = mtrans_1)
print(fit1)

## semi-markov model with covariates (severity & decade) affecting all transitions
fit2 <- semiMarkov(data = semi_df, states = states_1, mtrans = mtrans_1, cov = ent_sev)
print(fit2)

## semi-markov model with covariates (severity & decade) affecting only the transition from resting/available to pregnant
fit3 <- semiMarkov(data = semi_df, states = states_1, mtrans = mtrans_1, 
                   cov = ent_sev[, c(1,3)], 
                   cov_tra = list(c("12"), c("12")))
print(fit3)

## semi-markov model with covariates (severity binary & decade) affecting only the transition from resting/available to pregnant
fit3_bin <- semiMarkov(data = semi_df, states = states_1, mtrans = mtrans_1, 
                   cov = ent_sev[, c(2,3)], 
                   cov_tra = list(c("12"), c("12")))
print(fit3_bin)

## semi-markov model with covariates (severity, decade, scaled health) affecting only the transition from resting/available to pregnant
fit4 <- semiMarkov(data = semi_df, states = states_1, mtrans = mtrans_1, 
                   cov = ent_sev, cov_tra = list(c("12"),c("12"),c("12")))
print(fit4)

# Univariate Models
## semi-markov model with covariates (severity) affecting only the transition from resting/available to pregnant
fit5 <- semiMarkov(data = semi_df, states = states_1, mtrans = mtrans_1, 
                   cov = as.data.frame(ent_sev[, 1]), cov_tra = list(c("12")))
print(fit5)

## semi-markov model with covariates (severity yes/no) affecting only the transition from resting/available to pregnant
fit6 <- semiMarkov(data = semi_df, states = states_1, mtrans = mtrans_1, 
                   cov = as.data.frame(ent_sev[, 2]), cov_tra = list(c("12")))
print(fit6)

## semi-markov model with covariates (decade) affecting only the transition from resting/available to pregnant
fit7 <- semiMarkov(data = semi_df, states = states_1, mtrans = mtrans_1, 
                   cov = as.data.frame(ent_sev[, 3]), cov_tra = list(c("12")))
print(fit7)

## semi-markov model with covariates (scaled health) affecting only the transition from resting/available to pregnant
fit8 <- semiMarkov(data = semi_df, states = states_1, mtrans = mtrans_1, 
                   cov = as.data.frame(ent_sev[, 4]), cov_tra = list(c("12")))
print(fit8)


# Plot the Hazard - this does not look like I'd expected it to
plot(hazard(fit6, cov = 0), hazard(fit6, cov = 1), transitions = "12")
plot(hazard(fit6, cov = 0, type = "lambda"), hazard(fit6, cov = 1, type = "lambda"), 
     transitions = "12", legend.pos = c(3.75, 0.119),   cex = 0.8)

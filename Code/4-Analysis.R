### Analysis  ###########################################
# Purpose: Analyse the male and female fertility rates  #
# Author: Henrik-Alexander Schubert                     #
# E-Mail: schubert@demogr.mpg.de                        #
# Date: 30th May 2023                                   #
# Prerequisites: functions                              #
#########################################################

### Settings -----------------------------------------------------------------

rm(list = ls())

# Load the packages
source("Functions/Packages.R")
source("Functions/Graphics.R")
source("Functions/Functions.R")

# Load the data
load("Data/births_father.Rda")
load("Data/births_mother.Rda")
load("Data/exposure_females.Rda")
load("Data/exposure_males.Rda")


### Estimate the rates -------------------------------------------------------

# Merge 
males <- inner_join(births_fat, pop_f, by = c("year", "age_fat" = "age", "entity" = "geo_code"))

# Estimate the ASFR
asfr_m <- males %>% mutate(asfr_m = births / mid_year_pop)

# Estimate the TFR
tfr_m <- asfr_m %>% summarise(tfr_m = sum(asfr_m), .by = c(year, entity))

## Do the same for females --------------------------------------------------

# Merge 
females <- inner_join(births_mot, pop_f, by = c("year", "age_mot" = "age", "entity" = "geo_code"))

# Estimate the ASFR
asfr_f <- females %>% mutate(asfr_f = births / mid_year_pop)

# Estimate the TFR
tfr_f <- asfr_f %>% summarise(tfr_f = sum(asfr_f), .by = c(year, entity))


### JOin the data ---------------------------------------------------------

### Compare
male_female_asfr <- left_join(asfr_f, asfr_m, by = c("age_mot" = "age_fat", "year", "entity"), suffix = c("_f", "_m"))


# Join male and female TFR+
male_female_TFR <- inner_join(tfr_m, tfr_f)


### Estimate the TFR at the national level ---------------------------------

# National level TFR
asfr_national <- male_female_asfr %>% 
                    group_by(year, age_mot) %>%
                    summarise(exposure = sum(mid_year_pop_f),
                              births   = sum(births_f),
                              asfr     = births / exposure)

# Estimate the TFR at the national leve
tfr_national <- asfr_national %>% 
                    group_by(year) %>% 
                    summarise(TFR = sum(asfr))


### Plot the natioanl rates ---------------------------------------------------

# Plot the TFR trend
ggplot(tfr_national, aes(year, TFR)) +
  geom_point() +
  geom_line()  +
  scale_y_continuous(limits = c(0, 4), expand = c(0, 0)) +
  xlab("Year")

# Plot the age shift
ggplot(asfr_national, aes(age_mot, asfr, group = year, colour = year)) +
  geom_line() +
  scale_colour_gradient(low = MPIDRgreen, high = MPIDRyellow) +
  xlab("Age of the mother") +
  ylab("Age-specific fertility rate")

### Plot the male-female difference for several years -------------------------

# Plot the asfr
male_female_asfr %>% 
  filter(entity == 9 & year %in% c(1990, 2000, 2010, 2020)) %>% 
  ggplot(aes(x = age_mot)) +
  geom_line(aes(y = asfr_f, colour = "females"), size = 2) +
  geom_line(aes(y = asfr_m, colour = "males"), size = 2) +
  facet_wrap(~ year) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_colour_manual(values = c( MPIDRred, MPIDRblue), name = "Sex:") +
  ylab("Age-specific fertility rate") +
  xlab("Age")

### END ########################################################################  
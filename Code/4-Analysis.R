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
left_join(asfr_f, asfr_m, by = c("age_mot" = "age_fat", "year", "entity"), suffix = c("_f", "_m")) %>% 
  filter(entity == 2) %>% 
  ggplot(aes(x = age_mot)) +
  geom_line(aes(y = births_f), col = "red") +
  geom_line(aes(y = births_m), col = "blue") +
  facet_wrap(~ year)


# Join male and female TFR+
male_female_TFR <- inner_join(tfr_m, tfr_f)


### END ########################################################################  
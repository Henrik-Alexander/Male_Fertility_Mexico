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
                    summarise(exposure_f = sum(mid_year_pop_f),
                              births_f   = sum(births_f),
                              asfr_f     = births_f / exposure_f,
                              exposure_m = sum(mid_year_pop_m),
                              births_m   = sum(births_m),
                              asfr_m     = births_m / exposure_m)



# Estimate the TFR at the national leve
tfr_national <- asfr_national %>% 
                    group_by(year) %>% 
                    summarise(TFR_f = sum(asfr_f),
                              TFR_m = sum(asfr_m))

# Save the data
save(asfr_national, file = "Data/asfr_national_mexico.Rda")
save(tfr_national, file = "Data/tfr_national_mexico.Rda")

### Plot the natioanl rates ---------------------------------------------------

# Plot the TFR trend
ggplot(tfr_national, aes(year)) +
  geom_line(aes(y = TFR_f, colour = "females"), size = 1.4)  +
  geom_line(aes(y = TFR_m, colour = "males"), size = 1.4) +
  scale_y_continuous(limits = c(0, 4), expand = c(0, 0)) +
  scale_colour_manual(name = "TFR:", values = c(MPIDRred, MPIDRblue)) +
  xlab("Year") + ylab("Total fertility rate") + 
  theme(legend.position = c(0.8, 0.9))

# Plot the age shift
ggplot(asfr_national, aes(age_mot, asfr_f, group = year, colour = year)) +
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


### Plot the distribution over time

male_female_TFR %>% 
  pivot_longer(cols = starts_with("tfr"), names_prefix = "tfr_", values_to = "tfr", names_to = "sex") %>% 
  filter(sex != "TFR_ratio") %>% 
  ggplot(aes(as.factor(year), tfr, fill = sex, colour = sex)) +
  scale_fill_manual(values = c( MPIDRred, MPIDRblue), name = "Sex:") +
  scale_colour_manual(values = c( MPIDRred, MPIDRblue), name = "Sex:") +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.8, 0.8)) +
  ylab("Total fertility rate") +
  xlab("Year")


### END ########################################################################  
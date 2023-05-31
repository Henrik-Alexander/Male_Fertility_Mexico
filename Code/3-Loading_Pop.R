### Load population data ################################
# Purpose: Impute missing ages for father and mother    #
# Author: Henrik-Alexander Schubert                     #
# E-Mail: schubert@demogr.mpg.de                        #
# Date: 30th May 2023                                   #
# Prerequisites: functions                              #
#########################################################

### Settings - -------------------------------------------------------------

# Load the packages
source("Functions/Packages.R")
source("Functions/Graphics.R")
source("Functions/Functions.R")


# Dimensions 
years <- 1990:2021
ages <- 15:55

### Loading the population data -------------------------------------------

# Open the files
pop <- read.csv("Raw/pob_mit_proyecciones.csv")

# Rename
names(pop) <- c("id", "year", "entity", "geo_code", "age", "sex", "population")


# Estimate the lag value
pop <- pop %>%
  group_by(entity, age, sex) %>%
  mutate(lag_pop = lag(population, order_by = year), 
         lag_year = lag(year, order_by = year),
         mid_year_pop = (lag_pop + population) / 2) %>% 
  ungroup()


# Filter the data
pop <- pop %>% filter(age %in% ages & year %in% years)

# Make female and male data sets
pop_f <- pop %>% filter(sex == "Mujeres" & geo_code != 0) %>% select(-sex, -id)
pop_m <- pop %>% filter(sex == "Hombres" & geo_code != 0) %>% select(-sex, -id)

# Save the data
save(pop_f, file = "Data/exposure_females.Rda")
save(pop_m, file = "Data/exposure_males.Rda")

##########            END             ##############
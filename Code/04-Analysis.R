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
males <- inner_join(births_fat, pop_m,
                    by = c("year", "age_fat" = "age", "entity" = "geo_code"),
                    suffix = c("", "_name"))

# Estimate the ASFR
asfr_m <- males |> mutate(asfr_m = births / mid_year_pop)

# Estimate the TFR
tfr_m <- asfr_m |> summarise(tfr_m = sum(asfr_m), .by = c(year, entity))

## Do the same for females --------------------------------------------------

# Merge 
females <- inner_join(births_mot, pop_f,
                      by = c("year", "age_mot" = "age", "entity" = "geo_code"),
                      suffix = c("", "_name"))

# Estimate the ASFR
asfr_f <- females |> mutate(asfr_f = births / mid_year_pop)

# Estimate the TFR
tfr_f <- asfr_f |> summarise(tfr_f = sum(asfr_f), .by = c(year, entity))


### JOin the data ---------------------------------------------------------

### Compare
asfr_reg <- left_join(asfr_f, asfr_m,
                              by = c("age_mot" = "age_fat", "year", "entity", "entity_name"),
                              suffix = c("_f", "_m")) |> 
                    rename(age = age_mot)


# Join male and female TFR+
tfr_reg <- inner_join(tfr_m, tfr_f)


### Estimate the TFR at the national level ---------------------------------

# National level TFR
asfr_nat <- asfr_reg |>
                  group_by(year, age) |>
                  summarise(exposure_f = sum(mid_year_pop_f),
                            births_f   = sum(births_f),
                            asfr_f     = births_f / exposure_f,
                            exposure_m = sum(mid_year_pop_m),
                            births_m   = sum(births_m),
                            asfr_m     = births_m / exposure_m)



# Estimate the TFR at the national leve
tfr_nat <- asfr_nat |> 
                    group_by(year) |> 
                    summarise(tfr_f = sum(asfr_f),
                              tfr_m = sum(asfr_m))

### Decomposition national ----------------------------------------------------

# Plot the crude birth rate for men and women
cbr_nat <- asfr_nat |>
  group_by(year) |> 
  summarise(cbr_f = 1000 * sum(births_f) / sum(exposure_f),
            cbr_m = 1000 * sum(births_m) / sum(exposure_m))

# Estimate the decomposition
comp_asfr <- asfr_nat |> 
  group_by(year) |> 
  mutate(across(starts_with("asfr"), ~ .x * 1000)) |> 
  mutate(pop_share_f = pop_share(exposure_f),
         pop_share_m = pop_share(exposure_m),
         mean_pop    = averaging(pop_share_f, pop_share_m),
         mean_rate   = averaging(asfr_f, asfr_m),
         delta_rate  = (asfr_f - asfr_m) * mean_pop,
         delta_pop   = (pop_share_f - pop_share_m) * mean_rate,
         .groups     = "drop")

# Make the decomposition
decomp_nat <- comp_asfr %>%
  select(year, age, delta_rate, delta_pop) %>%
  pivot_longer(cols = starts_with("delta"),
               names_to = "component",
               values_to = "contribution") |> 
  inner_join( cbr_nat, by = "year") |> 
  mutate(contribution = contribution / (cbr_f - cbr_m))


### Decomposition regional ----------------------------------------------------

# Estimate the decomposition
comp_asfr_reg <- asfr_reg |>
  group_by(year, entity_name) |>
  mutate(pop_share_f = pop_share(mid_year_pop_f),
         pop_share_m = pop_share(mid_year_pop_m),
         delta_pop = difference(pop_share_f, pop_share_m),
         delta_rate = difference(asfr_f, asfr_m),
         change_rate = pop_share_f * delta_rate,
         change_pop  = asfr_f * delta_pop,
         change_inter = delta_pop * delta_rate)

# Make the decomposition
decomposition <- comp_asfr_reg |>
  select(year, age, change_rate, 
         change_pop, change_inter, entity_name) |>
  pivot_longer(cols = starts_with("change"),
               names_to = "component",
               values_to = "contribution") 

### Save the data -------------------------------------------------------------

save(asfr_nat, file = "Data/asfr_national_mexico.Rda")
save(tfr_nat, file = "Data/tfr_national_mexico.Rda")
save(asfr_reg, file = "Data/asfr_regional_mexico.Rda")
save(tfr_reg, file = "Data/tfr_regional_mexico.Rda")


### Plot the national rates ---------------------------------------------------

# Plot the TFR trend
ggplot(tfr_nat, aes(year)) +
  geom_line(aes(y = tfr_f, colour = "females"), linewidth = 1.4)  +
  geom_line(aes(y = tfr_m, colour = "males"), linewidth = 1.4) +
  scale_y_continuous(limits = c(0, 4), expand = c(0, 0)) +
  scale_colour_manual(name = "TFR:", values = c(MPIDRred, MPIDRblue)) +
  xlab("Year") + ylab("Total fertility rate") + 
  theme(legend.position = c(0.8, 0.9))

# Plot the age shift
ggplot(asfr_nat, aes(age, asfr_f, group = year, colour = year)) +
  geom_line() +
  scale_colour_gradient(low = MPIDRgreen, high = MPIDRyellow) +
  xlab("Age of the mother") +
  ylab("Age-specific fertility rate")


### Plot the male-female difference for several years -------------------------

# Plot the ASFR
asfr_reg |> 
  filter(entity == 9 & year %in% c(1990, 2000, 2010, 2020)) |> 
  ggplot(aes(x = age)) +
  geom_line(aes(y = asfr_f, colour = "females"), linewidth = 2) +
  geom_line(aes(y = asfr_m, colour = "males"), linewidth = 2) +
  facet_wrap(~ year) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_colour_manual(values = c( MPIDRred, MPIDRblue), name = "Sex:") +
  ylab("Age-specific fertility rate") +
  xlab("Age")


### Plot the distribution over time
tfr_reg |> 
  pivot_longer(cols = starts_with("tfr"), names_prefix = "tfr_", values_to = "tfr", names_to = "sex") |> 
  filter(sex != "TFR_ratio") |> 
  ggplot(aes(as.factor(year), tfr, fill = sex, colour = sex)) +
  scale_fill_manual(values = c( MPIDRred, MPIDRblue), name = "Sex:") +
  scale_colour_manual(values = c( MPIDRred, MPIDRblue), name = "Sex:") +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.8, 0.9)) +
  ylab("Total fertility rate") +
  xlab("Year")


### Plot the decompositions ---------------------------------------------------

# Plot the decomposition
decomp_nat %>% 
  filter(year %in% c(1990, 2000, 2010, 2020)) %>% 
  ggplot(aes(age, contribution, fill = component)) +
  geom_col(colour = "white", linewidth = 0.01) +
  geom_hline(yintercept = 0, colour = "black") +
  facet_wrap(~ year) +
  scale_fill_manual(values = c(MPIDRred,  MPIDRblue),
                    labels = c(expression(paste(Delta, "Population")),
                               expression(paste(Delta, "Rate"))),
                    name = "Component: ") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent) +
  ylab("% Contribution") + xlab("Age") +
  theme(legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.2, "cm"))


# Plot the time trend for population estimates
decomp_nat |> 
  filter(age %in% c(15, 25, 35, 45)) |> 
  ggplot(aes(x = year, contribution, colour = component, group = component)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, colour = "black") +
  scale_colour_manual(values = c(MPIDRred, MPIDRorange, MPIDRblue),
                      labels = c(expression(paste(Delta, "Population and rate")),
                                 expression(paste(Delta, "Population")),
                                 expression(paste(Delta, "Rate"))),
                      name = "Component: ") +
  scale_y_continuous(labels = scales::label_number_si()) +
  scale_x_continuous(expand = c(0, 0)) +
  facet_wrap( ~ age)


# Plot the decomposition
decomp_nat |> 
  filter(year %in% c(1990, 2020) &
           entity_name %in% c("Baja California Sur",
                              "Guerrero")) |> 
  ggplot(aes(age, contribution, fill = component)) +
  geom_col() +
  geom_hline(yintercept = 0, colour = "black") +
  facet_grid(entity_name~ year) +
  scale_fill_manual(values = c(MPIDRred, MPIDRorange, MPIDRblue),
                    labels = c(expression(paste(Delta, "Population and rate")),
                               expression(paste(Delta, "Population")),
                               expression(paste(Delta, "Rate"))),
                    name = "Component: ") +
  scale_x_continuous(expand = c(0, 0)) +
  ylab("Contribution") + xlab("Age")

### END ########################################################################  

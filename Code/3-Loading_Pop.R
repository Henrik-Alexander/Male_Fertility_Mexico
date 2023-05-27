################################################################################
#                                                                              #
#         Max-Planck Institute for Demographic Research                        #
#               Subnational Birth Squeezes                                     #
################################################################################

### Preperations -------------------------------------------------------------

# Load the packages
source("Functions/Packages.R")
source("Functions/Graphics.R")
source("Functions/Functions.R")


# Dimensions 
years <- 1990:2021
ages <- 15:55


### Loading the population data -------------------------------------------

# Path : https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.zip
# Zip tmp
zipfile <- "Raw/Mexico/Mexico_pop.zip"

# Define the path
path <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.zip"

# Download the data
GET(path, write_disk(zipfile, overwrite = FALSE), progress())

# Unzip the data
utils::unzip(zipfile = zipfile, exdir = "Raw/Mexico/")

# Load the pop
pop <- fread("Raw/Mexico/WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.csv",  header = TRUE)

# Filter
pop <- subset(pop, subset = Location == "Mexico", select = c(Time, AgeGrp, Variant, PopFemale, PopMale, PopTotal))

# Subset the pop
pop <- subset(pop, subset = Time %in% years & AgeGrp %in% ages)

# Rename variables
pop <- rename(pop, year = Time, age = AgeGrp, variant = Variant, females = PopFemale, males = PopMale, total = PopTotal)

# Make as numeric
pop$age <- as.integer(pop$age)

### Plot the pop --------------------------------------------------------


# Plot the age distribution
ggplot(pop, aes(age, alpha = year, group = year)) +
  geom_line(aes(y = males), colour = "darkblue") +
  geom_line(aes(y = females), colour = "red")


# Plot the sex ratio
ggplot(pop, aes(age, males / females, group = year, colour = year)) +
  geom_line()



#### Save the pop --------------------------------------------------------

save(pop, file = "Data/pop_national_mex.Rda")


#### Census data ---------------------------------------------------------


# Load the data
pop <- read_xlsx("Raw/Poblacion_01.xlsx", skip = 4)

# Clean the names
pop <- clean_names(pop)


# Rename the Variables
pop <- rename(pop, 
              region = entidad_federativa,
              age_group = grupo_quinquenal_de_edad,
              total_1990 = total_3,
              males_1990 = hombres_4,
              females_1990 = mujeres_5,
              total_1995 = total_6,
              males_1995 = hombres_7,
              females_1995 = mujeres_8,
              total_2000 = total_9,
              males_2000 = hombres_10,
              females_2000 = mujeres_11,
              total_2005 = total_12,
              males_2005 = hombres_13,
              females_2005 = mujeres_14,
              total_2010 = total_15,
              males_2010 = hombres_16,
              females_2010 = mujeres_17,
              total_2020 = total_18,
              males_2020 = hombres_19,
              females_2020 = mujeres_20,)

# Deselect missing values
pop <- subset(pop, !is.na(age_group))

# Clean the age variable
pop$age_group <- pop$age_group %>%
  str_remove_all(( "[a-z]*ñ*N*á*")) %>% 
  str_replace(., pattern = "  ", "-") %>% 
  str_remove_all(" ") %>% 
  factor(levels = c("0-4", "5-9","10-14", "15-19",
                    "20-24", "25-29", "30-34", "35-39",
                    "40-44", "45-49", "50-54", "55-59",
                    "60-64", "65-69", "70-74", "75-79", 
                    "80-84", "85-89", "90-94", "95-99",
                    "100-", ""))

# Reshape long
pop <- pivot_longer(pop, 
                    cols = total_1990:females_2020,
                    names_to = c("group", "year"),
                    values_to = "pop",
                    names_sep = "_" )

#### Plot the data --------------------------------------------------------

# Plot the distribution of men and women by age, region and year
pop %>% filter(group != "total") %>% 
  ggplot(aes(age_group, pop, colour = group, group = interaction(group, year), linetype = year)) +
  geom_line() +
  facet_wrap(~ region, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Save the data 
save(pop, file= "Data/pop_reg_Mex.Rda")


##########            END             ##############
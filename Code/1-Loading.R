### Load Births #########################################
# Purpose: raw birth data                               #
# Author: Henrik-Alexander Schubert                     #
# E-Mail: schubert@demogr.mpg.de                        #
# Date: 30th May 2023                                   #
# Prerequisites: cleaned birth data and functions       #
#########################################################

### Settings  ----------------------------------------------------------------

  # Load the packages
  source("Functions/Packages.R")
  source("Functions/Graphics.R")
  source("Functions/Functions.R")
  
  # Path:
  # https://en.www.inegi.org.mx/contenidos/programas/natalidad/microdatos/2021/natalidad_base_datos_2021_dbf.zip

### Preparations -------------------------------------------------------------

  # Years
  years <- 1990:2021
  
  # Create folders
  if(!all(file.exists(paste0("Raw/", years)))) map(paste0("Raw/", years), dir.create, showWarnings = FALSE)

  # Zipfile
  zipfile <- "Raw/Mexico"
  
### Get and clean the data ---------------------------------------------------
    
  # Load the Data
  if(!all(file.exists(paste0("Raw/Mexico", years, "_dbf.zip")))){
  map(years, load_data_MEX, zipfile)
  }
  
  # Unzip the data
  data <- map(years, unzip_MEX)
  
  # Clean the names
  data <- lapply(data, clean_names_MEX) 
  
  # Clean the data
  data <- lapply(data, clean_data_MEX) 
    
  # Save the data
  names(data) <- years
  
  # Select
  data <- lapply(data, select, c(entity, age_mot, age_fat, year, parity) )
  
  # Make a list of regions and years
  data <- bind_rows(data)
  
  # Save the data
  save(data, file = "Data/births_complete_MEX.Rda")


##########            END             ##############
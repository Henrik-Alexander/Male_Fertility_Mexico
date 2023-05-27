################################################################################
#               Subnational Birth Squeezes                                     #
################################################################################

### Preperations -------------------------------------------------------------

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
  if(!file.exists("Data/births_complete_MEX.Rda")) save(data, file = "Data/births_complete_MEX.Rda")


##########            END             ##############
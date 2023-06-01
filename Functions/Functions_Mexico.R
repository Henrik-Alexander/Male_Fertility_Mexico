#  Functions to load, clean and analyze the data for Mexico
#  Henrik-Alexander Schubert
#  Max-Planck-Institute for Demographic Research
#  University of Oxford


## Functions: Legend -----------------------------------------
## 1. load_data_MEX   = loads the data for Mexico
## 2. unzip_MEX       = unzips the birth data for Mexico
## 3. clean_names_MEX = cleans the variable names in the birth data
## 4. clean_data_MEX  = cleans the variables in the birth data
## 5. Impute_variable = conditional approach to imputation
## 7. impute_unconditional = unconditional approach to imputation


### Set the encoding ---------------------------------------------------

#Sys.setlocale("LC_ALL", "ES_ES.UTF-8")

### Load the state names -----------------------------------------------
if(!file.exists("Data/geogr_entities.Rda")){
  
  # Set the path
  website <- "https://en.wikipedia.org/wiki/Administrative_divisions_of_Mexico"
  
  # Reading the table
  website <- read_html(website)
  
  # Obtain the pieces of the web page
  entities <- html_node(page, ".wikitable")
  
  # Convert the html table element into a data frame
  entities <- html_table(entities, fill = TRUE)
  
  # Add a number
  entities$entity <- factor(1:nrow(entities))
  
  # Clean the data
  entities <- clean_names(entities) %>% 
    rename(entity_name = name_of_federative_entity) %>%
    select(entity_name, entity)
  
  # Save the data
  save(entities, file = "Data/geogr_entities.Rda")
}


#### Load data for mexico -----------------------------------

load_data_MEX <- function(year, zipfile){
  
  # Set the path
  path <- "https://en.www.inegi.org.mx/contenidos/programas/natalidad/microdatos/"
  mid <- "/natalidad_base_datos_"
  end <- "_dbf.zip"
  
  # Set the path
  pathtmp <- paste0(path, year, mid, year, end)
  
  # Set the zipfile
  ziptmp <- paste0(zipfile,year, end )
  
  # Download the data
  GET(pathtmp, write_disk(ziptmp, overwrite = FALSE), progress())
  

}

### Unzip the file -------------------------------------------

unzip_MEX <- function(year){

    # Set the path
    path <- "https://en.www.inegi.org.mx/contenidos/programas/natalidad/microdatos/"
    mid <- "/natalidad_base_datos_"
    end <- "_dbf.zip"
    
    # Set the zipfile
    ziptmp <- paste0(zipfile,year, end )
  
    # Unzip the data
    utils::unzip(zipfile = ziptmp, exdir =  paste0("Raw/Mexico/", year))
    
    # Year shortcut
    y <- str_sub(year, 3, 4)
    
    # Load the data
    tmp <- foreign::read.dbf(paste0("Raw/Mexico/",year, "/NACIM", y , ".dbf"))
    
    # Create a year column
    tmp$year <- year
    
    # Return the result
    return(tmp)

}

### Clean the names ------------------------------------------

## List of variables; number of waves in parantheses
# Entity of registration (32), municiplaity of registration (32), 
# age of the mother (32), age of the father (32), year of registration (32), year of occurence, parity(32), live births (32),
# marital status (32), education of mother (32), education of father (32), activity mother (32), activity father (32)

clean_names_MEX <- function(...){ 
  
  ### Assign 
  dat <- (...)
  
  # Make lower case
  dat <- clean_names(dat)
  
  # Filter
  dat <- subset(dat, select = c(ent_resid, mun_resid, edad_madn, edad_padn, ano_reg, # ano_nac,
                                orden_part, hijos_vivo, edociv_mad, escol_mad, escol_pad, act_mad, act_pad, year))
  
  # Rename 
  dat <- rename(dat, 
                entity = ent_resid,
                mun_res = mun_resid , 
                age_mot = edad_madn,
                age_fat = edad_padn, 
                year_reg= ano_reg, 
                #year_bir = ano_nac,
                parity = orden_part, 
                live_births = hijos_vivo,
                mar_mot = edociv_mad, 
                edu_mot = escol_mad, 
                edu_fat = escol_pad, 
                act_mot = act_mad,
                act_fat = act_pad,
                year = year)
  

  
  # Return the resul
  return(dat)
}


#### Clean the data Mexico ----------------------------------------

clean_data_MEX <- function(...){
  
  ### Assign 
  dat <- (...)
  
  # Make numeric
  dat <- mutate(dat, across(c(age_fat, age_mot, parity), as.integer))
  
  # Recode age as missing
  vars <- c("age_fat", "age_mot", "parity", "live_births", "mun_res", "entity")
  dat[, vars] <- sapply(dat[ ,vars], function(x) ifelse(x == 99, NA, x))
  
  # If a birth took place below age 12 recode to age 12
  try(dat[dat$age_mot < 12 & !is.na(dat$age_mot), ]$age_mot <- 12)
  try(dat[dat$age_fat < 12 & !is.na(dat$age_fat), ]$age_fat <- 12)
  
  # If a birth takes place at old age
  try(dat[dat$age_mot > 49 & !is.na(dat$age_mot), ]$age_mot <- 49)
  try(dat[dat$age_fat > 59 & !is.na(dat$age_fat), ]$age_fat <- 59)
  
  # Recode education and activity
  vars <- c("edu_fat", "edu_mot", "act_fat", "act_mot", "mar_mot")
  dat[, vars] <- sapply(dat[, vars], function(x) ifelse(x == 9, NA, x))
  
  # Recode
  dat$live_births <- ifelse(dat$live_births == 999, NA, dat$live_births)
  
  # Make factor
  dat <- mutate(dat, across(c(mar_mot, edu_mot, edu_fat, act_mot, act_fat), as.factor))
  
  
  return(dat)
}

### Change the coding of entities --------------------------------------

# State names
#data$entity <- factor(data$entity, 
#                      labels = c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Coahuila de Zaragoza", 
#                                "Colima", "Chiapas",  "Chihuahua", "Distrito Federal", "Durango", "Guanajuato", "Guerrero",  "Hidalgo",
#                                "Jalisco", "México","Michoacán de Ocampo", "Morelos","Nayarit","Nuevo León", "Oaxaca", "Puebla", "Querétaro",
#                                "Quintana Roo", "San Luis Potosí", "Sinaloa",  "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", 
#
#                              "Veracruz de Ignacio de la Llave", "Yucatán", "Zacatecas", "USA", "Other Latin American Countries", "Other countries"))

### Estimate mid-year population ---------------------------------------

mid_year_pop <- function(pop1, pop2){
  pop <- (pop1 + pop2) / 2
 return(pop) 
}


#### Impute ------------------------------------------------------------


##  Function to impute variable based on another variable
impute_variable <- function(data = d, outcome = age_mot, predictor = age_fat){
  
  # Filter the non-missing data
  nmiss     <- data %>% filter(!is.na({{outcome}}))

  # Filter the missing data
  miss      <- data %>% filter(is.na({{outcome}}) & !is.na({{predictor}})) %>% 
    group_by({{predictor}}) %>% count()
  
  # Estimate the conditional distribution
  cond_dist <- nmiss %>% 
    filter(!is.na({{predictor}})) %>%
    group_by({{predictor}}) %>% 
    mutate(Total = n()) %>% 
    group_by({{predictor}}, {{outcome}}) %>% 
    summarise(prob = n() / unique(Total), .groups = "drop") %>% 
    complete({{predictor}}, {{outcome}}, fill = list(prob = 0))
  
  # Join the cond_dist and miss
  tmp <- left_join(miss, cond_dist)

  # Estimate the imputed births
  tmp <- tmp %>% mutate(births = round(n * prob)) %>% 
    select({{predictor}}, {{outcome}},births)
  
  return(tmp)
}

#### Impute uncoditional --------------------------------------------

impute_unconditional <- function(data = d, outcome = age_mot){
  
  if(nrow(data) > 0){
  
  # Filter the non-missing data
  cond_dist <- data %>%
    filter(!is.na({{outcome}})) %>% 
    mutate(Total = n()) %>% 
    group_by({{outcome}}) %>% 
    summarise(prob = n() / unique(Total), .groups = "drop")
    
  # Filter the missing data
  miss      <- data %>% filter(is.na({{outcome}})) %>% count() %>% pull(n)
  
  # Estimate the imputed births
  cond_dist <- cond_dist %>% mutate(births = round(miss * prob))
  
  # Return 
  return(cond_dist)
  
  }else{
   
cat("NO data! \n")
    
  }
}


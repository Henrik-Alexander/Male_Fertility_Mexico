#################################################################################
#                       Subnational Birth Squeezs                               #
#                      Impute and calculate ASFRs                               #
#################################################################################

### Settings ##################################################################


## Last edited: 06.12.2021

  rm(list = ls())
  
  # load packages
  library(reshape2)
  library(tidyverse)
  library(data.table)
  library(usdata)
  
  # load functions
  source("Functions/Functions.R")


### Settings ##################################################################


  # Age range for men and women
  age_m <- 15:59
  age_f <- 15:49


### Read data #################################################################
  
  # Read birth data
  load("Data/US_states_births.rda")
  
  # Population counts
  load("Data/US_states_pop.rda")
  pop     <- pop[pop$age%in%11:99,]
  
  # Find year range
  min_year <- max(min(births$year),min(pop$year),na.rm=T)
  max_year <- min(max(births$year),max(pop$year)-1,na.rm=T)
  years    <- min_year:max_year



### Edit age variable #########################################################
  
  # Edit Texas: In 1989 missing values coded with '89' instead of '99'
  texas89 <- births$age_of_father%in%89 & births$state==44 & births$year==1989
  births$age_of_father[texas89] <- "Unknown"
  
  # Edit age variables: Restrict to age range (women)
  births$age_of_mother[births$age_of_mother<min(age_f)] <- min(age_f)
  births$age_of_mother[births$age_of_mother>max(age_f)] <- max(age_f)
  
  # Edit age variables: Restrict to age range (men)
  below <- paste(0:(min(age_m)-1))
  above <- paste((max(age_m)+1):99)
  births$age_of_father[births$age_of_father%in% below]  <- min(age_m)
  births$age_of_father[births$age_of_father%in% above] <- max(age_m)
  
  # Aggregate
  agg_formula <- as.formula("count~age_of_mother+age_of_father+year+state")
  births <- aggregate(agg_formula,data=births,FUN=sum)
  
  # Edit age of father
  births$age_of_father <- as.numeric(births$age_of_father)



### Objects for results #######################################################
  
  # Create male template objects
  f_male             <- matrix(data=0,ncol=length(years),nrow=length(age_m))
  rownames(f_male)   <- paste(age_m)
  colnames(f_male)   <- paste(years)
  
  # Create female template
  f_female           <- matrix(data=0,ncol=length(years),nrow=length(age_f))
  rownames(f_female) <- paste(age_f)
  colnames(f_female) <- paste(years)
  
  b_male             <- f_male
  b_female           <- f_female
  
  exposure_female <- f_female

### Get rates #################################################################

# List of states
states <- unique(births$state)

# Cycle over states
for(s in states)  {
  
  cat("STATE: ",s,"\n")
  
  # Cycle over years
  for(i in years) {
    
    # Mid-year population count men
    pop1_m <- pop[pop$year==i&pop$age%in%age_m&pop$state==s&pop$sex==1,c("age","pop")]
    pop2_m <- pop[pop$year==i+1&pop$age%in%age_m&pop$state==s&pop$sex==1,c("age","pop")]
    pop_m  <- (pop1_m+pop2_m)/2
    
    # Mid-year population count women
    pop1_f <- pop[pop$year==i&pop$age%in%age_m&pop$state==s&pop$sex==2,c("age","pop")]
    pop2_f <- pop[pop$year==i+1&pop$age%in%age_m&pop$state==s&pop$sex==2,c("age","pop")]
    pop_f  <- (pop1_f+pop2_f)/2
    
    # Birth counts: Marginals
    birth_year   <- births[births$year==i&births$state==s,]
    births_tmp_m <- aggregate(count~age_of_father,data=birth_year,sum)
    births_tmp_f <- aggregate(count~age_of_mother,data=birth_year,sum)
    
    # Births with missing age of father
    birth_mis      <- births[births$year==i&is.na(births$age_of_father)&births$state==s,]
    births_tmp_fna <- aggregate(count~age_of_mother,data=birth_mis,sum)
    
    # Ages of mother with births with missing age of father
    missing_age <- unique(births_tmp_fna$age_of_mother)
    
    # Distribute missing values: Loop over ages of mother
    for(j in missing_age) {
      
      # Fall-back option: If no births with age of father
      # then impute age of father as age of mother plus 3
      all_missing <- birth_year[birth_year$age_of_mother==j,"age_of_father"]
      all_missing <- all(is.na(all_missing))
      
      # If all missing: Create artificial counts
      if(all_missing) {
        
        fathers <- data.frame(count=births_tmp_fna[births_tmp_fna$age_of_mother==j,"count"],age_of_father=j+3)
        
        # If not all missing: Get counts          
      } else {
        
        fathers <- aggregate(count~age_of_father,data=birth_year[birth_year$age_of_mother==j,],sum)
        
      }
      
      # Calculate distribution for imputation
      fathers$count                       <- fathers$count/sum(fathers$count)
      # Where to add missing births in current distribution
      matching_ages                       <- match(fathers$age_of_father,births_tmp_m$age_of_father)
      # How many to add in current distribution
      distr_tmp                           <- births_tmp_fna[births_tmp_fna$age_of_mother==j,"count"]*fathers$count
      # Add to current distribution
      births_tmp_m[matching_ages,"count"] <- births_tmp_m[matching_ages,"count"]+distr_tmp
      
    }
    
    # Match ages of population counts and births
    popmatch_m <- !is.na(match(pop_m$age,births_tmp_m$age_of_father))
    popmatch_f <- !is.na(match(pop_f$age,births_tmp_f$age_of_mother))
    
    # Birth counts
    b_female[paste(age_f[popmatch_f]),paste(i)] <- births_tmp_f$count
    b_male[paste(age_m[popmatch_m]),paste(i)]   <- births_tmp_m$count
    
    # ASFRs
    f_female[paste(age_f[popmatch_f]),paste(i)] <- births_tmp_f$count/pop_f[popmatch_f,"pop"]
    f_male[paste(age_m[popmatch_m]),paste(i)]   <- births_tmp_m$count/pop_m[popmatch_m,"pop"]
    
    
    # exposure
    exposure_female[paste(age_f[popmatch_f]),paste(i)] <- pop_f[popmatch_f, "pop"]
    
    
  }
  
  # Assign results
  assign(paste0("f_male_",s),f_male)
  assign(paste0("f_female_",s),f_female)
  assign(paste0("b_male_",s),b_male)
  assign(paste0("b_female_",s),b_female)
  assign(paste0("exposure.", s), exposure_female)
  
}

# Save
tosave <- c(paste0("f_male_",1:51),
            paste0("f_female_",1:51),
            paste0("b_male_",1:51),
            paste0("b_female_",1:51),
            paste0("exposure.", 1:51))
save(list=tosave,file=("Data/US_states_ASFR.rda"))  


### END ########################################################################  
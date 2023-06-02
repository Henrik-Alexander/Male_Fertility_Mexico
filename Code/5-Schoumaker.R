### Spatial analysis  ###################################
# Purpose: Analyse the male and female fertility rates  #
# Author: Henrik-Alexander Schubert                     #
# E-Mail: schubert@demogr.mpg.de                        #
# Date: 30th May 2023                                   #
# Prerequisites: functions                              #
#########################################################

### Settings -----------------------------------------------------------------

# Load the packages
source("Functions/Packages.R")
source("Functions/Graphics.R")
source("Functions/Functions.R")


### Data wrangling -----------------------------------------------------------

# Load the mortality data
mortality <- fread("Raw/mortality_mexico_un.csv")

# Clean the names
mortalit <- mortality %>%
  clean_names() %>% 
  select(indicator_short_name, source, time, author, location, sex, age, value)

# Life table function
lifetable <- function(mx){
  
  ## ------------------------------------------------------------------------
  # let's first define our ages
  Age <- 0:length(mx)
  nages <- length(Age)
  
  # the length of the age interval
  nx <- c(diff(Age),1/mx[length(Age)])
  
  # the number of person years lived by those who die
  # following the HMD, we will use the Andreev & Kingkade formulas
  
  a0FUN <- function(sex,mx){
    ax <- NA
    if (sex == 'm') {
      ax[1] <- ifelse(mx[1]>=0.08307,0.29915,
                      ifelse(mx[1]<0.023,0.14929 - 1.99545 * mx[1],
                             0.02832 + 3.26021 * mx[1]))
    }
    
    if (sex == 'f') {
      ax[1] <- ifelse(mx[1]>=0.06891,0.31411,
                      ifelse(mx[1]<0.01724,0.14903 - 2.05527 * mx[1],
                             0.04667 + 3.88089 * mx[1]))
    }
    return(ax[1])
  }
  
  ax <- nx/2
  ax[1] <- a0FUN(sex='m',mx=mx)
  ax[nages] <- 1/mx[nages]
  
  # death probabilities: mx to qx conversion
  qx <-  nx * mx / (1 + nx * (1 - ax) * mx)
  qx[nages] = 1
  
  # survival probabilities
  px = 1 - qx
  
  # life table survivors with a radix of 100 000
  lx = rep(NA,nages)
  lx[1] = 100000
  for(i in 1:(nages-1)){
    lx[i+1] <- lx[i]*px[i]
  }
  
  # life table deaths
  dx = lx*qx
  dx[nages] = lx[nages]
  
  # person-years lived in the age interval
  Lx = nx * lx - nx * (1 - ax) * dx
  Lx[nages] = lx[nages] * ax[nages]
  
  # person-years lived above age x
  Tx = rev(cumsum(rev(Lx)))
  
  # remaining life expectancy
  ex <- Tx/lx
  
  # putting everything in a dataframe
  LT <- data.frame(Age=Age,
                   n=nx,
                   mx=round(mx,5),
                   qx=round(qx,5),
                   ax=round(ax,2),
                   lx=round(lx,0),
                   dx=round(dx,0),
                   Lx=round(Lx,0),
                   Tx=round(Tx,0),
                   ex=round(ex,2))
  
  
}




# Estimate the mean lenght of generation
generation_length <- function(x, lx, asfr){
  gen_len <- sum(x * lx * asfr) / sum(lx * asfr)
  return(gen_len)
}

# Estimate the growth rate
growth_rate <- function(px, asfr){
  r <- sum(asfr * px)
  return(r)
}

# Mean age of childbearing
mean_age <- function(age, asfr){
  res <- sum(asfr * age) / sum(asfr)
  return(res)
}

# Schoumaker function
schoumaker <- function(SRB = 1.05, pMAC, pMAF, r = 1, TM, TF){
  tmp <- 1 / SRB * pMAC / pMAF * exp(r * (TM - TF))
  return(tmp)
}


### Load the mortality data ------------------------------------------------




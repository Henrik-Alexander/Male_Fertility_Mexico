## Functions


#### Create directory structure -------------------

for(f in c("Data", "Results", "Figures")) {
  if(!file.exists(f)) dir.create(f)
}

#### Negate in function -------------------------------------

`%!in%` <- Negate(`%in%`)


#### Tabulate function --------------------------------------


tab <- function(...){
  
  tmp <- table(..., useNA = "always")
  return(tmp)
  
}


#### Load country specific functions ------------------------

source("Functions/Functions_Mexico.R")

#####               END               ########################





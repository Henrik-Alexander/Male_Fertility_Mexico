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

#### Life table function ------------------------------------

lifetable <- function(mx, sex = "M"){
  
# Let's first define our ages
age <- seq_along(mx)
nages <- length(age)

# The length of the age interval
nx <- c(diff(age),1/mx[length(age)])

# Estimate the number of person years lived by those who die
# following the HMD, we will use the Andreev & Kingkade formulas

a0FUN <- function(sex,mx){
  ax <- NA
  if (sex == 'M') {
    ax[1] <- ifelse(mx[1]>=0.08307,0.29915,
                    ifelse(mx[1]<0.023,0.14929 - 1.99545 * mx[1],
                           0.02832 + 3.26021 * mx[1]))
  }
  
  if (sex == 'F') {
    ax[1] <- ifelse(mx[1]>=0.06891,0.31411,
                    ifelse(mx[1]<0.01724,0.14903 - 2.05527 * mx[1],
                           0.04667 + 3.88089 * mx[1]))
  }
  return(ax[1])
}

ax <- nx/2
ax[1] <- a0FUN(sex=sex,mx=mx)
ax[nages] <- 1/mx[nages]

# Estimate the death probabilities: mx to qx conversion
qx <-  nx * mx / (1 + nx * (1 - ax) * mx)
qx[nages] = 1

# Estimate the survival probabilities
px = 1 - qx

# Estimate thelife table survivors with a radix of 100 000
lx = rep(NA,nages)
lx[1] = 100000
for(i in 1:(nages-1)){
  lx[i+1] <- lx[i]*px[i]
}

# Estimate the life table deaths
dx = lx*qx
dx[nages] = lx[nages]

# Estimate the person-years lived in the age interval
Lx = nx * lx - nx * (1 - ax) * dx
Lx[nages] = lx[nages] * ax[nages]

# Aggregate person-years lived above age x
Tx = rev(cumsum(rev(Lx)))

# Estimate remaining life expectancy
ex <- Tx/lx

# putting everything in a dataframe
LT <- data.frame(age=age,
                 n=nx,
                 mx=round(mx,5),
                 qx=round(qx,5),
                 px=round(1 - qx, 5),
                 ax=round(ax,2),
                 lx=round(lx,0),
                 dx=round(dx,0),
                 Lx=round(Lx,0),
                 Tx=round(Tx,0),
                 ex=round(ex,2))

return(LT)
}


#### Population Share ----------------------------------------

pop_share <- function(population){
  share <- population / sum(population, na.rm = TRUE)
  return(share)
}

#### Estimate the difference --------------------------------

difference <- function(x, y){
  diff <- y - x
  return(diff)
}

#### Average ------------------------------------------------

averaging <- function(x, y){
  m <- (y + x) / 2
  return(m)
}

#####               END               ########################





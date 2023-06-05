### Meta - File

# This file executes all the files in the /Code/ folder. 

#===================================================
# Requirements: 
# Functions are stored
# Packages are installed
# Population, mortality and shape data is loaded and saved in Raw
#====================================================

# 1. Load the birth data
source("Code/1-Loading.R")

# 2. Impute the births
source("Code/2-Impute_births.R")

# 3. Load the population data
source("Code/3-Loading_Pop.R")

# 4. Analysis
source("Code/4-Analysis.R")

# 5. Spatial analysis
source("Code/5-SpatialAnalysis.R")

# 6. Stable population theory
source("Code/6-StablePop.R")

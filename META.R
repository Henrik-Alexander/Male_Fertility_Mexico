### META  ###############################################
# Purpose: Run all the sub-files                        #
# Author: Henrik-Alexander Schubert                     #
# E-Mail: schubert@demogr.mpg.de                        #
# Date: 06th May 2023                                   #
# Pre-requisites:                                       #                 
# - Functions are stored                                #
# - Packages are installed                              #
# - Population, mortality, shape data is saved in Raw   #
#########################################################


# 1. Load the birth data
source("Code/01-Loading_Births.R")

# 2. Impute the births
source("Code/02-Impute_Births.R")

# 3. Load the population data
source("Code/03-Loading_Pop.R")

# 4. Demographic analysis
source("Code/04-Analysis.R")

# 5. Spatial analysis
source("Code/05-Spatial_Analysis.R")

# 6. Stable population theory
source("Code/06-Stable_Pop.R")


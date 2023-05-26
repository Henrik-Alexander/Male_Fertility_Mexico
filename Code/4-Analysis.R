################################################################################
#                                                                              #
#         Max-Planck-Institute for Demographic Research                        #
#               Subnational Birth Squeezes                                     #
################################################################################

### Preperations -------------------------------------------------------------

rm(list = ls())

# Load the packages
source("Functions/Packages.R")
source("Functions/Graphics.R")
source("Functions/Functions.R")

# Load the data
load("Data/births_complete_MEX.Rda")

### Data analysis ------------------------------------------------------------

# Function to get the missing values
nr_missing <- function(data){
  miss <- colMeans(is.na(data))
  miss$Year <- unique(data$year)
  return(miss)
}

# Estimate
missing <- lapply(data, nr_missing)


missing <- bind_rows(missing)

# 
missing_plot <- ggplot(missing, aes(Year, age_mot)) +
  geom_line(aes(col = "Age of Mother"), linewidth = 1.4) +
  geom_line(aes(y = age_fat, col = "Age of Father"), linewidth = 1.4) +
  ylab("Share missing") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.2), expand = c(0, 0)) +
  scale_colour_manual(values = c("Age of Mother" = "red", "Age of Father" = "blue")) +
  ggtitle("Share of missing values for 'Age of mother' and 'Age of mother'")

ggsave(missing_plot, filename = "Figures/share_missing_sex.pdf")

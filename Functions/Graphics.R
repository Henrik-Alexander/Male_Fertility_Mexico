### Male fertility in household surveys


## This file adjusts the graphic style for the reproduction of the graphs



### 1. Graphi Scheme -------------------------------


library(tidyverse)

# set theme
theme_set(theme_test(base_size = 14, base_family = "serif"))
theme_update(plot.margin = margin(0.1, 0.6, 0.1, 0.1, "cm"),
             panel.grid.major.y = element_line(colour = "grey80"),
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank(),
             panel.grid.minor.y = element_blank(),
             legend.background = element_rect(fill = "white", colour = "grey80"),
             legend.title = element_text(face = "bold"),
             axis.title.x = element_text(face = "bold", size = 14),
             legend.position = "bottom",
             axis.title.y = element_text(face = "bold", size = 14),
             plot.title = element_text(hjust = 0.5),
             title = element_text(face = "bold")
             
)

### Spatial analysis  ###################################
# Purpose: Analyse the male and female fertility rates  #
# Author: Henrik-Alexander Schubert                     #
# E-Mail: schubert@demogr.mpg.de                        #
# Date: 30th May 2023                                   #
# Prerequisites: functions                              #
#########################################################

### Settings -----------------------------------------------------------------

rm(list = ls())

# Load the packages
source("Functions/Packages.R")
source("Functions/Graphics.R")
source("Functions/Functions.R")

# Load the fertiltiy data
load("Data/tfr_regional_mexico.Rda")

# Set the coordinate reference system
crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Load the data world
world <- ne_countries(scale = "medium", returnclass = "sf")

### Data wrangling -----------------------------------------------------------

# Load the shape data
shape <- read_sf("Raw/Shape/00ent.shp")

# Create names
names(shape) <- c("entity", "code", "state", "geometry")

# Mutate the values
shape <- shape %>% select(entity, state, geometry) %>% 
  mutate(entity = factor(as.integer(entity)))

# Transform the coordinate system
shape <- st_transform(shape, crs = crs)

# Combine the shape data
tfr_spat <- inner_join(shape, tfr_reg, by = c("entity" = "entity"))


### Plotting  ---------------------------------------------------------------

# Plot
tfr_spat %>% filter(year == 2018) %>% 
  ggplot(aes(fill = tfr_f)) +
  geom_sf(data = world, fill = "grey") +
  geom_sf() +
  scale_fill_viridis_c(option = "B", direction = -1, name = "Female TFR") +
  ggtitle("Female TFR in the states of Mexico") +
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) +
  theme(panel.background =  element_rect(fill = "aliceblue"),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  coord_sf(xlim = c(-120, -85), ylim = c(12, 35), expand = FALSE)

# Plot
tfr_spat %>% filter(year == 2019) %>% 
  ggplot(aes(fill = tfr_m)) +
  geom_sf(data = world, fill = "grey") +
  geom_sf() +
  scale_fill_viridis_c(option = "B", direction = -1, name = "Male TFR") +
  ggtitle("Male TFR in the states of Mexico") + 
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) +
  theme(panel.background =  element_rect(fill = "aliceblue"),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  annotate(geom = "text", x = -91, y = 25, label = "Gulf of Mexico", 
           fontface = "italic", color = "grey22", size = 6 ) +
  coord_sf(xlim = c(-120, -85), ylim = c(12, 35), expand = FALSE)


### Male to female TFR Ratio ---------------------------------------

# Estimate the TFR male to TFR female ratio
tfr_reg <- tfr_reg %>%
                      mutate(tfr_ratio = tfr_m / tfr_f,
                      tfr_ratio_cat = cut(tfr_ratio, breaks = c(0, 0.95, 1.05, 100)))

# Join with spatial information
tfr_reg <- inner_join(tfr_reg, shape, by = c("entity" = "entity"))

# Plot
plot_panel_ratio <- tfr_reg %>%
  filter(year %in% c(1990, 2000, 2010, 2020)) %>% 
  ggplot(aes(geometry = geometry, fill = tfr_ratio)) +
    geom_sf(data = world, fill = "grey") +
    geom_sf() +
    facet_wrap(~ year, ncol = 2) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 1, name = "TFR men / TFR women:") +
    ggtitle("Birth squeezes in the states of Mexico") +
    annotation_scale(location = "bl", width_hint = 0.2) + 
    annotation_north_arrow(location = "bl", pad_x = unit(0.25, "in"), pad_y = unit(0.5, "in"), which_north = "true", style = north_arrow_fancy_orienteering) +
    theme(panel.background =  element_rect(fill = "aliceblue"),
          axis.title = element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank()) +
  annotate(geom = "text", x = -91, y = 25, label = "Gulf of Mexico", 
           fontface = "italic", color = "grey22", size = 4 ) +
  coord_sf(xlim = c(-120, -85), ylim = c(12, 35), expand = FALSE)

plot_panel_ratio

# save the plot
ggsave(plot_panel_ratio, filename = "Figures/panel_birthsqueeze_mexico.pdf", height = 20, width = 25, units = "cm")

### END ########################################################################  
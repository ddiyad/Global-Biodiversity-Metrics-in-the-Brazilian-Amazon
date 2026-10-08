#### packages ----
library(geobr) 
library(terra)
library(sf)
library(tidyverse)
library(viridis)
library(ggplot2)
library(rnaturalearth)
library(tmap)
library(ggmap)
library(raster)
library(ggnewscale)
library(biscale)
library(cowplot)
library(patchwork)

#### data  ----
GlobioMSA <- rast("Globio4_TerrestrialMSA_10sec_2015/TerrestrialMSA_2015_World.tif")
GlobioMSA <- project(GlobioMSA, "EPSG:4674")

Squam <- readRDS("amaz_squamates.rds")
Anu <- readRDS("amaz_anurans.rds")
Bir <- readRDS("amaz_birds.rds")
Mam <- readRDS("amaz_mammals.rds")

#### First step - define the region of interest   

location <- read_biomes(year = 2019) %>% 
  filter(name_biome == "Amazônia")

location <- st_make_valid(location)

# Create a hexagonal grid
grid <- st_make_grid(location, cellsize = 1, square = FALSE) ## 1 = 111 km
grid_sf <- st_sf(grid_id = 1:length(grid), geometry = grid)

# location hexagonal

loc_hex <- st_intersection(grid_sf, location)

# Remove slivers/artifacts from the intersection
loc_hex <- st_collection_extract(loc_hex, "POLYGON")
loc_hex <- st_make_valid(loc_hex)

#Crop raster into Amazonia 
MSA_cropped <- crop(GlobioMSA, location)
MSA_clipped <- mask(MSA_cropped, location)

#### data extraction per hexagon ----
sf::sf_use_s2(FALSE)

#### Cast species richness to location 

Squam <- st_cast(Squam, "MULTIPOLYGON")
Squam <- st_make_valid(Squam)
Squam <- st_transform(Squam, st_crs(loc_hex))
loc_Squam <- st_intersection(loc_hex, Squam) ##### 
loc_Squam <- st_simplify(loc_Squam, dTolerance = 0.0001)

Squam_richness <- loc_Squam %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(Squam = sum(squamates_SR, na.rm = TRUE))

#_______
Anu <- st_cast(Anu, "MULTIPOLYGON")
Anu <- st_make_valid(Anu)
Anu <- st_transform(Anu, st_crs(loc_hex))
loc_Anu <- st_intersection(loc_hex, Anu) ##### 
loc_Anu <- st_simplify(loc_Anu, dTolerance = 0.0001)

Anu_richness <- loc_Anu %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(Anu = sum(anurans_SR, na.rm = TRUE))

#_______
Bir <- st_cast(Bir, "MULTIPOLYGON")
Bir <- st_make_valid(Bir)
Bir <- st_transform(Bir, st_crs(loc_hex))
loc_Bir <- st_intersection(loc_hex, Bir) ##### 
loc_Bir <- st_simplify(loc_Bir, dTolerance = 0.0001)

Bir_richness <- loc_Bir %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(Bir = sum(birds_SR, na.rm = TRUE))

#_______
Mam <- st_cast(Mam, "MULTIPOLYGON")
Mam <- st_make_valid(Mam)
Mam <- st_transform(Mam, st_crs(loc_hex))
loc_Mam <- st_intersection(loc_hex, Mam) ##### 
loc_Mam <- st_simplify(loc_Mam, dTolerance = 0.0001)

Mam_richness <- loc_Mam %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(Mam = sum(mammals_SR, na.rm = TRUE))

##Merge all
Squam_Anu <- merge(Squam_richness, Anu_richness, by = "grid_id", all = FALSE)
Squam_Anu_Bir <- merge(Squam_Anu, Bir_richness, by = "grid_id", all = FALSE)
All_SR <- merge(Squam_Anu_Bir, Mam_richness, by = "grid_id", all = FALSE)

#_________
columns_to_sum <- c("Squam", "Anu", "Mam", "Bir")

All_SR_totals <- All_SR %>%
  group_by(grid_id) %>% # Group by the 'grid_id' column
  mutate(
    SR_All_total = sum(c_across(all_of(columns_to_sum)), na.rm = TRUE) # Calculate sum of specific columns within each group
  )

## Calculate average MSA per hexagon

location_hex_proj <- st_transform(loc_hex, crs(MSA_clipped))

avg_MSA_per_hexagon <- terra::extract(MSA_clipped, vect(location_hex_proj), fun = mean, na.rm = TRUE)

avg_MSA_per_hexagon$grid_id <- location_hex_proj$grid_id[avg_MSA_per_hexagon$ID]

avg_MSA_per_hexagon <- avg_MSA_per_hexagon %>% dplyr::select(grid_id, TerrestrialMSA_2015_World)

final_data_for_plot <- merge(All_SR_totals, avg_MSA_per_hexagon, by = "grid_id", all = FALSE)

sf <- full_join(final_data_for_plot, loc_hex)

your_data <- st_as_sf(sf)

bi_data_all <- bi_class(your_data,  SR_All_total, TerrestrialMSA_2015_World, style = "quantile", dim = 3 )

#### data extraction per hexagon ----

sf::sf_use_s2(FALSE)


#combining SR and MSA


bi_all <- ggplot() +
  geom_sf(data = bi_data_all, mapping = aes(fill = bi_class), color = NA, size = 0.0001, show.legend = FALSE) +
  bi_scale_fill(pal = "BlueYl", dim = 3) +
  bi_theme(
    base_size = 5
  )



bi_all_leg <- bi_legend(pal = "BlueYl",
                        dim = 3,
                        xlab = "SR",
                        ylab = "MSA",
                        size = 10,
)


finalPlot_all <- ggdraw() +
  draw_plot(bi_all, 0, 0, 1, 1) +
  draw_plot(bi_all_leg, 0, 0, 0.2, 0.3)



final_scatplot_all <- ggplot(final_data_for_plot, aes(x=final_data_for_plot$SR_All_total, y=final_data_for_plot$TerrestrialMSA_2015_World)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_minimal()



final_all <- final_scatplot_all + theme_bw(base_size = 5) + theme(axis.text = element_text(size = 10), axis.title = element_text(size = 11), plot.title = element_text(hjust = 0.5)) +
  labs(
    title = "Terrestrial Species Richness vs Terrestrial Means Species Abundance",
    x = "Species Richness",
    y = "Means Species Abundance"
  ) 


p5 <- wrap_elements((
  (finalPlot_all + labs(title = NULL)) |
    (final_all + labs(title = NULL))
) +
  plot_layout(widths = c(1.05, 1)) +
  plot_annotation(
    title = "Terrestrial Species Richness vs Terrestrial Means Species Abundance",
    theme = theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 11,
        margin = margin(b = 2)
      ),
      plot.margin = margin(0, 0, 0, 0)
    )
  ) &
  theme(
    plot.margin = margin(1, 1, 1, 1)
  ))

p5

ggsave("Finished Graphs/Species Richness vs MSA.png", 
       p5, width = 11, height = 5, dpi = 300)

all_model <- lm(SR_All_total ~ TerrestrialMSA_2015_World, data = final_data_for_plot)
summary_all_model <- summary(all_model)
all_r_squared_value <- summary_all_model$r.squared
all_r_squared_value
all_conf_int <- confint(all_model, level = 0.95)
all_conf_int

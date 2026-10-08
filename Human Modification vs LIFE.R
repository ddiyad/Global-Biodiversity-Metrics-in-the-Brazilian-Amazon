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
Land <- rast("Global_Human_Modification_gHM.tif")
LIFE <- rast("scaled_arable_0.25.tif")

Land <- project(Land, "EPSG:4674")
LIFE <- project(LIFE, "EPSG:4674")

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
Land_cropped <- crop(Land, location)
Land_clipped <- mask(Land, location)

LIFE_cropped <- crop(LIFE, location)
LIFE_clipped <- mask(LIFE_cropped, location)

#### data extraction per hexagon ----
sf::sf_use_s2(FALSE)

## Calculate average LandMod per hexagon

location_hex_proj <- st_transform(loc_hex, crs(Land_clipped))

avg_Land_per_hexagon <- terra::extract(Land_clipped, vect(location_hex_proj), fun = mean, na.rm = TRUE)

avg_Land_per_hexagon$grid_id <- location_hex_proj$grid_id[avg_Land_per_hexagon$ID]

avg_Land_per_hexagon <- avg_Land_per_hexagon %>% dplyr::select(grid_id, gHM)

## Calculate average LandMod per hexagon

location_hex_proj <- st_transform(loc_hex, crs(LIFE_clipped))

avg_LIFE_per_hexagon <- terra::extract(LIFE_clipped, vect(location_hex_proj), fun = mean, na.rm = TRUE)

avg_LIFE_per_hexagon$grid_id <- location_hex_proj$grid_id[avg_LIFE_per_hexagon$ID]

avg_LIFE_per_hexagon <- avg_Land_per_hexagon %>% dplyr::select(grid_id, all)

final_data_for_plot <- merge(avg_Land_per_hexagon, avg_LIFE_per_hexagon, by = "grid_id", all = FALSE)

sf <- full_join(final_data_for_plot, loc_hex)

your_data <- st_as_sf(sf)

m <- bi_class(your_data, gHM, all, style = "quantile", dim = 3 )

#### data extraction per hexagon ----

sf::sf_use_s2(FALSE)

#combining HM & LIFE

mappy <- ggplot() +
  geom_sf(data = m, mapping = aes(fill = bi_class), color = NA, size = 0.0001, show.legend = FALSE) +
  bi_scale_fill(pal = "BlueYl", dim = 3) +
  labs(
    title = ""
  ) +
  bi_theme(
    base_size = 5
  )

legend <- bi_legend(pal = "BlueYl",
                    dim = 3,
                    xlab = "HM",
                    ylab = "LIFE",
                    size = 10,
)

finalPlot <- ggdraw() +
  draw_plot(mappy, 0, 0, 1, 1) +
  draw_plot(legend, 0, 0, 0.2, 0.3)

## Scatterplot comparing SR to MSA (has 30 errors)

final_plot <- ggplot(final_data_for_plot, aes(x=final_data_for_plot$gHM, y=final_data_for_plot$all)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_minimal()

#add r squared to graph rounded to two decimals

final_all <- final_plot + theme_bw(base_size = 5) + theme(axis.text = element_text(size = 10), axis.title = element_text(size = 11), plot.title = element_text(hjust = 0.5)) +
  labs(
    title = "Human Modification vs LIFE",
    x = "Human Modification",
    y = "LIFE",
  ) 

p5 <- wrap_elements((
  (finalPlot + labs(title = NULL)) |
    (final_all + labs(title = NULL))
) +
  plot_layout(widths = c(1.05, 1)) +
  plot_annotation(
    title = "Human Modification vs LIFE",
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

ggsave("Finished Graphs/Human Modification vs LIFE.png", 
       p5, width = 11, height = 5, dpi = 300)

all_model <- lm(gHM ~ all, data = final_data_for_plot)
summary_all_model <- summary(all_model)
all_r_squared_value <- summary_all_model$r.squared
all_r_squared_value
all_conf_int <- confint(all_model, level = 0.95)
all_conf_int



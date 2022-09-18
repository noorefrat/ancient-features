library(tidyverse)
library(dplyr)
library(sf)
library(rnaturalearth)

# NOTE: It is not possible to set extents that crosses the 180° meridian in the function.
# This must be specified later in the ggplot function as lims (see Example 5).

project_data <-  function(
  df,  # a dataframe with Longitude, Latitude, and data
  projection = 8859, # 8859 is the crs code for equal earth WGS84 with 150° as center meridian
  xmin = -180, # minimum longitude of desired extent (in ° between -180 and 180) (default -180)
  xmax = 180, # maximum longitude of desired extent (in ° between -180 and 180) (default 180)
  ymin = -60, # minimum latitude of desired extent (in ° between -90, 90) (default -60)
  ymax = 85, # maximum latitude of desired extent (in ° between -90, 90) (default 85)
  labels_lat = -175, # position of latitude labels on longitude axis (in ° long) (default -175)
  labels_long = -52.5 # position of latitude labels on latitude axis (in ° lat) (default -52.5)
) {
  
#### Settings
  # target projection
  df_projection <-  4326 # projection code of the dataframe (e.g. 4326 for WGS84) (default 4326)
  # meridian where world map is split up
  split_meridian <- -30
  # deactivating s2 spherical geometry to make following map croppings possible
  sf_use_s2(FALSE)
  
#### Base map 
  # load coastline map
  if (!exists("base_map_initial")){
    base_map_initial <<- ne_coastline(scale = 110,
                                      returnclass = "sf")
  }
  # duplicate base_map for further processing
  base_map <- base_map_initial
  
  # create "split line" to split polygons that cross the splitting meridian
  split_line <- st_linestring(x = cbind(split_meridian,c(-90,90)), dim = "XY")
  split_line <- st_geometry(split_line) # makes it possible to assign crs
  st_crs(split_line) <- st_crs(base_map) # assign crs from base map to line
  
  # intersect line with continent polygons to identify polygons that cross splitting meridian
  base_map$intersects <- suppressMessages(st_intersects(base_map, split_line, sparse = F))
  base_map_intersects <- filter(base_map, intersects == T) # map with intersecting polygons
  base_map_cleaned <- filter(base_map, intersects == F) # map without intersecting polygons
  
  # crop polygons on both sides of splitting meridian separately
  bbox_left <- c(xmin = -180, xmax = split_meridian-0.000001, ymin = -90, ymax = 90)
  bbox_right <- c(xmin = split_meridian+0.000001, xmax = 180, ymin = -90, ymax = 90)
  # 0.000001 ensures that edges of right and left side do not have the exact same coordinates
  base_map_intersects_left <- suppressMessages(suppressWarnings(st_crop(base_map_intersects, bbox_left)))
  base_map_intersects_right <- suppressMessages(suppressWarnings(st_crop(base_map_intersects, bbox_right)))
  
  # combine all three maps
  base_map <- bind_rows(base_map_cleaned, base_map_intersects_left)
  base_map <- bind_rows(base_map, base_map_intersects_right)

  # crop map to desired plotting extent
  bbox_map <- c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  base_map <- suppressMessages(suppressWarnings(st_crop(base_map, bbox_map)))

  # reproject map
  base_map <- st_transform(base_map, crs = projection)

#### Graticule Labels
  # create data frame with grid labels (x & y are coordinates where label is located)
  loc_x <- labels_lat
  loc_y <- labels_long
  labels <- c("-20°", "0°", "20°", "40°", "60°", "80°", "100°", "120°", "140°", "160°", "180°",
              "-160°", "-140°", "-120°", "-100°", "-80°", "-60°", "-40°",
              "-80°", "-70°", "-60°", "-50°", "-40°", "-30°", "-20°", "-10°", "0°",
              "10°", "20°", "30°", "40°", "50°", "60°", "70°", "80°")
  x <- c(-20, 0, 20, 40, 60, 80, 100, 120, 140, 160, 180, -160, -140, -120, -100, -80, -60, -40,
         loc_x, loc_x, loc_x, loc_x, loc_x, loc_x, loc_x, loc_x, loc_x,
         loc_x, loc_x, loc_x, loc_x, loc_x, loc_x, loc_x, loc_x)
  y <- c(loc_y, loc_y, loc_y, loc_y, loc_y, loc_y, loc_y, loc_y, loc_y,
         loc_y, loc_y, loc_y, loc_y, loc_y, loc_y, loc_y, loc_y, loc_y,
         -80, -70, -60, -50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50, 60, 70, 80)
  grid_labels <- data.frame(labels, x, y)
  
  # create sf object
  grid_labels <- st_as_sf(grid_labels, coords = c("x", "y"), crs = 4326)

  # crop graticule labals to desired plotting extent
  bbox_grid <- c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  grid_labels <- suppressMessages(suppressWarnings(st_crop(grid_labels, bbox_grid)))
  
  # reproject graticule lables
  grid_labels <- st_transform(grid_labels, crs = projection)

#### Data
  # change name of coloumns to "Longitude" and "Latitude"
  if (any(names(df) %in% c('longitude')) | any(names(df) %in% c('latitude'))) {
    df$Longitude <- df$longitude
    df$Latitude <- df$latitude
  }
  if (any(names(df) %in% c('lon')) | any(names(df) %in% c('lat'))) {
    df$Longitude <- df$lon
    df$Latitude <- df$lat
  }
  if (any(names(df) %in% c('long')) | any(names(df) %in% c('lat'))) {
    df$Longitude <- df$long
    df$Latitude <- df$lat
  }
  
  # create an sf object
  df = st_as_sf(df, coords = c("Longitude", "Latitude"), crs = df_projection)

  # crop dataframe to desired plotting extent
  bbox_data <- c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  df <- suppressMessages(suppressWarnings(st_crop(df, bbox_data)))
  
  # reproject data frame to desired projection
  df <- st_transform(df, crs = projection)

####  ggplot as base plot with base map, graticule and graticule labels
  base_plot <- ggplot() +
    geom_sf(data = base_map,
            colour = "grey",
            fill = "transparent",
            size = .25,
    ) +
    scale_x_continuous(breaks = c(-20, 0, 20, 40, 60, 80, 100, 120, 140, 160, 180, -160, -140, -120, -100, -80, -60, -40)) +
    scale_y_continuous(breaks = c(-90, -80, -70, -60, -50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50, 60, 70, 80, 90)) +
    scale_size(range = c(10, 10)) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 16),
          legend.box.background = element_rect(fill = "white", color = "white"),
          panel.grid = element_line(linetype = "dashed", colour = "grey", size = .25),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank()
    ) +
    guides(color = guide_legend(override.aes = list(size = 6)))

#### Objects returned by function
  return(list(base_map = base_map,
              grid_labels = grid_labels,
              data = df,
              base_plot = base_plot))
}





# # Example usage
# library(ggplot2)

# # Some data to play with:
# autotyp_register <-  read.csv("https://raw.githubusercontent.com/autotyp/autotyp-data/master/data/csv/Register.csv", na.strings = "") %>%
#   filter(!is.na(Longitude) & !is.na(Subsistence)) %>%
#   droplevels()





# # Example 1: Whole World with graticule and labels
# autotyp_register.g <- project_data(df = autotyp_register)

# # ggplot
# autotyp_register.g$base_plot +
#   geom_sf(data = autotyp_register.g$data,
#           aes(color = Subsistence),
#           alpha = .5,
#           size = 2
#   ) +
#   scale_color_manual(name = "Subsistence:",
#                      values = c(`not hg` = "darkgreen",
#                                 `hg` = "blue",
#                                 split = "black")
#   ) +
#   geom_sf_text(data = autotyp_register.g$grid_labels,
#                aes(label = labels), size = 2, color = 'grey30'
#   )





# # Example 2: Whole World without graticule and labels
# autotyp_register.g <- project_data(df = autotyp_register)

# # ggplot
# autotyp_register.g$base_plot +
#   geom_sf(data = autotyp_register.g$data,
#           aes(color = Subsistence),
#           alpha = .5,
#           size = 2
#   ) +
#   scale_color_manual(name = "Subsistence:",
#                      values = c(`not hg` = "darkgreen",
#                                 `hg` = "blue",
#                                 split = "black")
#   ) +
#   theme(panel.grid = element_blank()
#   )





# # Example 3: Whole World with different base map
# autotyp_register.g <- project_data(df = autotyp_register)

# # ggplot
# autotyp_register.g$base_plot +
#   geom_sf(data = autotyp_register.g$base_map,
#           color = "black",
#           size = .75) +
#   geom_sf(data = autotyp_register.g$data,
#           aes(color = Subsistence),
#           alpha = .5,
#           size = 2
#   ) +
#   scale_color_manual(name = "Subsistence:",
#                      values = c(`not hg` = "darkgreen",
#                                 `hg` = "blue",
#                                 split = "black")
#   ) +
#   theme(panel.grid = element_blank()
#   )





# # Example 4: South America
# autotyp_register.g <- project_data(df = autotyp_register,
#                                    xmin = -83,
#                                    xmax = -33,
#                                    ymin = -58,
#                                    ymax = 13,
#                                    labels_lat = -82.5,
#                                    labels_long = -52.5
# )

# # ggplot
# autotyp_register.g$base_plot +
#   geom_sf(data = autotyp_register.g$data,
#         aes(color = Subsistence),
#         alpha = .5,
#         size = 2
#   ) +
#   scale_color_manual(name = "Subsistence:",
#                      values = c(`not hg` = "darkgreen",
#                                 `hg` = "blue",
#                                 split = "black")
#   ) +
#   geom_sf_text(data = autotyp_register.g$grid_labels,
#             aes(label = labels), size = 2, color = 'grey30'
#   )





# # Example 5: Polynesia
# autotyp_register.g <- project_data(df = autotyp_register,
#                                    labels_lat = -170,
#                                    labels_long = -45
# )

# # ggplot
# autotyp_register.g$base_plot +
#   geom_sf(data = autotyp_register.g$data,
#           aes(color = Subsistence),
#           alpha = .5,
#           size = 2
#   ) +
#   scale_color_manual(name = "Subsistence:",
#                      values = c(`not hg` = "darkgreen",
#                                 `hg` = "blue",
#                                 split = "black")
#   ) +
#   theme(panel.grid = element_blank()
#   ) +
#   lims(x = c(-1000000, 6000000), y = c(-6000000, 3000000))
#   # Due to pacific centered equal earth projection, coordinates are no longer
#   # -180° to 180° in x direction and -90° to 90° in y direction. Now they are
#   # -18000000 to 18000000 in x direction and -9000000 to 9000000 in y direction.
#   # for x-direction:
#   # -18000000 equals -30°
#   # -15000000 equals 0°
#   # -10000000 equals 50°
#   # -5000000 equals 100°
#   # 0 equals 150°
#   # 3000000 equals 180°/-180°
#   # 10000000 equals -110°
#   # 15000000 equals -60°
#   # 18000000 equals -30° 
#   # for y-direction: thee coordinates stay the same (just multiplied by 100000)

# # Graticules can  be added here via the package rnaturalearth.
# # There are different graticules available (graticules_1, graticules_5,
# # graticules_10, graticules_15, graticules_20, and graticules_30) the number
# # defines the steps in °:
# # rgdal is required since ne_download uses readOGR
# library(rgdal) # rgdal is required since ne_download uses readOGR
# graticules <- ne_download(scale = 110,
#                           type = "graticules_10",
#                           category = "physical",
#                           load = TRUE,
#                           returnclass = "sf")

# # the graticule then has to be reprojected
# graticules <- st_transform(graticules, crs = 8859)

# autotyp_register.g$base_plot +
#   geom_sf(data = autotyp_register.g$data,
#           aes(color = Subsistence),
#           alpha = .5,
#           size = 2
#   ) +
#   scale_color_manual(name = "Subsistence:",
#                      values = c(`not hg` = "darkgreen",
#                                 `hg` = "blue",
#                                 split = "black")
#   ) +
# # this can then simply be added to the plot like this:
# geom_sf(data = graticules,
#         linetype = "dashed",
#         colour = "grey",
#         size = .25) +
#   lims(x = c(-1000000, 6000000), y = c(-6000000, 3000000))



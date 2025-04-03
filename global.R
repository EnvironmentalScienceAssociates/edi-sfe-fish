options(dplyr.summarise.inform = FALSE)
library(shiny)
library(bslib)
library(shinyWidgets)
library(dplyr)
# library(tidyr)
library(leaflet)
library(leaflet.extras)
# library(ggplot2)
# library(plotly)
library(sf)
library(lubridate)

dt1 = readRDS("dt1.rds")
dt2 = setNames(vector("list", length(sources)), sources)

yr_min = min(dt1$Year, na.rm = TRUE)
yr_max = max(dt1$Year, na.rm = TRUE)

lat_min = min(dt1$Latitude, na.rm = TRUE)
lat_max = max(dt1$Latitude, na.rm = TRUE)
lon_min = min(dt1$Longitude, na.rm = TRUE)
lon_max = max(dt1$Longitude, na.rm = TRUE)

boundary_mat = matrix(c(lon_min, lon_min, lon_max, lon_max, lon_min,
                        lat_min, lat_max, lat_max, lat_min, lat_min), 
                      ncol = 2)
colnames(boundary_mat) = c("lon", "lat")

boundary = st_sfc(st_polygon(list(boundary_mat)), crs = 4326)

sources = levels(dt1$Source)
source_colors = c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462",
                  "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd")
pal = colorFactor(source_colors, sources)




# 
# 
# test = geojsonsf::geojson_sf(jsonify::to_json(out, unbox = T))
# 
# 
# 
# stations_sel = st_join(stations_sf, test, join = st_within) |> 
#   filter(!is.na(feature_type))
# 
# test2 = left_join(dt1, st_drop_geometry(stations_sel))


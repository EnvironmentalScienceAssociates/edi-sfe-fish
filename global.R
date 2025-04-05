options(dplyr.summarise.inform = FALSE)
library(shiny)
library(bslib)
library(shinyWidgets)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(sf)
library(lubridate)

if (!dir.exists("data")) dir.create("data")

sources = c("20mm", "Bay Study", "DJFMP", "EDSM", "FMWT", "Salvage", 
            "SKT", "SLS", "STN", "Suisun")
source_colors = c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462",
                  "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd")
pal = colorFactor(source_colors, sources)
sources_sel = c("20mm", "Bay Study")

data_files = c("dt1.rds", paste0("dt2-", gsub(" ", "", sources), ".rds"))

if (!all(file.exists(file.path("data", data_files)))){
  if (Sys.getenv("EgnyteKey") == ""){
    stop("Need to run prep-data.R to get data used in the Shiny app.")
  } else {
    remote_path = file.path("Shared", "Admin", "Practices", "Fish and Aquatic Science",
                            "Data Science", "EDI-SFE-Data")
    for (i in data_files){
      if (!file.exists(file.path("data", i))){
        Sys.sleep(0.4)
        # install egnyter with remotes::install_github("thinkelman-esa/egnyter")
        egnyter::download_file(file.path(remote_path, i),
                               file.path("data", i),
                               domain = "https://oneesa.egnyte.com",
                               token = Sys.getenv("EgnyteKey"))
      }
    }
  }
}

dt1 = readRDS(file.path("data", "dt1.rds"))
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


library(osmdata)
library(osmplotr)
library(sf)
library(dplyr)
library(slopes)
library(elevatr)
library(rgdal)
library(raster)
library(terra)
library(geodist)

# Get data from OSM
# Ensure version is 0.2.5 due to 405 errors with some previous versions e.g. 0.2.1
wgtn_lines <- opq(bbox = 'wellington city') %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf()

# convert roundabout polygons to lines
# without this the street network becomes disconnected at certain roundabouts
# For example, Riddiford Street, Mansfield Street & Russell Terrace
wgtn_lines_2 <- wgtn_lines |> 
  osm_poly2line()

wgtn_lines <- wgtn_lines_2$osm_lines

# Load DEM data
# vrt(
#   x = list.files(
#     path = "~/Documents/sdss-2023/lds-wellington-city-lidar-1m-dem-2019-2020-GTiff/",
#     pattern = "*.tif$",
#     full.names = TRUE
#     ),
#   filename = "dem.vrt",
#   overwrite = TRUE
# )

dem <- rast("dem.vrt")
dem_crs <- project(dem, crs(wgtn_lines))

# add elevation summaries to street network
wgtn_lines$slope = slope_raster(wgtn_lines, dem_crs)
wgtn_lines$slope = wgtn_lines$slope*100
wgtn_lines$length = st_length(wgtn_lines)

# add slope class
wgtn_lines$slope_class = wgtn_lines$slope %>%
  cut(
    breaks = c(0, 3, 5, 8, 10, 20, Inf),
    labels = c("0-3: flat", "3-5: mild", "5-8: medium", "8-10: hard", 
               "10-20: extreme", ">20: impossible"),
    right = F
  )

# write data
st_write(wgtn_lines |> dplyr::select(osm_id, name, highway, slope, length, slope_class),
         "~/Documents/sdss-2023/wgtn_network/wgtn_lines.shp")

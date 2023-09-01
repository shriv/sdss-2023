library(sf)
library(dplyr)
library(tmap)

wgtn_lines <- st_read("~/Documents/sdss-2023/wgtn_network/wgtn_lines.shp") |> 
  dplyr::select(-slp_cls)

wgtn_lines$slope_class = wgtn_lines$slope %>%
  cut(
    breaks = c(0, 3, 5, 8, 10, 20, Inf),
    labels = c("0-3: flat", "3-5: mild", "5-8: medium", "8-10: hard", 
               "10-20: extreme", ">20: impossible"),
    right = F
  )

# summary - all
round(
  prop.table(
    table(
      wgtn_lines |> pull(slope_class)
    )
  )*100, 1
)

# summary - residential
round(
  prop.table(
    table(
      wgtn_lines |> filter(highway == "residential") |>  pull(slope_class)
    )
  )*100, 1
)


## plot city slope
tmap_mode("view")
tmap_options(basemaps = leaflet::providers$CartoDB.Positron) #basemap

palredgreen = c("#267300", "#70A800", "#FFAA00", "#E60000", "#A80000", "#730000") #color palette

slopemap =
  tm_shape(wgtn_lines) +
  tm_lines(
    col = "slope_class",
    palette = palredgreen,
    lwd = 2, #line width
    title.col = "Slope [%]",
    popup.vars = c("Highway" = "highway",
                   "Length" = "length",
                   "Slope: " = "slope",
                   "Class: " = "slope_class"),
    popup.format = list(digits = 1),
    # id = "slope"
    id = "name" #if it gets too memory consuming, delete this line
  )

slopemap

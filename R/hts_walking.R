library(sf)
library(dplyr)
library(janitor)
library(haven)
library(tidygraph)
library(sfnetworks)
library(purrr)
library(tidyr)
library(tmap)
library(ggplot2)
library(rgdal)
library(raster)
library(terra)
library(geodist)
library(units)

# get data
ad_14 <- haven::read_sas("~/Documents/hts-data/year-14/ad.sas7bdat")
tr_14 <- haven::read_sas("~/Documents/hts-data/year-14/tr.sas7bdat")

ad_16 <- haven::read_sas("~/Documents/hts-data/year-16/ad.sas7bdat")
tr_16 <- haven::read_sas("~/Documents/hts-data/year-16/tr.sas7bdat")

ad_15 <- haven::read_sas("~/Documents/hts-data/year-15/ad.sas7bdat")
tr_15 <- haven::read_sas("~/Documents/hts-data/year-15/tr.sas7bdat")

# combine data
tr <- bind_rows(
  tr_14  |> 
    clean_names() |> 
    dplyr::select(year, samno, trmode, purpose, bestdist, tla, trmode,
                  startaddno, traddno, legno, numlegs, durmin),
  tr_16 |> 
    clean_names() |> 
    dplyr::select(year, samno, trmode, purpose, bestdist, tla, trmode,
                  startaddno, traddno, legno, numlegs, durmin),
  tr_15 |> 
    clean_names() |> 
    dplyr::select(year, samno, trmode, purpose, bestdist, tla, trmode,
                  startaddno, traddno, legno, numlegs, durmin)
)

ad <- bind_rows(
  ad_14 |> 
    dplyr::select(addno, Latitude, Longitude, Year, samno) |> 
    clean_names(), 
  ad_16 |> 
    dplyr::select(addno, Latitude, Longitude, Year, samno) |> 
    clean_names(), 
  ad_15 |> 
    dplyr::select(addno, Latitude, Longitude, Year, samno) |> 
    clean_names() 
)

# Filter walking trips
walk_wgtn <- tr |> 
  dplyr::filter(tla == "Wellington City" & trmode == 2 & numlegs == 1) |> 
  left_join(
    ad |> 
      rename_with(~ paste0("start_", .x, recycle0 = TRUE)),
    join_by(startaddno == start_addno, year == start_year, samno == start_samno)
  ) |> 
  left_join(
    ad |> 
      rename_with(~ paste0("end_", .x, recycle0 = TRUE)),
    join_by(traddno == end_addno, year == end_year, samno == end_samno)
  ) |> 
  distinct() |> 
  filter(!is.na(start_latitude)) |> 
  filter(!is.na(end_latitude))


wgtn_lines <- st_read("~/Documents/sdss-2023/wgtn_network/wgtn_lines.shp") |> 
  dplyr::select(-slp_cls)

wgtn_lines$slope_class = wgtn_lines$slope %>%
  cut(
    breaks = c(0, 3, 5, 8, 10, 20, Inf),
    labels = c("0-3: flat", "3-5: mild", "5-8: medium", "8-10: hard", 
               "10-20: extreme", ">20: impossible"),
    right = F
  )

# Routing with sfnetworks
wgtn_net <- wgtn_lines |> 
  sfnetworks::as_sfnetwork(directed = FALSE) |> 
  convert(to_spatial_subdivision, .clean = TRUE) |>
  activate("edges") |> 
  mutate(weight = edge_length())

# Create start and end points for HTS walking trips
start = st_as_sf(walk_wgtn |> dplyr::select(start_latitude, start_longitude),
                 crs = 4326, coords = c("start_longitude", "start_latitude"))


end = st_as_sf(walk_wgtn |> dplyr::select(end_latitude, end_longitude),
               crs = 4326, coords = c("end_longitude", "end_latitude"))

st_network_paths_mod <- function(from, to){
  return(try(st_network_paths(wgtn_net, from, to, weights = "weight")))
}

# Isolate nodes as sf dataframe
nodes_wgtn <- wgtn_net %>% activate("nodes") %>% st_as_sf()
from_index = st_nearest_feature(start, nodes_wgtn)
to_index = st_nearest_feature(end, nodes_wgtn)

orig_dest <- tibble(
  from_index = from_index, 
  to_index = to_index) |> 
  mutate(
    trip = dplyr::row_number()
  )

# Find shortest path routes for HTS walking trips in Wellington TLA
routes_df <- orig_dest %>%
  mutate(path = pmap(list(from = from_index, to = to_index), .f=st_network_paths_mod)) %>% 
  unnest(cols=c(path))

routes <- routes_df %>% 
  unnest(cols=c(edge_paths)) %>% 
  dplyr::select(trip, edge_paths) |> 
  inner_join(wgtn_net %>% 
               activate("edges") %>% 
               st_as_sf() %>% 
               mutate(edge = row_number()), by = c("edge_paths" = "edge"))  %>% 
  st_as_sf()

st_write(routes, "routes_3years_data/routes.shp")

# This file gets all geography data. 

library(tidyverse)
library(osmdata)
library(sf)

# The `osmdata` package facilitates OSM overpass queries.
# This is a vector of strings that will be passed as the
# value in a key value pair, with the key always being 
# "highway". Additionally, links are queried appending
# "_link" to the value. More info on these features can be found
# here: https://wiki.openstreetmap.org/wiki/Map_features.

queries <- c("motorway",
             "trunk",
             "primary",
             "secondary",
             "tertiary",
             "residential",
             "service",
             "unclassified")

# This code chunk is where data is queried from OSM.

roads <- map(queries, function(x) {
  opq("St. Louis, MO") %>%
    add_osm_feature(key = "highway", value = x) %>%
    add_osm_feature(key = "highway", value = paste0(x, "_link")) %>%
    osmdata_sf()
}) 

# Name the list elements so they can be referenced later.

names(roads) <- queries

# Read in city boundaries shapefile

city <- st_read("shapefiles/stl_boundary/stl_boundary.shp") %>%
  st_transform(., crs = st_crs(4326))

# Read in neighborhoods shapefile

hoods <- st_read("shapefiles/neighborhoods/Neighborhood_Boundaries.shp") %>%
  st_transform(., crs = st_crs(4326))

# Create alphabetically ordered vector of neighborhood names

hood_names <- hoods %>%
  select(NHD_NAME) %>%
  arrange(NHD_NAME) %>%
  unique() %>%
  .[[1]]


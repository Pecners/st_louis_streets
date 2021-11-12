
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
  opq("University City, MO") %>%
    add_osm_feature(key = "highway", value = x) %>%
    add_osm_feature(key = "highway", value = paste0(x, "_link")) %>%
    osmdata_sf()
}) 


u_city <- opq("University City, MO") %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  osmdata_sf()

uc <- u_city$osm_multipolygons %>%
  filter(name == "University City") %>%
  st_transform(., crs = st_crs(4326))

u_city_roads <- map(roads, function(x) {
  st_transform(x$osm_lines, crs = st_crs(4326)) %>%
    st_intersection(., uc)
})

names(u_city_roads) <- queries

bb <- st_bbox(uc)

sw_corner <- st_sfc(st_point(x = c(bb[["xmin"]], bb[["ymin"]])), crs = st_crs(4326))
se_corner <- st_sfc(st_point(x = c(bb[["xmax"]], bb[["ymin"]])), crs = st_crs(4326))
nw_corner <- st_sfc(st_point(x = c(bb[["xmin"]], bb[["ymax"]])), crs = st_crs(4326))

ew <- as.numeric(st_distance(sw_corner, se_corner))
ns <- as.numeric(st_distance(sw_corner, nw_corner))

if (ew > ns) {
  width <- 7
  height <- 7 * (ns / ew)
} else {
  width <- 7 * (ew / ns)
  height <- 7
}

size <- 0.01
background <- "black"
color <- "white"

u_city_roads$motorway %>%
  ggplot() +
  geom_sf(size = size, color = color) +
  geom_sf(data = u_city_roads$trunk, size = size, color = color) +
  geom_sf(data = u_city_roads$primary, size = size, color = color) +
  geom_sf(data = u_city_roads$secondary, size = size, color = color) +
  geom_sf(data = u_city_roads$tertiary, size = size, color = color) +
  geom_sf(data = u_city_roads$residential, size = size, color = color) +
  geom_sf(data = u_city_roads$service, size = size, color = color) +
  geom_sf(data = u_city_roads$unclassified, size = size, color = color) +
  theme_void() +
  theme(panel.background = element_rect(fill = background),
        plot.title = element_text(hjust = 0.5, 
                                  color = color,
                                  family = "serif", 
                                  face = "bold",
                                  size = 16)) +
  labs(title = "University City Streets")

ggsave("maps/University City Map.pdf", device = "pdf", bg = background,
       width = width, height = height)


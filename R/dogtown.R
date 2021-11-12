# Dogtown boundaries as defined here:
# https://www.google.com/maps/d/u/0/viewer?ie=UTF8&hl=en&oe=UTF8&msa=0&mid=1Rvs4Z3GI0RXw5f85XivUxwbgubE&ll=38.62833700000003%2C-90.292811&z=15

dogtown <- st_read("shapefiles/dogtown.kml") %>%
  st_transform(., crs = st_crs(4326)) %>%
  filter(Name == "Dogtown (St. Louis)") %>%
  st_polygonize()


dogtown_roads <- map(roads, function(x) {
  st_transform(x$osm_lines, crs = st_crs(4326)) %>%
    st_intersection(., dogtown)
})

bb <- st_bbox(dogtown)

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

dogtown_roads$motorway %>%
  ggplot() +
  geom_sf(size = size, color = color) +
  geom_sf(data = dogtown_roads$trunk, size = size, color = color) +
  geom_sf(data = dogtown_roads$primary, size = size, color = color) +
  geom_sf(data = dogtown_roads$secondary, size = size, color = color) +
  geom_sf(data = dogtown_roads$tertiary, size = size, color = color) +
  geom_sf(data = dogtown_roads$residential, size = size, color = color) +
  geom_sf(data = dogtown_roads$service, size = size, color = color) +
  geom_sf(data = dogtown_roads$unclassified, size = size, color = color) +
  theme_void() +
  theme(panel.background = element_rect(fill = background),
        plot.title = element_text(hjust = 0.5, 
                                  color = color,
                                  family = "serif", 
                                  face = "bold",
                                  size = 16)) +
  labs(title = "Dogtown Streets")

ggsave("maps/Dogtown Map.pdf", device = "pdf", bg = background,
       width = width, height = height)

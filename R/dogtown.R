
dogtown <- st_read("shapefiles/dogtown.kml") %>%
  st_transform(., crs = st_crs(4326))

dogtown_roads <- map(roads, function(x) {
  st_transform(x$osm_lines, crs = st_crs(4326)) %>%
    st_intersection(., dogtown)
})

size <- 0.01
background <- "black"
color <- "white"

city_roads$motorway %>%
  ggplot() +
  geom_sf(size = size, color = color) +
  geom_sf(data = city_roads$trunk, size = size, color = color) +
  geom_sf(data = city_roads$primary, size = size, color = color) +
  geom_sf(data = city_roads$secondary, size = size, color = color) +
  geom_sf(data = city_roads$tertiary, size = size, color = color) +
  geom_sf(data = city_roads$residential, size = size, color = color) +
  geom_sf(data = city_roads$service, size = size, color = color) +
  geom_sf(data = city_roads$unclassified, size = size, color = color) +
  theme_void() +
  theme(panel.background = element_rect(fill = background),
        plot.title = element_text(hjust = 0.5, 
                                  color = color,
                                  family = "serif", 
                                  face = "bold",
                                  size = 16)) +
  labs(title = "St. Louis Streets")

ggsave("maps/St. Louis Map.pdf", device = "pdf", bg = background)
# This code produces the maps.

# `make_maps()` is just a wrapper function.

make_maps <- function() {
  
  # Here we are iterating over every neighborhood.
  
  walk(hood_names, function(hood) {
    
    # Set up file name.
    
    h_name <- str_replace(str_to_title(hood), "/", "-")
    filename <- paste(h_name, "Map.pdf")
    
    # Filter for a single neighborhood.
    
    h <- hoods %>%
      filter(NHD_NAME == hood)
    
    # This code chunk performs an st_intersection operation
    # over the road elements to limit the geography to the
    # bounds of neighborhood.
    
    hood_roads <- map(roads, function(x) {
      st_transform(x$osm_lines, crs = st_crs(hoods)) %>%
        st_intersection(., h)
    })
    
    # Here I am computing the width:height ratio of the neighbohood,
    # which I will later use when defining the width and height of
    # the final files.
    
    
    bb <- st_bbox(h)
    
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
    
    # Set constants for plotting. 
    # size is the weight of the lines,
    # background sets plot and file background,
    # color sets line and text colors.
    
    size <- 0.01
    background <- "black"
    color <- "white"
    
    
    m <- hood_roads$motorway %>%
      ggplot() +
      geom_sf(size = size, color = color) +
      geom_sf(data = hood_roads$trunk, size = size, color = color) +
      geom_sf(data = hood_roads$primary, size = size, color = color) +
      geom_sf(data = hood_roads$secondary, size = size, color = color) +
      geom_sf(data = hood_roads$tertiary, size = size, color = color) +
      geom_sf(data = hood_roads$residential, size = size, color = color) +
      geom_sf(data = hood_roads$service, size = size, color = color) +
      geom_sf(data = hood_roads$unclassified, size = size, color = color) +
      theme_void() +
      theme(panel.background = element_rect(fill = background),
            plot.title = element_text(hjust = 0.5, 
                                      color = color,
                                      family = "serif", 
                                      face = "bold",
                                      size = 16)) +
      labs(title = hood)
    
    # Save the file as a pdf with specified arguments set above
    
    ggsave(filename = paste0("maps/", filename),
           device = "pdf", width = width, height = height, bg = background)
    
    # This last bit just prints progress as files are produced
    # since this takes a while to run.
    
    c <- which(hood_names == hood)
    print(paste0(c, " of ", length(hood_names), " maps created."))
  })
}



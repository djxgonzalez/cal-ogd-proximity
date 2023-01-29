##============================================================================##
## 2.01 - defines a function to make buffers around input oil and gas wells (as
## points) and another function to calculate buffer overlap between polygon
## (in this case, census block group) and the well buffer to conduct the areal
### apportionment exposure assessment

#----------------------------------------------------------------------------
# this function makes a buffer of a given radius around all input points,
# in this case, oil and gas wells

# around well points
makeWellBuffer <- function(data_wells, radius) { 
  data_wells <- data_wells %>%
    dplyr::select(longitude, latitude) %>%
    # converts long/lat tabular data to sf object
    st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(crs_nad83)) %>%
    st_transform(crs_projected) %>%  # projected crs needed for buffering
    st_buffer(dist = radius) %>%     # generates buffer; distance in meters
    st_union() %>%                   # merges overlapping polygons
    st_transform(crs_nad83)
  return(data_wells)
}


#----------------------------------------------------------------------------
# this function determines the proportion of overlap between two input polygons,
# in this case, a census block group and the well buffer

assessPolygonOverlap <- function(data_polygon, well_buffer, exp_variable) {
  
  # captures polygon area for comparison
  area_polygon <- st_area(data_polygon)  
  
  # determines area of intersection b/t polygon (block group) and well buffer;
  # if length non-zero there is overlap and we calculate area of overlap;
  # else if length is non-zero, there is no overlap and we assign 0
  area_intersection <- 
    ifelse(length(st_intersects(st_geometry(data_polygon), well_buffer)[[1]]) != 0,
           st_area(st_intersection(st_geometry(data_polygon), well_buffer)),
           0)
  
  # calculates proportion of overlap between the polygon (block group) and buffer
  data_polygon <- data_polygon %>%
    mutate(!!as.name(exp_variable) := (as.numeric(area_intersection) / 
                                         as.numeric(area_polygon)))
  
  # returns proportion overlap
  return(data_polygon)
}

##============================================================================##
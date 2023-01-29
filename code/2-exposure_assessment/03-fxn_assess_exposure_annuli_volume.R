##============================================================================##
## 2.03 - generalized function to assess exposure by taking the sum of the volume
## in 1-km radius annuli within 5 km of the centroid

# takes centroid coordinates ('centroid') as an sf object, generates 1-km annuli 
# around the centroid out to 5 km, and sums the volume of total oil and gas 
# production within each annulus

assessExposureAnnuliVolume <- function(centroid, 
                                       production,
                                       exp_variable_root) {
  
  # prepares the centroid dataset
  
  # captures date for feeding into the function below
  centroid_year       <- centroid$year
  centroid_lat        <- centroid$latitude
  centroid_long       <- centroid$longitude
  centroid_id         <- centroid$GEOID
  
  # makes sf object with only 'centroid_id'
  centroid <- centroid %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = crs_nad83) %>%
    dplyr::select(year)
  
  # prepares production data
  # generates 5 km buffer as a mask around centroid coordinates
  centroid_mask <- centroid %>% 
    st_transform(crs_projected) %>%
    st_buffer(dist = 5000) %>%
    st_transform(crs_nad83)
  
  # subsets to production that intersect with 'centroid_mask'  i.e., within 10 km of 
  # the maternal residence, and that have production period that overlaps with 
  # the date
  prod_within_5km <- production %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = crs_nad83) %>%
    # restricts to production within 5 km of the input centroid
    st_intersection(centroid_mask)

  # if there are production that have dates that intersect with the centroid interval,
  # counts and stores number of well sites within each annulus; otherwise, we
  # assign 0 to all annuli wihtout annuli functions to improve efficiency;
  # the variable names are flexible name to account for new, active, idle, or 
  # abandoned production
  
  if (nrow(prod_within_5km) > 0) {
    # makes annuli around the block group centroids in the 'centroid' data
    annulus0to762 <- centroid %>%
      st_transform(crs_projected) %>%
      st_buffer(dist = 762) %>%
      st_transform(crs_nad83)
    annulus762to1 <- centroid %>%
      st_transform(crs_projected) %>%
      st_buffer(dist = 1000) %>%
      st_transform(crs_nad83)
    annulus1to2 <- centroid %>% 
      st_transform(crs_projected) %>%
      st_buffer(dist = 2000) %>%
      st_transform(crs_nad83)
    annulus2to3 <- centroid %>% 
      st_transform(crs_projected) %>%
      st_buffer(dist = 3000) %>%
      st_transform(crs_nad83)
    annulus3to4 <- centroid %>% 
      st_transform(crs_projected) %>%
      st_buffer(dist = 4000) %>%
      st_transform(crs_nad83)
    annulus4to5 <- centroid %>% 
      st_transform(crs_projected) %>%
      st_buffer(dist = 5000) %>%
      st_transform(crs_nad83)
    
    # finalizes annuli by successively clipping differences in reverse order
    annulus4to5   <- st_difference(annulus4to5,   annulus3to4)
    annulus3to4   <- st_difference(annulus3to4,   annulus2to3)
    annulus2to3   <- st_difference(annulus2to3,   annulus1to2)
    annulus1to2   <- st_difference(annulus1to2,   annulus762to1)
    annulus762to1   <- st_difference(annulus762to1,   annulus0to762)
    
    # takes sum of total oil/gas production for production within each 1-km annulus
    centroid <- centroid %>%  
      as_tibble() %>%
      mutate(!!as.name(paste(exp_variable_root, sep = "", "0to762m")) :=   
               ifelse(nrow(st_intersection(annulus0to762, prod_within_5km)) == 0, 0,
                      sum(st_intersection(annulus0to762, prod_within_5km)$total_oil_gas_produced_annual)),
             !!as.name(paste(exp_variable_root, sep = "", "762to1km")) :=   
               ifelse(nrow(st_intersection(annulus762to1, prod_within_5km)) == 0, 0,
                      sum(st_intersection(annulus762to1, prod_within_5km)$total_oil_gas_produced_annual)),
             !!as.name(paste(exp_variable_root, sep = "", "1to2km")) := 
               ifelse(nrow(st_intersection(annulus1to2, prod_within_5km)) == 0, 0,
                      sum(st_intersection(annulus1to2, prod_within_5km)$total_oil_gas_produced_annual)),
             !!as.name(paste(exp_variable_root, sep = "", "2to3km")) := 
               ifelse(nrow(st_intersection(annulus2to3, prod_within_5km)) == 0, 0,
                      sum(st_intersection(annulus2to3, prod_within_5km)$total_oil_gas_produced_annual)),
             !!as.name(paste(exp_variable_root, sep = "", "3to4km")) :=
               ifelse(nrow(st_intersection(annulus3to4, prod_within_5km)) == 0, 0,
                      sum(st_intersection(annulus3to4, prod_within_5km)$total_oil_gas_produced_annual)),
             !!as.name(paste(exp_variable_root, sep = "", "4to5km")) :=
               ifelse(nrow(st_intersection(annulus4to5, prod_within_5km)) == 0, 0,
                      sum(st_intersection(annulus4to5, prod_within_5km)$total_oil_gas_produced_annual))) %>%
      dplyr::select(-geometry) %>%
      mutate(year = centroid_year) %>%
      mutate(GEOID = centroid_id)
    
  } else if (nrow(prod_within_5km) == 0) {
    
    centroid <- centroid %>% 
      as_tibble() %>%
      mutate(!!as.name(paste(exp_variable_root, sep = "", "0to762m"))  := 0,
             !!as.name(paste(exp_variable_root, sep = "", "762to1km")) := 0,
             !!as.name(paste(exp_variable_root, sep = "", "1to2km"))   := 0,
             !!as.name(paste(exp_variable_root, sep = "", "2to3km"))   := 0,
             !!as.name(paste(exp_variable_root, sep = "", "3to4km"))   := 0,
             !!as.name(paste(exp_variable_root, sep = "", "4to5km"))   := 0) %>%
      dplyr::select(-geometry) %>%
      mutate(year = centroid_year) %>%
      mutate(GEOID = centroid_id)
    
  }

  # returns the processed exposure data
  return(centroid)
  
}

##============================================================================##
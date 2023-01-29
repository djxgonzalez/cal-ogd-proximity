##============================================================================##
## 2.04 - for each block group-period, assesses exposure to wells in 
## preproduction, production, and postproduction using areal apportionment
## and a count of wells *or* sum of production within 3 km of the block group
## centroid; exports these data interim data for assembly in the next script

##---------------------------------------------------------------------------
## setup

# attaches functions .....................................................
library("parallel")  # for the `mclapply()` fxn
source("code/1-data_tidying/02-tidy_calgem_wells_data.R")
source("code/2-exposure_assessment/01-fxn_assess_exposure_areal_apportionment.R")
source("code/2-exposure_assessment/02-fxn_assess_exposure_annuli_count.R")
source("code/2-exposure_assessment/03-fxn_assess_exposure_annuli_volume.R")

# data input .............................................................
acs_2005_2009 <- read_sf("data/raw/us_census/acs/2009/2009_BlockGroups") #%>%
  select(BKGPIDFP00, geometry) %>% 
  rename(GEOID = BKGPIDFP00) %>%
  mutate(GEOID = as.factor(GEOID))
acs_2010_2014 <- read_sf("data/raw/us_census/acs/2014/2014_BlockGroups") %>%
  select(GEOID, geometry) %>% mutate(GEOID = as.factor(GEOID))
acs_2015_2019 <- read_sf("data/raw/us_census/acs/2019/2019_BlockGroups") %>%
  select(GEOID, geometry) %>% mutate(GEOID = as.factor(GEOID))
wells_interim       <- readRDS("data/interim/wells_interim.rds")
calgem_prod_monthly <- readRDS("data/interim/calgem_production_monthly.rds")

# data prep ..............................................................
wells_2005_2009 <- 
  tidyCalgemWellsData2(wells_interim, "1/1/2005", "12/31/2009")
wells_2010_2014 <- 
  tidyCalgemWellsData2(wells_interim, "1/1/2010", "12/31/2014")
wells_2015_2019 <- 
  tidyCalgemWellsData2(wells_interim, "1/1/2015", "12/31/2019")

# well buffers . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
wells_buffer_1km_2005_2009_preprod <-
  makeWellBuffer(subset(wells_2005_2009, preprod_during_period == 1), 1000)
wells_buffer_3km_2005_2009_preprod <- 
  makeWellBuffer(subset(wells_2005_2009, preprod_during_period == 1), 3000)
wells_buffer_1km_2010_2014_preprod <-
  makeWellBuffer(subset(wells_2010_2014, preprod_during_period == 1), 1000)
wells_buffer_3km_2010_2014_preprod <- 
  makeWellBuffer(subset(wells_2010_2014, preprod_during_period == 1), 3000)
wells_buffer_1km_2015_2019_preprod <- 
  makeWellBuffer(subset(wells_2015_2019, preprod_during_period == 1), 1000)
wells_buffer_3km_2015_2019_preprod <- 
  makeWellBuffer(subset(wells_2015_2019, preprod_during_period == 1), 3000)

wells_buffer_1km_2005_2009_prod <-
  makeWellBuffer(subset(wells_2005_2009, prod_during_period == 1), 1000)
wells_buffer_3km_2005_2009_prod <- 
  makeWellBuffer(subset(wells_2005_2009, prod_during_period == 1), 3000)
wells_buffer_1km_2010_2014_prod <-
  makeWellBuffer(subset(wells_2010_2014, prod_during_period == 1), 1000)
wells_buffer_3km_2010_2014_prod <- 
  makeWellBuffer(subset(wells_2010_2014, prod_during_period == 1), 3000)
wells_buffer_1km_2015_2019_prod <- 
  makeWellBuffer(subset(wells_2015_2019, prod_during_period == 1), 1000)
wells_buffer_3km_2015_2019_prod <- 
  makeWellBuffer(subset(wells_2015_2019, prod_during_period == 1), 3000)

wells_buffer_1km_2005_2009_postprod <-
  makeWellBuffer(subset(wells_2005_2009, postprod_during_period == 1), 1000)
wells_buffer_3km_2005_2009_postprod <- 
  makeWellBuffer(subset(wells_2005_2009, postprod_during_period == 1), 3000)
wells_buffer_1km_2010_2014_postprod <-
  makeWellBuffer(subset(wells_2010_2014, postprod_during_period == 1), 1000)
wells_buffer_3km_2010_2014_postprod <- 
  makeWellBuffer(subset(wells_2010_2014, postprod_during_period == 1), 3000)
wells_buffer_1km_2015_2019_postprod <- 
  makeWellBuffer(subset(wells_2015_2019, postprod_during_period == 1), 1000)
wells_buffer_3km_2015_2019_postprod <- 
  makeWellBuffer(subset(wells_2015_2019, postprod_during_period == 1), 3000)

# wells for counting . . . . . . . . . . . . . . . . . . . . . . . . . 
wells_preprod_2005_2009 <- wells_2005_2009 %>%
  filter(preprod_during_period == 1) %>%
  select(api_number, longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = crs_nad83)
wells_preprod_2010_2014 <- wells_2010_2014 %>%
  filter(preprod_during_period == 1) %>%
  select(api_number, longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = crs_nad83)
wells_preprod_2015_2019 <- wells_2015_2019 %>%
  filter(preprod_during_period == 1) %>%
  select(api_number, longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = crs_nad83)

wells_prod_2005_2009 <- wells_2005_2009 %>%
  filter(prod_during_period == 1) %>%
  select(api_number, longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = crs_nad83)
wells_prod_2010_2014 <- wells_2010_2014 %>%
  filter(prod_during_period == 1) %>%
  select(api_number, longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = crs_nad83)
wells_prod_2015_2019 <- wells_2015_2019 %>%
  filter(prod_during_period == 1) %>%
  select(api_number, longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = crs_nad83)

wells_postprod_2005_2009 <- wells_2005_2009 %>%
  filter(postprod_during_period == 1) %>%
  select(api_number, longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = crs_nad83)
wells_postprod_2010_2014 <- wells_2010_2014 %>%
  filter(postprod_during_period == 1) %>%
  select(api_number, longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = crs_nad83)
wells_postprod_2015_2019 <- wells_2015_2019 %>%
  filter(postprod_during_period == 1) %>%
  select(api_number, longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = crs_nad83)

# removes data we don't need to improve efficiency
rm(wells_interim, wells_2005_2009, wells_2010_2014, wells_2015_2019,
   calgem_prod_monthly)


##---------------------------------------------------------------------------
## Preproduction wells

#.........................................................................
# 2005-2009

# areal apportionment - 1 km . . . . . . . . . . . . . . . . . . . . . 
acs_data_in <- acs_2005_2009 %>% 
  st_intersection(wells_buffer_1km_2005_2009_preprod)
acs_data_in <- acs_2005_2009 %>% filter(GEOID %in% acs_data_in$GEOID)
acs_data_in <- split(acs_data_in, seq(1, nrow(acs_data_in)))
acs_2005_2009_exp_preprod_overlap_1km <- 
  mclapply(acs_data_in,  # if not on MacOS, use `lapply()` instead
           FUN          = assessPolygonOverlap,
           well_buffer  = wells_buffer_1km_2005_2009_preprod, 
           exp_variable = "prop_exp_preprod_1km_2005_2009")
acs_2005_2009_exp_preprod_overlap_1km <- 
  do.call("rbind", acs_2005_2009_exp_preprod_overlap_1km) %>%
  as_tibble() %>% select(-geometry) %>% mutate(period = as.factor("2005_2009"))
saveRDS(acs_2005_2009_exp_preprod_overlap_1km,
        "data/interim/acs_2005_2009_exp_preprod_overlap_1km.rds")

# annuli . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
acs_data_in <- acs_2005_2009 %>%
  st_intersection(wells_buffer_3km_2005_2009_preprod)
acs_data_in <- acs_2005_2009 %>% filter(GEOID %in% acs_data_in$GEOID)
acs_data_in <- split(acs_data_in, seq(1, nrow(acs_data_in)))
acs_2005_2009_exp_preprod_annuli <- 
  mclapply(acs_data_in,  # if not on MacOS, use `lapply()` instead
           FUN          = assessExposureAnnuliCount,
           wells        = wells_preprod_2005_2009, 
           exp_variable = "preprod_wells_2005_2009")
acs_2005_2009_exp_preprod_annuli <- 
  do.call("rbind", acs_2005_2009_exp_preprod_annuli) %>%
  mutate(period = as.factor("2005_2009"))
saveRDS(acs_2005_2009_exp_preprod_annuli,
        "data/interim/acs_2005_2009_exp_preprod_annuli.rds")

#.........................................................................
# 2010-2014

# areal apportionment - 1 km . . . . . . . . . . . . . . . . . . . . . 
acs_data_in <- acs_2010_2014 %>% 
  st_intersection(wells_buffer_1km_2010_2014_preprod)
acs_data_in <- acs_2010_2014 %>% filter(GEOID %in% acs_data_in$GEOID)
acs_data_in <- split(acs_data_in, seq(1, nrow(acs_data_in)))
acs_2010_2014_exp_preprod_overlap_1km <- 
  mclapply(acs_data_in,  # if not on MacOS, use `lapply()` instead
           FUN          = assessPolygonOverlap,
           well_buffer  = wells_buffer_1km_2010_2014_preprod, 
           exp_variable = "prop_exp_preprod_1km_2010_2014")
acs_2010_2014_exp_preprod_overlap_1km <- 
  do.call("rbind", acs_2010_2014_exp_preprod_overlap_1km) %>%
  as_tibble() %>% select(-geometry) %>% mutate(period = as.factor("2010_2014"))
saveRDS(acs_2010_2014_exp_preprod_overlap_1km,
        "data/interim/acs_2010_2014_exp_preprod_overlap_1km.rds")

# annuli . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
acs_data_in <- acs_2010_2014 %>% 
  st_intersection(wells_buffer_3km_2010_2014_preprod)
acs_data_in <- acs_2010_2014 %>% filter(GEOID %in% acs_data_in$GEOID)
acs_data_in <- split(acs_data_in, seq(1, nrow(acs_data_in)))
acs_2010_2014_exp_preprod_annuli <- 
  mclapply(acs_data_in,  # if not on MacOS, use `lapply()` instead
           FUN          = assessExposureAnnuliCount,
           wells        = wells_preprod_2010_2014, 
           exp_variable = "preprod_wells_2010_2014")
acs_2010_2014_exp_preprod_annuli <- 
  do.call("rbind", acs_2010_2014_exp_preprod_annuli) %>%
  mutate(period = as.factor("2010_2014"))
saveRDS(acs_2010_2014_exp_preprod_annuli,
        "data/interim/acs_2010_2014_exp_preprod_annuli.rds")

#.........................................................................
# 2015-2019

# areal apportionment - 1 km . . . . . . . . . . . . . . . . . . . . . 
acs_data_in <- acs_2015_2019 %>% 
  st_intersection(wells_buffer_1km_2015_2019_preprod)
acs_data_in <- acs_2015_2019 %>% filter(GEOID %in% acs_data_in$GEOID)
acs_data_in <- split(acs_data_in, seq(1, nrow(acs_data_in)))
acs_2015_2019_exp_preprod_overlap_1km <- 
  mclapply(acs_data_in,  # if not on MacOS, use `lapply()` instead
           FUN          = assessPolygonOverlap,
           well_buffer  = wells_buffer_1km_2015_2019_preprod, 
           exp_variable = "prop_exp_preprod_1km_2015_2019")
acs_2015_2019_exp_preprod_overlap_1km <- 
  do.call("rbind", acs_2015_2019_exp_preprod_overlap_1km) %>%
  as_tibble() %>% select(-geometry) %>% mutate(period = as.factor("2015_2019"))
saveRDS(acs_2015_2019_exp_preprod_overlap_1km,
        "data/interim/acs_2015_2019_exp_preprod_overlap_1km.rds")

# annuli . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
acs_data_in <- acs_2015_2019 %>% 
  st_intersection(wells_buffer_3km_2015_2019_preprod)
acs_data_in <- acs_2015_2019 %>% filter(GEOID %in% acs_data_in$GEOID)
acs_data_in <- split(acs_data_in, seq(1, nrow(acs_data_in)))
acs_2015_2019_exp_preprod_annuli <- 
  mclapply(acs_data_in,  # if not on MacOS, use `lapply()` instead
           FUN          = assessExposureAnnuliCount,
           wells        = wells_preprod_2015_2019, 
           exp_variable = "preprod_wells_2015_2019")
acs_2015_2019_exp_preprod_annuli <- 
  do.call("rbind", acs_2015_2019_exp_preprod_annuli) %>%
  mutate(period = as.factor("2015_2019"))
saveRDS(acs_2015_2019_exp_preprod_annuli,
        "data/interim/acs_2015_2019_exp_preprod_annuli.rds")


##---------------------------------------------------------------------------
## Production wells

#.........................................................................
# 2005-2009

# areal apportionment - 1 km . . . . . . . . . . . . . . . . . . . . . 
acs_data_in <- acs_2005_2009 %>% 
  st_intersection(wells_buffer_1km_2005_2009_prod)
acs_data_in <- acs_2005_2009 %>% filter(GEOID %in% acs_data_in$GEOID)
acs_data_in <- split(acs_data_in, seq(1, nrow(acs_data_in)))
acs_2005_2009_exp_prod_overlap_1km <- 
  mclapply(acs_data_in,  # if not on MacOS, use `lapply()` instead
           FUN          = assessPolygonOverlap,
           well_buffer  = wells_buffer_1km_2005_2009_prod, 
           exp_variable = "prop_exp_prod_1km_2005_2009")
acs_2005_2009_exp_prod_overlap_1km <- 
  do.call("rbind", acs_2005_2009_exp_prod_overlap_1km) %>%
  as_tibble() %>% select(-geometry) %>% mutate(period = as.factor("2005_2009"))
saveRDS(acs_2005_2009_exp_prod_overlap_1km,
        "data/interim/acs_2005_2009_exp_prod_overlap_1km.rds")

# annuli . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
acs_data_in <- acs_2005_2009 %>%
  st_intersection(wells_buffer_3km_2005_2009_prod)
acs_data_in <- acs_2005_2009 %>% filter(GEOID %in% acs_data_in$GEOID)
acs_data_in <- split(acs_data_in, seq(1, nrow(acs_data_in)))
acs_2005_2009_exp_prod_annuli <- 
  mclapply(acs_data_in,  # if not on MacOS, use `lapply()` instead
           FUN          = assessExposureAnnuliCount,
           wells        = wells_prod_2005_2009, 
           exp_variable = "prod_wells_2005_2009")
acs_2005_2009_exp_prod_annuli <- 
  do.call("rbind", acs_2005_2009_exp_prod_annuli) %>%
  mutate(period = as.factor("2005_2009"))
saveRDS(acs_2005_2009_exp_prod_annuli,
        "data/interim/acs_2005_2009_exp_prod_annuli.rds")

##### add prod volume

#.........................................................................
# 2010-2014

# areal apportionment - 1 km . . . . . . . . . . . . . . . . . . . . . 
acs_data_in <- acs_2010_2014 %>% 
  st_intersection(wells_buffer_1km_2010_2014_prod)
acs_data_in <- acs_2010_2014 %>% filter(GEOID %in% acs_data_in$GEOID)
acs_data_in <- split(acs_data_in, seq(1, nrow(acs_data_in)))
acs_2010_2014_exp_prod_overlap_1km <- 
  mclapply(acs_data_in,  # if not on MacOS, use `lapply()` instead
           FUN          = assessPolygonOverlap,
           well_buffer  = wells_buffer_1km_2010_2014_prod, 
           exp_variable = "prop_exp_prod_1km_2010_2014")
acs_2010_2014_exp_prod_overlap_1km <- 
  do.call("rbind", acs_2010_2014_exp_prod_overlap_1km) %>%
  as_tibble() %>% select(-geometry) %>% mutate(period = as.factor("2010_2014"))
saveRDS(acs_2010_2014_exp_prod_overlap_1km,
        "data/interim/acs_2010_2014_exp_prod_overlap_1km.rds")

# annuli . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
acs_data_in <- acs_2010_2014 %>% 
  st_intersection(wells_buffer_3km_2010_2014_prod)
acs_data_in <- acs_2010_2014 %>% filter(GEOID %in% acs_data_in$GEOID)
acs_data_in <- split(acs_data_in, seq(1, nrow(acs_data_in)))
acs_2010_2014_exp_prod_annuli <- 
  mclapply(acs_data_in,  # if not on MacOS, use `lapply()` instead
           FUN          = assessExposureAnnuliCount,
           wells        = wells_prod_2010_2014, 
           exp_variable = "prod_wells_2010_2014")
acs_2010_2014_exp_prod_annuli <- 
  do.call("rbind", acs_2010_2014_exp_prod_annuli) %>%
  mutate(period = as.factor("2010_2014"))
saveRDS(acs_2010_2014_exp_prod_annuli,
        "data/interim/acs_2010_2014_exp_prod_annuli.rds")

#.........................................................................
# 2015-2019

# areal apportionment - 1 km . . . . . . . . . . . . . . . . . . . . . 
acs_data_in <- acs_2015_2019 %>% 
  st_intersection(wells_buffer_1km_2015_2019_prod)
acs_data_in <- acs_2015_2019 %>% filter(GEOID %in% acs_data_in$GEOID)
acs_data_in <- split(acs_data_in, seq(1, nrow(acs_data_in)))
acs_2015_2019_exp_prod_overlap_1km <- 
  mclapply(acs_data_in,  # if not on MacOS, use `lapply()` instead
           FUN          = assessPolygonOverlap,
           well_buffer  = wells_buffer_1km_2015_2019_prod, 
           exp_variable = "prop_exp_prod_1km_2015_2019")
acs_2015_2019_exp_prod_overlap_1km <- 
  do.call("rbind", acs_2015_2019_exp_prod_overlap_1km) %>%
  as_tibble() %>% select(-geometry) %>% mutate(period = as.factor("2015_2019"))
saveRDS(acs_2015_2019_exp_prod_overlap_1km,
        "data/interim/acs_2015_2019_exp_prod_overlap_1km.rds")

# annuli . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
acs_data_in <- acs_2015_2019 %>% 
  st_intersection(wells_buffer_3km_2015_2019_prod)
acs_data_in <- acs_2015_2019 %>% filter(GEOID %in% acs_data_in$GEOID)
acs_data_in <- split(acs_data_in, seq(1, nrow(acs_data_in)))
acs_2015_2019_exp_prod_annuli <- 
  mclapply(acs_data_in,  # if not on MacOS, use `lapply()` instead
           FUN          = assessExposureAnnuliCount,
           wells        = wells_prod_2015_2019, 
           exp_variable = "prod_wells_2015_2019")
acs_2015_2019_exp_prod_annuli <- 
  do.call("rbind", acs_2015_2019_exp_prod_annuli) %>%
  mutate(period = as.factor("2015_2019"))
saveRDS(acs_2015_2019_exp_prod_annuli,
        "data/interim/acs_2015_2019_exp_prod_annuli.rds")

# Note: Prod volume done elsewhere

##---------------------------------------------------------------------------
## Postproduction wells

#.........................................................................
# 2005-2009

# areal apportionment - 1 km . . . . . . . . . . . . . . . . . . . . . 
acs_data_in <- acs_2005_2009 %>% 
  st_intersection(wells_buffer_1km_2005_2009_postprod)
acs_data_in <- acs_2005_2009 %>% filter(GEOID %in% acs_data_in$GEOID)
acs_data_in <- split(acs_data_in, seq(1, nrow(acs_data_in)))
acs_2005_2009_exp_postprod_overlap_1km <- 
  mclapply(acs_data_in,  # if not on MacOS, use `lapply()` instead
           FUN          = assessPolygonOverlap,
           well_buffer  = wells_buffer_1km_2005_2009_postprod, 
           exp_variable = "prop_exp_postprod_1km_2005_2009")
acs_2005_2009_exp_postprod_overlap_1km <- 
  do.call("rbind", acs_2005_2009_exp_postprod_overlap_1km) %>%
  as_tibble() %>% select(-geometry) %>% mutate(period = as.factor("2005_2009"))
saveRDS(acs_2005_2009_exp_postprod_overlap_1km,
        "data/interim/acs_2005_2009_exp_postprod_overlap_1km.rds")

# annuli . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
acs_data_in <- acs_2005_2009 %>%
  st_intersection(wells_buffer_3km_2005_2009_postprod)
acs_data_in <- acs_2005_2009 %>% filter(GEOID %in% acs_data_in$GEOID)
acs_data_in <- split(acs_data_in, seq(1, nrow(acs_data_in)))
acs_2005_2009_exp_postprod_annuli <- 
  mclapply(acs_data_in,  # if not on MacOS, use `lapply()` instead
           FUN          = assessExposureAnnuliCount,
           wells        = wells_postprod_2005_2009, 
           exp_variable = "postprod_wells_2005_2009")
acs_2005_2009_exp_postprod_annuli <- 
  do.call("rbind", acs_2005_2009_exp_postprod_annuli) %>%
  mutate(period = as.factor("2005_2009"))
saveRDS(acs_2005_2009_exp_postprod_annuli,
        "data/interim/acs_2005_2009_exp_postprod_annuli.rds")

#.........................................................................
# 2010-2014

# areal apportionment - 1 km . . . . . . . . . . . . . . . . . . . . . 
acs_data_in <- acs_2010_2014 %>% 
  st_intersection(wells_buffer_1km_2010_2014_postprod)
acs_data_in <- acs_2010_2014 %>% filter(GEOID %in% acs_data_in$GEOID)
acs_data_in <- split(acs_data_in, seq(1, nrow(acs_data_in)))
acs_2010_2014_exp_postprod_overlap_1km <- 
  mclapply(acs_data_in,  # if not on MacOS, use `lapply()` instead
           FUN          = assessPolygonOverlap,
           well_buffer  = wells_buffer_1km_2010_2014_postprod, 
           exp_variable = "prop_exp_postprod_1km_2010_2014")
acs_2010_2014_exp_postprod_overlap_1km <- 
  do.call("rbind", acs_2010_2014_exp_postprod_overlap_1km) %>%
  as_tibble() %>% select(-geometry) %>% mutate(period = as.factor("2010_2014"))
saveRDS(acs_2010_2014_exp_postprod_overlap_1km,
        "data/interim/acs_2010_2014_exp_postprod_overlap_1km.rds")

# annuli . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
acs_data_in <- acs_2010_2014 %>% 
  st_intersection(wells_buffer_3km_2010_2014_postprod)
acs_data_in <- acs_2010_2014 %>% filter(GEOID %in% acs_data_in$GEOID)
acs_data_in <- split(acs_data_in, seq(1, nrow(acs_data_in)))
acs_2010_2014_exp_postprod_annuli <- 
  mclapply(acs_data_in,  # if not on MacOS, use `lapply()` instead
           FUN          = assessExposureAnnuliCount,
           wells        = wells_postprod_2010_2014, 
           exp_variable = "postprod_wells_2010_2014")
acs_2010_2014_exp_postprod_annuli <- 
  do.call("rbind", acs_2010_2014_exp_postprod_annuli) %>%
  mutate(period = as.factor("2010_2014"))
saveRDS(acs_2010_2014_exp_postprod_annuli,
        "data/interim/acs_2010_2014_exp_postprod_annuli.rds")

#.........................................................................
# 2015-2019

# areal apportionment - 1 km . . . . . . . . . . . . . . . . . . . . . 
acs_data_in <- acs_2015_2019 %>% 
  st_intersection(wells_buffer_1km_2015_2019_postprod)
acs_data_in <- acs_2015_2019 %>% filter(GEOID %in% acs_data_in$GEOID)
acs_data_in <- split(acs_data_in, seq(1, nrow(acs_data_in)))
acs_2015_2019_exp_postprod_overlap_1km <- 
  mclapply(acs_data_in,  # if not on MacOS, use `lapply()` instead
           FUN          = assessPolygonOverlap,
           well_buffer  = wells_buffer_1km_2015_2019_postprod, 
           exp_variable = "prop_exp_postprod_1km_2015_2019")
acs_2015_2019_exp_postprod_overlap_1km <- 
  do.call("rbind", acs_2015_2019_exp_postprod_overlap_1km) %>%
  as_tibble() %>% select(-geometry) %>% mutate(period = as.factor("2015_2019"))
saveRDS(acs_2015_2019_exp_postprod_overlap_1km,
        "data/interim/acs_2015_2019_exp_postprod_overlap_1km.rds")

# annuli . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
acs_data_in <- acs_2015_2019 %>% 
  st_intersection(wells_buffer_3km_2015_2019_postprod)
acs_data_in <- acs_2015_2019 %>% filter(GEOID %in% acs_data_in$GEOID)
acs_data_in <- split(acs_data_in, seq(1, nrow(acs_data_in)))
acs_2015_2019_exp_postprod_annuli <- 
  mclapply(acs_data_in,  # if not on MacOS, use `lapply()` instead
           FUN          = assessExposureAnnuliCount,
           wells        = wells_postprod_2015_2019, 
           exp_variable = "postprod_wells_2015_2019")
acs_2015_2019_exp_postprod_annuli <- 
  do.call("rbind", acs_2015_2019_exp_postprod_annuli) %>%
  mutate(period = as.factor("2015_2019"))
saveRDS(acs_2015_2019_exp_postprod_annuli,
        "data/interim/acs_2015_2019_exp_postprod_annuli.rds")


##---------------------------------------------------------------------------
## Plugged wells - count, cross-sectional - this analysis is for exposure
## to all plugged wells using the 2015-2019 ACS block groups data; do this
## only for this time period since most wells were plugged before 2005

# data prep . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
wells_buffer_plugged_1km <- 
  makeWellBuffer(subset(wells_interim,
                        well_status %in% c("Plugged", "PluggedOnly")), 1000)
wells_plugged <- wells_interim %>%
  filter(well_status %in% c("Plugged", "PluggedOnly")) %>%
  select(api_number, longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = crs_nad83)

# areal apportionment - 1 km . . . . . . . . . . . . . . . . . . . . . 
acs_data_in <- acs_2015_2019 %>% 
  st_intersection(wells_buffer_plugged_1km)
acs_data_in <- acs_2015_2019 %>% filter(GEOID %in% acs_data_in$GEOID)
acs_data_in <- split(acs_data_in, seq(1, nrow(acs_data_in)))
acs_2015_2019_exp_plugged_overlap_1km <- 
  mclapply(acs_data_in,  # if not on MacOS, use `lapply()` instead
           FUN          = assessPolygonOverlap,
           well_buffer  = wells_buffer_plugged_1km, 
           exp_variable = "prop_exp_plugged_1km")
acs_2015_2019_exp_plugged_overlap_1km <- 
  do.call("rbind", acs_2015_2019_exp_plugged_overlap_1km) %>%
  as_tibble() %>% select(-geometry) %>% mutate(period = as.factor("2015_2019"))
saveRDS(acs_2015_2019_exp_plugged_overlap_1km,
        "data/interim/acs_2015_2019_exp_plugged_overlap_1km.rds")

##============================================================================##
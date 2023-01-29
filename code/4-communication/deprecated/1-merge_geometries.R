acs_joined_2009 <- read_csv("data/processed/acs_joined_2009.csv")
acs_joined_2014 <- read_csv("data/processed/acs_joined_2014.csv")
acs_joined_2019 <- read_csv("data/processed/acs_joined_2019.csv")

acs_joined_2009_2009_wells <- read_csv("data/processed/acs_joined_2009_2009_wells.csv")
acs_joined_2014_2009_wells <- read_csv("data/processed/acs_joined_2014_2009_wells.csv")
acs_joined_2019_2009_wells <- read_csv("data/processed/acs_joined_2019_2009_wells.csv")

acs_joined_2009_2014_wells <- read_csv("data/processed/acs_joined_2009_2014_wells.csv")
acs_joined_2014_2014_wells <- read_csv("data/processed/acs_joined_2014_2014_wells.csv")
acs_joined_2019_2014_wells <- read_csv("data/processed/acs_joined_2019_2014_wells.csv")

acs_joined_2009_2019_wells <- read_csv("data/processed/acs_joined_2009_2019_wells.csv")
acs_joined_2014_2019_wells <- read_csv("data/processed/acs_joined_2014_2019_wells.csv")
acs_joined_2019_2019_wells <- read_csv("data/processed/acs_joined_2019_2019_wells.csv")



shapefile_2009 <- read_sf("data/raw/uscensus/acs/2009/2009_BlockGroups")
shapefile_2014 <- read_sf("data/raw/uscensus/acs/2014/2014_BlockGroups")
shapefile_2019 <- read_sf("data/raw/uscensus/acs/2019/2019_BlockGroups")

# rename shapefile columns
shapefile_2009 <- shapefile_2009 %>%
  rename(GEOID = BKGPIDFP00)
shapefile_2009$GEOID <- paste("15000US", shapefile_2009$GEOID, sep="")
shapefile_2014$GEOID <- paste("15000US", shapefile_2014$GEOID, sep="")
shapefile_2019$GEOID <- paste("15000US", shapefile_2019$GEOID, sep="")

# get geometry columns
geom_2009 <- shapefile_2009 %>%
  select(GEOID, geometry)
geom_2014 <- shapefile_2014 %>%
  select(GEOID, geometry)
geom_2019 <- shapefile_2019 %>%
  select(GEOID, geometry)

# merge in geometry for wells matching year ranges
acs_joined_2009 <- acs_joined_2009 %>%
  select (-c(geometry))
acs_joined_2009 <- merge(acs_joined_2009, geom_2009, by = "GEOID")

acs_joined_2014 <- acs_joined_2014 %>%
  select (-c(geometry))
acs_joined_2014 <- merge(acs_joined_2014, geom_2014, by = "GEOID")

acs_joined_2019 <- acs_joined_2019 %>%
  select (-c(geometry))
acs_joined_2019 <- merge(acs_joined_2019, geom_2019, by = "GEOID")

# merge in geometry for 2009 wells
acs_joined_2009_2009_wells <- acs_joined_2009_2009_wells %>%
  select (-c(geometry))
acs_joined_2009_2009_wells <- merge(acs_joined_2009_2009_wells, geom_2009, by = "GEOID")

acs_joined_2014_2009_wells <- acs_joined_2014_2009_wells %>%
  select (-c(geometry))
acs_joined_2014_2009_wells <- merge(acs_joined_2014_2009_wells, geom_2014, by = "GEOID")

acs_joined_2019_2009_wells <- acs_joined_2019_2009_wells %>%
  select (-c(geometry))
acs_joined_2019_2009_wells <- merge(acs_joined_2019_2009_wells, geom_2019, by = "GEOID")

# merge in geometry for 2014 wells
acs_joined_2009_2014_wells <- acs_joined_2009_2014_wells %>%
  select (-c(geometry))
acs_joined_2009_2014_wells <- merge(acs_joined_2009_2014_wells, geom_2009, by = "GEOID")

acs_joined_2014_2014_wells <- acs_joined_2014_2014_wells %>%
  select (-c(geometry))
acs_joined_2014_2014_wells <- merge(acs_joined_2014_2014_wells, geom_2014, by = "GEOID")

acs_joined_2019_2014_wells <- acs_joined_2019_2014_wells %>%
  select (-c(geometry))
acs_joined_2019_2014_wells <- merge(acs_joined_2019_2014_wells, geom_2019, by = "GEOID")

# merge in geometry for 2019 wells
acs_joined_2009_2019_wells <- acs_joined_2009_2019_wells %>%
  select (-c(geometry))
acs_joined_2009_2019_wells <- merge(acs_joined_2009_2019_wells, geom_2009, by = "GEOID")

acs_joined_2014_2019_wells <- acs_joined_2014_2019_wells %>%
  select (-c(geometry))
acs_joined_2014_2019_wells <- merge(acs_joined_2014_2019_wells, geom_2014, by = "GEOID")

acs_joined_2019_2019_wells <- acs_joined_2019_2019_wells %>%
  select (-c(geometry))
acs_joined_2019_2019_wells <- merge(acs_joined_2019_2019_wells, geom_2019, by = "GEOID")

acs_joined_2009 <- st_as_sf(acs_joined_2009)
acs_joined_2014 <- st_as_sf(acs_joined_2014)
acs_joined_2019 <- st_as_sf(acs_joined_2019)

acs_joined_2009_2009_wells <- st_as_sf(acs_joined_2009_2009_wells)
acs_joined_2014_2009_wells <- st_as_sf(acs_joined_2014_2009_wells)
acs_joined_2019_2009_wells <- st_as_sf(acs_joined_2019_2009_wells)

acs_joined_2009_2014_wells <- st_as_sf(acs_joined_2009_2014_wells)
acs_joined_2014_2014_wells <- st_as_sf(acs_joined_2014_2014_wells)
acs_joined_2019_2014_wells <- st_as_sf(acs_joined_2019_2014_wells)

acs_joined_2009_2019_wells <- st_as_sf(acs_joined_2009_2019_wells)
acs_joined_2014_2019_wells <- st_as_sf(acs_joined_2014_2019_wells)
acs_joined_2019_2019_wells <- st_as_sf(acs_joined_2019_2019_wells)



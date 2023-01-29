# Creating 5-year plots of Black/African American people's exposure to preproduction 
# wells (areal apportionment, 3km) 
# Uses only data in LA County

# Uses9/20/21 output
# get year, desired exposure columns, and all census data
output <- read_rds("data/processed/output.rds") %>%
  dplyr::select(GEOID, year, buffer_overlap_1km_preprod_5year, buffer_overlap_3km_preprod_5year, c(325:386)) 
output <- output%>%
  mutate("More Than High School Diploma" = rowSums(output[,c(38:43)]))

shapefile_2009 <- read_sf("data/raw/uscensus/acs/2009/2009_BlockGroups")
shapefile_2014 <- read_sf("data/raw/uscensus/acs/2014/2014_BlockGroups")
shapefile_2019 <- read_sf("data/raw/uscensus/acs/2019/2019_BlockGroups")

# rename shapefile columns
shapefile_2009 <- shapefile_2009 %>%
  rename(GEOID = BKGPIDFP00)
shapefile_2009$GEOID <- paste("15000US", shapefile_2009$GEOID, sep="")

shapefile_2014$GEOID <- paste("15000US", shapefile_2014$GEOID, sep="")

shapefile_2019$GEOID <- paste("15000US", shapefile_2019$GEOID, sep="")

# filter output to just Los Angeles
output <- output %>%
  filter(startsWith(GEOID, "15000US06037"))

summary_3km <- output %>%
  mutate ("Total Population 3km" = 
            output$"Total Population"*buffer_overlap_3km_preprod_5year,
          "Black or African American Alone 3km" = 
            output$"Black or African American Alone"*buffer_overlap_3km_preprod_5year)

summary_3km_2009 <- st_as_sf(merge(summary_3km %>%
  filter(year == 2009), shapefile_2009, by = "GEOID"))
summary_3km_2014 <- st_as_sf(merge(summary_3km %>%
  filter(year == 2014), shapefile_2009, by = "GEOID"))
summary_3km_2019 <- st_as_sf(merge(summary_3km %>%
  filter(year == 2019), shapefile_2009, by = "GEOID"))

library(leaflet)
pal <- colorNumeric("plasma", domain = NULL, reverse = FALSE)
# leaflet(summary_3km_2009) %>%
#   addPolygons(stroke = FALSE, weight = 2, smoothFactor = 0.3,
#               fillOpacity = 1,
#               fillColor = pal(summary_3km_2009$"Black or African American Alone 3km"),
#               highlight = highlightOptions(weight = 5, fillOpacity = 1.0, 
#                                            bringToFront =TRUE))

LA_2009 <- ggplot(summary_3km_2009) +
  geom_sf(aes(fill = summary_3km_2009$`Black or African American Alone 3km`),lwd = 0)+
  scale_fill_gradientn(colors = c("white", "blue"), limits = c(-1, 3500))+
  theme(legend.position = "none") 
LA_2014 <- ggplot(summary_3km_2014) +
  geom_sf(aes(fill = summary_3km_2014$`Black or African American Alone 3km`),lwd = 0)+
  scale_fill_gradientn(colors = c("white", "blue"), limits = c(-1, 3500))+
  theme(legend.position = "none") 
LA_2019 <- ggplot(summary_3km_2019) +
  geom_sf(aes(fill = summary_3km_2019$`Black or African American Alone 3km`),lwd = 0)+
  scale_fill_gradientn(colors = c("white", "blue"), limits = c(-1, 3500))

library(grid)
grid.draw(cbind(ggplotGrob(LA_2009), ggplotGrob(LA_2014), ggplotGrob(LA_2019)))



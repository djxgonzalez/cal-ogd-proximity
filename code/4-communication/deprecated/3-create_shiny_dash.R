# uses acs_joined_2009_renamed,acs_joined_2014_renamed, acs_joined_2019_renamed 
# from 6-column_rename in data_tidying
acs_2009 <- acs_joined_2009_renamed
acs_2014 <- acs_joined_2014_renamed
acs_2019 <- acs_joined_2019_renamed
shapefile_2009 <- read_sf("data/raw/us_census/acs/2009/2009_BlockGroups")
shapefile_2014 <- read_sf("data/raw/us_census/acs/2014/2014_BlockGroups")
shapefile_2019 <- read_sf("data/raw/us_census/acs/2019/2019_BlockGroups")

# rename shapefile columns
shapefile_2009 <- shapefile_2009 %>%
  rename(GEOID = BKGPIDFP00)
shapefile_2009$GEOID <- paste("15000US", shapefile_2009$GEOID, sep="")

shapefile_2014$GEOID <- paste("15000US", shapefile_2014$GEOID, sep="")

shapefile_2019$GEOID <- paste("15000US", shapefile_2019$GEOID, sep="")

# merge geometry column
geom_2009 <- shapefile_2009 %>%
  select(GEOID, geometry)
acs_2009 <- acs_2009 %>%
  select (-c(geometry))
acs_2009 <- merge(acs_2009, geom_2009, by = "GEOID")

geom_2014 <- shapefile_2014 %>%
  select(GEOID, geometry)
acs_2014 <- acs_2014 %>%
  select (-c(geometry))
acs_2014 <- merge(acs_2014, geom_2014, by = "GEOID")

geom_2019 <- shapefile_2019 %>%
  select(GEOID, geometry)
acs_2019 <- acs_2019 %>%
  select (-c(geometry))
acs_2019 <- merge(acs_2019, geom_2019, by = "GEOID")

geom_2009_sf <- st_as_sf(acs_2009)

kern_2009 <- geom_2009_sf %>%   #### DG - where does `kern_2009`` come from?
  filter(startsWith(GEOID, "15000US06029")) %>%
  mutate(perc_white_alone = kern_2009$"White Alone"/kern_2009$"Total Population") %>%
  mutate(perc_hispanic_or_latino = kern_2009$"Hispanic or Latino"/kern_2009$"Total Population")

kern_2009$"Median Contract Rent" <- as.numeric(kern_2009$"Median Contract Rent")

la_2009 <- geom_2009_sf %>%
  filter(startsWith(GEOID, "15000US06037")) %>%
  mutate(perc_white_alone = la_2009$"White Alone"/la_2009$"Total Population") %>%
  mutate(perc_hispanic_or_latino = la_2009$"Hispanic or Latino"/la_2009$"Total Population")

la_2009$"Median Contract Rent" <- as.numeric(la_2009$"Median Contract Rent")


geom_2009_sf <- geom_2009_sf %>%
  mutate(perc_white_alone = geom_2009_sf$"White Alone"/geom_2009_sf$"Total Population") %>%
  mutate(perc_hispanic_or_latino = geom_2009_sf$"Hispanic or Latino"/geom_2009_sf$"Total Population")

geom_2009_sf$"Median Contract Rent" <- as.numeric(geom_2009_sf$"Median Contract Rent")



geom_2014_sf <- st_as_sf(acs_2014)

kern_2014 <- geom_2014_sf %>%
  filter(startsWith(GEOID, "15000US06029")) %>%
  mutate(perc_white_alone = kern_2014$"White Alone"/kern_2014$"Total Population") %>%
  mutate(perc_hispanic_or_latino = kern_2014$"Hispanic or Latino"/kern_2014$"Total Population")

kern_2014$"Median Contract Rent" <- as.numeric(kern_2014$"Median Contract Rent")

la_2014 <- geom_2014_sf %>%
  filter(startsWith(GEOID, "15000US06037")) %>%
  mutate(perc_white_alone = la_2014$"White Alone"/la_2014$"Total Population") %>%
  mutate(perc_hispanic_or_latino = la_2014$"Hispanic or Latino"/la_2014$"Total Population")

la_2014$"Median Contract Rent" <- as.numeric(la_2014$"Median Contract Rent")


geom_2014_sf <- geom_2014_sf %>%
  mutate(perc_white_alone = geom_2014_sf$"White Alone"/geom_2014_sf$"Total Population") %>%
  mutate(perc_hispanic_or_latino = geom_2014_sf$"Hispanic or Latino"/geom_2014_sf$"Total Population")

geom_2014_sf$"Median Contract Rent" <- as.numeric(geom_2014_sf$"Median Contract Rent")



geom_2019_sf <- st_as_sf(acs_2019)

kern_2019 <- geom_2019_sf %>%
  filter(startsWith(GEOID, "15000US06029")) %>%
  mutate(perc_white_alone = kern_2019$"White Alone"/kern_2019$"Total Population") %>%
  mutate(perc_hispanic_or_latino = kern_2019$"Hispanic or Latino"/kern_2019$"Total Population")

kern_2019$"Median Contract Rent" <- as.numeric(kern_2019$"Median Contract Rent")

la_2019 <- geom_2019_sf %>%
  filter(startsWith(GEOID, "15000US06037")) %>%
  mutate(perc_white_alone = la_2019$"White Alone"/la_2019$"Total Population") %>%
  mutate(perc_hispanic_or_latino = la_2019$"Hispanic or Latino"/la_2019$"Total Population")

la_2019$"Median Contract Rent" <- as.numeric(la_2019$"Median Contract Rent")


geom_2019_sf <- geom_2019_sf %>%
  mutate(perc_white_alone = geom_2019_sf$"White Alone"/geom_2019_sf$"Total Population") %>%
  mutate(perc_hispanic_or_latino = geom_2019_sf$"Hispanic or Latino"/geom_2019_sf$"Total Population")

geom_2019_sf$"Median Contract Rent" <- as.numeric(geom_2019_sf$"Median Contract Rent")

library(leaflet)
library(shiny)
pal <- colorNumeric("plasma", domain = NULL, reverse = FALSE)
choice_list <- c("buffer_overlap_762m", "buffer_overlap_1km", "buffer_overlap_3km", "buffer_overlap_5km", "buffer_overlap_10km",
                 "perc_white_alone",
                 "perc_hispanic_or_latino",
                 "Median Contract Rent")
  
ui <- fluidPage (
  navbarPage("Mapping Oil Well Data", 
             #theme = shinytheme("cosmo"),
             sidebarLayout(
               sidebarPanel(
                 radioButtons(inputId = "selected", "Select Variable",
                              choices = choice_list
                 )
               ),
               mainPanel(
                 leafletOutput("map", width = "100%", height = 900)
               )
             )
  )
)

server <- function(input, output) {
  filtered <- reactive ({
    as.data.frame(subset(geom_2019_sf, select = c(input$selected, "geometry")))
  })
  
  output$map <- renderLeaflet ({
    leaflet(filtered()) %>%
      addTiles() %>%
      addPolygons(data = geom_2019_sf, stroke = FALSE, smoothFactor = 0.3,
                  fillOpacity = 1,
                  fillColor = ~pal(filtered()[,1]),
                  highlight = highlightOptions(weight = 5, fillOpacity = 1.0, bringToFront =TRUE),
                  label = paste(input$selected, filtered()[,1], sep = ": "))
  })
  
}

shinyApp(ui = ui, server = server)

large_population <- acs_2014 %>% 
  filter(acs_2014$'Total Population' > 30000)
# examining population distributions -- "total population" is the number surveyed,
# not necessarily the number of people in the block group (though probably pretty close)
# ggplot (data = geom_2009_sf, aes(x = "", y = geom_2009_sf$"Total Population")) +
#   geom_boxplot() 
# ggplot (data = acs_2014, aes(x = "", y = acs_2014$'Total Population')) +
#   geom_boxplot() 
# ggplot (data = acs_2019, aes(x = "", y = acs_2019$'Total Population')) +
#   geom_boxplot() 



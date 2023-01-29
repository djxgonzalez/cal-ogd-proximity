# uses acs_joined_2009_renamed, acs_joined_2014_renamed, acs_joined_2019_renamed from 6-column_rename in data_tidying
# when running 6-column_rename, use second set of imports to get 2014 wells in preproduction only.
 
acs_2009_2014_wells <- acs_joined_2009_renamed
acs_2014_2014_wells <- acs_joined_2014_renamed
acs_2019_2014_wells <- acs_joined_2019_renamed
shapefile_2009 <- read_sf("data/raw/uscensus/acs/2009/2009_BlockGroups")
shapefile_2014 <- read_sf("data/raw/uscensus/acs/2014/2014_BlockGroups")
shapefile_2019 <- read_sf("data/raw/uscensus/acs/2019/2019_BlockGroups")

# rename shapefile columns
shapefile_2009 <- shapefile_2009 %>%
  rename(GEOID = BKGPIDFP00)
shapefile_2009$GEOID <- paste("15000US", shapefile_2009$GEOID, sep="")

shapefile_2014$GEOID <- paste("15000US", shapefile_2014$GEOID, sep="")

shapefile_2019$GEOID <- paste("15000US", shapefile_2019$GEOID, sep="")

# merge geometry column
geom_2009 <- shapefile_2009 %>%
  select(GEOID, geometry)
acs_2009_2014_wells <- acs_2009_2014_wells %>%
  select (-c(geometry))
acs_2009_2014_wells <- merge(acs_2009_2014_wells, geom_2009, by = "GEOID")

geom_2014 <- shapefile_2014 %>%
  select(GEOID, geometry)
acs_2014_2014_wells <- acs_2014_2014_wells %>%
  select (-c(geometry))
acs_2014_2014_wells <- merge(acs_2014_2014_wells, geom_2014, by = "GEOID")

geom_2019 <- shapefile_2019 %>%
  select(GEOID, geometry)
acs_2019_2014_wells <- acs_2019_2014_wells %>%
  select (-c(geometry))
acs_2019_2014_wells <- merge(acs_2019_2014_wells, geom_2019, by = "GEOID")

geom_2009_sf <- st_as_sf(acs_2009_2014_wells)

geom_2014_sf <- st_as_sf(acs_2014_2014_wells)

geom_2019_sf <- st_as_sf(acs_2019_2014_wells)







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
                              choices = choice_list,
                              selecte = "buffer_overlap_3km"
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
    as.data.frame(subset(geom_2009_sf, select = c(input$selected, "geometry")))
  })
  
  output$map <- renderLeaflet ({
    leaflet(filtered()) %>%
      addTiles() %>%
      addPolygons(data = geom_2009_sf, stroke = FALSE, smoothFactor = 0.3,
                  fillOpacity = 1,
                  fillColor = ~pal(filtered()[,1]),
                  highlight = highlightOptions(weight = 5, fillOpacity = 1.0, bringToFront =TRUE),
                  label = paste(input$selected, filtered()[,1], sep = ": "))
  })
  
}

shinyApp(ui = ui, server = server)


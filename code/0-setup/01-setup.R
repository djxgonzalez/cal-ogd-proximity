##============================================================================##
## 0.01 - attaches necessary packages and defines global variables

##---------------------------------------------------------------------------
## setup

# installs necessary packages for this R project; un-comment and run if needed
#install.packages("tidyverse", "lubridate", "sf", "ggspatial", "tidycensus")

# attaches necessary packages
library("tidyverse")
library("lubridate")
library("sf")

##---------------------------------------------------------------------------
## defines global variables

# makes a "not in" operator, the inverse of %in%
'%!in%' <- function(x,y)!('%in%'(x,y))

# coordinate reference system (CRS) for the project
crs_nad83     <- st_crs(4269) # NAD83 coordinate reference system
crs_projected <- st_crs(5070) # Albers Equal-Area Conic projection, contiguous US

##============================================================================##
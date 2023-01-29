#####===========================================================================
##### 0. Setup

# loads necessary packages and defines global variables
source("code/0-setup/01-setup.R")


#####===========================================================================
##### 1. Data Tidying

# attaches functions for tidying raw data
source("code/1-data_tidying/1-tidy_acs_data.R")
source("code/1-data_tidying/3-tidy_calgem_wells_data.R")
source("code/1-data_tidying/4-tidy_calgem_production_data.R") # not yet edited by Claire

# imports raw data, calls tidying functions, exports interim data
source("code/1-data_tidying/5-call_data_tidying.R")

# renames columns of ACS data
source("code/1-data_tidying/6-column_rename.R")


#####===========================================================================
##### 2. Exposure Assessment

# *NOTE* This code needs some revising, I'll get to work on that! -DG

### Functions for areal apportionment ........................................
source("code/2-exposure_assessment/1-asess_exposure_areal_apportionment.R")

### Creates buffer regions, exports shapefile and joined ACS data.............
source("code/2-exposure_assessment/2-call_exposure_areal_apportionment.R")

### Sum of production, 1-km annuli .........................................
#source("code/2-exposure_assessment/7.R")  # not written yet!


#####===========================================================================
##### 3. Analysis

# *Note:* To do; I have some code to get us started here!



#####===========================================================================
##### 4. Communication

# imports raw and processed data, preps data as needed, and generates Figure 1
source("code/4-communication/1-make_figure1.R")



##============================================================================##
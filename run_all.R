##============================================================================##
## TITLE: Longitudinally assessing populations in proximity to upstream oil and 
## gas production facilities in California: Persistent racial and socioeconomic 
## disparities
##
## AIMS: In this study, our aim was to assess the extent to which racially and 
## socioeconomically marginalized people are disproportionately exposed to oil 
## and gas wells in various stages of production in California. Also, to 
## determine whether disparities have been persistent over the past two decades.


##============================================================================
## 0. Setup

# loads necessary packages and defines global variables
source("code/0-setup/01-setup.R")

##============================================================================
## 1. Data Tidying

# attaches functions for tidying raw data
##### rename these
source("code/1-data_tidying/01-tidy_enverus_data.R")
source("code/1-data_tidying/02-tidy_calgem_production_data.R")
source("code/1-data_tidying/03-tidy_ipums_data.R")

# imports raw data, calls tidying functions, exports interim data
source("code/1-data_tidying/04-call_data_tidying.R")

##============================================================================
## 2. Exposure Assessment

# attaches functions for assessing exposur
source("code/2-exposure_assessment/01-fxn_assess_exposure_areal_apportionment.R")
source("code/2-exposure_assessment/02-fxn_assess_exposure_annuli_count.R")
source("code/2-exposure_assessment/03-fxn_assess_exposure_annuli_volume.R")

# imports data, calls exposure assessments, exports interim exposure data
source("code/2-exposure_assessment/04-assess_exposure_cities.R")

# assembles and exports analytic dataset
source("code/2-exposure_assessment/05-assemble_exposure_data.R")

##============================================================================
## 3. Analysis

# descriptive statistics are in the Rmd ##### confirm + edit as needed


##============================================================================
## 4. Communication

# imports raw and processed data, preps data as needed, and generates main 
# table and figures as well as supplemental figures
source("code/4-communication/01-make_table_1.R")
source("code/4-communication/02-make_figure_1.R")
source("code/4-communication/03-make_figure_2.R")
source("code/4-communication/04-make_figure_3.R")
source("code/4-communication/05-make_figure_4.R")
source("code/4-communication/06-make_figure_5.R")
source("code/4-communication/07-make_table_s1.R")
source("code/4-communication/08-make_figure_s1.R")

##============================================================================##
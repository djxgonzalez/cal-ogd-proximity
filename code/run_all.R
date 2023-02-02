##============================================================================##
## run_all.R - This script is a sort of table of contents for this R project.
## Most of the scripts necessary to conduct this project were written to run
## independently. In this `run_all` script, we assembled the elements of the 
## R project sequentially. However, we do not advise running this script in 
## whole, as the memory demands may cause R to crash.

## 0. Setup ==================================================================
## loads necessary packages and defines global variables
source("code/0-setup/01-setup.R")

## 1. Data Tidying ===========================================================
## attaches functions for tidying raw data, imports raw data, calls tidying 
## functions, exports interim data
source("code/1-data_tidying/01-tidy_acs_data.R")
source("code/1-data_tidying/02-tidy_calgem_wells_data.R")
source("code/1-data_tidying/03-call_data_tidying.R")
source("code/1-data_tidying/04-add_ses_variables.R")
source("code/1-data_tidying/05-call_data_tidying2.R")
source("code/1-data_tidying/06-rename_columns.R")

## 2. Exposure Assessment ====================================================
## defines functions to assess exposure
source("code/2-exposure_assessment/01-fxn_assess_exposure_areal_apportionment.R")
source("code/2-exposure_assessment/02-fxn_assess_exposure_annuli_count.R")
source("code/2-exposure_assessment/03-fxn_assess_exposure_annuli_volume.R")
# imports interim data, attaches and calls exposure assessment functions, 
# assembles and exports analytic dataset
source("code/2-exposure_assessment/04-call_exposure_assessment.R")
source("code/2-exposure_assessment/05-assemble_exposure_data.R")

## 3. Analysis ===============================================================
## conducts main and supplemental analyses and exports results as CSV files
source("code/3-analysis/01-calculate_risk_ratios.R")
source("code/3-analysis/02-calculate_risk_ratios_county_losangeles.R")
source("code/3-analysis/03-calculate_risk_ratios_county_orange.R")
source("code/3-analysis/04-calculate_risk_ratios_county_kern.R")
source("code/3-analysis/05-calculate_risk_ratios_by_quantile.R")
source("code/3-analysis/06-calculate_risk_ratios_by_quantile_county_losangeles.R")
source("code/3-analysis/07-calculate_risk_ratios_by_quantile_county_orange.R")
source("code/3-analysis/08-calculate_risk_ratios_by_quantile_county_kern.R")
source("code/3-analysis/09-calculate_change_prod_preprod.R")
source("code/3-analysis/10-calculate_change_prod_preprod.R")

## 4. Communication ==========================================================
## generates main figures and tables
source("code/4-communication/01-make_table_1.R")
source("code/4-communication/02-make_figure_1.R")
source("code/4-communication/03-make_figure_2.R")
source("code/4-communication/04-make_figure_3.R")
source("code/4-communication/05-make_figure_4.R")

##============================================================================##
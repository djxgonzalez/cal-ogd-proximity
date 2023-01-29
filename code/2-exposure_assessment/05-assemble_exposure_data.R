##============================================================================##
## 2.05 - assembles analytic data set with ACS and exposure variables

##---------------------------------------------------------------------------
## setup

# tidying function .......................................................
tidyACSData <- function(acs_data) {
  acs_data <- acs_data %>%
    as_tibble() %>%
    # appends renamed columns that we need for analysis
    mutate(block_group_id          = str_sub(GEOID, -12, -1)) %>%
    mutate(pop_total               = `Total Population`,
           pop_hispanic            = `Hispanic or Latino`,
           pop_nonhisp_am_indian   =
             `Not Hispanic or Latino: American Indian and Alaska Native Alone`,
           pop_nonhisp_asian       = `Not Hispanic or Latino: Asian Alone`,
           pop_nonhisp_black       = 
             `Not Hispanic or Latino: Black or African American Alone`,
           pop_nonhisp_white       = `Not Hispanic or Latino: White Alone`,
           pop_nonhisp_other       = 
             `Not Hispanic or Latino: Some Other Race Alone`,
           pop_nonhisp_two_or_more = `Not Hispanic or Latino: Two or More Races`,
           pop_educ_universe       = `Education Universe`,
           pop_educ_less_than_hs   = `Less Than High School Diploma`,
           pop_educ_hs             = `High School Diploma`,
           pop_educ_more_than_hs   = `Some College` + `Associate's Degree` +
             `Bachelor's Degree` + `Master's Degree` + 
             `Professional School Degree` + `Doctorate Degree`,
           pop_poverty_universe    = `Household Income Universe`,
           pop_below_poverty        =
             `Income in Past 12 Months Below Poverty Level`) %>%
    ##### don't forget renter status and linguistic iso
    # calculates population proportions at block group level
    mutate(prop_hispanic           = (pop_hispanic / pop_total),
           prop_nonhisp_am_indian  = (pop_nonhisp_am_indian / pop_total),
           prop_nonhisp_asian      = (pop_nonhisp_asian / pop_total),
           prop_nonhisp_black      = (pop_nonhisp_black / pop_total),
           prop_nonhisp_white      = (pop_nonhisp_white / pop_total),
           prop_nonhisp_other      = (pop_nonhisp_other / pop_total),
           prop_nonhisp_to_or_more = (pop_nonhisp_two_or_more / pop_total),
           prop_educ_less_than_hs  = (pop_educ_less_than_hs / pop_educ_universe),
           prop_educ_hs            = (pop_educ_hs / pop_educ_universe),
           prop_educ_more_than_hs  = (pop_educ_more_than_hs / pop_educ_universe),
           prop_below_poverty      = (pop_below_poverty / pop_poverty_universe)) %>%
    # keeps only the columns that we need
    select(c(block_group_id:prop_below_poverty))
  return(acs_data)
}

##---------------------------------------------------------------------------
## data input, prep, and assembly

# 2005 to 2009 data  .....................................................

acs_exposure_2005_2009 <- readRDS("data/interim/acs_2005_2009_california.rds") %>%
  tidyACSData() %>%
  mutate(GEOID  = block_group_id,
         period = as.factor("2005_2009")) %>%
  left_join(readRDS("data/interim/acs_2005_2009_exp_preprod_overlap_1km.rds"), 
            by = c("GEOID", "period")) %>%
  left_join(readRDS("data/interim/acs_2005_2009_exp_preprod_annuli.rds"), 
            by = c("GEOID", "period")) %>%
  mutate(GEOID = block_group_id) %>%
  left_join(readRDS("data/interim/acs_2005_2009_exp_prod_overlap_1km.rds"), 
            by = c("GEOID", "period")) %>%
  left_join(readRDS("data/interim/acs_2005_2009_exp_prod_annuli.rds"), 
            by = c("GEOID", "period")) %>%
  mutate(GEOID = block_group_id) %>%
  left_join(readRDS("data/interim/acs_2005_2009_exp_postprod_overlap_1km.rds"), 
            by = c("GEOID", "period")) %>%
  left_join(readRDS("data/interim/acs_2005_2009_exp_postprod_annuli.rds"), 
            by = c("GEOID", "period")) %>%
  left_join(readRDS("data/interim/acs_2005_2009_exp_prod_volume_annuli.rds"), 
            by = c("GEOID", "period")) %>%
  as_tibble() %>%
  mutate(prop_exp_preprod_1km =  # replaces unexposed NAs with 0s
           replace_na(prop_exp_preprod_1km_2005_2009, 0),
         preprod_wells_0_1km = 
           replace_na(preprod_wells_2005_2009_0_1km, 0),
         preprod_wells_1_2km = 
           replace_na(preprod_wells_2005_2009_1_2km, 0),
         preprod_wells_2_3km = 
           replace_na(preprod_wells_2005_2009_2_3km, 0),
         prop_exp_prod_1km = 
           replace_na(prop_exp_prod_1km_2005_2009, 0),
         prod_wells_0_1km = 
           replace_na(prod_wells_2005_2009_0_1km, 0),
         prod_wells_1_2km = 
           replace_na(prod_wells_2005_2009_1_2km, 0),
         prod_wells_2_3km = 
           replace_na(prod_wells_2005_2009_2_3km, 0),
         prop_exp_postprod_1km = 
           replace_na(prop_exp_postprod_1km_2005_2009, 0),
         postprod_wells_0_1km = 
           replace_na(postprod_wells_2005_2009_0_1km, 0),
         postprod_wells_1_2km = 
           replace_na(postprod_wells_2005_2009_1_2km, 0),
         postprod_wells_2_3km = 
           replace_na(postprod_wells_2005_2009_2_3km, 0),
         prod_volume_0_1km = 
           replace_na(prod_volume_2005_2009_0_1km, 0),
         prod_volume_1_2km = 
           replace_na(prod_volume_2005_2009_1_2km, 0),
         prod_volume_2_3km = 
           replace_na(prod_volume_2005_2009_2_3km, 0)) %>%
  select(GEOID, period, pop_total:prop_below_poverty,
         prop_exp_preprod_1km, preprod_wells_0_1km, preprod_wells_1_2km, 
         preprod_wells_2_3km, prop_exp_prod_1km, prod_wells_0_1km, 
         prod_wells_1_2km, prod_wells_2_3km, prop_exp_postprod_1km, 
         postprod_wells_0_1km, postprod_wells_1_2km, postprod_wells_2_3km, 
         prod_volume_0_1km, prod_volume_1_2km, prod_volume_2_3km) %>%
  mutate(GEOID = as.factor(GEOID))


# 2010 to 2014 data  .....................................................

acs_exposure_2010_2014 <- readRDS("data/interim/acs_2010_2014_california.rds") %>%
  tidyACSData() %>%
  mutate(GEOID = block_group_id,
         period = as.factor("2010_2014")) %>%
  left_join(readRDS("data/interim/acs_2010_2014_exp_preprod_overlap_1km.rds"), 
            by = c("GEOID", "period")) %>%
  left_join(readRDS("data/interim/acs_2010_2014_exp_preprod_annuli.rds"), 
            by = c("GEOID", "period")) %>%
  mutate(GEOID = block_group_id) %>%
  left_join(readRDS("data/interim/acs_2010_2014_exp_prod_overlap_1km.rds"), 
            by = c("GEOID", "period")) %>%
  left_join(readRDS("data/interim/acs_2010_2014_exp_prod_annuli.rds"), 
            by = c("GEOID", "period")) %>%
  mutate(GEOID = block_group_id) %>%
  left_join(readRDS("data/interim/acs_2010_2014_exp_postprod_overlap_1km.rds"), 
            by = c("GEOID", "period")) %>%
  left_join(readRDS("data/interim/acs_2010_2014_exp_postprod_annuli.rds"), 
            by = c("GEOID", "period")) %>%
  left_join(readRDS("data/interim/acs_2010_2014_exp_prod_volume_annuli.rds"), 
            by = c("GEOID", "period")) %>%
  as_tibble() %>%
  mutate(prop_exp_preprod_1km =  # replaces unexposed NAs with 0s
           replace_na(prop_exp_preprod_1km_2010_2014, 0),
         preprod_wells_0_1km = 
           replace_na(preprod_wells_2010_2014_0_1km, 0),
         preprod_wells_1_2km = 
           replace_na(preprod_wells_2010_2014_1_2km, 0),
         preprod_wells_2_3km = 
           replace_na(preprod_wells_2010_2014_2_3km, 0),
         prop_exp_prod_1km = 
           replace_na(prop_exp_prod_1km_2010_2014, 0),
         prod_wells_0_1km = 
           replace_na(prod_wells_2010_2014_0_1km, 0),
         prod_wells_1_2km = 
           replace_na(prod_wells_2010_2014_1_2km, 0),
         prod_wells_2_3km = 
           replace_na(prod_wells_2010_2014_2_3km, 0),
         prop_exp_postprod_1km = 
           replace_na(prop_exp_postprod_1km_2010_2014, 0),
         postprod_wells_0_1km = 
           replace_na(postprod_wells_2010_2014_0_1km, 0),
         postprod_wells_1_2km = 
           replace_na(postprod_wells_2010_2014_1_2km, 0),
         postprod_wells_2_3km = 
           replace_na(postprod_wells_2010_2014_2_3km, 0),
         prod_volume_0_1km = 
           replace_na(prod_volume_2010_2014_0_1km, 0),
         prod_volume_1_2km = 
           replace_na(prod_volume_2010_2014_1_2km, 0),
         prod_volume_2_3km = 
           replace_na(prod_volume_2010_2014_2_3km, 0)) %>%
  select(GEOID, period, pop_total:prop_below_poverty,
         prop_exp_preprod_1km, preprod_wells_0_1km, preprod_wells_1_2km, 
         preprod_wells_2_3km, prop_exp_prod_1km, prod_wells_0_1km, 
         prod_wells_1_2km, prod_wells_2_3km, prop_exp_postprod_1km, 
         postprod_wells_0_1km, postprod_wells_1_2km, postprod_wells_2_3km, 
         prod_volume_0_1km, prod_volume_1_2km, prod_volume_2_3km) %>%
  mutate(GEOID = as.factor(GEOID))


# 2015 to 2019 data  .....................................................

acs_exposure_2015_2019 <- readRDS("data/interim/acs_2015_2019_california.rds") %>%
  tidyACSData() %>%
  mutate(GEOID  = block_group_id,
         period = as.factor("2015_2019")) %>%
  left_join(readRDS("data/interim/acs_2015_2019_exp_preprod_overlap_1km.rds"), 
            by = c("GEOID", "period")) %>%
  left_join(readRDS("data/interim/acs_2015_2019_exp_preprod_annuli.rds"), 
            by = c("GEOID", "period")) %>%
  mutate(GEOID = block_group_id) %>%
  left_join(readRDS("data/interim/acs_2015_2019_exp_prod_overlap_1km.rds"), 
            by = c("GEOID", "period")) %>%
  left_join(readRDS("data/interim/acs_2015_2019_exp_prod_annuli.rds"), 
            by = c("GEOID", "period")) %>%
  mutate(GEOID = block_group_id) %>%
  left_join(readRDS("data/interim/acs_2015_2019_exp_postprod_overlap_1km.rds"), 
            by = c("GEOID", "period")) %>%
  left_join(readRDS("data/interim/acs_2015_2019_exp_postprod_annuli.rds"), 
            by = c("GEOID", "period")) %>%
  left_join(readRDS("data/interim/acs_2015_2019_exp_prod_volume_annuli.rds"), 
            by = c("GEOID", "period")) %>%
  left_join(readRDS("data/interim/acs_2015_2019_exp_plugged_overlap_1km.rds"), 
            by = c("GEOID", "period")) %>%
  as_tibble() %>%
  mutate(prop_exp_preprod_1km =  # replaces unexposed NAs with 0s
           replace_na(prop_exp_preprod_1km_2015_2019, 0),
         preprod_wells_0_1km = 
           replace_na(preprod_wells_2015_2019_0_1km, 0),
         preprod_wells_1_2km = 
           replace_na(preprod_wells_2015_2019_1_2km, 0),
         preprod_wells_2_3km = 
           replace_na(preprod_wells_2015_2019_2_3km, 0),
         prop_exp_prod_1km = 
           replace_na(prop_exp_prod_1km_2015_2019, 0),
         prod_wells_0_1km = 
           replace_na(prod_wells_2015_2019_0_1km, 0),
         prod_wells_1_2km = 
           replace_na(prod_wells_2015_2019_1_2km, 0),
         prod_wells_2_3km = 
           replace_na(prod_wells_2015_2019_2_3km, 0),
         prop_exp_postprod_1km = 
           replace_na(prop_exp_postprod_1km_2015_2019, 0),
         postprod_wells_0_1km = 
           replace_na(postprod_wells_2015_2019_0_1km, 0),
         postprod_wells_1_2km = 
           replace_na(postprod_wells_2015_2019_1_2km, 0),
         postprod_wells_2_3km = 
           replace_na(postprod_wells_2015_2019_2_3km, 0),
         prod_volume_0_1km = 
           replace_na(prod_volume_2015_2019_0_1km, 0),
         prod_volume_1_2km = 
           replace_na(prod_volume_2015_2019_1_2km, 0),
         prod_volume_2_3km = 
           replace_na(prod_volume_2015_2019_2_3km, 0),
         prop_exp_plugged_1km =  # note: we just have these data for 2015-2019
           replace_na(prop_exp_plugged_1km, 0)) %>%
  select(GEOID, period, pop_total:prop_below_poverty,
         prop_exp_preprod_1km, preprod_wells_0_1km, preprod_wells_1_2km, 
         preprod_wells_2_3km, prop_exp_prod_1km, prod_wells_0_1km, 
         prod_wells_1_2km, prod_wells_2_3km, prop_exp_postprod_1km, 
         postprod_wells_0_1km, postprod_wells_1_2km, postprod_wells_2_3km, 
         prod_volume_0_1km, prod_volume_1_2km, prod_volume_2_3km,
         prop_exp_plugged_1km) %>%
  mutate(GEOID = as.factor(GEOID))


##### TO DO --- add plugged wells prop

##---------------------------------------------------------------------------
## final data assembly and export

# binds rows for each period
acs_exposure_2005_2019 <- acs_exposure_2005_2009 %>%
  bind_rows(acs_exposure_2010_2014) %>%
  bind_rows(acs_exposure_2015_2019)

# exports processed dataset
saveRDS(acs_exposure_2005_2019, "data/processed/acs_exposure_2005_2019.rds")


##============================================================================##
## This code extracts prod volume exposure data from and older vintage of our
## processed dataset and prepares it for inclusion in the above workflow

# acs_exposure_all_years <- readRDS("data/deprecated/acs_exposure_all_years.rds")
# 
# acs_2005_2009_exp_prod_volume <- acs_exposure_all_years %>%
#   select(GEOID, year, prod_volume_0to762m_5year, prod_volume_762to1km_5year,
#          prod_volume_1to2km_5year, prod_volume_2to3km_5year) %>%
#   filter(year == 2009) %>%
#   mutate(GEOID = str_sub(GEOID, -12, -1),
#          period = as.factor("2005_2009"),
#          prod_volume_2005_2009_0_1km =
#            (prod_volume_0to762m_5year + prod_volume_762to1km_5year),
#          prod_volume_2005_2009_1_2km = prod_volume_1to2km_5year,
#          prod_volume_2005_2009_2_3km = prod_volume_2to3km_5year) %>%
#   select(GEOID, period, prod_volume_2005_2009_0_1km,
#          prod_volume_2005_2009_1_2km, prod_volume_2005_2009_2_3km)
# saveRDS(acs_2005_2009_exp_prod_volume,
#         "data/interim/acs_2005_2009_exp_prod_volume_annuli.rds")
# 
# acs_2010_2014_exp_prod_volume <- acs_exposure_all_years %>%
#   select(GEOID, year, prod_volume_0to762m_5year, prod_volume_762to1km_5year,
#          prod_volume_1to2km_5year, prod_volume_2to3km_5year) %>%
#   filter(year == 2014) %>%
#   mutate(GEOID = str_sub(GEOID, -12, -1),
#          period = as.factor("2010_2014"),
#          prod_volume_2010_2014_0_1km =
#            (prod_volume_0to762m_5year + prod_volume_762to1km_5year),
#          prod_volume_2010_2014_1_2km = prod_volume_1to2km_5year,
#          prod_volume_2010_2014_2_3km = prod_volume_2to3km_5year) %>%
#   select(GEOID, period, prod_volume_2010_2014_0_1km,
#          prod_volume_2010_2014_1_2km, prod_volume_2010_2014_2_3km)
# saveRDS(acs_2010_2014_exp_prod_volume,
#         "data/interim/acs_2010_2014_exp_prod_volume_annuli.rds")
# 
# acs_2015_2019_exp_prod_volume <- acs_exposure_all_years %>%
#   select(GEOID, year, prod_volume_0to762m_5year, prod_volume_762to1km_5year,
#          prod_volume_1to2km_5year, prod_volume_2to3km_5year) %>%
#   filter(year == 2019) %>%
#   mutate(GEOID = str_sub(GEOID, -12, -1),
#          period = as.factor("2015_2019"),
#          prod_volume_2015_2019_0_1km =
#            (prod_volume_0to762m_5year + prod_volume_762to1km_5year),
#          prod_volume_2015_2019_1_2km = prod_volume_1to2km_5year,
#          prod_volume_2015_2019_2_3km = prod_volume_2to3km_5year) %>%
#   select(GEOID, period, prod_volume_2015_2019_0_1km,
#          prod_volume_2015_2019_1_2km, prod_volume_2015_2019_2_3km)
# saveRDS(acs_2015_2019_exp_prod_volume,
#         "data/interim/acs_2015_2019_exp_prod_volume_annuli.rds")

##============================================================================##
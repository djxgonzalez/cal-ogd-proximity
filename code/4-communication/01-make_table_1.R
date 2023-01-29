##============================================================================##
## makes Table 1 - sociodemographic characteristics from each five-year ACS

##---------------------------------------------------------------------------
## sets up environment

# data input
acs_exposure_2005_2019 <- readRDS("data/processed/acs_exposure_all_years.rds")

# data prep
acs_exposure_2005_2009 <- acs_exposure_2005_2019 %>%
  filter(period == "2005_2009")
acs_exposure_2010_2014 <- acs_exposure_2005_2019 %>%
  filter(period == "2010_2014")
acs_exposure_2015_2019 <- acs_exposure_2005_2019 %>%
  filter(period == "2015_2019")


##---------------------------------------------------------------------------
## assembles tables

# 2005 to 2009 ...........................................................
table_1_2005_2009 <- 
  tibble(
    period                 = "2005_2009",
    pop_total              = sum(acs_exposure_2005_2009$pop_total),
    
    # race/ethnicity
    pop_hispanic           = sum(acs_exposure_2005_2009$pop_hispanic),
    perc_hispanic          = round((sum(acs_exposure_2005_2009$pop_hispanic) /
                                      sum(acs_exposure_2005_2009$pop_total)) * 100, 1),
    pop_nonhisp_am_indian  = sum(acs_exposure_2005_2009$pop_nonhisp_am_indian),
    perc_nonhisp_am_indian = round((sum(acs_exposure_2005_2009$pop_nonhisp_am_indian) /
                                      sum(acs_exposure_2005_2009$pop_total)) * 100, 1),
    pop_nonhisp_asian      = sum(acs_exposure_2005_2009$pop_nonhisp_asian),
    perc_nonhisp_asian     = round((sum(acs_exposure_2005_2009$pop_nonhisp_asian) /
                                      sum(acs_exposure_2005_2009$pop_total)) * 100, 1),
    pop_nonhisp_black      = sum(acs_exposure_2005_2009$pop_nonhisp_black),
    perc_nonhisp_black     = round((sum(acs_exposure_2005_2009$pop_nonhisp_black) /
                                      sum(acs_exposure_2005_2009$pop_total)) * 100, 1),
    pop_nonhisp_white      = sum(acs_exposure_2005_2009$pop_nonhisp_white),
    perc_nonhisp_white     = round((sum(acs_exposure_2005_2009$pop_nonhisp_white) /
                                      sum(acs_exposure_2005_2009$pop_total)) * 100, 1),
    pop_nonhisp_other      = sum(acs_exposure_2005_2009$pop_nonhisp_other),
    perc_nonhisp_other     = round((sum(acs_exposure_2005_2009$pop_nonhisp_other) /
                                      sum(acs_exposure_2005_2009$pop_total)) * 100, 1),
    pop_nonhisp_two_plus   = sum(acs_exposure_2005_2009$pop_nonhisp_two_or_more),
    perc_nonhisp_two_plus  = round((sum(acs_exposure_2005_2009$pop_nonhisp_two_or_more) /
                                      sum(acs_exposure_2005_2009$pop_total)) * 100, 1),
    
    # education
    pop_educ_less_than_hs  = sum(acs_exposure_2005_2009$pop_educ_less_than_hs),
    perc_educ_less_than_hs = round((sum(acs_exposure_2005_2009$pop_educ_less_than_hs) /
                                      sum(acs_exposure_2005_2009$pop_educ_universe)) * 100, 1),
    pop_educ_hs  = sum(acs_exposure_2005_2009$pop_educ_hs),
    perc_educ_hs = round((sum(acs_exposure_2005_2009$pop_educ_hs) /
                            sum(acs_exposure_2005_2009$pop_educ_universe)) * 100, 1),
    pop_educ_more_than_hs  = sum(acs_exposure_2005_2009$pop_educ_more_than_hs),
    perc_educ_more_than_hs = round((sum(acs_exposure_2005_2009$pop_educ_more_than_hs) /
                                      sum(acs_exposure_2005_2009$pop_educ_universe)) * 100, 1),
    
    # poverty
    pop_poverty  = sum(acs_exposure_2005_2009$pop_below_poverty),
    perc_poverty = round((sum(acs_exposure_2005_2009$pop_below_poverty) /
                            sum(acs_exposure_2005_2009$pop_poverty_universe)) * 100, 1)
    ##### TO DO: add renters and linguistically isolated
  )


# 2010 to 2014 ...........................................................
table_1_2010_2014 <- 
  tibble(
    period                 = "2010_2014",
    pop_total              = sum(acs_exposure_2010_2014$pop_total),
    
    # race/ethnicity
    pop_hispanic           = sum(acs_exposure_2010_2014$pop_hispanic),
    perc_hispanic          = round((sum(acs_exposure_2010_2014$pop_hispanic) /
                                      sum(acs_exposure_2010_2014$pop_total)) * 100, 1),
    pop_nonhisp_am_indian  = sum(acs_exposure_2010_2014$pop_nonhisp_am_indian),
    perc_nonhisp_am_indian = round((sum(acs_exposure_2010_2014$pop_nonhisp_am_indian) /
                                      sum(acs_exposure_2010_2014$pop_total)) * 100, 1),
    pop_nonhisp_asian      = sum(acs_exposure_2010_2014$pop_nonhisp_asian),
    perc_nonhisp_asian     = round((sum(acs_exposure_2010_2014$pop_nonhisp_asian) /
                                      sum(acs_exposure_2010_2014$pop_total)) * 100, 1),
    pop_nonhisp_black      = sum(acs_exposure_2010_2014$pop_nonhisp_black),
    perc_nonhisp_black     = round((sum(acs_exposure_2010_2014$pop_nonhisp_black) /
                                      sum(acs_exposure_2010_2014$pop_total)) * 100, 1),
    pop_nonhisp_white      = sum(acs_exposure_2010_2014$pop_nonhisp_white),
    perc_nonhisp_white     = round((sum(acs_exposure_2010_2014$pop_nonhisp_white) /
                                      sum(acs_exposure_2010_2014$pop_total)) * 100, 1),
    pop_nonhisp_other      = sum(acs_exposure_2010_2014$pop_nonhisp_other),
    perc_nonhisp_other     = round((sum(acs_exposure_2010_2014$pop_nonhisp_other) /
                                      sum(acs_exposure_2010_2014$pop_total)) * 100, 1),
    pop_nonhisp_two_plus   = sum(acs_exposure_2010_2014$pop_nonhisp_two_or_more),
    perc_nonhisp_two_plus  = round((sum(acs_exposure_2010_2014$pop_nonhisp_two_or_more) /
                                      sum(acs_exposure_2010_2014$pop_total)) * 100, 1),
    
    # education
    pop_educ_less_than_hs  = sum(acs_exposure_2010_2014$pop_educ_less_than_hs),
    perc_educ_less_than_hs = round((sum(acs_exposure_2010_2014$pop_educ_less_than_hs) /
                                      sum(acs_exposure_2010_2014$pop_educ_universe)) * 100, 1),
    pop_educ_hs  = sum(acs_exposure_2010_2014$pop_educ_hs),
    perc_educ_hs = round((sum(acs_exposure_2010_2014$pop_educ_hs) /
                            sum(acs_exposure_2010_2014$pop_educ_universe)) * 100, 1),
    pop_educ_more_than_hs  = sum(acs_exposure_2010_2014$pop_educ_more_than_hs),
    perc_educ_more_than_hs = round((sum(acs_exposure_2010_2014$pop_educ_more_than_hs) /
                                      sum(acs_exposure_2010_2014$pop_educ_universe)) * 100, 1),
    
    # poverty
    pop_poverty  = sum(acs_exposure_2010_2014$pop_below_poverty),
    perc_poverty = round((sum(acs_exposure_2010_2014$pop_below_poverty) /
                            sum(acs_exposure_2010_2014$pop_poverty_universe)) * 100, 1)
    ##### TO DO: add renters and linguistically isolated
  )


# 2015 to 2019 ...........................................................
table_1_2015_2019 <- 
  tibble(
    period                 = "2015_2019",
    pop_total              = sum(acs_exposure_2015_2019$pop_total),
    
    # race/ethnicity
    pop_hispanic           = sum(acs_exposure_2015_2019$pop_hispanic),
    perc_hispanic          = round((sum(acs_exposure_2015_2019$pop_hispanic) /
                                      sum(acs_exposure_2015_2019$pop_total)) * 100, 1),
    pop_nonhisp_am_indian  = sum(acs_exposure_2015_2019$pop_nonhisp_am_indian),
    perc_nonhisp_am_indian = round((sum(acs_exposure_2015_2019$pop_nonhisp_am_indian) /
                                      sum(acs_exposure_2015_2019$pop_total)) * 100, 1),
    pop_nonhisp_asian      = sum(acs_exposure_2015_2019$pop_nonhisp_asian),
    perc_nonhisp_asian     = round((sum(acs_exposure_2015_2019$pop_nonhisp_asian) /
                                      sum(acs_exposure_2015_2019$pop_total)) * 100, 1),
    pop_nonhisp_black      = sum(acs_exposure_2015_2019$pop_nonhisp_black),
    perc_nonhisp_black     = round((sum(acs_exposure_2015_2019$pop_nonhisp_black) /
                                      sum(acs_exposure_2015_2019$pop_total)) * 100, 1),
    pop_nonhisp_white      = sum(acs_exposure_2015_2019$pop_nonhisp_white),
    perc_nonhisp_white     = round((sum(acs_exposure_2015_2019$pop_nonhisp_white) /
                                      sum(acs_exposure_2015_2019$pop_total)) * 100, 1),
    pop_nonhisp_other      = sum(acs_exposure_2015_2019$pop_nonhisp_other),
    perc_nonhisp_other     = round((sum(acs_exposure_2015_2019$pop_nonhisp_other) /
                                      sum(acs_exposure_2015_2019$pop_total)) * 100, 1),
    pop_nonhisp_two_plus   = sum(acs_exposure_2015_2019$pop_nonhisp_two_or_more),
    perc_nonhisp_two_plus  = round((sum(acs_exposure_2015_2019$pop_nonhisp_two_or_more) /
                                      sum(acs_exposure_2015_2019$pop_total)) * 100, 1),
    
    # education
    pop_educ_less_than_hs  = sum(acs_exposure_2015_2019$pop_educ_less_than_hs),
    perc_educ_less_than_hs = round((sum(acs_exposure_2015_2019$pop_educ_less_than_hs) /
                                      sum(acs_exposure_2015_2019$pop_educ_universe)) * 100, 1),
    pop_educ_hs  = sum(acs_exposure_2015_2019$pop_educ_hs),
    perc_educ_hs = round((sum(acs_exposure_2015_2019$pop_educ_hs) /
                            sum(acs_exposure_2015_2019$pop_educ_universe)) * 100, 1),
    pop_educ_more_than_hs  = sum(acs_exposure_2015_2019$pop_educ_more_than_hs),
    perc_educ_more_than_hs = round((sum(acs_exposure_2015_2019$pop_educ_more_than_hs) /
                                      sum(acs_exposure_2015_2019$pop_educ_universe)) * 100, 1),
    
    # poverty
    pop_poverty  = sum(acs_exposure_2015_2019$pop_below_poverty),
    perc_poverty = round((sum(acs_exposure_2015_2019$pop_below_poverty) /
                            sum(acs_exposure_2015_2019$pop_poverty_universe)) * 100, 1)
    ##### TO DO: add renters and linguistically isolated
  )


##---------------------------------------------------------------------------
## binds table components

table_1 <- table_1_2005_2009 %>%
  bind_rows(table_1_2010_2014) %>%
  bind_rows(table_1_2015_2019)

##--------------------------------------------------------------------------
## exports table
write_csv(table_1, file = "output/tables/table_1.csv")

##============================================================================##
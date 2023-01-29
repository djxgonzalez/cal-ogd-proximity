##============================================================================##
## 3.03 - estimates with risk ratios for exposure to wells in
## preproduction, production, or postproduction for racial/ethnic and
## socioeconomic groups, stratified by time period for Orange County

#----------------------------------------------------------------------------
# setup

# imports and preps data necessary for this script
acs_exposure_2005_2019 <- readRDS("data/processed/acs_exposure_2005_2019.rds")

acs_exposure_2005_2009 <- acs_exposure_2005_2019 %>%
  filter(period == "2005_2009") %>%
  mutate(county =  substr(GEOID, 3, 5)) %>%  # extracts county ID from GEOID
  filter(county == "059")  # selects Orange county (FIPS code 059)
acs_exposure_2010_2014 <- acs_exposure_2005_2019 %>%
  filter(period == "2010_2014") %>%
  mutate(county =  substr(GEOID, 3, 5)) %>%  # extracts county ID from GEOID
  filter(county == "059")  # selects Orange county (FIPS code 059)
acs_exposure_2015_2019 <- acs_exposure_2005_2019 %>%
  filter(period == "2015_2019") %>%
  mutate(county =  substr(GEOID, 3, 5)) %>%  # extracts county ID from GEOID
  filter(county == "059")  # selects Orange county (FIPS code 059)


#----------------------------------------------------------------------------
# generates table with risk ratios for statewide population proportions
# for wells in *preproduction*

# 2005 to 2009 ...........................................................
table_rr_2005_2009_preprod <- 
  tibble(period              = "2005_2009",
         well_stage          = "preproduction",
         rr_hispanic         = 
           (sum(acs_exposure_2005_2009$pop_hispanic * 
                  acs_exposure_2005_2009$prop_exp_preprod_1km) /
              sum(acs_exposure_2005_2009$pop_total * 
                    acs_exposure_2005_2009$prop_exp_preprod_1km)) / 
           (sum(acs_exposure_2005_2009$pop_hispanic) /
              sum(acs_exposure_2005_2009$pop_total)),
         rr_nonhisp_am_indian =
           (sum(acs_exposure_2005_2009$pop_nonhisp_am_indian * 
                  acs_exposure_2005_2009$prop_exp_preprod_1km) /
              sum(acs_exposure_2005_2009$pop_total * 
                    acs_exposure_2005_2009$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2005_2009$pop_nonhisp_am_indian) /
              sum(acs_exposure_2005_2009$pop_total)),
         rr_nonhisp_asian    = 
           (sum(acs_exposure_2005_2009$pop_nonhisp_asian * 
                  acs_exposure_2005_2009$prop_exp_preprod_1km) /
              sum(acs_exposure_2005_2009$pop_total * 
                    acs_exposure_2005_2009$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2005_2009$pop_nonhisp_asian) /
              sum(acs_exposure_2005_2009$pop_total)),
         rr_nonhisp_black    = 
           (sum(acs_exposure_2005_2009$pop_nonhisp_black * 
                  acs_exposure_2005_2009$prop_exp_preprod_1km) /
              sum(acs_exposure_2005_2009$pop_total * 
                    acs_exposure_2005_2009$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2005_2009$pop_nonhisp_black) /
              sum(acs_exposure_2005_2009$pop_total)),
         rr_nonhisp_white    =
           (sum(acs_exposure_2005_2009$pop_nonhisp_white * 
                  acs_exposure_2005_2009$prop_exp_preprod_1km) /
              sum(acs_exposure_2005_2009$pop_total * 
                    acs_exposure_2005_2009$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2005_2009$pop_nonhisp_white) /
              sum(acs_exposure_2005_2009$pop_total)),
         rr_nonhisp_other    = 
           (sum(acs_exposure_2005_2009$pop_nonhisp_other * 
                  acs_exposure_2005_2009$prop_exp_preprod_1km) /
              sum(acs_exposure_2005_2009$pop_total * 
                    acs_exposure_2005_2009$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2005_2009$pop_nonhisp_other) /
              sum(acs_exposure_2005_2009$pop_total)),
         rr_nonhisp_two_plus = 
           (sum(acs_exposure_2005_2009$pop_nonhisp_two_or_more * 
                  acs_exposure_2005_2009$prop_exp_preprod_1km) /
              sum(acs_exposure_2005_2009$pop_total * 
                    acs_exposure_2005_2009$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2005_2009$pop_nonhisp_two_or_more) /
              sum(acs_exposure_2005_2009$pop_total)),
         
         # educational attainment
         rr_educ_less_than_hs = 
           (sum(acs_exposure_2005_2009$pop_educ_less_than_hs * 
                  acs_exposure_2005_2009$prop_exp_preprod_1km) /
              sum(acs_exposure_2005_2009$pop_educ_universe * 
                    acs_exposure_2005_2009$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2005_2009$pop_educ_less_than_hs) /
              sum(acs_exposure_2005_2009$pop_educ_universe)),
         rr_educ_hs           =
           (sum(acs_exposure_2005_2009$pop_educ_hs * 
                  acs_exposure_2005_2009$prop_exp_preprod_1km) /
              sum(acs_exposure_2005_2009$pop_educ_universe * 
                    acs_exposure_2005_2009$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2005_2009$pop_educ_hs) /
              sum(acs_exposure_2005_2009$pop_educ_universe)),
         rr_educ_more_than_hs = 
           (sum(acs_exposure_2005_2009$pop_educ_more_than_hs * 
                  acs_exposure_2005_2009$prop_exp_preprod_1km) /
              sum(acs_exposure_2005_2009$pop_educ_universe * 
                    acs_exposure_2005_2009$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2005_2009$pop_educ_more_than_hs) /
              sum(acs_exposure_2005_2009$pop_educ_universe)),
         
         # poverty
         rr_poverty           = 
           (sum(acs_exposure_2005_2009$pop_below_poverty * 
                  acs_exposure_2005_2009$prop_exp_preprod_1km) /
              sum(acs_exposure_2005_2009$pop_poverty_universe * 
                    acs_exposure_2005_2009$prop_exp_preprod_1km) /
              (sum(acs_exposure_2005_2009$pop_below_poverty) /
                 sum(acs_exposure_2005_2009$pop_poverty_universe))),
         
         # renter-occupied households
         rr_renters           = 
           (sum(acs_exposure_2005_2009$housing_occupied_renters * 
                  acs_exposure_2005_2009$prop_exp_preprod_1km) /
              sum(acs_exposure_2005_2009$housing_occupied_all * 
                    acs_exposure_2005_2009$prop_exp_preprod_1km) /
              (sum(acs_exposure_2005_2009$housing_occupied_renters) /
                 sum(acs_exposure_2005_2009$housing_occupied_all))),
         
         # linguistically isolated people
         rr_ling_isolated     = 
           (sum(acs_exposure_2005_2009$pop_linguistically_isolated_over18 * 
                  acs_exposure_2005_2009$prop_exp_preprod_1km) /
              sum(acs_exposure_2005_2009$pop_total_over18 * 
                    acs_exposure_2005_2009$prop_exp_preprod_1km) /
              (sum(acs_exposure_2005_2009$pop_linguistically_isolated_over18) /
                 sum(acs_exposure_2005_2009$pop_total_over18))),
         
         # voter turnout
         rr_nonvoters           = NA,  # no voting data for 2005-2009
  )

# 2010 to 2014 ...........................................................
table_rr_2010_2014_preprod <- 
  tibble(period              = "2010_2014",
         well_stage          = "preproduction",
         rr_hispanic         = 
           (sum(acs_exposure_2010_2014$pop_hispanic * 
                  acs_exposure_2010_2014$prop_exp_preprod_1km) /
              sum(acs_exposure_2010_2014$pop_total * 
                    acs_exposure_2010_2014$prop_exp_preprod_1km)) / 
           (sum(acs_exposure_2010_2014$pop_hispanic) /
              sum(acs_exposure_2010_2014$pop_total)),
         rr_nonhisp_am_indian =
           (sum(acs_exposure_2010_2014$pop_nonhisp_am_indian * 
                  acs_exposure_2010_2014$prop_exp_preprod_1km) /
              sum(acs_exposure_2010_2014$pop_total * 
                    acs_exposure_2010_2014$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2010_2014$pop_nonhisp_am_indian) /
              sum(acs_exposure_2010_2014$pop_total)),
         rr_nonhisp_asian    = 
           (sum(acs_exposure_2010_2014$pop_nonhisp_asian * 
                  acs_exposure_2010_2014$prop_exp_preprod_1km) /
              sum(acs_exposure_2010_2014$pop_total * 
                    acs_exposure_2010_2014$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2010_2014$pop_nonhisp_asian) /
              sum(acs_exposure_2010_2014$pop_total)),
         rr_nonhisp_black    = 
           (sum(acs_exposure_2010_2014$pop_nonhisp_black * 
                  acs_exposure_2010_2014$prop_exp_preprod_1km) /
              sum(acs_exposure_2010_2014$pop_total * 
                    acs_exposure_2010_2014$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2010_2014$pop_nonhisp_black) /
              sum(acs_exposure_2010_2014$pop_total)),
         rr_nonhisp_white    =
           (sum(acs_exposure_2010_2014$pop_nonhisp_white * 
                  acs_exposure_2010_2014$prop_exp_preprod_1km) /
              sum(acs_exposure_2010_2014$pop_total * 
                    acs_exposure_2010_2014$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2010_2014$pop_nonhisp_white) /
              sum(acs_exposure_2010_2014$pop_total)),
         rr_nonhisp_other    = 
           (sum(acs_exposure_2010_2014$pop_nonhisp_other * 
                  acs_exposure_2010_2014$prop_exp_preprod_1km) /
              sum(acs_exposure_2010_2014$pop_total * 
                    acs_exposure_2010_2014$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2010_2014$pop_nonhisp_other) /
              sum(acs_exposure_2010_2014$pop_total)),
         rr_nonhisp_two_plus = 
           (sum(acs_exposure_2010_2014$pop_nonhisp_two_or_more * 
                  acs_exposure_2010_2014$prop_exp_preprod_1km) /
              sum(acs_exposure_2010_2014$pop_total * 
                    acs_exposure_2010_2014$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2010_2014$pop_nonhisp_two_or_more) /
              sum(acs_exposure_2010_2014$pop_total)),
         
         # educational attainment
         rr_educ_less_than_hs = 
           (sum(acs_exposure_2010_2014$pop_educ_less_than_hs * 
                  acs_exposure_2010_2014$prop_exp_preprod_1km) /
              sum(acs_exposure_2010_2014$pop_educ_universe * 
                    acs_exposure_2010_2014$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2010_2014$pop_educ_less_than_hs) /
              sum(acs_exposure_2010_2014$pop_educ_universe)),
         rr_educ_hs           =
           (sum(acs_exposure_2010_2014$pop_educ_hs * 
                  acs_exposure_2010_2014$prop_exp_preprod_1km) /
              sum(acs_exposure_2010_2014$pop_educ_universe * 
                    acs_exposure_2010_2014$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2010_2014$pop_educ_hs) /
              sum(acs_exposure_2010_2014$pop_educ_universe)),
         rr_educ_more_than_hs = 
           (sum(acs_exposure_2010_2014$pop_educ_more_than_hs * 
                  acs_exposure_2010_2014$prop_exp_preprod_1km) /
              sum(acs_exposure_2010_2014$pop_educ_universe * 
                    acs_exposure_2010_2014$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2010_2014$pop_educ_more_than_hs) /
              sum(acs_exposure_2010_2014$pop_educ_universe)),
         
         # poverty
         rr_poverty           = 
           (sum(acs_exposure_2010_2014$pop_below_poverty * 
                  acs_exposure_2010_2014$prop_exp_preprod_1km) /
              sum(acs_exposure_2010_2014$pop_poverty_universe * 
                    acs_exposure_2010_2014$prop_exp_preprod_1km) /
              (sum(acs_exposure_2010_2014$pop_below_poverty) /
                 sum(acs_exposure_2010_2014$pop_poverty_universe))),
         
         # renter-occupied households
         rr_renters           = 
           (sum(acs_exposure_2010_2014$housing_occupied_renters * 
                  acs_exposure_2010_2014$prop_exp_preprod_1km) /
              sum(acs_exposure_2010_2014$housing_occupied_all * 
                    acs_exposure_2010_2014$prop_exp_preprod_1km) /
              (sum(acs_exposure_2010_2014$housing_occupied_renters) /
                 sum(acs_exposure_2010_2014$housing_occupied_all))),
         
         # linguistically isolated people
         rr_ling_isolated     = 
           (sum(acs_exposure_2010_2014$pop_linguistically_isolated_over18 * 
                  acs_exposure_2010_2014$prop_exp_preprod_1km) /
              sum(acs_exposure_2010_2014$pop_total_over18 * 
                    acs_exposure_2010_2014$prop_exp_preprod_1km) /
              (sum(acs_exposure_2010_2014$pop_linguistically_isolated_over18) /
                 sum(acs_exposure_2010_2014$pop_total_over18))),
         
         # voter turnout
         rr_nonvoters           = 
           (sum(acs_exposure_2010_2014$pop_nonvoters * 
                  acs_exposure_2010_2014$prop_exp_preprod_1km, na.rm = T) /
              sum(acs_exposure_2010_2014$pop_total_over18 * 
                    acs_exposure_2010_2014$prop_exp_preprod_1km) /
              (sum(acs_exposure_2010_2014$pop_nonvoters, na.rm = T) /
                 sum(acs_exposure_2010_2014$pop_total_over18)))
  )

# 2015 to 2019 ...........................................................
table_rr_2015_2019_preprod <- 
  tibble(period              = "2015_2019",
         well_stage          = "preproduction",
         rr_hispanic         = 
           (sum(acs_exposure_2015_2019$pop_hispanic * 
                  acs_exposure_2015_2019$prop_exp_preprod_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_preprod_1km)) / 
           (sum(acs_exposure_2015_2019$pop_hispanic) /
              sum(acs_exposure_2015_2019$pop_total)),
         rr_nonhisp_am_indian =
           (sum(acs_exposure_2015_2019$pop_nonhisp_am_indian * 
                  acs_exposure_2015_2019$prop_exp_preprod_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2015_2019$pop_nonhisp_am_indian) /
              sum(acs_exposure_2015_2019$pop_total)),
         rr_nonhisp_asian    = 
           (sum(acs_exposure_2015_2019$pop_nonhisp_asian * 
                  acs_exposure_2015_2019$prop_exp_preprod_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2015_2019$pop_nonhisp_asian) /
              sum(acs_exposure_2015_2019$pop_total)),
         rr_nonhisp_black    = 
           (sum(acs_exposure_2015_2019$pop_nonhisp_black * 
                  acs_exposure_2015_2019$prop_exp_preprod_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2015_2019$pop_nonhisp_black) /
              sum(acs_exposure_2015_2019$pop_total)),
         rr_nonhisp_white    =
           (sum(acs_exposure_2015_2019$pop_nonhisp_white * 
                  acs_exposure_2015_2019$prop_exp_preprod_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2015_2019$pop_nonhisp_white) /
              sum(acs_exposure_2015_2019$pop_total)),
         rr_nonhisp_other    = 
           (sum(acs_exposure_2015_2019$pop_nonhisp_other * 
                  acs_exposure_2015_2019$prop_exp_preprod_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2015_2019$pop_nonhisp_other) /
              sum(acs_exposure_2015_2019$pop_total)),
         rr_nonhisp_two_plus = 
           (sum(acs_exposure_2015_2019$pop_nonhisp_two_or_more * 
                  acs_exposure_2015_2019$prop_exp_preprod_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2015_2019$pop_nonhisp_two_or_more) /
              sum(acs_exposure_2015_2019$pop_total)),
         
         # educational attainment
         rr_educ_less_than_hs = 
           (sum(acs_exposure_2015_2019$pop_educ_less_than_hs * 
                  acs_exposure_2015_2019$prop_exp_preprod_1km) /
              sum(acs_exposure_2015_2019$pop_educ_universe * 
                    acs_exposure_2015_2019$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2015_2019$pop_educ_less_than_hs) /
              sum(acs_exposure_2015_2019$pop_educ_universe)),
         rr_educ_hs           =
           (sum(acs_exposure_2015_2019$pop_educ_hs * 
                  acs_exposure_2015_2019$prop_exp_preprod_1km) /
              sum(acs_exposure_2015_2019$pop_educ_universe * 
                    acs_exposure_2015_2019$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2015_2019$pop_educ_hs) /
              sum(acs_exposure_2015_2019$pop_educ_universe)),
         rr_educ_more_than_hs = 
           (sum(acs_exposure_2015_2019$pop_educ_more_than_hs * 
                  acs_exposure_2015_2019$prop_exp_preprod_1km) /
              sum(acs_exposure_2015_2019$pop_educ_universe * 
                    acs_exposure_2015_2019$prop_exp_preprod_1km)) /
           (sum(acs_exposure_2015_2019$pop_educ_more_than_hs) /
              sum(acs_exposure_2015_2019$pop_educ_universe)),
         
         # poverty
         rr_poverty           = 
           (sum(acs_exposure_2015_2019$pop_below_poverty * 
                  acs_exposure_2015_2019$prop_exp_preprod_1km) /
              sum(acs_exposure_2015_2019$pop_poverty_universe * 
                    acs_exposure_2015_2019$prop_exp_preprod_1km) /
              (sum(acs_exposure_2015_2019$pop_below_poverty) /
                 sum(acs_exposure_2015_2019$pop_poverty_universe))),
         
         # renter-occupied households
         rr_renters           = 
           (sum(acs_exposure_2015_2019$housing_occupied_renters * 
                  acs_exposure_2015_2019$prop_exp_preprod_1km) /
              sum(acs_exposure_2015_2019$housing_occupied_all * 
                    acs_exposure_2015_2019$prop_exp_preprod_1km) /
              (sum(acs_exposure_2015_2019$housing_occupied_renters) /
                 sum(acs_exposure_2015_2019$housing_occupied_all))),
         
         # linguistically isolated people
         rr_ling_isolated     = 
           (sum(acs_exposure_2015_2019$pop_linguistically_isolated_over18 * 
                  acs_exposure_2015_2019$prop_exp_preprod_1km) /
              sum(acs_exposure_2015_2019$pop_total_over18 * 
                    acs_exposure_2015_2019$prop_exp_preprod_1km) /
              (sum(acs_exposure_2015_2019$pop_linguistically_isolated_over18) /
                 sum(acs_exposure_2015_2019$pop_total_over18))),
         
         # voter turnout
         rr_nonvoters           = 
           (sum(acs_exposure_2015_2019$pop_nonvoters * 
                  acs_exposure_2015_2019$prop_exp_preprod_1km, na.rm = T) /
              sum(acs_exposure_2015_2019$pop_total_over18 * 
                    acs_exposure_2015_2019$prop_exp_preprod_1km) /
              (sum(acs_exposure_2015_2019$pop_nonvoters, na.rm = T) /
                 sum(acs_exposure_2015_2019$pop_total_over18)))
  )



#----------------------------------------------------------------------------
# generates table with risk ratios for statewide population proportions
# for wells in *production*

# 2005 to 2009 ...........................................................
table_rr_2005_2009_prod <- 
  tibble(period              = "2005_2009",
         well_stage          = "production",
         rr_hispanic         = 
           (sum(acs_exposure_2005_2009$pop_hispanic * 
                  acs_exposure_2005_2009$prop_exp_prod_1km) /
              sum(acs_exposure_2005_2009$pop_total * 
                    acs_exposure_2005_2009$prop_exp_prod_1km)) / 
           (sum(acs_exposure_2005_2009$pop_hispanic) /
              sum(acs_exposure_2005_2009$pop_total)),
         rr_nonhisp_am_indian =
           (sum(acs_exposure_2005_2009$pop_nonhisp_am_indian * 
                  acs_exposure_2005_2009$prop_exp_prod_1km) /
              sum(acs_exposure_2005_2009$pop_total * 
                    acs_exposure_2005_2009$prop_exp_prod_1km)) /
           (sum(acs_exposure_2005_2009$pop_nonhisp_am_indian) /
              sum(acs_exposure_2005_2009$pop_total)),
         rr_nonhisp_asian    = 
           (sum(acs_exposure_2005_2009$pop_nonhisp_asian * 
                  acs_exposure_2005_2009$prop_exp_prod_1km) /
              sum(acs_exposure_2005_2009$pop_total * 
                    acs_exposure_2005_2009$prop_exp_prod_1km)) /
           (sum(acs_exposure_2005_2009$pop_nonhisp_asian) /
              sum(acs_exposure_2005_2009$pop_total)),
         rr_nonhisp_black    = 
           (sum(acs_exposure_2005_2009$pop_nonhisp_black * 
                  acs_exposure_2005_2009$prop_exp_prod_1km) /
              sum(acs_exposure_2005_2009$pop_total * 
                    acs_exposure_2005_2009$prop_exp_prod_1km)) /
           (sum(acs_exposure_2005_2009$pop_nonhisp_black) /
              sum(acs_exposure_2005_2009$pop_total)),
         rr_nonhisp_white    =
           (sum(acs_exposure_2005_2009$pop_nonhisp_white * 
                  acs_exposure_2005_2009$prop_exp_prod_1km) /
              sum(acs_exposure_2005_2009$pop_total * 
                    acs_exposure_2005_2009$prop_exp_prod_1km)) /
           (sum(acs_exposure_2005_2009$pop_nonhisp_white) /
              sum(acs_exposure_2005_2009$pop_total)),
         rr_nonhisp_other    = 
           (sum(acs_exposure_2005_2009$pop_nonhisp_other * 
                  acs_exposure_2005_2009$prop_exp_prod_1km) /
              sum(acs_exposure_2005_2009$pop_total * 
                    acs_exposure_2005_2009$prop_exp_prod_1km)) /
           (sum(acs_exposure_2005_2009$pop_nonhisp_other) /
              sum(acs_exposure_2005_2009$pop_total)),
         rr_nonhisp_two_plus = 
           (sum(acs_exposure_2005_2009$pop_nonhisp_two_or_more * 
                  acs_exposure_2005_2009$prop_exp_prod_1km) /
              sum(acs_exposure_2005_2009$pop_total * 
                    acs_exposure_2005_2009$prop_exp_prod_1km)) /
           (sum(acs_exposure_2005_2009$pop_nonhisp_two_or_more) /
              sum(acs_exposure_2005_2009$pop_total)),
         
         # educational attainment
         rr_educ_less_than_hs = 
           (sum(acs_exposure_2005_2009$pop_educ_less_than_hs * 
                  acs_exposure_2005_2009$prop_exp_prod_1km) /
              sum(acs_exposure_2005_2009$pop_educ_universe * 
                    acs_exposure_2005_2009$prop_exp_prod_1km)) /
           (sum(acs_exposure_2005_2009$pop_educ_less_than_hs) /
              sum(acs_exposure_2005_2009$pop_educ_universe)),
         rr_educ_hs           =
           (sum(acs_exposure_2005_2009$pop_educ_hs * 
                  acs_exposure_2005_2009$prop_exp_prod_1km) /
              sum(acs_exposure_2005_2009$pop_educ_universe * 
                    acs_exposure_2005_2009$prop_exp_prod_1km)) /
           (sum(acs_exposure_2005_2009$pop_educ_hs) /
              sum(acs_exposure_2005_2009$pop_educ_universe)),
         rr_educ_more_than_hs = 
           (sum(acs_exposure_2005_2009$pop_educ_more_than_hs * 
                  acs_exposure_2005_2009$prop_exp_prod_1km) /
              sum(acs_exposure_2005_2009$pop_educ_universe * 
                    acs_exposure_2005_2009$prop_exp_prod_1km)) /
           (sum(acs_exposure_2005_2009$pop_educ_more_than_hs) /
              sum(acs_exposure_2005_2009$pop_educ_universe)),
         
         # poverty
         rr_poverty           = 
           (sum(acs_exposure_2005_2009$pop_below_poverty * 
                  acs_exposure_2005_2009$prop_exp_prod_1km) /
              sum(acs_exposure_2005_2009$pop_poverty_universe * 
                    acs_exposure_2005_2009$prop_exp_prod_1km) /
              (sum(acs_exposure_2005_2009$pop_below_poverty) /
                 sum(acs_exposure_2005_2009$pop_poverty_universe))),
         
         # renter-occupied households
         rr_renters           = 
           (sum(acs_exposure_2005_2009$housing_occupied_renters * 
                  acs_exposure_2005_2009$prop_exp_prod_1km) /
              sum(acs_exposure_2005_2009$housing_occupied_all * 
                    acs_exposure_2005_2009$prop_exp_prod_1km) /
              (sum(acs_exposure_2005_2009$housing_occupied_renters) /
                 sum(acs_exposure_2005_2009$housing_occupied_all))),
         
         # linguistically isolated people
         rr_ling_isolated     = 
           (sum(acs_exposure_2005_2009$pop_linguistically_isolated_over18 * 
                  acs_exposure_2005_2009$prop_exp_prod_1km) /
              sum(acs_exposure_2005_2009$pop_total_over18 * 
                    acs_exposure_2005_2009$prop_exp_prod_1km) /
              (sum(acs_exposure_2005_2009$pop_linguistically_isolated_over18) /
                 sum(acs_exposure_2005_2009$pop_total_over18))),
         
         # voter turnout
         rr_nonvoters           = NA,  # no voting data for 2005-2009
  )

# 2010 to 2014 ...........................................................
table_rr_2010_2014_prod <- 
  tibble(period              = "2010_2014",
         well_stage          = "production",
         rr_hispanic         = 
           (sum(acs_exposure_2010_2014$pop_hispanic * 
                  acs_exposure_2010_2014$prop_exp_prod_1km) /
              sum(acs_exposure_2010_2014$pop_total * 
                    acs_exposure_2010_2014$prop_exp_prod_1km)) / 
           (sum(acs_exposure_2010_2014$pop_hispanic) /
              sum(acs_exposure_2010_2014$pop_total)),
         rr_nonhisp_am_indian =
           (sum(acs_exposure_2010_2014$pop_nonhisp_am_indian * 
                  acs_exposure_2010_2014$prop_exp_prod_1km) /
              sum(acs_exposure_2010_2014$pop_total * 
                    acs_exposure_2010_2014$prop_exp_prod_1km)) /
           (sum(acs_exposure_2010_2014$pop_nonhisp_am_indian) /
              sum(acs_exposure_2010_2014$pop_total)),
         rr_nonhisp_asian    = 
           (sum(acs_exposure_2010_2014$pop_nonhisp_asian * 
                  acs_exposure_2010_2014$prop_exp_prod_1km) /
              sum(acs_exposure_2010_2014$pop_total * 
                    acs_exposure_2010_2014$prop_exp_prod_1km)) /
           (sum(acs_exposure_2010_2014$pop_nonhisp_asian) /
              sum(acs_exposure_2010_2014$pop_total)),
         rr_nonhisp_black    = 
           (sum(acs_exposure_2010_2014$pop_nonhisp_black * 
                  acs_exposure_2010_2014$prop_exp_prod_1km) /
              sum(acs_exposure_2010_2014$pop_total * 
                    acs_exposure_2010_2014$prop_exp_prod_1km)) /
           (sum(acs_exposure_2010_2014$pop_nonhisp_black) /
              sum(acs_exposure_2010_2014$pop_total)),
         rr_nonhisp_white    =
           (sum(acs_exposure_2010_2014$pop_nonhisp_white * 
                  acs_exposure_2010_2014$prop_exp_prod_1km) /
              sum(acs_exposure_2010_2014$pop_total * 
                    acs_exposure_2010_2014$prop_exp_prod_1km)) /
           (sum(acs_exposure_2010_2014$pop_nonhisp_white) /
              sum(acs_exposure_2010_2014$pop_total)),
         rr_nonhisp_other    = 
           (sum(acs_exposure_2010_2014$pop_nonhisp_other * 
                  acs_exposure_2010_2014$prop_exp_prod_1km) /
              sum(acs_exposure_2010_2014$pop_total * 
                    acs_exposure_2010_2014$prop_exp_prod_1km)) /
           (sum(acs_exposure_2010_2014$pop_nonhisp_other) /
              sum(acs_exposure_2010_2014$pop_total)),
         rr_nonhisp_two_plus = 
           (sum(acs_exposure_2010_2014$pop_nonhisp_two_or_more * 
                  acs_exposure_2010_2014$prop_exp_prod_1km) /
              sum(acs_exposure_2010_2014$pop_total * 
                    acs_exposure_2010_2014$prop_exp_prod_1km)) /
           (sum(acs_exposure_2010_2014$pop_nonhisp_two_or_more) /
              sum(acs_exposure_2010_2014$pop_total)),
         
         # educational attainment
         rr_educ_less_than_hs = 
           (sum(acs_exposure_2010_2014$pop_educ_less_than_hs * 
                  acs_exposure_2010_2014$prop_exp_prod_1km) /
              sum(acs_exposure_2010_2014$pop_educ_universe * 
                    acs_exposure_2010_2014$prop_exp_prod_1km)) /
           (sum(acs_exposure_2010_2014$pop_educ_less_than_hs) /
              sum(acs_exposure_2010_2014$pop_educ_universe)),
         rr_educ_hs           =
           (sum(acs_exposure_2010_2014$pop_educ_hs * 
                  acs_exposure_2010_2014$prop_exp_prod_1km) /
              sum(acs_exposure_2010_2014$pop_educ_universe * 
                    acs_exposure_2010_2014$prop_exp_prod_1km)) /
           (sum(acs_exposure_2010_2014$pop_educ_hs) /
              sum(acs_exposure_2010_2014$pop_educ_universe)),
         rr_educ_more_than_hs = 
           (sum(acs_exposure_2010_2014$pop_educ_more_than_hs * 
                  acs_exposure_2010_2014$prop_exp_prod_1km) /
              sum(acs_exposure_2010_2014$pop_educ_universe * 
                    acs_exposure_2010_2014$prop_exp_prod_1km)) /
           (sum(acs_exposure_2010_2014$pop_educ_more_than_hs) /
              sum(acs_exposure_2010_2014$pop_educ_universe)),
         
         # poverty
         rr_poverty           = 
           (sum(acs_exposure_2010_2014$pop_below_poverty * 
                  acs_exposure_2010_2014$prop_exp_prod_1km) /
              sum(acs_exposure_2010_2014$pop_poverty_universe * 
                    acs_exposure_2010_2014$prop_exp_prod_1km) /
              (sum(acs_exposure_2010_2014$pop_below_poverty) /
                 sum(acs_exposure_2010_2014$pop_poverty_universe))),
         
         # renter-occupied households
         rr_renters           = 
           (sum(acs_exposure_2010_2014$housing_occupied_renters * 
                  acs_exposure_2010_2014$prop_exp_prod_1km) /
              sum(acs_exposure_2010_2014$housing_occupied_all * 
                    acs_exposure_2010_2014$prop_exp_prod_1km) /
              (sum(acs_exposure_2010_2014$housing_occupied_renters) /
                 sum(acs_exposure_2010_2014$housing_occupied_all))),
         
         # linguistically isolated people
         rr_ling_isolated     = 
           (sum(acs_exposure_2010_2014$pop_linguistically_isolated_over18 * 
                  acs_exposure_2010_2014$prop_exp_prod_1km) /
              sum(acs_exposure_2010_2014$pop_total_over18 * 
                    acs_exposure_2010_2014$prop_exp_prod_1km) /
              (sum(acs_exposure_2010_2014$pop_linguistically_isolated_over18) /
                 sum(acs_exposure_2010_2014$pop_total_over18))),
         
         # voter turnout
         rr_nonvoters           = 
           (sum(acs_exposure_2010_2014$pop_nonvoters * 
                  acs_exposure_2010_2014$prop_exp_prod_1km, na.rm = T) /
              sum(acs_exposure_2010_2014$pop_total_over18 * 
                    acs_exposure_2010_2014$prop_exp_prod_1km) /
              (sum(acs_exposure_2010_2014$pop_nonvoters, na.rm = T) /
                 sum(acs_exposure_2010_2014$pop_total_over18)))
  )

# 2015 to 2019 ...........................................................
table_rr_2015_2019_prod <- 
  tibble(period              = "2015_2019",
         well_stage          = "production",
         rr_hispanic         = 
           (sum(acs_exposure_2015_2019$pop_hispanic * 
                  acs_exposure_2015_2019$prop_exp_prod_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_prod_1km)) / 
           (sum(acs_exposure_2015_2019$pop_hispanic) /
              sum(acs_exposure_2015_2019$pop_total)),
         rr_nonhisp_am_indian =
           (sum(acs_exposure_2015_2019$pop_nonhisp_am_indian * 
                  acs_exposure_2015_2019$prop_exp_prod_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_prod_1km)) /
           (sum(acs_exposure_2015_2019$pop_nonhisp_am_indian) /
              sum(acs_exposure_2015_2019$pop_total)),
         rr_nonhisp_asian    = 
           (sum(acs_exposure_2015_2019$pop_nonhisp_asian * 
                  acs_exposure_2015_2019$prop_exp_prod_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_prod_1km)) /
           (sum(acs_exposure_2015_2019$pop_nonhisp_asian) /
              sum(acs_exposure_2015_2019$pop_total)),
         rr_nonhisp_black    = 
           (sum(acs_exposure_2015_2019$pop_nonhisp_black * 
                  acs_exposure_2015_2019$prop_exp_prod_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_prod_1km)) /
           (sum(acs_exposure_2015_2019$pop_nonhisp_black) /
              sum(acs_exposure_2015_2019$pop_total)),
         rr_nonhisp_white    =
           (sum(acs_exposure_2015_2019$pop_nonhisp_white * 
                  acs_exposure_2015_2019$prop_exp_prod_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_prod_1km)) /
           (sum(acs_exposure_2015_2019$pop_nonhisp_white) /
              sum(acs_exposure_2015_2019$pop_total)),
         rr_nonhisp_other    = 
           (sum(acs_exposure_2015_2019$pop_nonhisp_other * 
                  acs_exposure_2015_2019$prop_exp_prod_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_prod_1km)) /
           (sum(acs_exposure_2015_2019$pop_nonhisp_other) /
              sum(acs_exposure_2015_2019$pop_total)),
         rr_nonhisp_two_plus = 
           (sum(acs_exposure_2015_2019$pop_nonhisp_two_or_more * 
                  acs_exposure_2015_2019$prop_exp_prod_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_prod_1km)) /
           (sum(acs_exposure_2015_2019$pop_nonhisp_two_or_more) /
              sum(acs_exposure_2015_2019$pop_total)),
         
         # educational attainment
         rr_educ_less_than_hs = 
           (sum(acs_exposure_2015_2019$pop_educ_less_than_hs * 
                  acs_exposure_2015_2019$prop_exp_prod_1km) /
              sum(acs_exposure_2015_2019$pop_educ_universe * 
                    acs_exposure_2015_2019$prop_exp_prod_1km)) /
           (sum(acs_exposure_2015_2019$pop_educ_less_than_hs) /
              sum(acs_exposure_2015_2019$pop_educ_universe)),
         rr_educ_hs           =
           (sum(acs_exposure_2015_2019$pop_educ_hs * 
                  acs_exposure_2015_2019$prop_exp_prod_1km) /
              sum(acs_exposure_2015_2019$pop_educ_universe * 
                    acs_exposure_2015_2019$prop_exp_prod_1km)) /
           (sum(acs_exposure_2015_2019$pop_educ_hs) /
              sum(acs_exposure_2015_2019$pop_educ_universe)),
         rr_educ_more_than_hs = 
           (sum(acs_exposure_2015_2019$pop_educ_more_than_hs * 
                  acs_exposure_2015_2019$prop_exp_prod_1km) /
              sum(acs_exposure_2015_2019$pop_educ_universe * 
                    acs_exposure_2015_2019$prop_exp_prod_1km)) /
           (sum(acs_exposure_2015_2019$pop_educ_more_than_hs) /
              sum(acs_exposure_2015_2019$pop_educ_universe)),
         
         # poverty
         rr_poverty           = 
           (sum(acs_exposure_2015_2019$pop_below_poverty * 
                  acs_exposure_2015_2019$prop_exp_prod_1km) /
              sum(acs_exposure_2015_2019$pop_poverty_universe * 
                    acs_exposure_2015_2019$prop_exp_prod_1km) /
              (sum(acs_exposure_2015_2019$pop_below_poverty) /
                 sum(acs_exposure_2015_2019$pop_poverty_universe))),
         
         # renter-occupied households
         rr_renters           = 
           (sum(acs_exposure_2015_2019$housing_occupied_renters * 
                  acs_exposure_2015_2019$prop_exp_prod_1km) /
              sum(acs_exposure_2015_2019$housing_occupied_all * 
                    acs_exposure_2015_2019$prop_exp_prod_1km) /
              (sum(acs_exposure_2015_2019$housing_occupied_renters) /
                 sum(acs_exposure_2015_2019$housing_occupied_all))),
         
         # linguistically isolated people
         rr_ling_isolated     = 
           (sum(acs_exposure_2015_2019$pop_linguistically_isolated_over18 * 
                  acs_exposure_2015_2019$prop_exp_prod_1km) /
              sum(acs_exposure_2015_2019$pop_total_over18 * 
                    acs_exposure_2015_2019$prop_exp_prod_1km) /
              (sum(acs_exposure_2015_2019$pop_linguistically_isolated_over18) /
                 sum(acs_exposure_2015_2019$pop_total_over18))),
         
         # voter turnout
         rr_nonvoters           = 
           (sum(acs_exposure_2015_2019$pop_nonvoters * 
                  acs_exposure_2015_2019$prop_exp_prod_1km, na.rm = T) /
              sum(acs_exposure_2015_2019$pop_total_over18 * 
                    acs_exposure_2015_2019$prop_exp_prod_1km) /
              (sum(acs_exposure_2015_2019$pop_nonvoters, na.rm = T) /
                 sum(acs_exposure_2015_2019$pop_total_over18)))
  )


#----------------------------------------------------------------------------
# generates table with risk ratios for statewide population proportions
# for wells in *postproduction*

# 2005 to 2009 ...........................................................
table_rr_2005_2009_postprod <- 
  tibble(period              = "2005_2009",
         well_stage          = "postproduction",
         rr_hispanic         = 
           (sum(acs_exposure_2005_2009$pop_hispanic * 
                  acs_exposure_2005_2009$prop_exp_postprod_1km) /
              sum(acs_exposure_2005_2009$pop_total * 
                    acs_exposure_2005_2009$prop_exp_postprod_1km)) / 
           (sum(acs_exposure_2005_2009$pop_hispanic) /
              sum(acs_exposure_2005_2009$pop_total)),
         rr_nonhisp_am_indian =
           (sum(acs_exposure_2005_2009$pop_nonhisp_am_indian * 
                  acs_exposure_2005_2009$prop_exp_postprod_1km) /
              sum(acs_exposure_2005_2009$pop_total * 
                    acs_exposure_2005_2009$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2005_2009$pop_nonhisp_am_indian) /
              sum(acs_exposure_2005_2009$pop_total)),
         rr_nonhisp_asian    = 
           (sum(acs_exposure_2005_2009$pop_nonhisp_asian * 
                  acs_exposure_2005_2009$prop_exp_postprod_1km) /
              sum(acs_exposure_2005_2009$pop_total * 
                    acs_exposure_2005_2009$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2005_2009$pop_nonhisp_asian) /
              sum(acs_exposure_2005_2009$pop_total)),
         rr_nonhisp_black    = 
           (sum(acs_exposure_2005_2009$pop_nonhisp_black * 
                  acs_exposure_2005_2009$prop_exp_postprod_1km) /
              sum(acs_exposure_2005_2009$pop_total * 
                    acs_exposure_2005_2009$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2005_2009$pop_nonhisp_black) /
              sum(acs_exposure_2005_2009$pop_total)),
         rr_nonhisp_white    =
           (sum(acs_exposure_2005_2009$pop_nonhisp_white * 
                  acs_exposure_2005_2009$prop_exp_postprod_1km) /
              sum(acs_exposure_2005_2009$pop_total * 
                    acs_exposure_2005_2009$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2005_2009$pop_nonhisp_white) /
              sum(acs_exposure_2005_2009$pop_total)),
         rr_nonhisp_other    = 
           (sum(acs_exposure_2005_2009$pop_nonhisp_other * 
                  acs_exposure_2005_2009$prop_exp_postprod_1km) /
              sum(acs_exposure_2005_2009$pop_total * 
                    acs_exposure_2005_2009$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2005_2009$pop_nonhisp_other) /
              sum(acs_exposure_2005_2009$pop_total)),
         rr_nonhisp_two_plus = 
           (sum(acs_exposure_2005_2009$pop_nonhisp_two_or_more * 
                  acs_exposure_2005_2009$prop_exp_postprod_1km) /
              sum(acs_exposure_2005_2009$pop_total * 
                    acs_exposure_2005_2009$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2005_2009$pop_nonhisp_two_or_more) /
              sum(acs_exposure_2005_2009$pop_total)),
         
         # educational attainment
         rr_educ_less_than_hs = 
           (sum(acs_exposure_2005_2009$pop_educ_less_than_hs * 
                  acs_exposure_2005_2009$prop_exp_postprod_1km) /
              sum(acs_exposure_2005_2009$pop_educ_universe * 
                    acs_exposure_2005_2009$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2005_2009$pop_educ_less_than_hs) /
              sum(acs_exposure_2005_2009$pop_educ_universe)),
         rr_educ_hs           =
           (sum(acs_exposure_2005_2009$pop_educ_hs * 
                  acs_exposure_2005_2009$prop_exp_postprod_1km) /
              sum(acs_exposure_2005_2009$pop_educ_universe * 
                    acs_exposure_2005_2009$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2005_2009$pop_educ_hs) /
              sum(acs_exposure_2005_2009$pop_educ_universe)),
         rr_educ_more_than_hs = 
           (sum(acs_exposure_2005_2009$pop_educ_more_than_hs * 
                  acs_exposure_2005_2009$prop_exp_postprod_1km) /
              sum(acs_exposure_2005_2009$pop_educ_universe * 
                    acs_exposure_2005_2009$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2005_2009$pop_educ_more_than_hs) /
              sum(acs_exposure_2005_2009$pop_educ_universe)),
         
         # poverty
         rr_poverty           = 
           (sum(acs_exposure_2005_2009$pop_below_poverty * 
                  acs_exposure_2005_2009$prop_exp_postprod_1km) /
              sum(acs_exposure_2005_2009$pop_poverty_universe * 
                    acs_exposure_2005_2009$prop_exp_postprod_1km) /
              (sum(acs_exposure_2005_2009$pop_below_poverty) /
                 sum(acs_exposure_2005_2009$pop_poverty_universe))),
         
         # renter-occupied households
         rr_renters           = 
           (sum(acs_exposure_2005_2009$housing_occupied_renters * 
                  acs_exposure_2005_2009$prop_exp_postprod_1km) /
              sum(acs_exposure_2005_2009$housing_occupied_all * 
                    acs_exposure_2005_2009$prop_exp_postprod_1km) /
              (sum(acs_exposure_2005_2009$housing_occupied_renters) /
                 sum(acs_exposure_2005_2009$housing_occupied_all))),
         
         # linguistically isolated people
         rr_ling_isolated     = 
           (sum(acs_exposure_2005_2009$pop_linguistically_isolated_over18 * 
                  acs_exposure_2005_2009$prop_exp_postprod_1km) /
              sum(acs_exposure_2005_2009$pop_total_over18 * 
                    acs_exposure_2005_2009$prop_exp_postprod_1km) /
              (sum(acs_exposure_2005_2009$pop_linguistically_isolated_over18) /
                 sum(acs_exposure_2005_2009$pop_total_over18))),
         
         # voter turnout
         rr_nonvoters           = NA,  # no voting data for 2005-2009
  )

# 2010 to 2014 ...........................................................
table_rr_2010_2014_postprod <- 
  tibble(period              = "2010_2014",
         well_stage          = "postproduction",
         rr_hispanic         = 
           (sum(acs_exposure_2010_2014$pop_hispanic * 
                  acs_exposure_2010_2014$prop_exp_postprod_1km) /
              sum(acs_exposure_2010_2014$pop_total * 
                    acs_exposure_2010_2014$prop_exp_postprod_1km)) / 
           (sum(acs_exposure_2010_2014$pop_hispanic) /
              sum(acs_exposure_2010_2014$pop_total)),
         rr_nonhisp_am_indian =
           (sum(acs_exposure_2010_2014$pop_nonhisp_am_indian * 
                  acs_exposure_2010_2014$prop_exp_postprod_1km) /
              sum(acs_exposure_2010_2014$pop_total * 
                    acs_exposure_2010_2014$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2010_2014$pop_nonhisp_am_indian) /
              sum(acs_exposure_2010_2014$pop_total)),
         rr_nonhisp_asian    = 
           (sum(acs_exposure_2010_2014$pop_nonhisp_asian * 
                  acs_exposure_2010_2014$prop_exp_postprod_1km) /
              sum(acs_exposure_2010_2014$pop_total * 
                    acs_exposure_2010_2014$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2010_2014$pop_nonhisp_asian) /
              sum(acs_exposure_2010_2014$pop_total)),
         rr_nonhisp_black    = 
           (sum(acs_exposure_2010_2014$pop_nonhisp_black * 
                  acs_exposure_2010_2014$prop_exp_postprod_1km) /
              sum(acs_exposure_2010_2014$pop_total * 
                    acs_exposure_2010_2014$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2010_2014$pop_nonhisp_black) /
              sum(acs_exposure_2010_2014$pop_total)),
         rr_nonhisp_white    =
           (sum(acs_exposure_2010_2014$pop_nonhisp_white * 
                  acs_exposure_2010_2014$prop_exp_postprod_1km) /
              sum(acs_exposure_2010_2014$pop_total * 
                    acs_exposure_2010_2014$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2010_2014$pop_nonhisp_white) /
              sum(acs_exposure_2010_2014$pop_total)),
         rr_nonhisp_other    = 
           (sum(acs_exposure_2010_2014$pop_nonhisp_other * 
                  acs_exposure_2010_2014$prop_exp_postprod_1km) /
              sum(acs_exposure_2010_2014$pop_total * 
                    acs_exposure_2010_2014$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2010_2014$pop_nonhisp_other) /
              sum(acs_exposure_2010_2014$pop_total)),
         rr_nonhisp_two_plus = 
           (sum(acs_exposure_2010_2014$pop_nonhisp_two_or_more * 
                  acs_exposure_2010_2014$prop_exp_postprod_1km) /
              sum(acs_exposure_2010_2014$pop_total * 
                    acs_exposure_2010_2014$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2010_2014$pop_nonhisp_two_or_more) /
              sum(acs_exposure_2010_2014$pop_total)),
         
         # educational attainment
         rr_educ_less_than_hs = 
           (sum(acs_exposure_2010_2014$pop_educ_less_than_hs * 
                  acs_exposure_2010_2014$prop_exp_postprod_1km) /
              sum(acs_exposure_2010_2014$pop_educ_universe * 
                    acs_exposure_2010_2014$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2010_2014$pop_educ_less_than_hs) /
              sum(acs_exposure_2010_2014$pop_educ_universe)),
         rr_educ_hs           =
           (sum(acs_exposure_2010_2014$pop_educ_hs * 
                  acs_exposure_2010_2014$prop_exp_postprod_1km) /
              sum(acs_exposure_2010_2014$pop_educ_universe * 
                    acs_exposure_2010_2014$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2010_2014$pop_educ_hs) /
              sum(acs_exposure_2010_2014$pop_educ_universe)),
         rr_educ_more_than_hs = 
           (sum(acs_exposure_2010_2014$pop_educ_more_than_hs * 
                  acs_exposure_2010_2014$prop_exp_postprod_1km) /
              sum(acs_exposure_2010_2014$pop_educ_universe * 
                    acs_exposure_2010_2014$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2010_2014$pop_educ_more_than_hs) /
              sum(acs_exposure_2010_2014$pop_educ_universe)),
         
         # poverty
         rr_poverty           = 
           (sum(acs_exposure_2010_2014$pop_below_poverty * 
                  acs_exposure_2010_2014$prop_exp_postprod_1km) /
              sum(acs_exposure_2010_2014$pop_poverty_universe * 
                    acs_exposure_2010_2014$prop_exp_postprod_1km) /
              (sum(acs_exposure_2010_2014$pop_below_poverty) /
                 sum(acs_exposure_2010_2014$pop_poverty_universe))),
         
         # renter-occupied households
         rr_renters           = 
           (sum(acs_exposure_2010_2014$housing_occupied_renters * 
                  acs_exposure_2010_2014$prop_exp_postprod_1km) /
              sum(acs_exposure_2010_2014$housing_occupied_all * 
                    acs_exposure_2010_2014$prop_exp_postprod_1km) /
              (sum(acs_exposure_2010_2014$housing_occupied_renters) /
                 sum(acs_exposure_2010_2014$housing_occupied_all))),
         
         # linguistically isolated people
         rr_ling_isolated     = 
           (sum(acs_exposure_2010_2014$pop_linguistically_isolated_over18 * 
                  acs_exposure_2010_2014$prop_exp_postprod_1km) /
              sum(acs_exposure_2010_2014$pop_total_over18 * 
                    acs_exposure_2010_2014$prop_exp_postprod_1km) /
              (sum(acs_exposure_2010_2014$pop_linguistically_isolated_over18) /
                 sum(acs_exposure_2010_2014$pop_total_over18))),
         
         # voter turnout
         rr_nonvoters           = 
           (sum(acs_exposure_2010_2014$pop_nonvoters * 
                  acs_exposure_2010_2014$prop_exp_postprod_1km, na.rm = T) /
              sum(acs_exposure_2010_2014$pop_total_over18 * 
                    acs_exposure_2010_2014$prop_exp_postprod_1km) /
              (sum(acs_exposure_2010_2014$pop_nonvoters, na.rm = T) /
                 sum(acs_exposure_2010_2014$pop_total_over18)))
  )

# 2015 to 2019 ...........................................................
table_rr_2015_2019_postprod <- 
  tibble(period              = "2015_2019",
         well_stage          = "postproduction",
         rr_hispanic         = 
           (sum(acs_exposure_2015_2019$pop_hispanic * 
                  acs_exposure_2015_2019$prop_exp_postprod_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_postprod_1km)) / 
           (sum(acs_exposure_2015_2019$pop_hispanic) /
              sum(acs_exposure_2015_2019$pop_total)),
         rr_nonhisp_am_indian =
           (sum(acs_exposure_2015_2019$pop_nonhisp_am_indian * 
                  acs_exposure_2015_2019$prop_exp_postprod_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2015_2019$pop_nonhisp_am_indian) /
              sum(acs_exposure_2015_2019$pop_total)),
         rr_nonhisp_asian    = 
           (sum(acs_exposure_2015_2019$pop_nonhisp_asian * 
                  acs_exposure_2015_2019$prop_exp_postprod_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2015_2019$pop_nonhisp_asian) /
              sum(acs_exposure_2015_2019$pop_total)),
         rr_nonhisp_black    = 
           (sum(acs_exposure_2015_2019$pop_nonhisp_black * 
                  acs_exposure_2015_2019$prop_exp_postprod_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2015_2019$pop_nonhisp_black) /
              sum(acs_exposure_2015_2019$pop_total)),
         rr_nonhisp_white    =
           (sum(acs_exposure_2015_2019$pop_nonhisp_white * 
                  acs_exposure_2015_2019$prop_exp_postprod_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2015_2019$pop_nonhisp_white) /
              sum(acs_exposure_2015_2019$pop_total)),
         rr_nonhisp_other    = 
           (sum(acs_exposure_2015_2019$pop_nonhisp_other * 
                  acs_exposure_2015_2019$prop_exp_postprod_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2015_2019$pop_nonhisp_other) /
              sum(acs_exposure_2015_2019$pop_total)),
         rr_nonhisp_two_plus = 
           (sum(acs_exposure_2015_2019$pop_nonhisp_two_or_more * 
                  acs_exposure_2015_2019$prop_exp_postprod_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2015_2019$pop_nonhisp_two_or_more) /
              sum(acs_exposure_2015_2019$pop_total)),
         
         # educational attainment
         rr_educ_less_than_hs = 
           (sum(acs_exposure_2015_2019$pop_educ_less_than_hs * 
                  acs_exposure_2015_2019$prop_exp_postprod_1km) /
              sum(acs_exposure_2015_2019$pop_educ_universe * 
                    acs_exposure_2015_2019$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2015_2019$pop_educ_less_than_hs) /
              sum(acs_exposure_2015_2019$pop_educ_universe)),
         rr_educ_hs           =
           (sum(acs_exposure_2015_2019$pop_educ_hs * 
                  acs_exposure_2015_2019$prop_exp_postprod_1km) /
              sum(acs_exposure_2015_2019$pop_educ_universe * 
                    acs_exposure_2015_2019$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2015_2019$pop_educ_hs) /
              sum(acs_exposure_2015_2019$pop_educ_universe)),
         rr_educ_more_than_hs = 
           (sum(acs_exposure_2015_2019$pop_educ_more_than_hs * 
                  acs_exposure_2015_2019$prop_exp_postprod_1km) /
              sum(acs_exposure_2015_2019$pop_educ_universe * 
                    acs_exposure_2015_2019$prop_exp_postprod_1km)) /
           (sum(acs_exposure_2015_2019$pop_educ_more_than_hs) /
              sum(acs_exposure_2015_2019$pop_educ_universe)),
         
         # poverty
         rr_poverty           = 
           (sum(acs_exposure_2015_2019$pop_below_poverty * 
                  acs_exposure_2015_2019$prop_exp_postprod_1km) /
              sum(acs_exposure_2015_2019$pop_poverty_universe * 
                    acs_exposure_2015_2019$prop_exp_postprod_1km) /
              (sum(acs_exposure_2015_2019$pop_below_poverty) /
                 sum(acs_exposure_2015_2019$pop_poverty_universe))),
         
         # renter-occupied households
         rr_renters           = 
           (sum(acs_exposure_2015_2019$housing_occupied_renters * 
                  acs_exposure_2015_2019$prop_exp_postprod_1km) /
              sum(acs_exposure_2015_2019$housing_occupied_all * 
                    acs_exposure_2015_2019$prop_exp_postprod_1km) /
              (sum(acs_exposure_2015_2019$housing_occupied_renters) /
                 sum(acs_exposure_2015_2019$housing_occupied_all))),
         
         # linguistically isolated people
         rr_ling_isolated     = 
           (sum(acs_exposure_2015_2019$pop_linguistically_isolated_over18 * 
                  acs_exposure_2015_2019$prop_exp_postprod_1km) /
              sum(acs_exposure_2015_2019$pop_total_over18 * 
                    acs_exposure_2015_2019$prop_exp_postprod_1km) /
              (sum(acs_exposure_2015_2019$pop_linguistically_isolated_over18) /
                 sum(acs_exposure_2015_2019$pop_total_over18))),
         
         # voter turnout
         rr_nonvoters           = 
           (sum(acs_exposure_2015_2019$pop_nonvoters * 
                  acs_exposure_2015_2019$prop_exp_postprod_1km, na.rm = T) /
              sum(acs_exposure_2015_2019$pop_total_over18 * 
                    acs_exposure_2015_2019$prop_exp_postprod_1km) /
              (sum(acs_exposure_2015_2019$pop_nonvoters, na.rm = T) /
                 sum(acs_exposure_2015_2019$pop_total_over18)))
  )


#----------------------------------------------------------------------------
# generates table with risk ratios for statewide population proportions
# for *plugged* wells

# 2015 to 2019 ...........................................................
table_rr_2015_2019_plugged <- 
  tibble(period              = "2015_2019",
         well_stage          = "plugged",
         rr_hispanic         = 
           (sum(acs_exposure_2015_2019$pop_hispanic * 
                  acs_exposure_2015_2019$prop_exp_plugged_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_plugged_1km)) / 
           (sum(acs_exposure_2015_2019$pop_hispanic) /
              sum(acs_exposure_2015_2019$pop_total)),
         rr_nonhisp_am_indian =
           (sum(acs_exposure_2015_2019$pop_nonhisp_am_indian * 
                  acs_exposure_2015_2019$prop_exp_plugged_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_plugged_1km)) /
           (sum(acs_exposure_2015_2019$pop_nonhisp_am_indian) /
              sum(acs_exposure_2015_2019$pop_total)),
         rr_nonhisp_asian    = 
           (sum(acs_exposure_2015_2019$pop_nonhisp_asian * 
                  acs_exposure_2015_2019$prop_exp_plugged_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_plugged_1km)) /
           (sum(acs_exposure_2015_2019$pop_nonhisp_asian) /
              sum(acs_exposure_2015_2019$pop_total)),
         rr_nonhisp_black    = 
           (sum(acs_exposure_2015_2019$pop_nonhisp_black * 
                  acs_exposure_2015_2019$prop_exp_plugged_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_plugged_1km)) /
           (sum(acs_exposure_2015_2019$pop_nonhisp_black) /
              sum(acs_exposure_2015_2019$pop_total)),
         rr_nonhisp_white    =
           (sum(acs_exposure_2015_2019$pop_nonhisp_white * 
                  acs_exposure_2015_2019$prop_exp_plugged_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_plugged_1km)) /
           (sum(acs_exposure_2015_2019$pop_nonhisp_white) /
              sum(acs_exposure_2015_2019$pop_total)),
         rr_nonhisp_other    = 
           (sum(acs_exposure_2015_2019$pop_nonhisp_other * 
                  acs_exposure_2015_2019$prop_exp_plugged_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_plugged_1km)) /
           (sum(acs_exposure_2015_2019$pop_nonhisp_other) /
              sum(acs_exposure_2015_2019$pop_total)),
         rr_nonhisp_two_plus = 
           (sum(acs_exposure_2015_2019$pop_nonhisp_two_or_more * 
                  acs_exposure_2015_2019$prop_exp_plugged_1km) /
              sum(acs_exposure_2015_2019$pop_total * 
                    acs_exposure_2015_2019$prop_exp_plugged_1km)) /
           (sum(acs_exposure_2015_2019$pop_nonhisp_two_or_more) /
              sum(acs_exposure_2015_2019$pop_total)),
         
         # educational attainment
         rr_educ_less_than_hs = 
           (sum(acs_exposure_2015_2019$pop_educ_less_than_hs * 
                  acs_exposure_2015_2019$prop_exp_plugged_1km) /
              sum(acs_exposure_2015_2019$pop_educ_universe * 
                    acs_exposure_2015_2019$prop_exp_plugged_1km)) /
           (sum(acs_exposure_2015_2019$pop_educ_less_than_hs) /
              sum(acs_exposure_2015_2019$pop_educ_universe)),
         rr_educ_hs           =
           (sum(acs_exposure_2015_2019$pop_educ_hs * 
                  acs_exposure_2015_2019$prop_exp_plugged_1km) /
              sum(acs_exposure_2015_2019$pop_educ_universe * 
                    acs_exposure_2015_2019$prop_exp_plugged_1km)) /
           (sum(acs_exposure_2015_2019$pop_educ_hs) /
              sum(acs_exposure_2015_2019$pop_educ_universe)),
         rr_educ_more_than_hs = 
           (sum(acs_exposure_2015_2019$pop_educ_more_than_hs * 
                  acs_exposure_2015_2019$prop_exp_plugged_1km) /
              sum(acs_exposure_2015_2019$pop_educ_universe * 
                    acs_exposure_2015_2019$prop_exp_plugged_1km)) /
           (sum(acs_exposure_2015_2019$pop_educ_more_than_hs) /
              sum(acs_exposure_2015_2019$pop_educ_universe)),
         
         # poverty
         rr_poverty           = 
           (sum(acs_exposure_2015_2019$pop_below_poverty * 
                  acs_exposure_2015_2019$prop_exp_plugged_1km) /
              sum(acs_exposure_2015_2019$pop_poverty_universe * 
                    acs_exposure_2015_2019$prop_exp_plugged_1km) /
              (sum(acs_exposure_2015_2019$pop_below_poverty) /
                 sum(acs_exposure_2015_2019$pop_poverty_universe))),
         
         # renter-occupied households
         rr_renters           = 
           (sum(acs_exposure_2015_2019$housing_occupied_renters * 
                  acs_exposure_2015_2019$prop_exp_plugged_1km) /
              sum(acs_exposure_2015_2019$housing_occupied_all * 
                    acs_exposure_2015_2019$prop_exp_plugged_1km) /
              (sum(acs_exposure_2015_2019$housing_occupied_renters) /
                 sum(acs_exposure_2015_2019$housing_occupied_all))),
         
         # linguistically isolated people
         rr_ling_isolated     = 
           (sum(acs_exposure_2015_2019$pop_linguistically_isolated_over18 * 
                  acs_exposure_2015_2019$prop_exp_plugged_1km) /
              sum(acs_exposure_2015_2019$pop_total_over18 * 
                    acs_exposure_2015_2019$prop_exp_plugged_1km) /
              (sum(acs_exposure_2015_2019$pop_linguistically_isolated_over18) /
                 sum(acs_exposure_2015_2019$pop_total_over18))),
         
         # voter turnout
         rr_nonvoters           = 
           (sum(acs_exposure_2015_2019$pop_nonvoters * 
                  acs_exposure_2015_2019$prop_exp_plugged_1km, na.rm = T) /
              sum(acs_exposure_2015_2019$pop_total_over18 * 
                    acs_exposure_2015_2019$prop_exp_plugged_1km) /
              (sum(acs_exposure_2015_2019$pop_nonvoters, na.rm = T) /
                 sum(acs_exposure_2015_2019$pop_total_over18)))
  )

##--------------------------------------------------------------------------
## binds table components

table_rr <- table_rr_2005_2009_preprod %>%
  bind_rows(table_rr_2010_2014_preprod) %>%
  bind_rows(table_rr_2015_2019_preprod) %>%
  bind_rows(table_rr_2005_2009_prod) %>%
  bind_rows(table_rr_2010_2014_prod) %>%
  bind_rows(table_rr_2015_2019_prod) %>%
  bind_rows(table_rr_2005_2009_postprod) %>%
  bind_rows(table_rr_2010_2014_postprod) %>%
  bind_rows(table_rr_2015_2019_postprod) %>%
  bind_rows(table_rr_2015_2019_plugged)


##--------------------------------------------------------------------------
## exports table
write_csv(table_rr, file = "output/results/table_rr_county_orange.csv")

##============================================================================##
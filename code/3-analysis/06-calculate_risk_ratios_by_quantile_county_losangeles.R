##============================================================================##
## 3.06 - determines risk ratios for exposure to wells in each stage stratified
## by racial/ethnic and socioeconomic indicators - also stratified by quantile
## of exposure - restricted to Los Angeles County

#----------------------------------------------------------------------------
# setup

# imports and preps data necessary for this script
acs_exposure_2005_2019 <- 
  readRDS("data/processed/acs_exposure_2005_2019.rds") %>%
  filter(substr(GEOID, 1, 5) == "06037")  # restricts to Los Angeles County

acs_exposure_2005_2009 <- acs_exposure_2005_2019 %>%
  filter(period == "2005_2009") %>%
  filter(substr(GEOID, 1, 5) == "06037")  # restricts to Los Angeles County
acs_exposure_2010_2014 <- acs_exposure_2005_2019 %>%
  filter(period == "2010_2014") %>%
  filter(substr(GEOID, 1, 5) == "06037")  # restricts to Los Angeles County
acs_exposure_2015_2019 <- acs_exposure_2005_2019 %>%
  filter(period == "2015_2019") %>%
  filter(substr(GEOID, 1, 5) == "06037")  # restricts to Los Angeles County


#----------------------------------------------------------------------------
# generates table with risk ratios for statewide population proportions
# for wells in *preproduction*

# 2005 to 2009 ...........................................................

table_rr_quantiles_2005_2009_preprod <- acs_exposure_2005_2009 %>%
  filter(preprod_wells_0_1km > 0) %>%
  mutate(quantile = as.factor(ntile(preprod_wells_0_1km, 2))) %>%
  group_by(quantile) %>%
  summarize(period              = "2005_2009",
            well_stage          = "preproduction",
            rr_hispanic         = 
              (sum(pop_hispanic * prop_exp_preprod_1km) /
                 sum(pop_total * prop_exp_preprod_1km)) / 
              (sum(acs_exposure_2005_2009$pop_hispanic) /
                 sum(acs_exposure_2005_2009$pop_total)),
            rr_nonhisp_am_indian =
              (sum(pop_nonhisp_am_indian * prop_exp_preprod_1km) /
                 sum(pop_total * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2005_2009$pop_nonhisp_am_indian) /
                 sum(acs_exposure_2005_2009$pop_total)),
            rr_nonhisp_asian    = 
              (sum(pop_nonhisp_asian * prop_exp_preprod_1km) /
                 sum(pop_total * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2005_2009$pop_nonhisp_asian) /
                 sum(acs_exposure_2005_2009$pop_total)),
            rr_nonhisp_black    = 
              (sum(pop_nonhisp_black * prop_exp_preprod_1km) /
                 sum(pop_total * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2005_2009$pop_nonhisp_black) /
                 sum(acs_exposure_2005_2009$pop_total)),
            rr_nonhisp_white    =
              (sum(pop_nonhisp_white * prop_exp_preprod_1km) /
                 sum(pop_total * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2005_2009$pop_nonhisp_white) /
                 sum(acs_exposure_2005_2009$pop_total)),
            rr_nonhisp_other    = 
              (sum(pop_nonhisp_other * prop_exp_preprod_1km) /
                 sum(pop_total * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2005_2009$pop_nonhisp_other) /
                 sum(acs_exposure_2005_2009$pop_total)),
            rr_nonhisp_two_plus = 
              (sum(pop_nonhisp_two_or_more * prop_exp_preprod_1km) /
                 sum(pop_total * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2005_2009$pop_nonhisp_two_or_more) /
                 sum(acs_exposure_2005_2009$pop_total)),
            
            # educational attainment
            rr_educ_less_than_hs = 
              (sum(pop_educ_less_than_hs * prop_exp_preprod_1km) /
                 sum(pop_educ_universe * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2005_2009$pop_educ_less_than_hs) /
                 sum(acs_exposure_2005_2009$pop_educ_universe)),
            rr_educ_hs           =
              (sum(pop_educ_hs * prop_exp_preprod_1km) /
                 sum(pop_educ_universe * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2005_2009$pop_educ_hs) /
                 sum(acs_exposure_2005_2009$pop_educ_universe)),
            rr_educ_more_than_hs = 
              (sum(pop_educ_more_than_hs * prop_exp_preprod_1km) /
                 sum(pop_educ_universe * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2005_2009$pop_educ_more_than_hs) /
                 sum(acs_exposure_2005_2009$pop_educ_universe)),
            
            # poverty
            rr_poverty           = 
              (sum(pop_below_poverty * prop_exp_preprod_1km) /
                 sum(pop_poverty_universe * prop_exp_preprod_1km) /
                 (sum(acs_exposure_2005_2009$pop_below_poverty) /
                    sum(acs_exposure_2005_2009$pop_poverty_universe))),
            
            # renter-occupied households
            rr_renters           = 
              (sum(housing_occupied_renters * 
                     prop_exp_preprod_1km) /
                 sum(housing_occupied_all * prop_exp_preprod_1km) /
                 (sum(acs_exposure_2005_2009$housing_occupied_renters) /
                    sum(acs_exposure_2005_2009$housing_occupied_all))),
            
            # linguistically isolated people
            rr_ling_isolated     = 
              (sum(pop_linguistically_isolated_over18 * 
                     prop_exp_preprod_1km) /
                 sum(pop_total_over18 * prop_exp_preprod_1km) /
                 (sum(acs_exposure_2005_2009$pop_linguistically_isolated_over18) /
                    sum(acs_exposure_2005_2009$pop_total_over18))),
            
            # voter turnout
            rr_nonvoters           = NA,  # no voting data for 2005-2009
  )

# 2010 to 2014 ...........................................................
table_rr_quantiles_2010_2014_preprod <-  acs_exposure_2010_2014 %>%
  filter(preprod_wells_0_1km > 0) %>%
  mutate(quantile = as.factor(ntile(preprod_wells_0_1km, 2))) %>%
  group_by(quantile) %>%
  summarize(period              = "2010_2014",
            well_stage          = "preproduction",
            rr_hispanic         = 
              (sum(pop_hispanic * prop_exp_preprod_1km) /
                 sum(pop_total * prop_exp_preprod_1km)) / 
              (sum(acs_exposure_2010_2014$pop_hispanic) /
                 sum(acs_exposure_2010_2014$pop_total)),
            rr_nonhisp_am_indian =
              (sum(pop_nonhisp_am_indian * prop_exp_preprod_1km) /
                 sum(pop_total * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2010_2014$pop_nonhisp_am_indian) /
                 sum(acs_exposure_2010_2014$pop_total)),
            rr_nonhisp_asian    = 
              (sum(pop_nonhisp_asian * prop_exp_preprod_1km) /
                 sum(pop_total * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2010_2014$pop_nonhisp_asian) /
                 sum(acs_exposure_2010_2014$pop_total)),
            rr_nonhisp_black    = 
              (sum(pop_nonhisp_black * prop_exp_preprod_1km) /
                 sum(pop_total * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2010_2014$pop_nonhisp_black) /
                 sum(acs_exposure_2010_2014$pop_total)),
            rr_nonhisp_white    =
              (sum(pop_nonhisp_white * prop_exp_preprod_1km) /
                 sum(pop_total * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2010_2014$pop_nonhisp_white) /
                 sum(acs_exposure_2010_2014$pop_total)),
            rr_nonhisp_other    = 
              (sum(pop_nonhisp_other * prop_exp_preprod_1km) /
                 sum(pop_total * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2010_2014$pop_nonhisp_other) /
                 sum(acs_exposure_2010_2014$pop_total)),
            rr_nonhisp_two_plus = 
              (sum(pop_nonhisp_two_or_more * prop_exp_preprod_1km) /
                 sum(pop_total * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2010_2014$pop_nonhisp_two_or_more) /
                 sum(acs_exposure_2010_2014$pop_total)),
            
            # educational attainment
            rr_educ_less_than_hs = 
              (sum(pop_educ_less_than_hs * prop_exp_preprod_1km) /
                 sum(pop_educ_universe * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2010_2014$pop_educ_less_than_hs) /
                 sum(acs_exposure_2010_2014$pop_educ_universe)),
            rr_educ_hs           =
              (sum(pop_educ_hs * prop_exp_preprod_1km) /
                 sum(pop_educ_universe * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2010_2014$pop_educ_hs) /
                 sum(acs_exposure_2010_2014$pop_educ_universe)),
            rr_educ_more_than_hs = 
              (sum(pop_educ_more_than_hs * prop_exp_preprod_1km) /
                 sum(pop_educ_universe * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2010_2014$pop_educ_more_than_hs) /
                 sum(acs_exposure_2010_2014$pop_educ_universe)),
            
            # poverty
            rr_poverty           = 
              (sum(pop_below_poverty * prop_exp_preprod_1km) /
                 sum(pop_poverty_universe * prop_exp_preprod_1km) /
                 (sum(acs_exposure_2010_2014$pop_below_poverty) /
                    sum(acs_exposure_2010_2014$pop_poverty_universe))),
            
            # renter-occupied households
            rr_renters           = 
              (sum(housing_occupied_renters * prop_exp_preprod_1km) /
                 sum(housing_occupied_all * prop_exp_preprod_1km) /
                 (sum(acs_exposure_2010_2014$housing_occupied_renters) /
                    sum(acs_exposure_2010_2014$housing_occupied_all))),
            
            # linguistically isolated people
            rr_ling_isolated     = 
              (sum(pop_linguistically_isolated_over18 * prop_exp_preprod_1km) /
                 sum(pop_total_over18 * prop_exp_preprod_1km) /
                 (sum(acs_exposure_2010_2014$pop_linguistically_isolated_over18) /
                    sum(acs_exposure_2010_2014$pop_total_over18))),
            
            # voter turnout
            rr_nonvoters           = 
              (sum(pop_nonvoters * prop_exp_preprod_1km, na.rm = T) /
                 sum(pop_total_over18 * prop_exp_preprod_1km) /
                 (sum(acs_exposure_2010_2014$pop_nonvoters, na.rm = T) /
                    sum(acs_exposure_2010_2014$pop_total_over18)))
  )

# 2015 to 2019 ...........................................................
table_rr_quantiles_2015_2019_preprod <-  acs_exposure_2015_2019 %>%
  filter(preprod_wells_0_1km > 0) %>%
  mutate(quantile = as.factor(ntile(preprod_wells_0_1km, 2))) %>%
  group_by(quantile) %>%
  summarize(period              = "2015_2019",
            well_stage          = "preproduction",
            rr_hispanic         = 
              (sum(pop_hispanic * prop_exp_preprod_1km) /
                 sum(pop_total * prop_exp_preprod_1km)) / 
              (sum(acs_exposure_2015_2019$pop_hispanic) /
                 sum(acs_exposure_2015_2019$pop_total)),
            rr_nonhisp_am_indian =
              (sum(pop_nonhisp_am_indian * prop_exp_preprod_1km) /
                 sum(pop_total * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2015_2019$pop_nonhisp_am_indian) /
                 sum(acs_exposure_2015_2019$pop_total)),
            rr_nonhisp_asian    = 
              (sum(pop_nonhisp_asian * prop_exp_preprod_1km) /
                 sum(pop_total * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2015_2019$pop_nonhisp_asian) /
                 sum(acs_exposure_2015_2019$pop_total)),
            rr_nonhisp_black    = 
              (sum(pop_nonhisp_black * prop_exp_preprod_1km) /
                 sum(pop_total * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2015_2019$pop_nonhisp_black) /
                 sum(acs_exposure_2015_2019$pop_total)),
            rr_nonhisp_white    =
              (sum(pop_nonhisp_white * prop_exp_preprod_1km) /
                 sum(pop_total * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2015_2019$pop_nonhisp_white) /
                 sum(acs_exposure_2015_2019$pop_total)),
            rr_nonhisp_other    = 
              (sum(pop_nonhisp_other * prop_exp_preprod_1km) /
                 sum(pop_total * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2015_2019$pop_nonhisp_other) /
                 sum(acs_exposure_2015_2019$pop_total)),
            rr_nonhisp_two_plus = 
              (sum(pop_nonhisp_two_or_more * prop_exp_preprod_1km) /
                 sum(pop_total * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2015_2019$pop_nonhisp_two_or_more) /
                 sum(acs_exposure_2015_2019$pop_total)),
            
            # educational attainment
            rr_educ_less_than_hs = 
              (sum(pop_educ_less_than_hs * prop_exp_preprod_1km) /
                 sum(pop_educ_universe * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2015_2019$pop_educ_less_than_hs) /
                 sum(acs_exposure_2015_2019$pop_educ_universe)),
            rr_educ_hs           =
              (sum(pop_educ_hs * prop_exp_preprod_1km) /
                 sum(pop_educ_universe * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2015_2019$pop_educ_hs) /
                 sum(acs_exposure_2015_2019$pop_educ_universe)),
            rr_educ_more_than_hs = 
              (sum(pop_educ_more_than_hs * prop_exp_preprod_1km) /
                 sum(pop_educ_universe * prop_exp_preprod_1km)) /
              (sum(acs_exposure_2015_2019$pop_educ_more_than_hs) /
                 sum(acs_exposure_2015_2019$pop_educ_universe)),
            
            # poverty
            rr_poverty           = 
              (sum(pop_below_poverty * prop_exp_preprod_1km) /
                 sum(pop_poverty_universe * prop_exp_preprod_1km) /
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
              (sum(pop_linguistically_isolated_over18 * 
                     prop_exp_preprod_1km) /
                 sum(pop_total_over18 * prop_exp_preprod_1km) /
                 (sum(acs_exposure_2015_2019$pop_linguistically_isolated_over18) /
                    sum(acs_exposure_2015_2019$pop_total_over18))),
            
            # voter turnout
            rr_nonvoters           = 
              (sum(pop_nonvoters * prop_exp_preprod_1km, na.rm = T) /
                 sum(pop_total_over18 * prop_exp_preprod_1km) /
                 (sum(acs_exposure_2015_2019$pop_nonvoters, na.rm = T) /
                    sum(acs_exposure_2015_2019$pop_total_over18)))
  )



#----------------------------------------------------------------------------
# generates table with risk ratios for statewide population proportions
# for wells in *production*

# 2005 to 2009 ...........................................................
table_rr_quantiles_2005_2009_prod <- 
  acs_exposure_2005_2009 %>%
  filter(prod_wells_0_1km > 0) %>%
  mutate(quantile = as.factor(ntile(prod_wells_0_1km, 5))) %>%
  group_by(quantile) %>%
  summarize(period              = "2005_2009",
            well_stage          = "production",
            rr_hispanic         = 
              (sum(pop_hispanic * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) / 
              (sum(acs_exposure_2005_2009$pop_hispanic) /
                 sum(acs_exposure_2005_2009$pop_total)),
            rr_nonhisp_am_indian =
              (sum(pop_nonhisp_am_indian * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2005_2009$pop_nonhisp_am_indian) /
                 sum(acs_exposure_2005_2009$pop_total)),
            rr_nonhisp_asian    = 
              (sum(pop_nonhisp_asian * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2005_2009$pop_nonhisp_asian) /
                 sum(acs_exposure_2005_2009$pop_total)),
            rr_nonhisp_black    = 
              (sum(pop_nonhisp_black * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2005_2009$pop_nonhisp_black) /
                 sum(acs_exposure_2005_2009$pop_total)),
            rr_nonhisp_white    =
              (sum(pop_nonhisp_white * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2005_2009$pop_nonhisp_white) /
                 sum(acs_exposure_2005_2009$pop_total)),
            rr_nonhisp_other    = 
              (sum(pop_nonhisp_other * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2005_2009$pop_nonhisp_other) /
                 sum(acs_exposure_2005_2009$pop_total)),
            rr_nonhisp_two_plus = 
              (sum(pop_nonhisp_two_or_more * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2005_2009$pop_nonhisp_two_or_more) /
                 sum(acs_exposure_2005_2009$pop_total)),
            
            # educational attainment
            rr_educ_less_than_hs = 
              (sum(pop_educ_less_than_hs * prop_exp_prod_1km) /
                 sum(pop_educ_universe * prop_exp_prod_1km)) /
              (sum(acs_exposure_2005_2009$pop_educ_less_than_hs) /
                 sum(acs_exposure_2005_2009$pop_educ_universe)),
            rr_educ_hs           =
              (sum(pop_educ_hs * prop_exp_prod_1km) /
                 sum(pop_educ_universe * prop_exp_prod_1km)) /
              (sum(acs_exposure_2005_2009$pop_educ_hs) /
                 sum(acs_exposure_2005_2009$pop_educ_universe)),
            rr_educ_more_than_hs = 
              (sum(pop_educ_more_than_hs * prop_exp_prod_1km) /
                 sum(pop_educ_universe * prop_exp_prod_1km)) /
              (sum(acs_exposure_2005_2009$pop_educ_more_than_hs) /
                 sum(acs_exposure_2005_2009$pop_educ_universe)),
            
            # poverty
            rr_poverty           = 
              (sum(pop_below_poverty * prop_exp_prod_1km) /
                 sum(pop_poverty_universe * prop_exp_prod_1km) /
                 (sum(acs_exposure_2005_2009$pop_below_poverty) /
                    sum(acs_exposure_2005_2009$pop_poverty_universe))),
            
            # renter-occupied households
            rr_renters           = 
              (sum(housing_occupied_renters * prop_exp_prod_1km) /
                 sum(housing_occupied_all * prop_exp_prod_1km) /
                 (sum(acs_exposure_2005_2009$housing_occupied_renters) /
                    sum(acs_exposure_2005_2009$housing_occupied_all))),
            
            # linguistically isolated people
            rr_ling_isolated     = 
              (sum(pop_linguistically_isolated_over18 * prop_exp_prod_1km) /
                 sum(pop_total_over18 * prop_exp_prod_1km) /
                 (sum(acs_exposure_2005_2009$pop_linguistically_isolated_over18) /
                    sum(acs_exposure_2005_2009$pop_total_over18))),
            
            # voter turnout
            rr_nonvoters           = NA,  # no voting data for 2005-2009
  )

# 2010 to 2014 ...........................................................
table_rr_quantiles_2010_2014_prod <-  
  acs_exposure_2010_2014 %>%
  filter(prod_wells_0_1km > 0) %>%
  mutate(quantile = as.factor(ntile(prod_wells_0_1km, 5))) %>%
  group_by(quantile) %>%
  summarize(period              = "2010_2014",
            well_stage          = "production",
            rr_hispanic         = 
              (sum(pop_hispanic * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) / 
              (sum(acs_exposure_2010_2014$pop_hispanic) /
                 sum(acs_exposure_2010_2014$pop_total)),
            rr_nonhisp_am_indian =
              (sum(pop_nonhisp_am_indian * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2010_2014$pop_nonhisp_am_indian) /
                 sum(acs_exposure_2010_2014$pop_total)),
            rr_nonhisp_asian    = 
              (sum(pop_nonhisp_asian * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2010_2014$pop_nonhisp_asian) /
                 sum(acs_exposure_2010_2014$pop_total)),
            rr_nonhisp_black    = 
              (sum(pop_nonhisp_black * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2010_2014$pop_nonhisp_black) /
                 sum(acs_exposure_2010_2014$pop_total)),
            rr_nonhisp_white    =
              (sum(pop_nonhisp_white * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2010_2014$pop_nonhisp_white) /
                 sum(acs_exposure_2010_2014$pop_total)),
            rr_nonhisp_other    = 
              (sum(pop_nonhisp_other * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2010_2014$pop_nonhisp_other) /
                 sum(acs_exposure_2010_2014$pop_total)),
            rr_nonhisp_two_plus = 
              (sum(pop_nonhisp_two_or_more * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2010_2014$pop_nonhisp_two_or_more) /
                 sum(acs_exposure_2010_2014$pop_total)),
            
            # educational attainment
            rr_educ_less_than_hs = 
              (sum(pop_educ_less_than_hs * prop_exp_prod_1km) /
                 sum(pop_educ_universe * prop_exp_prod_1km)) /
              (sum(acs_exposure_2010_2014$pop_educ_less_than_hs) /
                 sum(acs_exposure_2010_2014$pop_educ_universe)),
            rr_educ_hs           =
              (sum(pop_educ_hs * prop_exp_prod_1km) /
                 sum(pop_educ_universe * prop_exp_prod_1km)) /
              (sum(acs_exposure_2010_2014$pop_educ_hs) /
                 sum(acs_exposure_2010_2014$pop_educ_universe)),
            rr_educ_more_than_hs = 
              (sum(pop_educ_more_than_hs * prop_exp_prod_1km) /
                 sum(pop_educ_universe * prop_exp_prod_1km)) /
              (sum(acs_exposure_2010_2014$pop_educ_more_than_hs) /
                 sum(acs_exposure_2010_2014$pop_educ_universe)),
            
            # poverty
            rr_poverty           = 
              (sum(pop_below_poverty * prop_exp_prod_1km) /
                 sum(pop_poverty_universe * prop_exp_prod_1km) /
                 (sum(acs_exposure_2010_2014$pop_below_poverty) /
                    sum(acs_exposure_2010_2014$pop_poverty_universe))),
            
            # renter-occupied households
            rr_renters           = 
              (sum(housing_occupied_renters * prop_exp_prod_1km) /
                 sum(housing_occupied_all * prop_exp_prod_1km) /
                 (sum(acs_exposure_2010_2014$housing_occupied_renters) /
                    sum(acs_exposure_2010_2014$housing_occupied_all))),
            
            # linguistically isolated people
            rr_ling_isolated     = 
              (sum(pop_linguistically_isolated_over18 * 
                     prop_exp_prod_1km) /
                 sum(pop_total_over18 * prop_exp_prod_1km) /
                 (sum(acs_exposure_2010_2014$pop_linguistically_isolated_over18) /
                    sum(acs_exposure_2010_2014$pop_total_over18))),
            
            # voter turnout
            rr_nonvoters           = 
              (sum(pop_nonvoters * prop_exp_prod_1km, na.rm = T) /
                 sum(pop_total_over18 * prop_exp_prod_1km) /
                 (sum(acs_exposure_2010_2014$pop_nonvoters, na.rm = T) /
                    sum(acs_exposure_2010_2014$pop_total_over18)))
  )

# 2015 to 2019 ...........................................................
table_rr_quantiles_2015_2019_prod <- 
  acs_exposure_2015_2019 %>%
  filter(prod_wells_0_1km > 0) %>%
  mutate(quantile = as.factor(ntile(prod_wells_0_1km, 5))) %>%
  group_by(quantile) %>%
  summarize(period              = "2015_2019",
            well_stage          = "production",
            rr_hispanic         = 
              (sum(pop_hispanic * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) / 
              (sum(acs_exposure_2015_2019$pop_hispanic) /
                 sum(acs_exposure_2015_2019$pop_total)),
            rr_nonhisp_am_indian =
              (sum(pop_nonhisp_am_indian * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2015_2019$pop_nonhisp_am_indian) /
                 sum(acs_exposure_2015_2019$pop_total)),
            rr_nonhisp_asian    = 
              (sum(pop_nonhisp_asian * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2015_2019$pop_nonhisp_asian) /
                 sum(acs_exposure_2015_2019$pop_total)),
            rr_nonhisp_black    = 
              (sum(pop_nonhisp_black * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2015_2019$pop_nonhisp_black) /
                 sum(acs_exposure_2015_2019$pop_total)),
            rr_nonhisp_white    =
              (sum(pop_nonhisp_white * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2015_2019$pop_nonhisp_white) /
                 sum(acs_exposure_2015_2019$pop_total)),
            rr_nonhisp_other    = 
              (sum(pop_nonhisp_other * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2015_2019$pop_nonhisp_other) /
                 sum(acs_exposure_2015_2019$pop_total)),
            rr_nonhisp_two_plus = 
              (sum(pop_nonhisp_two_or_more * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2015_2019$pop_nonhisp_two_or_more) /
                 sum(acs_exposure_2015_2019$pop_total)),
            
            # educational attainment
            rr_educ_less_than_hs = 
              (sum(pop_educ_less_than_hs * prop_exp_prod_1km) /
                 sum(pop_educ_universe * prop_exp_prod_1km)) /
              (sum(acs_exposure_2015_2019$pop_educ_less_than_hs) /
                 sum(acs_exposure_2015_2019$pop_educ_universe)),
            rr_educ_hs           =
              (sum(pop_educ_hs * prop_exp_prod_1km) /
                 sum(pop_educ_universe * prop_exp_prod_1km)) /
              (sum(acs_exposure_2015_2019$pop_educ_hs) /
                 sum(acs_exposure_2015_2019$pop_educ_universe)),
            rr_educ_more_than_hs = 
              (sum(pop_educ_more_than_hs * prop_exp_prod_1km) /
                 sum(pop_educ_universe * prop_exp_prod_1km)) /
              (sum(acs_exposure_2015_2019$pop_educ_more_than_hs) /
                 sum(acs_exposure_2015_2019$pop_educ_universe)),
            
            # poverty
            rr_poverty           = 
              (sum(pop_below_poverty * prop_exp_prod_1km) /
                 sum(pop_poverty_universe * prop_exp_prod_1km) /
                 (sum(acs_exposure_2015_2019$pop_below_poverty) /
                    sum(acs_exposure_2015_2019$pop_poverty_universe))),
            
            # renter-occupied households
            rr_renters           = 
              (sum(housing_occupied_renters * prop_exp_prod_1km) /
                 sum(housing_occupied_all * prop_exp_prod_1km) /
                 (sum(acs_exposure_2015_2019$housing_occupied_renters) /
                    sum(acs_exposure_2015_2019$housing_occupied_all))),
            
            # linguistically isolated people
            rr_ling_isolated     = 
              (sum(pop_linguistically_isolated_over18 * prop_exp_prod_1km) /
                 sum(pop_total_over18 * prop_exp_prod_1km) /
                 (sum(acs_exposure_2015_2019$pop_linguistically_isolated_over18) /
                    sum(acs_exposure_2015_2019$pop_total_over18))),
            
            # voter turnout
            rr_nonvoters           = 
              (sum(pop_nonvoters * prop_exp_prod_1km, na.rm = T) /
                 sum(pop_total_over18 * prop_exp_prod_1km) /
                 (sum(acs_exposure_2015_2019$pop_nonvoters, na.rm = T) /
                    sum(acs_exposure_2015_2019$pop_total_over18)))
  )


#----------------------------------------------------------------------------
# generates table with risk ratios for statewide population proportions
# for *production volume*

# 2005 to 2009 ...........................................................
table_rr_quantiles_2005_2009_prod_vol <- 
  acs_exposure_2005_2009 %>%
  filter(prod_volume_0_1km > 0) %>%
  mutate(quantile = as.factor(ntile(prod_volume_0_1km, 5))) %>%
  group_by(quantile) %>%
  summarize(period              = "2005_2009",
            well_stage          = "production_volume",
            rr_hispanic         = 
              (sum(pop_hispanic * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) / 
              (sum(acs_exposure_2005_2009$pop_hispanic) /
                 sum(acs_exposure_2005_2009$pop_total)),
            rr_nonhisp_am_indian =
              (sum(pop_nonhisp_am_indian * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2005_2009$pop_nonhisp_am_indian) /
                 sum(acs_exposure_2005_2009$pop_total)),
            rr_nonhisp_asian    = 
              (sum(pop_nonhisp_asian * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2005_2009$pop_nonhisp_asian) /
                 sum(acs_exposure_2005_2009$pop_total)),
            rr_nonhisp_black    = 
              (sum(pop_nonhisp_black * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2005_2009$pop_nonhisp_black) /
                 sum(acs_exposure_2005_2009$pop_total)),
            rr_nonhisp_white    =
              (sum(pop_nonhisp_white * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2005_2009$pop_nonhisp_white) /
                 sum(acs_exposure_2005_2009$pop_total)),
            rr_nonhisp_other    = 
              (sum(pop_nonhisp_other * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2005_2009$pop_nonhisp_other) /
                 sum(acs_exposure_2005_2009$pop_total)),
            rr_nonhisp_two_plus = 
              (sum(pop_nonhisp_two_or_more * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2005_2009$pop_nonhisp_two_or_more) /
                 sum(acs_exposure_2005_2009$pop_total)),
            
            # educational attainment
            rr_educ_less_than_hs = 
              (sum(pop_educ_less_than_hs * prop_exp_prod_1km) /
                 sum(pop_educ_universe * prop_exp_prod_1km)) /
              (sum(acs_exposure_2005_2009$pop_educ_less_than_hs) /
                 sum(acs_exposure_2005_2009$pop_educ_universe)),
            rr_educ_hs           =
              (sum(pop_educ_hs * prop_exp_prod_1km) /
                 sum(pop_educ_universe * prop_exp_prod_1km)) /
              (sum(acs_exposure_2005_2009$pop_educ_hs) /
                 sum(acs_exposure_2005_2009$pop_educ_universe)),
            rr_educ_more_than_hs = 
              (sum(pop_educ_more_than_hs * prop_exp_prod_1km) /
                 sum(pop_educ_universe * prop_exp_prod_1km)) /
              (sum(acs_exposure_2005_2009$pop_educ_more_than_hs) /
                 sum(acs_exposure_2005_2009$pop_educ_universe)),
            
            # poverty
            rr_poverty           = 
              (sum(pop_below_poverty * prop_exp_prod_1km) /
                 sum(pop_poverty_universe * prop_exp_prod_1km) /
                 (sum(acs_exposure_2005_2009$pop_below_poverty) /
                    sum(acs_exposure_2005_2009$pop_poverty_universe))),
            
            # renter-occupied households
            rr_renters           = 
              (sum(housing_occupied_renters * prop_exp_postprod_1km) /
                 sum(housing_occupied_all * prop_exp_postprod_1km) /
                 (sum(acs_exposure_2005_2009$housing_occupied_renters) /
                    sum(acs_exposure_2005_2009$housing_occupied_all))),
            
            # linguistically isolated people
            rr_ling_isolated     = 
              (sum(pop_linguistically_isolated_over18 * 
                     prop_exp_postprod_1km) /
                 sum(pop_total_over18 * prop_exp_postprod_1km) /
                 (sum(acs_exposure_2005_2009$pop_linguistically_isolated_over18) /
                    sum(acs_exposure_2005_2009$pop_total_over18))),
            
            # voter turnout
            rr_nonvoters           = NA,  # no voting data for 2005-2009
  )

# 2010 to 2014 ...........................................................
table_rr_quantiles_2010_2014_prod_vol <- 
  acs_exposure_2010_2014 %>%
  filter(prod_volume_0_1km > 0) %>%
  mutate(quantile = as.factor(ntile(prod_volume_0_1km, 5))) %>%
  group_by(quantile) %>%
  summarize(period              = "2010_2014",
            well_stage          = "production_volume",
            rr_hispanic         = 
              (sum(pop_hispanic * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) / 
              (sum(acs_exposure_2010_2014$pop_hispanic) /
                 sum(acs_exposure_2010_2014$pop_total)),
            rr_nonhisp_am_indian =
              (sum(pop_nonhisp_am_indian * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2010_2014$pop_nonhisp_am_indian) /
                 sum(acs_exposure_2010_2014$pop_total)),
            rr_nonhisp_asian    = 
              (sum(pop_nonhisp_asian * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2010_2014$pop_nonhisp_asian) /
                 sum(acs_exposure_2010_2014$pop_total)),
            rr_nonhisp_black    = 
              (sum(pop_nonhisp_black * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2010_2014$pop_nonhisp_black) /
                 sum(acs_exposure_2010_2014$pop_total)),
            rr_nonhisp_white    =
              (sum(pop_nonhisp_white * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2010_2014$pop_nonhisp_white) /
                 sum(acs_exposure_2010_2014$pop_total)),
            rr_nonhisp_other    = 
              (sum(pop_nonhisp_other * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2010_2014$pop_nonhisp_other) /
                 sum(acs_exposure_2010_2014$pop_total)),
            rr_nonhisp_two_plus = 
              (sum(pop_nonhisp_two_or_more * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2010_2014$pop_nonhisp_two_or_more) /
                 sum(acs_exposure_2010_2014$pop_total)),
            
            # educational attainment
            rr_educ_less_than_hs = 
              (sum(pop_educ_less_than_hs * prop_exp_prod_1km) /
                 sum(pop_educ_universe * prop_exp_prod_1km)) /
              (sum(acs_exposure_2010_2014$pop_educ_less_than_hs) /
                 sum(acs_exposure_2010_2014$pop_educ_universe)),
            rr_educ_hs           =
              (sum(pop_educ_hs * prop_exp_prod_1km) /
                 sum(pop_educ_universe * prop_exp_prod_1km)) /
              (sum(acs_exposure_2010_2014$pop_educ_hs) /
                 sum(acs_exposure_2010_2014$pop_educ_universe)),
            rr_educ_more_than_hs = 
              (sum(pop_educ_more_than_hs * prop_exp_prod_1km) /
                 sum(pop_educ_universe * prop_exp_prod_1km)) /
              (sum(acs_exposure_2010_2014$pop_educ_more_than_hs) /
                 sum(acs_exposure_2010_2014$pop_educ_universe)),
            
            # poverty
            rr_poverty           = 
              (sum(pop_below_poverty * prop_exp_prod_1km) /
                 sum(pop_poverty_universe * prop_exp_prod_1km) /
                 (sum(acs_exposure_2010_2014$pop_below_poverty) /
                    sum(acs_exposure_2010_2014$pop_poverty_universe))),
            
            # renter-occupied households
            rr_renters           = 
              (sum(housing_occupied_renters * prop_exp_postprod_1km) /
                 sum(housing_occupied_all * prop_exp_postprod_1km) /
                 (sum(acs_exposure_2010_2014$housing_occupied_renters) /
                    sum(acs_exposure_2010_2014$housing_occupied_all))),
            
            # linguistically isolated people
            rr_ling_isolated     = 
              (sum(pop_linguistically_isolated_over18 * 
                     prop_exp_postprod_1km) /
                 sum(pop_total_over18 * prop_exp_postprod_1km) /
                 (sum(acs_exposure_2010_2014$pop_linguistically_isolated_over18) /
                    sum(acs_exposure_2010_2014$pop_total_over18))),
            
            # voter turnout
            rr_nonvoters           = 
              (sum(pop_nonvoters * prop_exp_postprod_1km, na.rm = T) /
                 sum(pop_total_over18 * prop_exp_postprod_1km) /
                 (sum(acs_exposure_2010_2014$pop_nonvoters, na.rm = T) /
                    sum(acs_exposure_2010_2014$pop_total_over18)))
  )

# 2015 to 2019 ...........................................................
table_rr_quantiles_2015_2019_prod_vol <- 
  acs_exposure_2015_2019 %>%
  filter(prod_volume_0_1km > 0) %>%
  mutate(quantile = as.factor(ntile(prod_volume_0_1km, 5))) %>%
  group_by(quantile) %>%
  summarize(period              = "2015_2019",
            well_stage          = "production_volume",
            rr_hispanic         = 
              (sum(pop_hispanic * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) / 
              (sum(acs_exposure_2015_2019$pop_hispanic) /
                 sum(acs_exposure_2015_2019$pop_total)),
            rr_nonhisp_am_indian =
              (sum(pop_nonhisp_am_indian * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2015_2019$pop_nonhisp_am_indian) /
                 sum(acs_exposure_2015_2019$pop_total)),
            rr_nonhisp_asian    = 
              (sum(pop_nonhisp_asian * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2015_2019$pop_nonhisp_asian) /
                 sum(acs_exposure_2015_2019$pop_total)),
            rr_nonhisp_black    = 
              (sum(pop_nonhisp_black * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2015_2019$pop_nonhisp_black) /
                 sum(acs_exposure_2015_2019$pop_total)),
            rr_nonhisp_white    =
              (sum(pop_nonhisp_white * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2015_2019$pop_nonhisp_white) /
                 sum(acs_exposure_2015_2019$pop_total)),
            rr_nonhisp_other    = 
              (sum(pop_nonhisp_other * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2015_2019$pop_nonhisp_other) /
                 sum(acs_exposure_2015_2019$pop_total)),
            rr_nonhisp_two_plus = 
              (sum(pop_nonhisp_two_or_more * prop_exp_prod_1km) /
                 sum(pop_total * prop_exp_prod_1km)) /
              (sum(acs_exposure_2015_2019$pop_nonhisp_two_or_more) /
                 sum(acs_exposure_2015_2019$pop_total)),
            
            # educational attainment
            rr_educ_less_than_hs = 
              (sum(pop_educ_less_than_hs * prop_exp_prod_1km) /
                 sum(pop_educ_universe * prop_exp_prod_1km)) /
              (sum(acs_exposure_2015_2019$pop_educ_less_than_hs) /
                 sum(acs_exposure_2015_2019$pop_educ_universe)),
            rr_educ_hs           =
              (sum(pop_educ_hs * prop_exp_prod_1km) /
                 sum(pop_educ_universe * prop_exp_prod_1km)) /
              (sum(acs_exposure_2015_2019$pop_educ_hs) /
                 sum(acs_exposure_2015_2019$pop_educ_universe)),
            rr_educ_more_than_hs = 
              (sum(pop_educ_more_than_hs * prop_exp_prod_1km) /
                 sum(pop_educ_universe * prop_exp_prod_1km)) /
              (sum(acs_exposure_2015_2019$pop_educ_more_than_hs) /
                 sum(acs_exposure_2015_2019$pop_educ_universe)),
            
            # poverty
            rr_poverty           = 
              (sum(pop_below_poverty * prop_exp_prod_1km) /
                 sum(pop_poverty_universe * prop_exp_prod_1km) /
                 (sum(acs_exposure_2015_2019$pop_below_poverty) /
                    sum(acs_exposure_2015_2019$pop_poverty_universe))),
            
            # renter-occupied households
            rr_renters           = 
              (sum(housing_occupied_renters * prop_exp_postprod_1km) /
                 sum(housing_occupied_all * prop_exp_postprod_1km) /
                 (sum(acs_exposure_2015_2019$housing_occupied_renters) /
                    sum(acs_exposure_2015_2019$housing_occupied_all))),
            
            # linguistically isolated people
            rr_ling_isolated     = 
              (sum(pop_linguistically_isolated_over18 * 
                     prop_exp_postprod_1km) /
                 sum(pop_total_over18 * prop_exp_postprod_1km) /
                 (sum(acs_exposure_2015_2019$pop_linguistically_isolated_over18) /
                    sum(acs_exposure_2015_2019$pop_total_over18))),
            
            # voter turnout
            rr_nonvoters           = 
              (sum(pop_nonvoters * prop_exp_postprod_1km, na.rm = T) /
                 sum(pop_total_over18 * prop_exp_postprod_1km) /
                 (sum(acs_exposure_2015_2019$pop_nonvoters, na.rm = T) /
                    sum(acs_exposure_2015_2019$pop_total_over18)))
  )


#----------------------------------------------------------------------------
# generates table with risk ratios for statewide population proportions
# for wells in *postproduction*

# 2005 to 2009 ...........................................................
table_rr_quantiles_2005_2009_postprod <- 
  acs_exposure_2005_2009 %>%
  filter(postprod_wells_0_1km > 0) %>%
  mutate(quantile = as.factor(ntile(postprod_wells_0_1km, 3))) %>%
  group_by(quantile) %>%
  summarize(period              = "2005_2009",
            well_stage          = "postproduction",
            rr_hispanic         = 
              (sum(pop_hispanic * prop_exp_postprod_1km) /
                 sum(pop_total * prop_exp_postprod_1km)) / 
              (sum(acs_exposure_2005_2009$pop_hispanic) /
                 sum(acs_exposure_2005_2009$pop_total)),
            rr_nonhisp_am_indian =
              (sum(pop_nonhisp_am_indian * prop_exp_postprod_1km) /
                 sum(pop_total * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2005_2009$pop_nonhisp_am_indian) /
                 sum(acs_exposure_2005_2009$pop_total)),
            rr_nonhisp_asian    = 
              (sum(pop_nonhisp_asian * prop_exp_postprod_1km) /
                 sum(pop_total * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2005_2009$pop_nonhisp_asian) /
                 sum(acs_exposure_2005_2009$pop_total)),
            rr_nonhisp_black    = 
              (sum(pop_nonhisp_black * prop_exp_postprod_1km) /
                 sum(pop_total * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2005_2009$pop_nonhisp_black) /
                 sum(acs_exposure_2005_2009$pop_total)),
            rr_nonhisp_white    =
              (sum(pop_nonhisp_white * prop_exp_postprod_1km) /
                 sum(pop_total * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2005_2009$pop_nonhisp_white) /
                 sum(acs_exposure_2005_2009$pop_total)),
            rr_nonhisp_other    = 
              (sum(pop_nonhisp_other * prop_exp_postprod_1km) /
                 sum(pop_total * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2005_2009$pop_nonhisp_other) /
                 sum(acs_exposure_2005_2009$pop_total)),
            rr_nonhisp_two_plus = 
              (sum(pop_nonhisp_two_or_more * prop_exp_postprod_1km) /
                 sum(pop_total * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2005_2009$pop_nonhisp_two_or_more) /
                 sum(acs_exposure_2005_2009$pop_total)),
            
            # educational attainment
            rr_educ_less_than_hs = 
              (sum(pop_educ_less_than_hs * prop_exp_postprod_1km) /
                 sum(pop_educ_universe * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2005_2009$pop_educ_less_than_hs) /
                 sum(acs_exposure_2005_2009$pop_educ_universe)),
            rr_educ_hs           =
              (sum(pop_educ_hs * prop_exp_postprod_1km) /
                 sum(pop_educ_universe * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2005_2009$pop_educ_hs) /
                 sum(acs_exposure_2005_2009$pop_educ_universe)),
            rr_educ_more_than_hs = 
              (sum(pop_educ_more_than_hs * prop_exp_postprod_1km) /
                 sum(pop_educ_universe * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2005_2009$pop_educ_more_than_hs) /
                 sum(acs_exposure_2005_2009$pop_educ_universe)),
            
            # poverty
            rr_poverty           = 
              (sum(pop_below_poverty * prop_exp_postprod_1km) /
                 sum(pop_poverty_universe * prop_exp_postprod_1km) /
                 (sum(acs_exposure_2005_2009$pop_below_poverty) /
                    sum(acs_exposure_2005_2009$pop_poverty_universe))),
            
            # renter-occupied households
            rr_renters           = 
              (sum(housing_occupied_renters * prop_exp_postprod_1km) /
                 sum(housing_occupied_all * prop_exp_postprod_1km) /
                 (sum(acs_exposure_2005_2009$housing_occupied_renters) /
                    sum(acs_exposure_2005_2009$housing_occupied_all))),
            
            # linguistically isolated people
            rr_ling_isolated     = 
              (sum(pop_linguistically_isolated_over18 * 
                     prop_exp_postprod_1km) /
                 sum(pop_total_over18 * prop_exp_postprod_1km) /
                 (sum(acs_exposure_2005_2009$pop_linguistically_isolated_over18) /
                    sum(acs_exposure_2005_2009$pop_total_over18))),
            
            # voter turnout
            rr_nonvoters           = NA,  # no voting data for 2005-2009
  )

# 2010 to 2014 ...........................................................
table_rr_quantiles_2010_2014_postprod <- 
  acs_exposure_2010_2014 %>%
  filter(postprod_wells_0_1km > 0) %>%
  mutate(quantile = as.factor(ntile(postprod_wells_0_1km, 3))) %>%
  group_by(quantile) %>%
  summarize(period              = "2010_2014",
            well_stage          = "postproduction",
            rr_hispanic         = 
              (sum(pop_hispanic * prop_exp_postprod_1km) /
                 sum(pop_total * prop_exp_postprod_1km)) / 
              (sum(acs_exposure_2010_2014$pop_hispanic) /
                 sum(acs_exposure_2010_2014$pop_total)),
            rr_nonhisp_am_indian =
              (sum(pop_nonhisp_am_indian * prop_exp_postprod_1km) /
                 sum(pop_total * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2010_2014$pop_nonhisp_am_indian) /
                 sum(acs_exposure_2010_2014$pop_total)),
            rr_nonhisp_asian    = 
              (sum(pop_nonhisp_asian * prop_exp_postprod_1km) /
                 sum(pop_total * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2010_2014$pop_nonhisp_asian) /
                 sum(acs_exposure_2010_2014$pop_total)),
            rr_nonhisp_black    = 
              (sum(pop_nonhisp_black * prop_exp_postprod_1km) /
                 sum(pop_total * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2010_2014$pop_nonhisp_black) /
                 sum(acs_exposure_2010_2014$pop_total)),
            rr_nonhisp_white    =
              (sum(pop_nonhisp_white * prop_exp_postprod_1km) /
                 sum(pop_total * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2010_2014$pop_nonhisp_white) /
                 sum(acs_exposure_2010_2014$pop_total)),
            rr_nonhisp_other    = 
              (sum(pop_nonhisp_other * prop_exp_postprod_1km) /
                 sum(pop_total * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2010_2014$pop_nonhisp_other) /
                 sum(acs_exposure_2010_2014$pop_total)),
            rr_nonhisp_two_plus = 
              (sum(pop_nonhisp_two_or_more * prop_exp_postprod_1km) /
                 sum(pop_total * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2010_2014$pop_nonhisp_two_or_more) /
                 sum(acs_exposure_2010_2014$pop_total)),
            
            # educational attainment
            rr_educ_less_than_hs = 
              (sum(pop_educ_less_than_hs * prop_exp_postprod_1km) /
                 sum(pop_educ_universe * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2010_2014$pop_educ_less_than_hs) /
                 sum(acs_exposure_2010_2014$pop_educ_universe)),
            rr_educ_hs           =
              (sum(pop_educ_hs * prop_exp_postprod_1km) /
                 sum(pop_educ_universe * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2010_2014$pop_educ_hs) /
                 sum(acs_exposure_2010_2014$pop_educ_universe)),
            rr_educ_more_than_hs = 
              (sum(pop_educ_more_than_hs * prop_exp_postprod_1km) /
                 sum(pop_educ_universe * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2010_2014$pop_educ_more_than_hs) /
                 sum(acs_exposure_2010_2014$pop_educ_universe)),
            
            # poverty
            rr_poverty           = 
              (sum(pop_below_poverty * prop_exp_postprod_1km) /
                 sum(pop_poverty_universe * prop_exp_postprod_1km) /
                 (sum(acs_exposure_2010_2014$pop_below_poverty) /
                    sum(acs_exposure_2010_2014$pop_poverty_universe))),
            
            # renter-occupied households
            rr_renters           = 
              (sum(housing_occupied_renters * prop_exp_postprod_1km) /
                 sum(housing_occupied_all * prop_exp_postprod_1km) /
                 (sum(acs_exposure_2010_2014$housing_occupied_renters) /
                    sum(acs_exposure_2010_2014$housing_occupied_all))),
            
            # linguistically isolated people
            rr_ling_isolated     = 
              (sum(pop_linguistically_isolated_over18 * 
                     prop_exp_postprod_1km) /
                 sum(pop_total_over18 * prop_exp_postprod_1km) /
                 (sum(acs_exposure_2010_2014$pop_linguistically_isolated_over18) /
                    sum(acs_exposure_2010_2014$pop_total_over18))),
            
            # voter turnout
            rr_nonvoters           = 
              (sum(pop_nonvoters * prop_exp_postprod_1km, na.rm = T) /
                 sum(pop_total_over18 * prop_exp_postprod_1km) /
                 (sum(acs_exposure_2010_2014$pop_nonvoters, na.rm = T) /
                    sum(acs_exposure_2010_2014$pop_total_over18)))
  )

# 2015 to 2019 ...........................................................
table_rr_quantiles_2015_2019_postprod <- 
  acs_exposure_2015_2019 %>%
  filter(postprod_wells_0_1km > 0) %>%
  mutate(quantile = as.factor(ntile(postprod_wells_0_1km, 3))) %>%
  group_by(quantile) %>%
  summarize(period              = "2015_2019",
            well_stage          = "postproduction",
            rr_hispanic         = 
              (sum(pop_hispanic * prop_exp_postprod_1km) /
                 sum(pop_total * prop_exp_postprod_1km)) / 
              (sum(acs_exposure_2015_2019$pop_hispanic) /
                 sum(acs_exposure_2015_2019$pop_total)),
            rr_nonhisp_am_indian =
              (sum(pop_nonhisp_am_indian * prop_exp_postprod_1km) /
                 sum(pop_total * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2015_2019$pop_nonhisp_am_indian) /
                 sum(acs_exposure_2015_2019$pop_total)),
            rr_nonhisp_asian    = 
              (sum(pop_nonhisp_asian * prop_exp_postprod_1km) /
                 sum(pop_total * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2015_2019$pop_nonhisp_asian) /
                 sum(acs_exposure_2015_2019$pop_total)),
            rr_nonhisp_black    = 
              (sum(pop_nonhisp_black * prop_exp_postprod_1km) /
                 sum(pop_total * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2015_2019$pop_nonhisp_black) /
                 sum(acs_exposure_2015_2019$pop_total)),
            rr_nonhisp_white    =
              (sum(pop_nonhisp_white * prop_exp_postprod_1km) /
                 sum(pop_total * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2015_2019$pop_nonhisp_white) /
                 sum(acs_exposure_2015_2019$pop_total)),
            rr_nonhisp_other    = 
              (sum(pop_nonhisp_other * prop_exp_postprod_1km) /
                 sum(pop_total * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2015_2019$pop_nonhisp_other) /
                 sum(acs_exposure_2015_2019$pop_total)),
            rr_nonhisp_two_plus = 
              (sum(pop_nonhisp_two_or_more * prop_exp_postprod_1km) /
                 sum(pop_total * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2015_2019$pop_nonhisp_two_or_more) /
                 sum(acs_exposure_2015_2019$pop_total)),
            
            # educational attainment
            rr_educ_less_than_hs = 
              (sum(pop_educ_less_than_hs * prop_exp_postprod_1km) /
                 sum(pop_educ_universe * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2015_2019$pop_educ_less_than_hs) /
                 sum(acs_exposure_2015_2019$pop_educ_universe)),
            rr_educ_hs           =
              (sum(pop_educ_hs * prop_exp_postprod_1km) /
                 sum(pop_educ_universe * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2015_2019$pop_educ_hs) /
                 sum(acs_exposure_2015_2019$pop_educ_universe)),
            rr_educ_more_than_hs = 
              (sum(pop_educ_more_than_hs * prop_exp_postprod_1km) /
                 sum(pop_educ_universe * prop_exp_postprod_1km)) /
              (sum(acs_exposure_2015_2019$pop_educ_more_than_hs) /
                 sum(acs_exposure_2015_2019$pop_educ_universe)),
            
            # poverty
            rr_poverty           = 
              (sum(pop_below_poverty * prop_exp_postprod_1km) /
                 sum(pop_poverty_universe * prop_exp_postprod_1km) /
                 (sum(acs_exposure_2015_2019$pop_below_poverty) /
                    sum(acs_exposure_2015_2019$pop_poverty_universe))),
            
            # renter-occupied households
            rr_renters           = 
              (sum(housing_occupied_renters * prop_exp_postprod_1km) /
                 sum(housing_occupied_all * prop_exp_postprod_1km) /
                 (sum(acs_exposure_2015_2019$housing_occupied_renters) /
                    sum(acs_exposure_2015_2019$housing_occupied_all))),
            
            # linguistically isolated people
            rr_ling_isolated     = 
              (sum(pop_linguistically_isolated_over18 * 
                     prop_exp_postprod_1km) /
                 sum(pop_total_over18 * prop_exp_postprod_1km) /
                 (sum(acs_exposure_2015_2019$pop_linguistically_isolated_over18) /
                    sum(acs_exposure_2015_2019$pop_total_over18))),
            
            # voter turnout
            rr_nonvoters           = 
              (sum(pop_nonvoters * prop_exp_postprod_1km, na.rm = T) /
                 sum(pop_total_over18 * prop_exp_postprod_1km) /
                 (sum(acs_exposure_2015_2019$pop_nonvoters, na.rm = T) /
                    sum(acs_exposure_2015_2019$pop_total_over18)))
  )


##--------------------------------------------------------------------------
## binds table components

table_rr_quantiles <- table_rr_quantiles_2005_2009_preprod %>%
  bind_rows(table_rr_quantiles_2010_2014_preprod) %>%
  bind_rows(table_rr_quantiles_2015_2019_preprod) %>%
  bind_rows(table_rr_quantiles_2005_2009_prod) %>%
  bind_rows(table_rr_quantiles_2010_2014_prod) %>%
  bind_rows(table_rr_quantiles_2015_2019_prod) %>%
  bind_rows(table_rr_quantiles_2005_2009_prod_vol) %>%
  bind_rows(table_rr_quantiles_2010_2014_prod_vol) %>%
  bind_rows(table_rr_quantiles_2015_2019_prod_vol) %>%
  bind_rows(table_rr_quantiles_2005_2009_postprod) %>%
  bind_rows(table_rr_quantiles_2010_2014_postprod) %>%
  bind_rows(table_rr_quantiles_2015_2019_postprod)


##--------------------------------------------------------------------------
## exports table
write_csv(table_rr_quantiles, 
          file = "output/results/table_rr_quantiles_county_losangeles.csv")


##============================================================================##

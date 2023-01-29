##============================================================================##
## makes Table 1 - sociodemographic characteristics from each five-year ACS

##---------------------------------------------------------------------------
## sets up environment

# data input
acs_exposure_2015_2019 <-
  readRDS("data/processed/acs_exposure_2005_2019.rds") %>%
  filter(period == "2015_2019")
ca_county_fips <- read_csv("data/raw/us_census/ca_county_fips.csv") %>%
  mutate(county_fips = as.factor(substr(county_fips, 2, 4)),
         county_name = as.factor(county_name))

##---------------------------------------------------------------------------
## assembles tables

table_s06 <- acs_exposure_2015_2019 %>% 
  mutate(county_fips = as.factor(substr(GEOID, 3, 5)),
         pop_exp_preprod  = round((prop_exp_preprod_1km  * pop_total), digits = 0),
         pop_exp_prod     = round((prop_exp_prod_1km     * pop_total), digits = 0),
         pop_exp_postprod = round((prop_exp_postprod_1km * pop_total), digits = 0),
         pop_exp_plugged  = round((prop_exp_plugged_1km  * pop_total), digits = 0)) %>% 
  group_by(county_fips) %>% 
  summarize(n_exp_preprod  = sum(pop_exp_preprod),
            n_exp_prod     = sum(pop_exp_prod),
            n_exp_postprod = sum(pop_exp_postprod),
            n_exp_plugged  = sum(pop_exp_plugged)) %>% 
  left_join(ca_county_fips, by = "county_fips") %>% 
  select(-county_fips)
  

##--------------------------------------------------------------------------
## exports table
write_csv(table_s06, "output/tables/table_s06.csv")

##============================================================================##
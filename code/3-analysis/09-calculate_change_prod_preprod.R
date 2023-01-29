##============================================================================##
## 3.09 - determines the racial/ethnic composition of block groups where 
## the number of preproduction wells and the production volume increased,
## decreased, and stayed the same from 2010-2014 and 2015-2019

#----------------------------------------------------------------------------
# setup

# imports and preps data necessary for this script
data <- readRDS("data/processed/acs_exposure_2005_2019.rds") %>%
  select(c("GEOID", "period", 
           "pop_total", "pop_hispanic", "pop_nonhisp_am_indian",
           "pop_nonhisp_asian", "pop_nonhisp_black", "pop_nonhisp_white",
           "pop_nonhisp_other", "pop_nonhisp_two_or_more", 
           "prod_volume_0_1km", "preprod_wells_0_1km")) %>%
  filter(period %in% c("2010_2014", "2015_2019")) %>%
  mutate(prod_volume_0_1km = ifelse(is.na(prod_volume_0_1km), 0, prod_volume_0_1km),
         preprod_wells_0_1km = ifelse(is.na(preprod_wells_0_1km), 0, preprod_wells_0_1km))

data_2010_2014 <- data %>%
  filter(period == "2010_2014")
data_2015_2019 <- data %>%
  filter(period == "2015_2019")
colnames(data_2010_2014) <- paste(colnames(data_2010_2014), "2010_2014", sep = "_")
colnames(data_2015_2019) <- paste(colnames(data_2015_2019), "2015_2019", sep = "_")

data <- merge(data_2010_2014, data_2015_2019, by.x = "GEOID_2010_2014", by.y = "GEOID_2015_2019") %>%
  mutate(group_prod = ifelse(prod_volume_0_1km_2015_2019 > prod_volume_0_1km_2010_2014, "increased",
                                    ifelse(prod_volume_0_1km_2015_2019 < prod_volume_0_1km_2010_2014, 
                                           "decreased", "same")),
         group_preprod = ifelse(preprod_wells_0_1km_2015_2019 > preprod_wells_0_1km_2010_2014, "increased",
                                       ifelse(preprod_wells_0_1km_2015_2019 < preprod_wells_0_1km_2010_2014, 
                                              "decreased", "same")),
         change_prod = (prod_volume_0_1km_2015_2019-prod_volume_0_1km_2010_2014),
         change_preprod = (preprod_wells_0_1km_2015_2019-preprod_wells_0_1km_2010_2014))

#----------------------------------------------------------------------------
# generates table with demographic characteristics for block groups where
# *preproduction* increased, decreased, or stayed the same
preprod <- data %>%
  select("group_preprod", 
         "pop_total_2015_2019", "pop_hispanic_2015_2019", 
         "pop_nonhisp_am_indian_2015_2019", "pop_nonhisp_asian_2015_2019", 
         "pop_nonhisp_black_2015_2019", "pop_nonhisp_white_2015_2019", 
         "pop_nonhisp_other_2015_2019", "pop_nonhisp_two_or_more_2015_2019") %>%
  mutate(pop_other_2015_2019 = rowSums(cbind(pop_nonhisp_am_indian_2015_2019,
                                             pop_nonhisp_other_2015_2019, 
                                             pop_nonhisp_two_or_more_2015_2019))) %>%
  select(-c(pop_nonhisp_am_indian_2015_2019,
            pop_nonhisp_other_2015_2019, 
            pop_nonhisp_two_or_more_2015_2019)) %>%
  group_by(group_preprod) %>%
  summarise_each(funs(sum)) %>%
  mutate(across(c(2:7), .fns = ~./pop_total_2015_2019)) %>%
  select(-pop_total_2015_2019) %>%
  gather("key", "value", -c(group_preprod)) 
  
# generates table with demographic characteristics for block groups where
# *production* increased, decreased, or stayed the same
prod <- data %>%
  select("group_prod", 
         "pop_total_2015_2019", "pop_hispanic_2015_2019", 
         "pop_nonhisp_am_indian_2015_2019", "pop_nonhisp_asian_2015_2019", 
         "pop_nonhisp_black_2015_2019", "pop_nonhisp_white_2015_2019", 
         "pop_nonhisp_other_2015_2019", "pop_nonhisp_two_or_more_2015_2019") %>%
  mutate(pop_other_2015_2019 = rowSums(cbind(pop_nonhisp_am_indian_2015_2019,
                                             pop_nonhisp_other_2015_2019, 
                                             pop_nonhisp_two_or_more_2015_2019))) %>%
  select(-c(pop_nonhisp_am_indian_2015_2019,
            pop_nonhisp_other_2015_2019, 
            pop_nonhisp_two_or_more_2015_2019)) %>%
  group_by(group_prod) %>%
  summarise_each(funs(sum)) %>%
  mutate(across(c(2:7), .fns = ~./pop_total_2015_2019)) %>%
  select(-pop_total_2015_2019) %>%
  gather("key", "value", -c(group_prod)) 

##--------------------------------------------------------------------------
## exports table
write_csv(preprod, 
          file = "output/results/table_change_preprod_count.csv")
write_csv(prod, 
          file = "output/results/table_change_prod_volume.csv")

##============================================================================##





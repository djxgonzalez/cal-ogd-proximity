##============================================================================##
## 1.06 - Incorporates SES variables

##---------------------------------------------------------------------------
## adds voting data, combined for 2012 and 2016

acs_exposure_2005_2019 <- readRDS("data/processed/acs_exposure_2005_2019.rds")
cal_voting <- # preps voting data; this is available for 2010-2014 and 2015-2019
  read_csv("data/raw/voting_records/CA_blockgroup_wVulnerabilityMetrics.csv") %>%
  mutate(GEOID = as.factor(str_pad(geoid, 12, pad = "0"))) %>%
  select(GEOID, pct_vt1216)
acs_exposure_2005_2019 <- acs_exposure_2005_2019 %>%  # joins data voting data
  left_join(cal_voting, by = "GEOID") %>%
  mutate(voters_quantile = as.factor(ntile(pct_vt1216, 5)))
saveRDS(acs_exposure_2005_2019, 
        "data/processed/acs_exposure_2005_2019.rds")  # export


##---------------------------------------------------------------------------
merged <- merge(voting_2012, voting_2016, by = "BLOCK_GROUP_ID")

# investigate missing block groups
diff_2012 <- setdiff(voting_2012$BLOCK_GROUP_ID, merged$BLOCK_GROUP_ID)
diff_2016 <- setdiff(voting_2016$BLOCK_GROUP_ID, merged$BLOCK_GROUP_ID)

merged <- merged %>%
  mutate(prop_voters = (vot_tot16 + vot_tot12)/(reg_tot16+reg_tot12)) %>%
  select(BLOCK_GROUP_ID, prop_voters)

saveRDS(merged, "data/interim/voting_records.rds")

##============================================================================##
##============================================================================##
## 1.04 - tidies ACS data to make 3 interim ACS datasets

library("stringr")
library("readxl")

# import 2005-2009 data
seq11_2009 <- read_excel("data/raw/us_census/acs/2009/Original/Seq11_edited.xlsx")
seq12_2009 <- read_excel("data/raw/us_census/acs/2009/Original/Seq12_edited.xlsx")
seq13_2009 <- read_excel("data/raw/us_census/acs/2009/Original/Seq13_edited.xlsx")
seq40_2009 <- read_excel("data/raw/us_census/acs/2009/Original/Seq40_edited.xlsx")
seq50_2009 <- read_excel("data/raw/us_census/acs/2009/Original/Seq50_edited.xlsx")
seq53_2009 <- read_excel("data/raw/us_census/acs/2009/Original/Seq53_edited.xlsx")
seq98_2009 <- read_excel("data/raw/us_census/acs/2009/Original/Seq98_edited.xlsx")
seq99_2009 <- read_excel("data/raw/us_census/acs/2009/Original/Seq99_edited.xlsx")
# additions as of 5.24.22
seq42_2009 <- read_excel("data/raw/us_census/acs/2009/Original/Seq42_edited.xlsx")

# import 2010-2014 data
seq3_2014 <- read_excel("data/raw/us_census/acs/2014/Original/Seq3_edited.xlsx")
seq4_2014 <- read_excel("data/raw/us_census/acs/2014/Original/Seq4_edited.xlsx")
seq5_2014 <- read_excel("data/raw/us_census/acs/2014/Original/Seq5_edited.xlsx")
seq42_2014 <- read_excel("data/raw/us_census/acs/2014/Original/Seq42_edited.xlsx")
seq53_2014 <- read_excel("data/raw/us_census/acs/2014/Original/Seq53_edited.xlsx")
seq58_2014 <- read_excel("data/raw/us_census/acs/2014/Original/Seq58_edited.xlsx")
seq104_2014 <- read_excel("data/raw/us_census/acs/2014/Original/Seq104_edited.xlsx")
seq105_2014 <- read_excel("data/raw/us_census/acs/2014/Original/Seq105_edited.xlsx")

# import 2015-2019 data
seq2_2019 <- read_excel("data/raw/us_census/acs/2019/Original/Seq2_edited.xlsx")
seq3_2019 <- read_excel("data/raw/us_census/acs/2019/Original/Seq3_edited.xlsx")
seq4_2019 <- read_excel("data/raw/us_census/acs/2019/Original/Seq4_edited.xlsx")
seq42_2019 <- read_excel("data/raw/us_census/acs/2019/Original/Seq42_edited.xlsx")
seq53_2019 <- read_excel("data/raw/us_census/acs/2019/Original/Seq53_edited.xlsx")
seq58_2019 <- read_excel("data/raw/us_census/acs/2019/Original/Seq58_edited.xlsx")
seq113_2019 <- read_excel("data/raw/us_census/acs/2019/Original/Seq113_edited.xlsx")
seq114_2019 <- read_excel("data/raw/us_census/acs/2019/Original/Seq114_edited.xlsx")
seq2_2019 <- seq2_2019 %>%
  rename("Geography Name" = ...9)

# tidy data with specific columns for detailed tables
source("code/1-data_tidying/1-tidy_acs_data.R")
seq11_2009 <- tidyACSData(seq11_2009, "B01003")
seq12_2009 <- tidyACSData(seq12_2009, "B02001")
seq13_2009a <- tidyACSData(seq13_2009, "B03003")
seq13_2009b <- tidyACSData(seq13_2009, "B03002")
seq40_2009 <- tidyACSData(seq40_2009, "B15002")
seq50_2009 <- tidyACSData(seq50_2009, "B17017")
seq53_2009 <- tidyACSData(seq53_2009, "B19001_")
seq98_2009 <- tidyACSData(seq98_2009, "B25058")
seq99_2009 <- tidyACSData(seq99_2009, "B25070")

seq3_2014 <- tidyACSData(seq3_2014, "B01003")
seq4_2014 <- tidyACSData(seq4_2014, "B02001")
seq5a_2014 <- tidyACSData(seq5_2014, "B03003")
seq5b_2014 <- tidyACSData(seq5_2014, "B03002")
seq42_2014 <- tidyACSData(seq42_2014, "B15002")
seq53_2014 <- tidyACSData(seq53_2014, "B17017")
seq58_2014 <- tidyACSData(seq58_2014, "B19001_")
seq104_2014 <- tidyACSData(seq104_2014, "B25058")
seq105_2014 <- tidyACSData(seq105_2014, "B25070")

seq2_2019 <- tidyACSData(seq2_2019, "B01003")
seq3_2019 <- tidyACSData(seq3_2019, "B02001")
seq4a_2019 <- tidyACSData(seq4_2019, "B03003")
seq4b_2019 <- tidyACSData(seq4_2019, "B03002")
seq42_2019 <- tidyACSData(seq42_2019, "B15002")
seq53_2019 <- tidyACSData(seq53_2019, "B17017")
seq58_2019 <- tidyACSData(seq58_2019, "B19001_")
seq113_2019 <- tidyACSData(seq113_2019, "B25058")
seq114_2019 <- tidyACSData(seq114_2019, "B25070")

# combine datasets for each interval
data_2009 <- merge(seq11_2009, seq12_2009, by = c("LOGRECNO...7", "GEOID", "Geography.Name")) %>%
             merge(seq13_2009a, by = c("LOGRECNO...7", "GEOID", "Geography.Name")) %>%
             merge(seq13_2009b, by = c("LOGRECNO...7", "GEOID", "Geography.Name")) %>%
             merge(seq40_2009, by = c("LOGRECNO...7", "GEOID", "Geography.Name")) %>%
             merge(seq50_2009, by = c("LOGRECNO...7", "GEOID", "Geography.Name")) %>%
             merge(seq53_2009, by = c("LOGRECNO...7", "GEOID", "Geography.Name")) %>%
             merge(seq98_2009, by = c("LOGRECNO...7", "GEOID", "Geography.Name")) %>%
             merge(seq99_2009, by = c("LOGRECNO...7", "GEOID", "Geography.Name"))

data_2014 <- merge(seq3_2014, seq4_2014, by = c("LOGRECNO...7", "GEOID", "Geography.Name")) %>%
             merge(seq5a_2014, by = c("LOGRECNO...7", "GEOID", "Geography.Name")) %>%
             merge(seq5b_2014, by = c("LOGRECNO...7", "GEOID", "Geography.Name")) %>%
             merge(seq42_2014, by = c("LOGRECNO...7", "GEOID", "Geography.Name")) %>%
             merge(seq53_2014, by = c("LOGRECNO...7", "GEOID", "Geography.Name")) %>%
             merge(seq58_2014, by = c("LOGRECNO...7", "GEOID", "Geography.Name")) %>%
             merge(seq104_2014, by = c("LOGRECNO...7", "GEOID", "Geography.Name")) %>%
             merge(seq105_2014, by = c("LOGRECNO...7", "GEOID", "Geography.Name"))

data_2019 <- merge(seq2_2019, seq3_2019, by = c("LOGRECNO...7", "GEOID", "Geography.Name")) %>%
             merge(seq4a_2019, by = c("LOGRECNO...7", "GEOID", "Geography.Name")) %>%
             merge(seq4b_2019, by = c("LOGRECNO...7", "GEOID", "Geography.Name")) %>%
             merge(seq42_2019, by = c("LOGRECNO...7", "GEOID", "Geography.Name")) %>%
             merge(seq53_2019, by = c("LOGRECNO...7", "GEOID", "Geography.Name")) %>%
             merge(seq58_2019, by = c("LOGRECNO...7", "GEOID", "Geography.Name")) %>%
             merge(seq113_2019, by = c("LOGRECNO...7", "GEOID", "Geography.Name")) %>%
             merge(seq114_2019, by = c("LOGRECNO...7", "GEOID", "Geography.Name"))

# rename columns
source("code/1-data_tidying/6-column_rename.R")
data_2009 <- columnRename(data_2009)
data_2014 <- columnRename(data_2014)
data_2019 <- columnRename(data_2019)

#save interim ACS data
saveRDS (data_2009, file = "data/interim/acs_data_2009.rds")
saveRDS (data_2014, file = "data/interim/acs_data_2014.rds")
saveRDS (data_2019, file = "data/interim/acs_data_2019.rds")

##============================================================================##
## import from census

library("tidycensus")
census_api_key("f050313c3434806bd842edd164be92e986e5d8d5",
               install = TRUE)

acs_2015_2019 <- 
  get_acs(geography = "block group",
          )




##============================================================================##
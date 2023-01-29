##============================================================================##
## 1.04.2 - tidies ACS data to make 3 interim ACS datasets; adds additional
## variables obtained between 5.24.2022 and 6.3.2022, including data pulled 
## from census website directly and data obtained through the `tidycensus` API

##---------------------------------------------------------------------------
## setup

library("readxl")
library("stringr")
library("tidycensus")
source("code/1-data_tidying/01-tidy_acs_data.R")

# gets population total by block group and time period from existing dataset
pop_block_group_period <- 
  readRDS("data/processed/acs_exposure_2005_2019.rds") %>%
  select(GEOID, period, pop_total, pct_vt1216)


##---------------------------------------------------------------------------
## For data from ACS 2005-2009, DJXG downloaded the data from the census website 
## and followed CM's method from the other data tidying script to incorporate
## the prepped census excel spreadsheets into the full dataset

# import 2005-2009 data
seq10_2009 <- read_excel("data/raw/us_census/acs/2009/Original/Seq10_edited.xlsx")
seq42_2009 <- read_excel("data/raw/us_census/acs/2009/Original/Seq42_edited.xlsx")
seq96_2009 <- read_excel("data/raw/us_census/acs/2009/Original/Seq96_edited.xlsx")

# tidy data with specific columns for detailed tables
seq10_2009a <- tidyACSData(seq10_2009, "B01001_003") %>%
  mutate(age_under_0_4ys_male = as.numeric(B01001_003)) %>% 
  select(GEOID, age_under_0_4ys_male)
seq10_2009b <- tidyACSData(seq10_2009, "B01001_004") %>%
  mutate(age_under_5_9ys_male = as.numeric(B01001_004)) %>% 
  select(GEOID, age_under_5_9ys_male)
seq10_2009c <- tidyACSData(seq10_2009, "B01001_005") %>%
  mutate(age_under_10_14ys_male = as.numeric(B01001_005)) %>% 
  select(GEOID, age_under_10_14ys_male)
seq10_2009d <- tidyACSData(seq10_2009, "B01001_006") %>%
  mutate(age_under_15_17ys_male = as.numeric(B01001_006)) %>% 
  select(GEOID, age_under_15_17ys_male)
seq10_2009e <- tidyACSData(seq10_2009, "B01001_027") %>%
  mutate(age_under_0_4ys_female = as.numeric(B01001_027)) %>% 
  select(GEOID, age_under_0_4ys_female)
seq10_2009f <- tidyACSData(seq10_2009, "B01001_028") %>%
  mutate(age_under_5_9ys_female = as.numeric(B01001_028)) %>% 
  select(GEOID, age_under_5_9ys_female)
seq10_2009g <- tidyACSData(seq10_2009, "B01001_029") %>%
  mutate(age_under_10_14ys_female = as.numeric(B01001_029)) %>% 
  select(GEOID, age_under_10_14ys_female)
seq10_2009h <- tidyACSData(seq10_2009, "B01001_030") %>%
  mutate(age_under_15_17ys_female = as.numeric(B01001_030)) %>% 
  select(GEOID, age_under_15_17ys_female)

seq42_2009a <- tidyACSData(seq42_2009, "B16004_025") %>%
  mutate(language_18_64yrs_english_only = as.numeric(B16004_025)) %>% 
  select(GEOID, language_18_64yrs_english_only)
seq42_2009b <- tidyACSData(seq42_2009, "B16004_027") %>% 
  mutate(language_18_64yrs_spanish_very_well = as.numeric(B16004_027)) %>%
  select(GEOID, language_18_64yrs_spanish_very_well)
seq42_2009c <- tidyACSData(seq42_2009, "B16004_032") %>% 
  mutate(language_18_64yrs_indoeuro_very_well = as.numeric(B16004_032)) %>%
  select(GEOID, language_18_64yrs_indoeuro_very_well)
seq42_2009d <- tidyACSData(seq42_2009, "B16004_037") %>% 
  mutate(language_18_64yrs_api_very_well = as.numeric(B16004_037)) %>%
  select(GEOID, language_18_64yrs_api_very_well)
seq42_2009e <- tidyACSData(seq42_2009, "B16004_042") %>% 
  mutate(language_18_64yrs_other_very_well = as.numeric(B16004_042)) %>%
  select(GEOID, language_18_64yrs_other_very_well)
seq42_2009f <- tidyACSData(seq42_2009, "B16004_047") %>% 
  mutate(language_over65yrs_english_only = as.numeric(B16004_047)) %>%
  select(GEOID, language_over65yrs_english_only)
seq42_2009g <- tidyACSData(seq42_2009, "B16004_049") %>% 
  mutate(language_over65yrs_spanish_very_well = as.numeric(B16004_049)) %>%
  select(GEOID, language_over65yrs_spanish_very_well)
seq42_2009h <- tidyACSData(seq42_2009, "B16004_054") %>% 
  mutate(language_over65yrs_indoeuro_very_well = as.numeric(B16004_054)) %>%
  select(GEOID, language_over65yrs_indoeuro_very_well)
seq42_2009i <- tidyACSData(seq42_2009, "B16004_059") %>% 
  mutate(language_over65yrs_api_very_well = as.numeric(B16004_059)) %>%
  select(GEOID, language_over65yrs_api_very_well)
seq42_2009j <- tidyACSData(seq42_2009, "B16004_064") %>% 
  mutate(language_over65yrs_other_very_well = as.numeric(B16004_064)) %>%
  select(GEOID, language_over65yrs_other_very_well)

seq96_2009a <- tidyACSData(seq96_2009, "B25003_001") %>% 
  mutate(housing_occupied_all = as.numeric(B25003_001)) %>%
  select(GEOID, housing_occupied_all)
seq96_2009b <- tidyACSData(seq96_2009, "B25003_003") %>% 
  mutate(housing_occupied_renters = as.numeric(B25003_003)) %>%
  select(GEOID, housing_occupied_renters)

# combine datasets for each interval
acs_2005_2009_new_vars <- seq10_2009a %>%
  left_join(seq10_2009b, by = c("GEOID")) %>%
  left_join(seq10_2009c, by = c("GEOID")) %>%
  left_join(seq10_2009d, by = c("GEOID")) %>%
  left_join(seq10_2009e, by = c("GEOID")) %>%
  left_join(seq10_2009f, by = c("GEOID")) %>%
  left_join(seq10_2009g, by = c("GEOID")) %>%
  left_join(seq10_2009h, by = c("GEOID")) %>%
  left_join(seq42_2009a, by = c("GEOID")) %>%
  left_join(seq42_2009b, by = c("GEOID")) %>%
  left_join(seq42_2009c, by = c("GEOID")) %>%
  left_join(seq42_2009d, by = c("GEOID")) %>%
  left_join(seq42_2009e, by = c("GEOID")) %>%
  left_join(seq42_2009f, by = c("GEOID")) %>%
  left_join(seq42_2009g, by = c("GEOID")) %>%
  left_join(seq42_2009h, by = c("GEOID")) %>%
  left_join(seq42_2009i, by = c("GEOID")) %>%
  left_join(seq42_2009j, by = c("GEOID")) %>%
  left_join(seq96_2009a, by = c("GEOID")) %>%
  left_join(seq96_2009b, by = c("GEOID")) %>%
  mutate(GEOID  = substr(GEOID, 8, 19),
         period = "2005_2009") %>%
  # adds total population data from existing ACS dataset we already have
  left_join(pop_block_group_period, by = c("GEOID", "period")) %>%
  # makes variables we'll use in the analysis, step 1
  mutate(pop_total_over18     = 
           pop_total - 
           (age_under_0_4ys_male + age_under_5_9ys_male +
              age_under_10_14ys_male + age_under_15_17ys_male +
              age_under_0_4ys_female + age_under_5_9ys_female +
              age_under_10_14ys_female + age_under_15_17ys_female)) %>% 
  # makes variables we'll use in the analysis, step 2
  mutate(pop_linguistically_isolated_over18 = 
           pop_total_over18 - 
           (language_18_64yrs_english_only +
              language_18_64yrs_spanish_very_well +
              language_18_64yrs_indoeuro_very_well +
              language_18_64yrs_api_very_well +
              language_18_64yrs_other_very_well +
              language_over65yrs_english_only +
              language_over65yrs_spanish_very_well +
              language_over65yrs_indoeuro_very_well +
              language_over65yrs_api_very_well +
              language_over65yrs_other_very_well),
         pop_voters = NA) %>%  # no voting data for 2005-2009
  # selects the variables we need for the analysis
  select(GEOID, period, 
         pop_total_over18, 
         pop_linguistically_isolated_over18,
         pop_voters,
         housing_occupied_all,
         housing_occupied_renters)



##---------------------------------------------------------------------------
## For data from the 2010-2014 and 2015-2019 5-year ACS, DJXG downloaded 
## variables directly from the census API using the `tidycensus` package;
## this was available for more recent ACS and is more streamlined the process
## above (which involved prep work in Excel)

#.........................................................................
# ACS 2010-2014
vars_2010_2014 <- load_variables(2014, "acs5", cache = TRUE)

acs_2010_2014_new_vars <- 
  # gets data from ACS API
  get_acs(geography = "block group",
          state     = "06",  # code for California
          survey    = "acs5",
          year      = 2014, 
          variables =
            # we need a count of adults by block group; to get that, we
            # subtract under 18 population (stratified by sex) from the 
            # total population
            c(age_under_0_4ys_male                  = "B01001_003",
              age_under_5_9ys_male                  = "B01001_004",
              age_under_10_14ys_male                = "B01001_005",
              age_under_15_17ys_male                = "B01001_006",
              age_under_0_4ys_female                = "B01001_027",
              age_under_5_9ys_female                = "B01001_028",
              age_under_10_14ys_female              = "B01001_029",
              age_under_15_17ys_female              = "B01001_030",
              # the universe of households vs. renter-occupied households,
              # to determine the proportion of renters by block group
              housing_occupied_all                  = "B25003_001",
              housing_occupied_renters              = "B25003_003",
              # these are count of ppl who speak non-English languages and
              # speak English "very well"; subtract from all 18-64 year-old
              # adults to get people who don't, i.e., linguistically isolated
              language_18_64yrs_english_only        = "B16004_025",
              language_18_64yrs_spanish_very_well   = "B16004_027",
              language_18_64yrs_indoeuro_very_well  = "B16004_032",
              language_18_64yrs_api_very_well       = "B16004_037",
              language_18_64yrs_other_very_well     = "B16004_042",
              language_over65yrs_english_only       = "B16004_047",
              language_over65yrs_spanish_very_well  = "B16004_049",
              language_over65yrs_indoeuro_very_well = "B16004_054",
              language_over65yrs_api_very_well      = "B16004_059",
              language_over65yrs_other_very_well    = "B16004_064")) %>% 
  select(-moe) %>%  # we don't use margin of error in this study
  pivot_wider(names_from = variable, values_from = c(estimate)) %>% 
  mutate(period = "2010_2014") %>%
  # adds total population data from existing ACS dataset we already have
  left_join(pop_block_group_period, by = c("GEOID", "period")) %>%
  # makes variables we'll use in the analysis, step 1
  mutate(pop_total_over18     = 
           pop_total - 
           (age_under_0_4ys_male + age_under_5_9ys_male +
              age_under_10_14ys_male + age_under_15_17ys_male +
              age_under_0_4ys_female + age_under_5_9ys_female +
              age_under_10_14ys_female + age_under_15_17ys_female)) %>% 
  # makes variables we'll use in the analysis, step 2
  mutate(pop_linguistically_isolated_over18 = 
           pop_total_over18 - 
           (language_18_64yrs_english_only +
              language_18_64yrs_spanish_very_well +
              language_18_64yrs_indoeuro_very_well +
              language_18_64yrs_api_very_well +
              language_18_64yrs_other_very_well +
              language_over65yrs_english_only +
              language_over65yrs_spanish_very_well +
              language_over65yrs_indoeuro_very_well +
              language_over65yrs_api_very_well +
              language_over65yrs_other_very_well),
         pop_voters = (pop_total_over18 * pct_vt1216)) %>% 
  # selects the variables we need for the analysis
  select(GEOID, period, 
         pop_total_over18, 
         pop_linguistically_isolated_over18,
         pop_voters,
         housing_occupied_all,
         housing_occupied_renters)


#.........................................................................
# ACS 2015-2019

acs_2015_2019_new_vars <- 
  # gets data from ACS API
  get_acs(geography = "block group",
          state     = "06",  # code for California
          survey    = "acs5",
          year      = 2019, 
          variables =
            # we need a count of adults by block group; to get that, we
            # subtract under 18 population (stratified by sex) from the 
            # total population
            c(age_under_0_4ys_male                  = "B01001_003",
              age_under_5_9ys_male                  = "B01001_004",
              age_under_10_14ys_male                = "B01001_005",
              age_under_15_17ys_male                = "B01001_006",
              age_under_0_4ys_female                = "B01001_027",
              age_under_5_9ys_female                = "B01001_028",
              age_under_10_14ys_female              = "B01001_029",
              age_under_15_17ys_female              = "B01001_030",
              # the universe of households vs. renter-occupied households,
              # to determine the proportion of renters by block group
              housing_occupied_all                  = "B25003_001",
              housing_occupied_renters              = "B25003_003",
              # these are count of ppl who speak non-English languages and
              # speak English "very well"; subtract from all 18-64 year-old
              # adults to get people who don't, i.e., linguistically isolated
              language_18_64yrs_english_only        = "B16004_025",
              language_18_64yrs_spanish_very_well   = "B16004_027",
              language_18_64yrs_indoeuro_very_well  = "B16004_032",
              language_18_64yrs_api_very_well       = "B16004_037",
              language_18_64yrs_other_very_well     = "B16004_042",
              language_over65yrs_english_only       = "B16004_047",
              language_over65yrs_spanish_very_well  = "B16004_049",
              language_over65yrs_indoeuro_very_well = "B16004_054",
              language_over65yrs_api_very_well      = "B16004_059",
              language_over65yrs_other_very_well    = "B16004_064")) %>% 
  select(-moe) %>%  # we don't use margin of error in this study
  pivot_wider(names_from = variable, values_from = c(estimate)) %>% 
  mutate(period = "2015_2019") %>%
  # adds total population data from existing ACS dataset we already have
  left_join(pop_block_group_period, by = c("GEOID", "period")) %>%
  # makes variables we'll use in the analysis, step 1
  mutate(pop_total_over18     = 
           pop_total - 
           (age_under_0_4ys_male + age_under_5_9ys_male +
              age_under_10_14ys_male + age_under_15_17ys_male +
              age_under_0_4ys_female + age_under_5_9ys_female +
              age_under_10_14ys_female + age_under_15_17ys_female)) %>% 
  # makes variables we'll use in the analysis, step 2
  mutate(pop_linguistically_isolated_over18 = 
           pop_total_over18 - 
           (language_18_64yrs_english_only +
              language_18_64yrs_spanish_very_well +
              language_18_64yrs_indoeuro_very_well +
              language_18_64yrs_api_very_well +
              language_18_64yrs_other_very_well +
              language_over65yrs_english_only +
              language_over65yrs_spanish_very_well +
              language_over65yrs_indoeuro_very_well +
              language_over65yrs_api_very_well +
              language_over65yrs_other_very_well),
         pop_voters = (pop_total_over18 * pct_vt1216)) %>% 
  # selects the variables we need for the analysis
  select(GEOID, period, 
         pop_total_over18, 
         pop_linguistically_isolated_over18,
         pop_voters,
         housing_occupied_all,
         housing_occupied_renters)


##---------------------------------------------------------------------------
## binds processed data for 2005-2009, 2010-2014, and 2015-2019, then joins
## with existing ACS exposure data

# binds new datasets
acs_2005_2019_new_vars <- acs_2005_2009_new_vars %>%
  bind_rows(acs_2010_2014_new_vars) %>% 
  bind_rows(acs_2015_2019_new_vars) %>% 
  select(-pop_voters) %>% 
  mutate(pop_nonvoters = pop_total_over18 * (1 - (pct_vt1216 / 100)))

# joins with existing dataset
acs_exposure_2005_2019b <-
  readRDS("data/processed/acs_exposure_2005_2019.rds") %>% 
  left_join(acs_2005_2019_new_vars, by = c("GEOID", "period")) %>%
  select(-voters_quantile)

# exports data
saveRDS(acs_exposure_2005_2019b,
        "data/processed/acs_exposure_2005_2019.rds")


##============================================================================##

acs_exposure_2005_2019b <- acs_exposure_2005_2019

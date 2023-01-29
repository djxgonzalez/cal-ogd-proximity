##============================================================================##
## makes Table S1 - descriptive stats, wells, each time period

##---------------------------------------------------------------------------
## sets up environment

# attaches packages necessary for this script (and not already in 01-setup.R)
library("flextable")  # or table1? idk
source("code/1-data_tidying/02-tidy_calgem_wells_data.R")

# data input
acs_exposure  <- readRDS("data/processed/acs_exposure_all_years.rds")
wells_interim <- readRDS("data/interim/wells_interim.rds")

##---------------------------------------------------------------------------
## assembles table

tidy_all <- tidyCalgemWellsData2(wells_interim, "1/1/2005", "12/31/2019")
tidy_2009 <- tidyCalgemWellsData2(wells_interim,  "1/1/2005", "12/31/2009")
tidy_2014 <- tidyCalgemWellsData2(wells_interim,  "1/1/2010", "12/31/2014")
tidy_2019 <- tidyCalgemWellsData2(wells_interim,  "1/1/2015", "12/31/2019")

# 5-year preproduction wells (have in preprod_count_bg_5year of acs_expoure data)
preprod_5year <- tapply(acs_exposure$preprod_count_bg_5year, acs_exposure$year,
                        FUN=sum)

# total preproduction wells
preprod_2005_2019 <- tidy_all %>%
  filter(preprod_start_to_end == 1) %>%
  nrow()

# count producing wells (need to use wells_interim data)
prod_2005_2019 <- tidy_all %>%
  filter(prod_start_to_end == 1) %>%
  nrow()
prod_2009 <- tidy_2009 %>%
  filter(prod_start_to_end == 1) %>%
  nrow()
prod_2014 <- tidy_2014 %>%
  filter(prod_start_to_end == 1) %>%
  nrow()
prod_2019 <- tidy_2019 %>%
  filter(prod_start_to_end == 1) %>%
  nrow()

# production volume (have in prod_volume_bg_5year of acs_exposure data)
prod_vol_5year <- tapply(acs_exposure$prod_volume_bg_5year, acs_exposure$year, FUN=sum)
prod_vol_2005_2019 <- sum(prod_vol_5year)
prod_vol_5year <- round(prod_vol_5year, -7)/1000000
prod_vol_2005_2019 <- round(prod_vol_2005_2019, -7)/1000000

#prod_vol_5year <- lapply(prod_vol_5year, signif, 3)
#prod_vol_2005_2019 <- Reduce("+", prod_vol_5year)


# wells where production ended
prod_end_2005_2019 <- wells_interim %>%
  filter(prod_end <= "2019-12-31" & prod_end >= "2005-01-01")
prod_end_2009 <- wells_interim %>%
  filter(prod_end <= "2009-12-31" & prod_end >= "2005-01-01") %>%
  nrow()
prod_end_2014 <- wells_interim %>%
  filter(prod_end <= "2014-12-31" & prod_end >= "2010-01-01") %>%
  nrow()
prod_end_2019 <- wells_interim %>%
  filter(prod_end <= "2019-12-31" & prod_end >= "2015-01-01") %>%
  nrow()
prod_end_2005_2019 <- nrow(prod_end_2005_2019)

# plugged and abandoned wells (well_status is Plugged or PluggedOnly)
plugged_2005_2019 <- tidy_all %>%
  # get wells that weren't producing or in preproduction
  filter(prod_start_to_end != 1 && preprod_start_to_end != 1) %>% 
  filter(well_status %in% c("Plugged", "PluggedOnly")) %>%
  nrow()

table_col1 <- c(preprod_5year[1], prod_2009, prod_vol_5year[1],
                prod_end_2009, '-')
table_col2 <- c(preprod_5year[2], prod_2014, prod_vol_5year[2],
                prod_end_2014, '-')
table_col3 <- c(preprod_5year[3], prod_2019, prod_vol_5year[3],
                prod_end_2019, '-')
table_col4 <- c(preprod_2005_2019, prod_2005_2019, prod_vol_2005_2019,
                prod_end_2005_2019, plugged_2005_2019)

table <- data.frame(cbind(table_col1, table_col2, table_col3, table_col4))
colnames(table) = c("2005-2009", "2010-2014", "2015-2019", "All Years")
rownames(table) = c("Wells spudded or completed (n)", "Wells producing (n)",
                    "Total Production volume (Millions of BOE)",
                    "Wells where production ended (n)",
                    "Plugged and abandoned wells (n)")

table_s1 <- table %>% add_rownames() %>% flextable() 
table_s1
##---------------------------------------------------------------------------
## exports tables
save_as_html(table_s1, path = "output/figures/table_s1.html")


##----------------------------------------------------------------------------##

wells

##============================================================================##
##============================================================================##
## 1.02 - functions to tidy wells data, starting from raw wells data or interim 
## wells datasets

# cleans and prepares raw DOGGR input data for further analysis
tidyCalgemWellsData <- function(wells, di_wells, start, end) {
  # expects start and end in "mm/dd/yyyy" format
  start <- as.Date(start, format = "%m/%d/%Y")
  end <- as.Date(end, format = "%m/%d/%Y")
  
  # captures well coordinates so we can re-join them later
  well_coords <- wells %>%
    dplyr::select(API, Longitude, Latitude) %>%
    rename(api_number = API,
           longitude  = Longitude,
           latitude   = Latitude) %>%
    mutate(api_number = as.factor(api_number))
  
  di_wells <- di_wells %>% 
    mutate(api_number = as.factor(api_number))
  
  # tidies wells data
  wells <- wells %>%
    
    # converts api_number from character to factor
    mutate(api_number = as.factor(API)) %>%
    
    # joins the prod start/end dates and cumulative amnt to the CalGEM wells
    left_join(di_wells, by = "api_number") %>%
    
    # renames lat/long columns
    rename(latitude  = Latitude,
           longitude = Longitude)
  
  # converts dates columns to date class
  wells$date_spudded <-
    wells$SpudDate %>%   # don't use select(), causes error
    gsub(pattern = " 0:00:00", replacement = "") %>%  # drops time
    as.Date(format = "%m/%d/%Y")  # forms date
  wells$date_completed <-  # date of completion
    wells$CompDate %>% 
    gsub(pattern = " 0:00:00", replacement = "") %>%
    as.Date(format = "%m/%d/%Y")
  wells$date_abandoned <-
    wells$AbdDate %>% 
    gsub(pattern = " 0:00:00", replacement = "") %>%
    as.Date(format = "%m/%d/%Y")
  wells$prod_start <- wells$prod_start %>%
    as.Date(format = "%Y-%m-%d")
  wells$prod_end <- wells$prod_end %>%
    as.Date(format = "%Y-%m-%d")
  
  # defines exposure period
  wells <- wells %>%
    
    # assume exposure period starts 7 days before spudding / after completion,
    # or 14 days before completion if no spud date,
    # or 14 days after spudding if no completion date
    
    # duration of development period, in days
    mutate(dev_time   = date_completed - date_spudded) %>%
    
    # beginning of first exposure period (applies to all wells)
    mutate(preprod_exp1_begin = if_else(is.na(date_spudded),
                                        date_completed - 14, # if no spud date
                                        date_spudded - 7),   # if spud date
           # end of first exposure period
           preprod_exp1_end   = if_else(is.na(date_completed),
                                        date_spudded + 14,
                                        date_completed + 7)) %>%
    
    # second exposure period, applies to wells with long development period
    #   defined as more than 180 days between spudding and completion
    mutate(preprod_exp2_begin = if_else(dev_time > 180,
                                        date_completed - 14,
                                        as.Date(NA)),
           preprod_exp2_end   = if_else(dev_time > 180,
                                        date_completed + 14,
                                        as.Date(NA))) %>%
    
    # adds variables with intervals for preproduction periods
    mutate(preprod_exp_interval1 = as.interval(preprod_exp1_end -
                                                 preprod_exp1_begin,
                                               preprod_exp1_begin),
           preprod_exp_interval2 = as.interval(preprod_exp2_end -
                                                 preprod_exp2_begin,
                                               preprod_exp2_begin)) %>%
    
    # keeps wells drilled during the study period; for purposes of
    # this study, the study periods are "start" to "end"; that means
    # either the spud date, completion date, or both are in this period
    mutate(preprod_during_period_int1 = 
             int_overlaps(preprod_exp_interval1, 
                          as.interval(end - start,
                                      start)),
           preprod_during_period_int2 =
             int_overlaps(preprod_exp_interval2, 
                          as.interval(end - start,
                                      start))) %>%
    # converts NAs to 0s
    mutate(preprod_during_period_int1 =
             case_when(is.na(preprod_during_period_int1) ~ 0,  # replaces NAs
                       # below two lines necessary to pass values through
                       preprod_during_period_int1 == 0 ~ 0,
                       preprod_during_period_int1 == 1 ~ 1),
           preprod_during_period_int2 = 
             case_when(is.na(preprod_during_period_int2) ~ 0,  # replaces NAs
                       # below two lines necessary to pass values through
                       preprod_during_period_int2 == 0 ~ 0,
                       preprod_during_period_int2 == 1 ~ 1)) %>%
    
    # makes indicator for whether well was drilled during study period,
    # taking both exposure periods into account
    # also ensures that dates make sense (completion occurs after spudding)
    mutate(preprod_during_period = if_else(preprod_during_period_int1 + 
                                            preprod_during_period_int2 >= 1 
                                          & (date_completed >= date_spudded | 
                                               is.na(date_completed) | 
                                               is.na(date_spudded)),
                                          1, 0)) %>%
    
    # makes indicator for whether well was *producing* during study period
    # duration of production period (days)
    mutate(prod_interval = case_when(!is.na(prod_end) ~ 
                                       # if we have date for prod_end
                                       prod_end - prod_start,
                                     # if missing date for prod_end...
                                     is.na(prod_end) ~ 
                                       # ...cut off at end of study
                                       end - prod_start)) %>%
    
    # beginning of first exposure period (applies to all wells)
    mutate(prod_exp_begin = if_else(is.na(prod_start),
                                    prod_end - 14, # if no production start date
                                    prod_start - 7),   # if production start date present
           # end of first exposure period
           prod_exp_end   = if_else(is.na(prod_end),
                                    prod_start + 14,
                                    prod_end + 7)) %>%
    
    # if 'prod_end' occurs *after* the study period replace with end of study period
    mutate(prod_exp_end = if_else(prod_exp_end > end
                                  & prod_start <= end ,
                                  # if date after end of period and production started before period, replace
                                  end,
                                  # else, keep originial date
                                  prod_exp_end)) %>%
    
    # makes production exposure intervals, the reported start and end dates 
    mutate(prod_exp_interval = as.interval(prod_exp_end - prod_exp_begin, 
                                           prod_exp_begin)) %>%
    
    # makes indicator for whether well was productive during study period,
    # for purposes of this study, the study periods are "start" to 
    # "end"; that means either the spud date, completion date, 
    # or both are in this period
    mutate(prod_during_period =
             int_overlaps(prod_exp_interval, 
                          as.interval(end - start,
                                      start))) %>%
    
    # drops unneeded 'prod_exp_interval' column
    dplyr::select(-prod_exp_interval) %>%
    
    # converts NAs to 0s
    mutate(prod_during_period =
             case_when(is.na(prod_during_period) ~ 0,  # replaces NAs
                       # below two lines necessary to pass values through
                       prod_during_period == 0 ~ 0,
                       prod_during_period == 1 ~ 1)) %>%
    
    # renames necessary additional columns
    mutate(well_status   = Status,
           well_type     = Type,
           field_name    = FieldName,
           district_name = District,
           county_name   = County) %>%
    
    # keeps only variables we need
    dplyr::select(api_number:county_name) %>%
    
    # joins with well coordinates
    left_join(well_coords, by = "api_number")
  
  # returns processed dataset
  return(wells)
}

##---------------------------------------------------------------------------

# cleans and prepares full interim dataset for further analysis, 
# for use with interim dataset that doesn't need di_wells columns
# (this is the datset that already has prod_cumulative, prod_start,
# and prod_end as column names)

tidyCalgemWellsData2 <- function(wells, start, end) {
  # expects start and end in "mm/dd/yyyy" format
  start <- as.Date(start, format = "%m/%d/%Y")
  end <- as.Date(end, format = "%m/%d/%Y")
  
  # captures well coordinates so we can re-join them later
  well_coords <- wells %>%
    dplyr::select(api_number, longitude, latitude) %>%
    mutate(api_number = as.factor(api_number))
  
  # tidies wells data
  wells <- wells %>%
    
    # converts api_number from character to factor
    mutate(api_number = as.factor(api_number)) %>%
    dplyr::select(-c(longitude, latitude))
  
  # converts dates columns to date class
  wells$date_spudded <- wells$date_spudded %>%
    as.Date(format = "%m-%d-%Y")  # forms date
  wells$date_completed <- wells$date_completed %>%
    as.Date(format = "%m-%d-%Y")  # forms date
  wells$prod_start <- wells$prod_start %>%
    as.Date(format = "%m-%d-%Y")  # forms date
  wells$prod_end <- wells$prod_end %>%
    as.Date(format = "%m-%d-%Y")  # forms date
  
  # defines exposure period
  wells <- wells %>%
    # assume exposure period starts 7 days before spudding / after completion,
    # or 14 days before completion if no spud date,
    # or 14 days after spudding if no completion date
    
    # duration of development period, in days
    mutate(dev_time   = date_completed - date_spudded) %>%
    
    # beginning of first exposure period (applies to all wells)
    mutate(preprod_exp1_begin = if_else(is.na(date_spudded),
                                        date_completed - 14, # if no spud date
                                        date_spudded - 7),   # if spud date
           # end of first exposure period
           preprod_exp1_end   = if_else(is.na(date_completed),
                                        date_spudded + 14,
                                        date_completed + 7)) %>%
    
    # second exposure period, applies to wells with long development period
    #   defined as more than 180 days between spudding and completion
    mutate(preprod_exp2_begin = if_else(dev_time > 180,
                                        date_completed - 14,
                                        as.Date(NA)),
           preprod_exp2_end   = if_else(dev_time > 180,
                                        date_completed + 14,
                                        as.Date(NA))) %>%
    
    # adds variables with intervals for preproduction periods
    mutate(preprod_exp_interval1 = as.interval(preprod_exp1_end -
                                                 preprod_exp1_begin,
                                               preprod_exp1_begin),
           preprod_exp_interval2 = as.interval(preprod_exp2_end -
                                                 preprod_exp2_begin,
                                               preprod_exp2_begin)) %>%
    
    # keeps wells drilled during the study period; for purposes of
    # this study, the study periods are "start" to "end"; that means
    # either the spud date, completion date, or both are in this period
    mutate(preprod_during_period_int1 = 
             int_overlaps(preprod_exp_interval1, 
                          as.interval(end - start,
                                      start)),
           preprod_during_period_int2 =
             int_overlaps(preprod_exp_interval2, 
                          as.interval(end - start,
                                      start))) %>%
    # converts NAs to 0s
    mutate(preprod_during_period_int1 =
             case_when(is.na(preprod_during_period_int1) ~ 0,  # replaces NAs
                       # below two lines necessary to pass values through
                       preprod_during_period_int1 == 0 ~ 0,
                       preprod_during_period_int1 == 1 ~ 1),
           preprod_during_period_int2 = 
             case_when(is.na(preprod_during_period_int2) ~ 0,  # replaces NAs
                       # below two lines necessary to pass values through
                       preprod_during_period_int2 == 0 ~ 0,
                       preprod_during_period_int2 == 1 ~ 1)) %>%
    
    # makes indicator for whether well was drilled during study period,
    # taking both exposure periods into account
    # also ensures that dates make sense (completion occurs after spudding)
    mutate(preprod_during_period = if_else(preprod_during_period_int1 + 
                                            preprod_during_period_int2 >= 1 
                                          & (date_completed >= date_spudded | 
                                               is.na(date_completed) | 
                                               is.na(date_spudded)),
                                          1, 0)) %>%
    
    # makes indicator for whether well was *producing* during study period
    # duration of production period (days)
    mutate(prod_interval = case_when(!is.na(prod_end) ~ 
                                       # if we have date for prod_end
                                       prod_end - prod_start,
                                     # if missing date for prod_end...
                                     is.na(prod_end) ~ 
                                       # ...cut off at end of study
                                       end - prod_start)) %>%
    
    # beginning of first exposure period (applies to all wells)
    mutate(prod_exp_begin = if_else(is.na(prod_start),
                                    prod_end - 14,   # if no prod start date
                                    prod_start - 7), # if prod start date present
           # end of first exposure period
           prod_exp_end   = if_else(is.na(prod_end),
                                    prod_start + 14,
                                    prod_end + 7)) %>%
    
    # if 'prod_end' occurs *after* the study period replace w/ study period end
    mutate(prod_exp_end = if_else(prod_exp_end > end
                                  & prod_start <= end,
                                  # if date after end of period and production 
                                  # started before period, replace
                                  end,
                                  # else, keep original date
                                  prod_exp_end)) %>%
    
    # makes production exposure intervals, the reported start and end dates 
    mutate(prod_exp_interval = as.interval(prod_exp_end - prod_exp_begin, 
                                           prod_exp_begin)) %>%
    # makes indicator for whether well was in production during study period
    mutate(prod_during_period =
             int_overlaps(prod_exp_interval, 
                          as.interval(end - start,
                                      start))) %>%
    dplyr::select(-prod_exp_interval) %>%
    # converts NAs to 0s
    mutate(prod_during_period =
             case_when(is.na(prod_during_period) ~ 0,  # replaces NAs
                       # below two lines necessary to pass values through
                       prod_during_period == 0 ~ 0,
                       prod_during_period == 1 ~ 1)) %>%
    
    # makes indicator for whether well was in postproduction during study period
    mutate(postprod_during_period =
             postprod_start %within% as.interval(end - start, start)) %>%
             #int_overlaps(postprod_start, as.interval(end - start, start))) %>%
    # converts NAs to 0s
    mutate(postprod_during_period =
             case_when(is.na(prod_during_period) ~ 0,  # replaces NAs
                       # below two lines necessary to pass values through
                       postprod_during_period == 0 ~ 0,
                       postprod_during_period == 1 ~ 1)) %>%
    
    # joins with well coordinates
    left_join(well_coords, by = "api_number")
  
  # returns processed dataset
  return(wells)
}

##============================================================================##
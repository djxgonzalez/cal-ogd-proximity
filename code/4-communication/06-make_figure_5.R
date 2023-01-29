##============================================================================##
## generates figure 5

output <- readRDS("data/processed/acs_exposure_all_years.rds")

# get all wells with some preprod in 2005-2009
filtered_preprod <- output %>%
  filter(year == 2009) %>%
  mutate(preprod_count_1km_5year =
           preprod_count_0to762m_5year + preprod_count_762to1km_5year)

filtered_preprod <- filtered_preprod %>%
  filter(!is.na(preprod_count_1km_5year) & preprod_count_1km_5year != 0) %>%
  select(c(1, 2, 325:387))

ggplot(filtered_preprod) +
  geom_point(aes(x = preprod_count_1km_5year, 
                 y = (filtered_preprod$'Total Population' - 
                        filtered_preprod$"Not Hispanic or Latino: White Alone")/
                   filtered_preprod$'Total Population')) +
  geom_smooth(method="lm")

# get all wells with some prod in 2005-2009
filtered_prod <- output %>%
  filter(year == 2009) %>%
  mutate(prod_volume_1km_5year = 
           prod_volume_0to762m_5year + prod_volume_762to1km_5year)

filtered_prod <- filtered_prod %>%
  filter(!is.na(prod_volume_1km_5year) & prod_volume_1km_5year != 0) %>%
  select(c(1, 2, 325:387))

ggplot(filtered_prod) +
  geom_point(aes(x = log10(prod_volume_1km_5year),
                 y = (filtered_prod$'Total Population' - 
                        filtered_prod$"Not Hispanic or Latino: White Alone")/
                   filtered_prod$'Total Population'), alpha = 0.2)

# in dec 10 meeting
hist(filtered_preprod$preprod_count_1km_5year, breaks = 125)

filtered_preprod <- filtered_preprod %>%
  mutate(group = ifelse(preprod_count_1km_5year == 1, 1,
                        ifelse(preprod_count_1km_5year <= 20, 2, 3)))

ggplot(filtered_preprod, 
       aes(x = as.factor(filtered_preprod$group), 
           y = filtered_preprod$'Hispanic or Latino' / 
             filtered_preprod$'Total Population')) +
  geom_boxplot() +
  geom_jitter(alpha = .2) 

ggplot(filtered_preprod, aes(y = filtered_preprod$'Hispanic or Latino' / 
                               filtered_preprod$'Total Population', 
                             fill = as.factor(group))) +
  geom_boxplot()

# Get statewide proportion of non-white, non-hispanic?
# Need to convey that the number of preproduction wells in bg with high preproduction 
#   is literally as many wells as 100 of the 1-well bg. Could do a cut at the 50% of 
#   preprod wells mark?
# Make plot that David drew in teh slack (stacked)
#   Divide x axis by 25s and the 1, 2-20, 21+

# for prod, 1-99,999, 100,000-10,000,000-1, +


##============================================================================##
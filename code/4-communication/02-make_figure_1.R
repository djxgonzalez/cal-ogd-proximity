##============================================================================##
## imports raw and processed data, preps data as needed, and generates Figure 1

##---------------------------------------------------------------------------
## sets up environment

library("ggspatial")

# data input ...................................... ......................
acs_exposure_all_years <- readRDS("data/processed/acs_exposure_all_years.rds")
cal_counties <- st_read("data/raw/us_census/admin_shp/CA_counties.shp") %>%
  st_transform(crs_nad83)
calgem_prod_monthly <- readRDS("data/interim/calgem_prod_monthly_monthly.rds")
price_midway_sunset <- read_csv("data/raw/us_eia/midway_sunset_usd_barrel.csv")
wells_interim <- readRDS("data/interim/wells_interim.rds") 

##---------------------------------------------------------------------------
## data prep

#.........................................................................
# makes layers for figure components

# data for figure 1a
cal_boundary <- cal_counties %>%
  st_union()
lyr_wells_all_1km <- wells_interim %>%
  filter(latitude > 0) %>%
  filter(well_status %in% c("Active", "New", "Idle", "Plugged", "PluggedOnly",
                            "Unknown")) %>% 
  select(longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs    = crs_nad83) %>%
  st_transform(crs_projected) %>%
  st_buffer(dist = 1000) %>%  # distance in meters
  st_union() %>%
  st_transform(crs_nad83) %>%
  st_intersection(cal_boundary)
lyr_wells_all_3km <- wells_interim %>%
  filter(latitude > 0) %>%
  filter(well_status %in% c("Active", "New", "Idle", "Plugged", "PluggedOnly",
                            "Unknown")) %>% 
  select(longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs    = crs_nad83) %>%
  st_transform(crs_projected) %>%
  st_buffer(dist = 3000) %>%  # distance in meters
  st_union() %>%
  st_transform(crs_nad83) %>%
  st_intersection(cal_boundary)
lyr_wells_all_10km <- wells_interim %>%
  filter(latitude > 0) %>%
  filter(well_status %in% c("Active", "New", "Idle", "Plugged", "PluggedOnly",
                            "Unknown")) %>% 
  select(longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs    = crs_nad83) %>%
  st_transform(crs_projected) %>%
  st_buffer(dist = 10000) %>%  # distance in meters
  st_union() %>%
  st_transform(crs_nad83) %>%
  st_intersection(cal_boundary)

# data for figure 1c
wells_spudded <- wells_interim %>%
  mutate(month_year = paste(month(date_spudded), "01",
                            year(date_spudded),  sep = "/")) %>%
  mutate(month_year = as.Date(month_year, format = "%m/%d/%Y")) %>%
  filter(month_year >= as.Date("2005-01-01") &  
           month_year <= as.Date("2019-12-31")) %>%
  mutate(year = year(month_year)) %>%
  group_by(year) %>%
  summarize(Spudded = n())
wells_completed <- wells_interim %>%
  mutate(month_year = paste(month(date_completed), "01",
                            year(date_completed),  sep = "/")) %>%
  mutate(month_year = as.Date(month_year, format = "%m/%d/%Y")) %>%
  filter(month_year >= as.Date("2005-01-01") &  
           month_year <= as.Date("2019-12-31")) %>% 
  mutate(year = year(month_year)) %>%
  group_by(year) %>%
  summarize(Completed = n())
data_figure_1c <- wells_spudded %>% left_join(wells_completed) %>%
  pivot_longer(cols = Spudded:Completed)#, names_to = "event")

# data for figure 1d
data_figure_1d <- calgem_prod_monthly %>%
  mutate(year = year(prod_month_year)) %>%
  filter(year >= 2005) %>%
  mutate(year = as.factor(year)) %>%
  group_by(year) %>%
  summarize(total_oil_gas = sum(total_oil_gas_produced, na.rm = TRUE))


##---------------------------------------------------------------------------
## generates and exports figure panels

#.........................................................................
# Figure 1a - map of 1 km, 3km,  and 10 km buffers around all wells at any
# stage of production

figure_1a <- ggplot() +
  geom_sf(data = cal_counties, fill = NA, color = "#dedede", lwd = 0.1) +
  geom_sf(data = lyr_wells_all_10km,
          aes(alpha = 0.2), fill = "#000000", color = NA) +
  geom_sf(data = lyr_wells_all_3km,
          aes(alpha = 0.4), fill = "#000000", color = NA) +
    geom_sf(data = lyr_wells_all_1km,
          aes(alpha = 0.6), fill = "#b2182b", color = NA) +
  geom_sf(data = st_intersection(cal_counties, lyr_wells_all_10km),
          fill = NA, color = "white", lwd = 0.1) +
  geom_sf(data = cal_boundary, fill = NA, color = "black", lwd = 0.7) +
  theme_void() +
  theme(panel.background = element_rect(fill  = "white", color = "white"),
        panel.grid       = element_line(color = "white"),
        legend.position = "none")
ggsave(filename = "figure_1a.png", plot = figure_1a, device = "png",
       height = 7, width = 6, path = "output/figures/components")


#.........................................................................
# Figure 1b - price of barrel of oil at Midway-Sunset

figure_1b <- price_midway_sunset %>%
  mutate(date = paste("01-", date, sep = "")) %>%
  mutate(date = as.Date(date, format = "%d-%b-%Y")) %>%
  filter(date >= "2005-01-01") %>%
  ggplot() +
  geom_line(aes(date, midway_sunset_usd_barrel)) +
  ylim(0, 122) +
  labs(x = "", y = "") +
  theme_classic() +
  theme(axis.line.x  = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y  = element_blank())
ggsave(filename = "figure_1b.png", plot = figure_1b, device = "png",  # export
       height = 1.5, width = 5.8, path = "output/figures/components/")


#.........................................................................
# Figure 1c - count of wells in preproduction (spudded or com)

figure_1c <- data_figure_1c %>%
  ggplot() + 
  geom_bar(aes(year, value, fill = name), 
           stat = "identity", alpha = 0.8) + 
  scale_fill_manual(values = c("#FF7F00", "#FFCFA0")) +
  labs(x = "", y = "") + 
  theme_classic() +
  theme(axis.line.x  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.text.y  = element_blank(),
        legend.position = "none")
ggsave(filename = "figure_1c.png", plot = figure_1c, device = "png",
       height = 1.5, width = 5.8, path = "output/figures/components")


#.........................................................................
# Figure 1d - sum of oil/gas production by year

figure_1d <- data_figure_1d %>%
  ggplot() +
  geom_bar(aes(year, total_oil_gas), 
           fill = "#8A2BE2", stat = "identity", alpha = 0.8) +
  labs(x = "", y = "") + 
  theme_classic() +
  theme(axis.line.x  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.text.y  = element_blank())
ggsave(filename = "figure_1d.png", plot = figure_1d, device = "png",
       height = 1.5, width = 5.8, path = "output/figures/components/")


#.........................................................................
# Figure 1e - count of wells entering postproduction (by year)

figure_1e <- wells_interim %>%
  mutate(year = year(postprod_start)) %>%
  filter(year %in% c(2005:2019)) %>% 
  mutate(year = as.factor(year)) %>%
  group_by(year) %>%
  summarize(n = n()) %>%
  mutate(year = as.numeric(year)) %>%
  ggplot() + 
  geom_bar(aes(year, n), 
           stat = "identity", alpha = 0.8) + 
  labs(x = "", y = "") + 
  theme_classic() +
  theme(axis.line.x  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.text.y  = element_blank(),
        legend.position = "none")
figure_1e
ggsave(filename = "figure_1e.png", plot = figure_1e, device = "png",
       height = 1, width = 5.8, path = "output/figures/components")

##============================================================================##
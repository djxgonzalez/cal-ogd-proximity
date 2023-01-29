# uses acs_joined_2009_renamed,acs_joined_2014_renamed, acs_joined_2019_renamed from 6-column_rename in data_tidying
# can be run directly after getting acs_joined_year_renamed, either with wells in the same year range or different year range than the acs data

# Make dataset with race/ethnicity variables and the distance information
selected_2009 <- acs_joined_2009_renamed %>%
  select(c(4:15, 41:45))
selected_2014 <- acs_joined_2014_renamed %>%
  select(c(4:15, 41:45))
selected_2019 <- acs_joined_2019_renamed %>%
  select(c(4:15, 41:45))

#This is formula option 1 (sum of the number of people of x race or ethnicity exposed in tracts / sum of the number of people of x race or ethnicity)
# Gives you the x in "x proportion of people of __ race/ethnicity are near wells"
# get_762m_2009_perc <- function(x) sum(x*selected_2009$buffer_overlap_762m)/sum(x)
# get_1km_2009_perc <- function(x) sum(x*selected_2009$buffer_overlap_1km)/sum(x)
# get_3km_2009_perc <- function(x) sum(x*selected_2009$buffer_overlap_3km)/sum(x)
# get_5km_2009_perc <- function(x) sum(x*selected_2009$buffer_overlap_5km)/sum(x)
# get_10km_2009_perc <- function(x) sum(x*selected_2009$buffer_overlap_10km)/sum(x)

# Option 2 (gives you the x in "x proportion of people near wells are __ race or ethnicity")
#David! Great news! Figured out the function :)
get_perc <- function(x, data, var) {
  sum(x * data[var]) / sum(data["Total Population"] * data[var])
}

# gives the x in "x proportion of people outside the radii of wells are __ race or ethnicity"
get_pop_perc <- function(x, data, var) {
  sum(x * (1 - data[var])) / sum(data["Total Population"] * (1 - data[var]))
}
  
# fyi, and no biggie, but I like to  keep lines of code to 80 char for readability
perc_2009_762m <- lapply(selected_2009[1:12],  
                         get_perc, 
                         selected_2009, 
                         "buffer_overlap_762m") %>%  # piping for readability, too
  data.frame()
perc_2014_762m <- lapply(selected_2014[1:12], get_perc,  selected_2014, 
                         "buffer_overlap_762m") %>%
  data.frame()
perc_2019_762m <- data.frame(lapply(selected_2019[1:12], get_perc, selected_2019, "buffer_overlap_762m"))

perc_2009_1km <- data.frame(lapply(selected_2009[1:12], get_perc, selected_2009, "buffer_overlap_1km"))
perc_2014_1km <- data.frame(lapply(selected_2014[1:12], get_perc, selected_2014, "buffer_overlap_1km"))
perc_2019_1km <- data.frame(lapply(selected_2019[1:12], get_perc, selected_2019, "buffer_overlap_1km"))

perc_2009_3km <- data.frame(lapply(selected_2009[1:12], get_perc, selected_2009, "buffer_overlap_3km"))
perc_2014_3km <- data.frame(lapply(selected_2014[1:12], get_perc, selected_2014, "buffer_overlap_3km"))
perc_2019_3km <- data.frame(lapply(selected_2019[1:12], get_perc, selected_2019, "buffer_overlap_3km"))

perc_2009_5km <- data.frame(lapply(selected_2009[1:12], get_perc, selected_2009, "buffer_overlap_5km"))
perc_2014_5km <- data.frame(lapply(selected_2014[1:12], get_perc, selected_2014, "buffer_overlap_5km"))
perc_2019_5km <- data.frame(lapply(selected_2019[1:12], get_perc, selected_2019, "buffer_overlap_5km"))

perc_2009_10km <- data.frame(lapply(selected_2009[1:12], get_perc, selected_2009, "buffer_overlap_10km"))
perc_2014_10km <- data.frame(lapply(selected_2014[1:12], get_perc, selected_2014, "buffer_overlap_10km"))
perc_2019_10km <- data.frame(lapply(selected_2019[1:12], get_perc, selected_2019, "buffer_overlap_10km"))


pop_perc_2009_762m <- data.frame(lapply(selected_2009[1:12], get_pop_perc, selected_2009, "buffer_overlap_762m"))
pop_perc_2014_762m <- data.frame(lapply(selected_2014[1:12], get_pop_perc, selected_2014, "buffer_overlap_762m"))
pop_perc_2019_762m <- data.frame(lapply(selected_2019[1:12], get_pop_perc, selected_2019, "buffer_overlap_762m"))

pop_perc_2009_1km <- data.frame(lapply(selected_2009[1:12], get_pop_perc, selected_2009, "buffer_overlap_1km"))
pop_perc_2014_1km <- data.frame(lapply(selected_2014[1:12], get_pop_perc, selected_2014, "buffer_overlap_1km"))
pop_perc_2019_1km <- data.frame(lapply(selected_2019[1:12], get_pop_perc, selected_2019, "buffer_overlap_1km"))

pop_perc_2009_3km <- data.frame(lapply(selected_2009[1:12], get_pop_perc, selected_2009, "buffer_overlap_3km"))
pop_perc_2014_3km <- data.frame(lapply(selected_2014[1:12], get_pop_perc, selected_2014, "buffer_overlap_3km"))
pop_perc_2019_3km <- data.frame(lapply(selected_2019[1:12], get_pop_perc, selected_2019, "buffer_overlap_3km"))

pop_perc_2009_5km <- data.frame(lapply(selected_2009[1:12], get_pop_perc, selected_2009, "buffer_overlap_5km"))
pop_perc_2014_5km <- data.frame(lapply(selected_2014[1:12], get_pop_perc, selected_2014, "buffer_overlap_5km"))
pop_perc_2019_5km <- data.frame(lapply(selected_2019[1:12], get_pop_perc, selected_2019, "buffer_overlap_5km"))

pop_perc_2009_10km <- data.frame(lapply(selected_2009[1:12], get_pop_perc, selected_2009, "buffer_overlap_10km"))
pop_perc_2014_10km <- data.frame(lapply(selected_2014[1:12], get_pop_perc, selected_2014, "buffer_overlap_10km"))
pop_perc_2019_10km <- data.frame(lapply(selected_2019[1:12], get_pop_perc, selected_2019, "buffer_overlap_10km"))



perc_762m <- rbind(perc_2009_762m, perc_2014_762m, perc_2019_762m) %>%
  mutate(year = c(2009, 2014, 2019))

perc_1km <- rbind(perc_2009_1km, perc_2014_1km, perc_2019_1km) %>%
  mutate(year = c(2009, 2014, 2019))

perc_3km <- rbind(perc_2009_3km, perc_2014_3km, perc_2019_3km) %>%
  mutate(year = c(2009, 2014, 2019))

perc_5km <- rbind(perc_2009_5km, perc_2014_5km, perc_2019_5km) %>%
  mutate(year = c(2009, 2014, 2019))

perc_10km <- rbind(perc_2009_10km, perc_2014_10km, perc_2019_10km) %>%
  mutate(year = c(2009, 2014, 2019))

pop_perc_762m <- rbind(pop_perc_2009_762m, pop_perc_2014_762m, pop_perc_2019_762m) %>%
  mutate(year = c(2009, 2014, 2019))

pop_perc_1km <- rbind(pop_perc_2009_1km, pop_perc_2014_1km, pop_perc_2019_1km) %>%
  mutate(year = c(2009, 2014, 2019))

pop_perc_3km <- rbind(pop_perc_2009_3km, pop_perc_2014_3km, pop_perc_2019_3km) %>%
  mutate(year = c(2009, 2014, 2019))

pop_perc_5km <- rbind(pop_perc_2009_5km, pop_perc_2014_5km, pop_perc_2019_5km) %>%
  mutate(year = c(2009, 2014, 2019))

pop_perc_10km <- rbind(pop_perc_2009_10km, pop_perc_2014_10km, pop_perc_2019_10km) %>%
  mutate(year = c(2009, 2014, 2019))


# make plots (dotted line is proportion of the population outside radius that is the race/ethnicity,
# solid line is proportion of the population inside radius that is the race/ethnicity)

melted_perc_762m <- melt(perc_762m, id.vars="year")
melted_pop_perc_762m <- melt(pop_perc_762m, id.vars="year")
ggplot(melted_perc_762m) +
  geom_line(aes(x = year, y = value, group = variable, color = variable)) +
  geom_line(data = melted_pop_perc_762m, aes(x = year, y = value, group = variable, color = variable), linetype = "dashed") +
  ggtitle ("Radius 762 m")


melted_perc_1km <- melt(perc_1km, id.vars="year")
melted_pop_perc_1km <- melt(pop_perc_1km, id.vars="year")
ggplot(melted_perc_1km) +
  geom_line(aes(x = year, y = value, group = variable, color = variable))+
  geom_line(data = melted_pop_perc_1km, aes(x = year, y = value, group = variable, color = variable), linetype = "dashed") +
  ggtitle ("Radius 1 km")


melted_perc_3km <- melt(perc_3km, id.vars="year")
melted_pop_perc_3km <- melt(pop_perc_3km, id.vars="year")
ggplot(melted_perc_3km) +
  geom_line(aes(x = year, y = value, group = variable, color = variable)) +
  geom_line(data = melted_pop_perc_3km, aes(x = year, y = value, group = variable, color = variable), linetype = "dashed") +
  ggtitle ("Radius 3 km")


melted_perc_5km <- melt(perc_5km, id.vars="year")
melted_pop_perc_5km <- melt(pop_perc_5km, id.vars="year")
ggplot(melted_perc_5km) +
  geom_line(aes(x = year, y = value, group = variable, color = variable)) +
  geom_line(data = melted_pop_perc_5km, aes(x = year, y = value, group = variable, color = variable), linetype = "dashed") +
  ggtitle ("Radius 5 km")

melted_perc_10km <- melt(perc_10km, id.vars="year")
melted_pop_perc_10km <- melt(pop_perc_5km, id.vars="year")
ggplot(melted_perc_10km) +
  geom_line(aes(x = year, y = value, group = variable, color = variable)) +
  geom_line(data = melted_pop_perc_10km, aes(x = year, y = value, group = variable, color = variable), linetype = "dashed") +
  ggtitle ("Radius 10 km")


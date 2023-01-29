#============================================================================##
## makes Figure S5 - Demographics of people in block groups where production
## volume increased, decreased, or stayed the same.

##---------------------------------------------------------------------------
## sets up environment

# data input
prod <-
  read_csv("output/results/table_change_prod_volume.csv")

# reorder factors
prod$key = factor(prod$key, levels = c("pop_other_2015_2019",
                                       "pop_nonhisp_asian_2015_2019", "pop_nonhisp_black_2015_2019", 
                                       "pop_nonhisp_white_2015_2019", "pop_hispanic_2015_2019"))

prod$group_prod = factor(prod$group_prod, 
                         levels = c("decreased", "same", "increased"))

# create labels
prod <-
  prod %>%
  arrange(desc(key)) %>%
  mutate(y_label = cumsum(value) - 0.5 * value) # get height for labels in center of bars

##---------------------------------------------------------------------------
## generates figure components
figure_s5 <- 
  ggplot(prod, aes(x = group_prod, y = value, group = key, fill = key))+ 
  geom_col() +
  xlab("Change in production Well Count (2010-2014 to 2015-2019)")+
  ylab("Proportion of People (2019)") +
  ylim(0, 1) +
  scale_fill_manual(name = "", values = c("#EF5B5B", "#A4A9AD", "#276FBF",
                                          "#20A39E", "#FFBA49"),
                    labels = c("Other", "Non-Hispanic Asian", 
                               "Non-Hispanic Black",
                               "Non-Hispanic White", "Hispanic")) +
  geom_text(aes(y = y_label, label = round(value, 2)), colour = "white") +
  theme_classic()
figure_s5

##---------------------------------------------------------------------------
## exports figure
ggsave(filename = "figure_s05.png", plot = figure_s5, device = "png",
       width = 5.5, height = 6, path = "output/figures")

##----------------------------------------------------------------------------##

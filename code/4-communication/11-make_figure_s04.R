#============================================================================##
## makes Figure S4 - Demographics of people in block groups where preproduction
## well count increased, decreased, or stayed the same.

##---------------------------------------------------------------------------
## sets up environment

# data input
preprod <-
  read_csv("output/results/table_change_preprod_count.csv") %>%
  group_by(group_preprod) 

# reorder factors
preprod$key = factor(preprod$key, levels = c("pop_other_2015_2019",
  "pop_nonhisp_asian_2015_2019", "pop_nonhisp_black_2015_2019", 
  "pop_nonhisp_white_2015_2019", "pop_hispanic_2015_2019"))

preprod$group_preprod = factor(preprod$group_preprod, 
                     levels = c("decreased", "same", "increased"))

# create labels
preprod <-
  preprod %>%
  arrange(desc(key)) %>%
  mutate(y_label = cumsum(value) - 0.5 * value) # get height for labels in center of bars

##---------------------------------------------------------------------------
## generates figure components
figure_s4 <- ggplot(preprod, aes(x = group_preprod, y = value, group = key, 
                 fill = key))+ 
  geom_col() +
  xlab("Change in Preproduction Well Count (2010-2014 to 2015-2019)")+
  ylab("Proportion of People (2019)") +
  scale_fill_manual(name = "", values = c("#EF5B5B", "#A4A9AD", "#276FBF", "#20A39E", "#FFBA49"),
                    labels = c("Other", "Non-Hispanic Asian", "Non-Hispanic Black",
                               "Non-Hispanic White", "Hispanic")) +
  geom_text(aes(y = y_label, label = round(value, 2)), colour = "white") +
  theme_classic()
figure_s4

##---------------------------------------------------------------------------
## exports figure

ggsave(filename = "figure_s04.png", plot = figure_s4, device = "png",
       width = 5.5, height = 6, path = "output/figures")

##----------------------------------------------------------------------------##

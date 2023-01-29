##============================================================================##
## 4.13 - generates Figure S6, like Fig. 3 but restricted to Los Angeles County

#----------------------------------------------------------------------------
# setup

# imports packages necessary for this script
# library("grid")
# library("ggpubr")
# library("reshape2")

# data input and prep
table_rr <- read_csv("output/results/table_rr_county_losangeles.csv") %>%
  pivot_longer(cols         = rr_hispanic:rr_nonvoters,
               names_prefix = "rr_",
               names_to     = "group",
               values_to    = "rr") %>%
  mutate(well_stage = 
           case_when(well_stage == "preproduction"  ~ "1 preproduction",
                     well_stage == "production"     ~ "2 production",
                     well_stage == "postproduction" ~ "3 postproduction"))


#----------------------------------------------------------------------------
# assembles panels

# panel A - race/ethnicity
figure_s6a <- table_rr %>%
  filter(well_stage != "plugged") %>%
  filter(group %in% c("hispanic", "nonhisp_am_indian", "nonhisp_asian",
                      "nonhisp_black", "nonhisp_white")) %>%
  ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#000000") +
  geom_point(aes(period, rr, color = group, shape = group), size = 2) +
  geom_line(aes(period, rr, group = group, color = group)) +
  scale_color_manual(name = "", values = c("#88498F",     # purple
                                           "#FFBA49",     # yellow
                                           "#20A39E",     # teal
                                           "#EF5B5B",     # red
                                           "#276FBF")) +  # blue
  ylim(0.5, 2) +
  labs(x = "", y = "Risk ratio") +
  theme_classic() +
  theme(legend.position  = "none",
        strip.background = element_blank(),
        strip.text.x     = element_blank(),
        axis.line.x      = element_blank(),
        axis.text.x      = element_blank(),
        axis.ticks.x     = element_blank()) +
  facet_wrap( ~ well_stage, ncol = 3)

# panel B - socioeconomic indicators
figure_s6b <- table_rr %>%
  filter(well_stage != "plugged") %>%
  filter(group %in% c("poverty", "educ_less_than_hs", "renters",
                      "ling_isolated", "nonvoters")) %>%
  ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#000000") +
  geom_point(aes(period, rr, color = group, shape = group), size = 2) +
  geom_line(aes(period, rr, group = group, color = group)) +
  scale_color_manual(name = "", values = c("#FFA500",     # orange
                                           "#000000",     # black
                                           "#fb9a99",     # pink
                                           "#4daf4a",     # green
                                           "#878787")) +  # gray
  ylim(0.6, 1.3) +
  labs(x = "", y = "Risk ratio") +
  theme_classic() +
  theme(legend.position  = "none",
        strip.background = element_blank(),
        strip.text.x     = element_blank(),
        axis.line.x      = element_blank(),
        axis.text.x      = element_blank(),
        axis.ticks.x     = element_blank()) +
  facet_wrap( ~ well_stage, ncol = 3)

# panel C - just the x-axis
figure_s6c <- table_rr %>%
  filter(well_stage != "plugged") %>%
  filter(group %in% c("hispanic")) %>%
  ggplot() +
  geom_point(aes(period, rr, color = group)) +
  ylim(0.6, 1.4) +
  labs(x = "", y = "Risk ratio") +
  scale_x_discrete(labels = c("2005-2009", "2010-2014", "2015-2019")) +
  theme_classic() +
  theme(legend.position  = "none",
        strip.background = element_blank()) +
  facet_wrap( ~ well_stage, ncol = 3)


##---------------------------------------------------------------------------
## exports figure components

ggsave(filename = "figure_s6a.png", plot = figure_s6a, device = "png",
       width = 7, height = 3,  path = "output/figures/components")
ggsave(filename = "figure_s6b.png", plot = figure_s6b, device = "png",
       width = 7, height = 2, path = "output/figures/components")
ggsave(filename = "figure_s6c.png", plot = figure_s6c, device = "png",
       width = 7, height = 1.6, path = "output/figures/components")

##============================================================================##
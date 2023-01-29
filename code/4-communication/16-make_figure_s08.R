##============================================================================##
## makes Figure S8 - like Figure S1 but for SES indicators, restricted to
## Los Angeles County

##---------------------------------------------------------------------------
## sets up environment

# attaches packages necessary for this script (and not already in 01-setup.R)
library("patchwork") 

# data input
table_quantiles_prod_vol <- 
  read_csv("output/results/table_rr_quantiles_county_losangeles.csv") %>%
  filter(well_stage        == "production_volume") %>%
  mutate(quantile           = as.factor(quantile)) %>%
  pivot_longer(cols         = rr_hispanic:rr_nonvoters,
               names_prefix = "rr_",
               names_to     = "group",
               values_to    = "rr")


##---------------------------------------------------------------------------
## generates figure components

figure_s8a <- table_quantiles_prod_vol %>%
  filter(group %in% c("poverty", "educ_less_than_hs", "renters",
                      "ling_isolated", "nonvoters")) %>%
  ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#000000") +
  geom_segment(aes(quantile, rr, yend = 1, xend = quantile, color = group),
               lwd = 2.4) +
  geom_point(aes(quantile, rr, color = group), shape = 15, size = 2) +  
  scale_color_manual(name = "", values = c("#FFA500",     # orange
                                           "#000000",     # black
                                           "#fb9a99",     # pink
                                           "#4daf4a",     # green
                                           "#878787")) +  # gray
  ylim(0.4, 1.9) +
  labs(x = "Quintile of exposure to production volume", y = "Risk ratio") +
  theme_classic() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  facet_wrap(period ~ group, ncol = 5)
figure_s8a

##---------------------------------------------------------------------------
## exports figure

ggsave(filename = "figure_s8a.png", plot = figure_s8a, device = "png",
       width = 5.5, height = 6, path = "output/figures/components")

##----------------------------------------------------------------------------##

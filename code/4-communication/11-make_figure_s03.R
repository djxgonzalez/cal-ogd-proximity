##============================================================================##
## makes Figure S3 - ...

##---------------------------------------------------------------------------
## sets up environment

# attaches packages necessary for this script (and not already in 01-setup.R)
library("patchwork") 

# data input
table_quantiles_postprod_wells <-
  read_csv("output/results/table_rr_quantiles.csv") %>%
  filter(well_stage        == "postproduction") %>%
  mutate(quantile           = as.factor(quantile)) %>%
  pivot_longer(cols         = rr_hispanic:rr_poverty,
               names_prefix = "rr_",
               names_to     = "group",
               values_to    = "rr")


##---------------------------------------------------------------------------
## generates figure components

figure_s3 <- table_quantiles_postprod_wells %>%
  filter(group %in% c("hispanic", "nonhisp_am_indian", "nonhisp_asian",
                      "nonhisp_black", "nonhisp_white")) %>%
  ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#000000") +
  geom_segment(aes(quantile, rr, yend = 1, xend = quantile, color = group),
               lwd = 2.4) +
  geom_point(aes(quantile, rr, color = group), shape = 15, size = 2) +  
  scale_color_manual(name = "", values = c("#88498F",     # purple
                                           "#FFBA49",     # yellow
                                           "#20A39E",     # teal
                                           "#EF5B5B",     # red
                                           "#276FBF")) +  # blue
  ylim(0, 2.2) +
  labs(x = "Quantile of exposure to well retirements", y = "Risk ratio") +
  theme_classic() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  facet_wrap(period ~ group, ncol = 5)
figure_s3

##---------------------------------------------------------------------------
## exports figure

ggsave(filename = "figure_s3a.png", plot = figure_s3, device = "png",
       width = 5.5, height = 6, path = "output/figures/components")

##----------------------------------------------------------------------------##

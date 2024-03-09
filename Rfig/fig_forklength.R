# fig_forklength
# written by: sean Kinard
# last edit: 2023-05-19

#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

d_forklength <- read_csv('Rout/d_forklength.csv') %>% fix_site_order()
anova_forklength <- read_csv('Rout/ANOVA_forklength.csv')
THSD_forklength <- read_csv('Rout/THSD_forklength.csv')

#----------------------------------------------------------------------------
# histogram_forklength
#----------------------------------------------------------------------------

# family_fl_frequency
d_lessthan170 <- d_forklength %>%
  filter(forklength <= 170) %>%
  filter(! is.na(family))

d_over150 <- d_forklength %>%
  filter(forklength > 170) %>%
  filter(! is.na(family))

histogram_forklength_caption <- "Histogram of fish forklengths at Semi-Arid, Transition, and Sub-Humid sites surveyed in April-June of 2018. Bar chunks are colored by taxonomic family. to ease visual comparison six spotted gar (*L.oculatus*) with forklengths over 170 mm were omitted; one at the transition site, and five at the sub-humid site. Whole community average forklength is smaller in Semi-Arid compared to Transition and Sub-Humid sites (*p*-values = `r THSD_forklength_70vs50`, `r THSD_forklength_85vs50` respectively), due to a lack of centrarchids and spotted gar."

histogram_forklength <- d_lessthan170 %>%
  ggplot(aes(forklength, fill = family), color = 'black') +
  geom_histogram(binwidth=10) +
  facet_wrap(~site, nrow=3) +
  scale_fill_brewer(palette="BrBG", direction = -1) +
  theme_bw(base_size = 12) +
  labs(fill='Taxonomic\nFamily') +
  ylab('Count') +
  xlab('Forklength (mm)')

#----------------------------------------------------------------------------
# boxplot_forklength
#----------------------------------------------------------------------------

boxplot_forklength_caption <- "Boxplot of fish forklengths at Semi-Arid, Transition, and Sub-Humid sites surveyed in April-June of 2018. Bar chunks are colored by taxonomic family and red diamonds represent mean values. Whole community average forklength is smaller in Semi-Arid compared to Transition and Sub-Humid sites (*p*-values = `r THSD_forklength_70vs50`, `r THSD_forklength_85vs50` respectively), due to a lack of centrarchids and spotted gar."

boxplot_forklength <- d_forklength %>%
  filter(! is.na(family)) %>%
  group_by(site) %>%
  summarize(fl_median = median(forklength),
            fl_mean = mean(forklength),
            fl_mean_trim = mean(forklength, trim = .05)) %>%
  right_join(d_forklength) %>%
  ggplot(aes(site, forklength)) +
  geom_boxplot(aes(fill = site),color='black') +
  geom_point(aes(site, fl_mean), shape=23, size = 3, color='black', fill='red') +
  scale_fill_manual(values=my_colors) +
  theme_bw(base_size = 12) +
  theme(axis.title.x=element_blank()) + 
  ylab('Fork length (mm)') +
  scale_y_log10() +
  geom_signif(comparison = list(c("Semi-Arid", "Transition"),
                                c("Semi-Arid", "Sub-Humid")),
              test = "t.test",
              textsize = 8,
              col = "red",
              y_position = c(2.35,2.57),
              tip_length=0,
              map_signif_level = TRUE) +
  theme(legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black"))

#----------------------------------------------------------------------------
# End fig_forklength
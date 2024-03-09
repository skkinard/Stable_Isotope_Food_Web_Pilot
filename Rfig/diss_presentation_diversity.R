# fig_diversity
# Sean Kinard
# last edit: 2023-05-19

#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

hill <- read_csv('Rout/diversity_estimates.csv')
THSD_shannon_f <- read_csv('Rout/THSD_shannon_f.csv')
THSD_shannon_i <- read_csv('Rout/THSD_shannon_i.csv')
#----------------------------------------------------------------------------
# boxplot_diversity
#----------------------------------------------------------------------------

boxplot_diversity_caption <- "Boxplots of species richness, Shannon-Wiener, and Simpson values for fish and invertebrate communities at Semi-Arid, Transition, and Sub-Humid sites surveyed April-June of 2018. Boxplot colors darken with increasing precipitation and red diamonds represent mean values. Fish species richness and evenness were lowest at the Semi-Arid site and highest at the Sub-humid site. Invertebrate species richness and evenness were greatest at the Transition site."

my_colors <- c('yellow2', 'green3', 'skyblue1')
my_colors2 <- c('yellow', 'chartreuse', 'cyan')

boxplot_diversity <- hill %>%
  mutate(Site = fct_relevel(Site, 'Semi-Arid', 'Transition', 'Sub-Humid')) %>%
  ggplot(aes(x=Site, y=Observed)) +
  facet_grid(facets=Guild ~ Diversity, scales='free') +
  geom_boxplot(aes(fill=Site),color='black') +
  geom_point(aes(y=average),
             shape=23, size = 3, color='black', fill='red') +
  scale_fill_manual(values=my_colors) +
  dark_theme_grey(base_size = 18) +
  theme(legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
# richness = # of species
# Shannon ~ # common species
# Simpson ~ # dominant species

#----------------------------------------------------------------------------
# boxplot_shannon_f
#----------------------------------------------------------------------------

boxplot_shannon_f_caption <- "Boxplot of Shannon-Wiener diversity for fish communities at Semi-Arid, Transition, and Sub-Humid sites surveyed April-June of 2018. Boxplot colors darken with increasing precipitation and red diamonds represent mean values. Fish diversity were lowest at the Semi-Arid site and highest at the Sub-humid site."

boxplot_shannon_f <- hill %>%
  mutate(Site = str_replace_all(Site, 'Transition', 'Mesic')) %>%
  mutate(Site = fct_relevel(Site, 'Semi-Arid', 'Mesic', 'Sub-Humid')) %>%
  filter(Diversity == 'Shannon-Wiener') %>%
  ggplot(aes(x=Site, y=Observed)) +
  facet_wrap(~Guild, ncol=2) +
  geom_boxplot(aes(fill=Site),color='black') +
  geom_point(aes(y=average),
             shape=23, size = 4, color='black', fill='red') +
  scale_fill_manual(values=my_colors) +
  dark_theme_grey(base_size = 28) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("Shannon Diversity") +
  theme(legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black"))

#----------------------------------------------------------------------------
# boxplot_shannon_i
#----------------------------------------------------------------------------

boxplot_shannon_i_caption <- "Boxplot of Shannon-Wiener diversity for invertebrate communities at Semi-Arid, Transition, and Sub-Humid sites surveyed April-June of 2018. Boxplot colors darken with increasing precipitation and red diamonds represent mean values. Invertebrate diversity is highest at the Transition site."

boxplot_shannon_i <- hill %>%
  mutate(Site = fct_relevel(Site, 'Semi-Arid', 'Transition', 'Sub-Humid')) %>%
  filter(Diversity == 'Shannon-Wiener') %>%
  filter(Guild == 'Invertebrate') %>%
  ggplot(aes(x=Site, y=Observed)) +
  geom_boxplot(aes(fill=Site),color='black') +
  geom_point(aes(y=average),
             shape=23, size = 4, color='black', fill='red') +
  ylim(0,23) +
  scale_fill_manual(values=my_colors) +
  dark_theme_grey(base_size = 14) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("Shannon Diversity") +
  theme(legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#----------------------------------------------------------------------------
# table_diversity
#----------------------------------------------------------------------------

table_diversity_caption <- "Table of species richness, Shannon-Wiener, and Simpson means \u00b1 standard error for fish and invertebrate communities at Semi-Arid, Transition, and Sub-Humid sites surveyed April-June of 2018. Fish species richness and evenness were lowest at the Semi-Arid site and highest at the Sub-humid site. Invertebrate species richness and evenness were greatest at the Transition site."

table_diversity <- hill %>%
  mutate(average = sprintf("%.2f", round(average, 2)),
         se = sprintf("%.2f", round(se, 2))) %>%
  mutate(value = paste(average, "\u00b1", se, sep=' ')) %>%
  select(Site, Guild, Diversity, value) %>%
  unique() %>%
  pivot_wider(names_from = Site, values_from = value)

write_csv(table_diversity, 'Rfig/table_diversity.csv')

#----------------------------------------------------------------------------
# table_THSD_diversity
#----------------------------------------------------------------------------

table_THSD_diversity_caption <- "Comparisons of mean Shannon-Wiener indices of fish or invertebrates across sites using Tukey's 'Honest Significant Difference' method. Comparisons are described in terms of the differences between means, the lower interval limit, upper interval limit, and associated p-value. Invertebrate diversity is greater at the Transition site compared to Semi-Arid and Sub-Humid sites(*p*-values = `r THSD_shannon_i_70vs55`, `r THSD_shannon_i_85vs70` respectively)"

# Table: ANOVA Tukey comparisons: total abundance ~ site
table_THSD_diversity <- full_join(THSD_shannon_i, THSD_shannon_f) %>%
  mutate(`p adj` = ifelse(abs(`p adj`) < .01, 
                          formatC(`p adj`, format = 'e', digits = 1),
                          sprintf('%.2f', signif(`p adj`,2))),
         diff = round(diff, 1),
         lwr = round(lwr, 1),
         upr = round(upr, 1)) %>%
  select(Guild, everything()) %>%
  rename('TukeyHSD Comparison' = Comparison, Difference = diff, Lower = lwr, Upper = upr, 'p-value' = `p adj`)

write_csv(table_THSD_diversity, 'Rfig/THSD_table_diversity.csv')

#----------------------------------------------------------------------------
# End fig_diversity
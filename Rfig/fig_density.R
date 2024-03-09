# fig_density
# Sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

d_density <- read_csv('Rout/d_density_all.csv')
THSD_density_f <- read_csv('Rout/THSD_density_f.csv')
THSD_density_i <- read_csv('Rout/THSD_density_i.csv')
#----------------------------------------------------------------------------
# Boxplot Fish density (boxplot_density_f)
#----------------------------------------------------------------------------

caption_density_boxplot_fish <- "Boxplot of fish densities at semi-arid, transition, and sub-humid sites surveyed April-June in 2018. The semi-arid site had greater fish densities than the transition or the sub-humid sites (*p*-values = `r THSD_density_f_70vs55`, `r THSD_density_f_85vs55` respectively)."

density_boxplot_fish <- d_density %>%
  filter(guild == 'fish') %>%
  select(site, month, density_total, density_total_mean) %>%
  distinct() %>%
  ggplot(aes(x=site, y=density_total)) +
  geom_boxplot(aes(fill = site),color='black') +
  geom_point(aes(y=density_total_mean),
             shape=23, size = 4, color='black', fill='red') +
  scale_fill_manual(values=my_colors) +
  xlab(element_blank()) +
  ylim(0,2.5) +
  ylab(bquote(Fish / m^2)) +
  theme_bw(base_size = 10) +
  theme(legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave(plot = density_boxplot_fish,
       filename = 'Rfig/density_boxplot_fish.png',
       height=9,
       width=13,
       units='cm')
#----------------------------------------------------------------------------
# Boxplot Invertebrate density (boxplot_density_i)
#----------------------------------------------------------------------------

caption_density_boxplot_invertebrate <- "Boxplot of invertebrate densities at semi-arid, transition, and sub-humid sites surveyed with kicknets April-June in 2018. Box colors darken with rising annual precipitation and red diamonds represent site mean values. Visually, invertebrate densities appear greatest at the transition site, although Tukey comparisons between site means lack statistical significance."

# Invertebrate density vs site
density_boxplot_invertebrate <- d_density %>%
  filter(guild == 'invertebrate') %>%
  select(site, month, density_total, density_total_mean) %>%
  distinct() %>%
  ggplot(aes(x=site, y=density_total)) +
  geom_boxplot(aes(fill = site),color='black') +
  geom_point(aes(y=density_total_mean),
             shape=23, size = 4, color='black', fill='red') +
  scale_fill_manual(values=my_colors) +
  xlab(element_blank()) +
  ylab(bquote(Invertebrates / m^2)) +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave(plot = density_boxplot_invertebrate,
       filename = 'Rfig/density_boxplot_invertebrate.png',
       height=9,
       width=13,
       units='cm')

#----------------------------------------------------------------------------
# Table (table_density_all)
#----------------------------------------------------------------------------

# Table: ANOVA Tukey comparisons: density abundance ~ site
table_density_THSD <- full_join(THSD_density_f, THSD_density_i) %>%
  select(Guild, everything()) %>%
  mutate(diff = formatC(`diff`, format = 'e', digits = 1),
         lwr = formatC(`lwr`, format = 'e', digits = 1),
         upr = formatC(`upr`, format = 'e', digits = 1),
         `p adj` = ifelse(abs(`p adj`) < .01, 
                          formatC(`p adj`, format = 'e', digits = 1),
                          sprintf('%.2f', signif(`p adj`,2))),
         `p adj` = ifelse(`p adj` < .05, 
                          paste(`p adj`, '**', sep = ''),
                          ifelse(`p adj` < .1, 
                                 paste(`p adj`, '*', sep = ''),
                                 `p adj`))) %>%
  rename('TukeyHSD Comparison' = Comparison, 
         Difference = diff, 
         Lower = lwr, 
         Upper = upr, 
         'p-value' = `p adj`)

# table with summary stats (mean plus or minus standard deviation)
table_density_stats <- d_density %>% 
  select(site, guild, density_total_mean, density_total_sd) %>%
  unique() %>%
  mutate(density_total_sd = ifelse(guild == 'invertebrate',
                                   round(density_total_sd, 0),
                                   round(density_total_sd, 1)),
         density_total_mean = ifelse(guild == 'invertebrate',
                                     round(density_total_mean, 0),
                                     round(density_total_mean, 1))) %>%
  mutate(density_summary = paste(density_total_mean, 
                                 expression('\u00B1'),
                                 density_total_sd)) %>%
  mutate(guild = str_to_title(guild)) %>%
  arrange(guild, site) %>%
  select(site, guild, density_summary) %>%
  pivot_wider(names_from = site, values_from = density_summary) %>%
  rename(Guild=guild)

# combine THSD and summary table
table_density <- right_join(table_density_stats, table_density_THSD) %>%
  select(-Lower, -Upper)

write_csv(table_density, 'Rfig/density_table.csv')

caption_table_density <-  "Summary statistics and mean comparisons of fish or invertebrates densities between sites using Tukey's 'Honest Significant Difference' method. Summary statistics for each site include the mean ± the standard deviation for collections during April-June of 2018. Tukey Comparisons are described in terms of the differences between means and the associated *p*-value. The semi-arid site had greater fish densities than the transition or the sub-humid sites (*p*-values = `r THSD_density_f_70vs55`, `r THSD_density_f_85vs55` respectively)."

#----------------------------------------------------------------------------
# End fig_density
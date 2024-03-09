# isotopic_TL_vs_size
# Sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

d <-  read_csv('Rout/TL_d.csv')

#----------------------------------------------------------------------------
plot_TL_density <- d %>%
  filter(kingdom=='Animal') %>%
  ggplot(aes(trophic_level_N, fill = site)) +
  facet_wrap(~guild) +
  geom_density(alpha=.8) +
  scale_fill_manual(values = my_colors) +
  theme_bw(base_size = 14)

plot_TL_box <- d %>%
  filter(species %in% common_species) %>%
  mutate(species = fct_relevel(species, "Corbiculidae", "Coenagrionidae", 
                               "P.latipinna", "L.macrochirus", "L.cyanellus", 
                               "L.gulosus")) %>%
  ggplot(aes(species, trophic_level_N)) +
  geom_boxplot(alpha=.8) +
  theme_bw(base_size = 14)

# Size does not affect trophic_level (no observable ontogenetic shift in nitrogen signatures within taxa)
plot_TL_size <- d %>%
  filter(species %in% c("P.latipinna", 
                        "L.macrochirus", 
                        "L.cyanellus", 
                        "L.gulosus")) %>%
  mutate(species = fct_relevel(species,
                               "P.latipinna", 
                               "L.macrochirus", 
                               "L.cyanellus", 
                               "L.gulosus")) %>%
  ggplot(aes(x=size_max, y=trophic_level_N, fill = site)) +
  facet_wrap(~species) +
  geom_jitter(shape=23) +
  geom_smooth(aes(color = site), method = 'lm') +
  scale_fill_manual(values = my_colors2) +
  scale_color_manual(values = my_colors2) +
  theme_bw(base_size = 14)

#----------------------------------------------------------------------------
# ANOVA
#----------------------------------------------------------------------------

# perform three-way ANOVA: isotopic_trophic_level ~ site * species * size
my_aov_3way <- aov(trophic_level_N ~ site * species * size_max, data=filter(d, guild == 'Fish'))
table_my_aov_3way <- as_tibble(summary(my_aov_3way)[[1]]) %>%
  mutate('T-test' = noquote(rownames(summary(my_aov_3way)[[1]]))) %>%
  rename(df = Df, 'F-stat'= 'F value', 'p-value' = 'Pr(>F)')%>%
  select('T-test', 'F-stat', df, 'p-value') %>%
  as.data.frame()
table_my_aov_3way$`T-test` <- str_to_title(table_my_aov_3way$`T-test`)
table_my_aov_3way <- table_my_aov_3way[1:7,]
(anova_TL_3 <- table_my_aov_3way)
summary(my_aov_3way)
# site and species and site*species have effects on trophic level

# perform Two-way ANOVA: isotopic_trophic_level ~ site * species
## ANOVA: density abundance
my_aov_2way <- aov(trophic_level_N ~ site * species, data=filter(d, species %in% common_species))
table_aov_2way <- as_tibble(summary(my_aov_2way)[[1]]) %>%
  mutate('T-test' = noquote(rownames(summary(my_aov_2way)[[1]]))) %>%
  rename(df = Df, 'F-stat'= 'F value', 'p-value' = 'Pr(>F)')%>%
  select('T-test', 'F-stat', df, 'p-value') %>%
  as.data.frame()
table_aov_2way$`T-test` <- str_to_title(table_aov_2way$`T-test`)
table_aov_2way <- table_aov_2way[1:2,]
(anova_TL_2 <- table_aov_2way)

## Tukey HSD: density abundance
TukeyHSD(my_aov_2way)
# report F-statistic, degrees of freedom, and p-value from the model output
(THSD_TL_2 <- TukeyHSD(my_aov_2way)$site %>%
    as_tibble(rownames='Comparison') %>%
    full_join(TukeyHSD(my_aov_2way)$species %>%
                as_tibble(rownames='Comparison') ) %>%
    full_join(TukeyHSD(my_aov_2way)$`site:species` %>%
                as_tibble(rownames='Comparison'))) %>% filter(`p adj` < 0.05)

# check homogeneity of variance assumption
plot(my_aov_2way, 1)
# points 10 and 16 are detected as outliers, which can severely affect normality and homogeneity of variance. it can be useful to remove outliers to meet the test assumptions

# Use Levene's test to check the homogeneity of variance (leveneTest() in package 'car')
leveneTest(trophic_level_N ~ site * species, data=filter(d, kingdom == 'Animal'))
# from the output above we can see that the p-value is not less than the significance level of 0.05. This means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.

#----------------------------------------------------------------------------
# End isotopic_TL_vs_size
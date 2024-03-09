# 01_density_calc
# written by: sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Description
#----------------------------------------------------------------------------
# The script calculates and visualizes the density of fish and invertebrate communities, conducts ANOVA tests comparing density among different sites for fish and invertebrates separately, and exports the results along with the dataset.
#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

d <- read_csv('Rdat/communities.csv')

#----------------------------------------------------------------------------
# Calculate density
#----------------------------------------------------------------------------
# calculate density (fish originally reported in [#/ reach]. inverts reported in [# / square meter])
d <- d %>%
  mutate(density = ifelse(guild == 'fish', abun / stream_area, 
                          abun)) %>% 
  group_by(site, lowest_taxon) %>% 
  summarize(density_mean = mean(density),
            density_sd = sd(density)) %>%
  right_join(d) %>%
  mutate(density = ifelse(guild == 'fish', abun / stream_area, 
                          abun))

# total fish or invertebrate densities
d <- d %>%
  mutate(density_total = ifelse(guild =='fish', total_abun / stream_area,
                                total_abun)) %>% 
  group_by(site, guild) %>% 
  summarize(density_total_mean = mean(density_total),
            density_total_sd = sd(density_total)) %>%
  right_join(d) %>%
  mutate(density_total = ifelse(guild =='fish', total_abun / stream_area,
                                total_abun))

# relevel sites
d <- d %>% 
  mutate(site = as.factor(site),
         site = fct_relevel(site, 'Semi-Arid', 'Transition', 'Sub-Humid'))

#----------------------------------------------------------------------------
#  ANOVA Fish density vs site (anova_density_f)
#----------------------------------------------------------------------------

## ANOVA: density abundance
my_aov <- aov(density_total~site, 
              data = d %>%
                filter(guild == 'fish') %>%
                select(site, month, density_total) %>% 
                distinct())
table_my_aov <- as_tibble(summary(my_aov)[[1]]) %>%
  mutate('T-test' = noquote(rownames(summary(my_aov)[[1]]))) %>%
  rename(df = Df, 'F-stat'= 'F value', 'p-value' = 'Pr(>F)')%>%
  select('T-test', 'F-stat', df, 'p-value') %>%
  as.data.frame()
table_my_aov$`T-test` <- str_to_title(table_my_aov$`T-test`)
table_my_aov <- table_my_aov[1:2,]
(anova_density_f <- table_my_aov)

## Tukey HSD: density abundance
TukeyHSD(my_aov)
# report F-statistic, degrees of freedom, and p-value from the model output
(THSD_density_f <- TukeyHSD(my_aov)$site %>%
    as_tibble(rownames='Comparison') %>%
  mutate(Guild = 'Fish'))

write_csv(THSD_density_f, 'Rout/THSD_density_f.csv')

#----------------------------------------------------------------------------
# ANOVA Invertebrate density vs site (anova_density_i)
#----------------------------------------------------------------------------

## ANOVA: density abundance
my_aov <- aov(density_total~site, data = d %>%
                filter(guild == 'invertebrate') %>%
                select(site, month, density_total) %>% 
                distinct())
table_my_aov <- as_tibble(summary(my_aov)[[1]]) %>%
  mutate('T-test' = noquote(rownames(summary(my_aov)[[1]]))) %>%
  rename(df = Df, 'F-stat'= 'F value', 'p-value' = 'Pr(>F)')%>%
  select('T-test', 'F-stat', df, 'p-value') %>%
  as.data.frame()
table_my_aov$`T-test` <- str_to_title(table_my_aov$`T-test`)
table_my_aov <- table_my_aov[1:2,]
(anova_density_i <- table_my_aov)

## Tukey HSD: density abundance
TukeyHSD(my_aov)
# report F-statistic, degrees of freedom, and p-value from the model output
THSD_density_i <- TukeyHSD(my_aov)$site %>%
    as_tibble(rownames='Comparison') %>%
  mutate(Guild = 'Invertebrate')

write_csv(THSD_density_i, 'Rout/THSD_density_i.csv')

write_csv(d, 'Rout/d_density_all.csv')
   
#----------------------------------------------------------------------------
# End 01_density_calc
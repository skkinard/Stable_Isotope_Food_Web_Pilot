# 01_forklength_calc
# written by: sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Description
#----------------------------------------------------------------------------
# The script visualizes and conducts statistical comparisons for size distributions of whole fish communities and common fish families (Centrarchidae, Poeciliidae, Lacustreidae).

#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

df <- read_csv('Rdat/tidy_forklength.csv') 
dt <- read_csv('Rdat/tidy_fish.csv')

# merge forklength with taxonomic information in abundance data
colnames(dt)
dm <- dt %>% 
  mutate(lowest_taxon = paste(substr(genus, 1,1), species, sep='.')) %>% 
  select(order, family, genus, species, lowest_taxon) %>%
  unique() %>%
  right_join(df)

# filter forklength data for April-June of 2018
dm <- dm %>%
  filter(site == 'SFC' | site == 'AR' | site == 'GC') %>%
  filter(collection > as_date('2018-04-29') &  collection < as_date('2018-07-20')) %>%
  mutate(site = ifelse(site=='SFC', 'Semi-Arid',
                       ifelse(site == 'AR', 'Transition', 'Sub-Humid'))) %>%
  mutate(site = fct_relevel(site, 'Semi-Arid', 'Transition', 'Sub-Humid')) %>%
  mutate(month = ifelse(as_date(collection) < '2018-06-01', 'May',
                        ifelse(as_date(collection) < '2018-06-30', 'June', 'July' ))) %>%
  mutate(month = fct_relevel(month, 'May', 'June', 'July'))

#----------------------------------------------------------------------------
# Size: Exploration Functions
#----------------------------------------------------------------------------

## Frequency Plot
my_fl_freq <- function(my_data) {my_data %>%
    ggplot(aes(forklength, color = site)) +
    geom_freqpoly(binwidth=10) +
    facet_grid(facets = my_data$month, rows=3) }

## Statistics Table
my_fl_stats <- function(my_data) {my_data %>%
    group_by(site) %>%
    summarize('Average' = mean(forklength),
              'Standard Error' = sd(forklength)/(sqrt(length(forklength))),
              'n' = length(forklength),
              'Median' = median(forklength),
              'Minimum' = min(forklength),
              'Maximum' = max(forklength) ) }

## ANOVA
my_fl_aov <- function(my_data) {
  # Anova community
  my_aov <- aov(forklength~site+month, data = my_data)
  
  table_my_aov <- as_tibble(summary(my_aov)[[1]]) %>%
    mutate('T-test' = noquote(rownames(summary(my_aov)[[1]]))) %>%
    rename(df = Df, 'F-stat'= 'F value', 'p-value' = 'Pr(>F)')%>%
    select('T-test', 'F-stat', df, 'p-value') %>%
    as.data.frame()
  
  table_my_aov$`T-test` <- str_to_title(table_my_aov$`T-test`)
  
  table_my_aov <- table_my_aov[1:2,]
  
  print(table_my_aov) }

## Tukey HSD
my_fl_Tukey <- function(my_data) {
  
  my_aov <- aov(forklength~site+month, data = my_data)
  
  TukeyHSD(my_aov)
  # report F-statistic, degrees of freedom, and p-value from the model output
  tukey1 <- TukeyHSD(my_aov)$site %>%
    as_tibble(rownames='comparison') 
  
  tukey2 <- TukeyHSD(my_aov)$month %>%
    as_tibble(rownames='comparison') 
  
  (table_tukey_forklength_community <- full_join(tukey1, tukey2)) }

#----------------------------------------------------------------------------
# Explore with frequency plot, summary stats, anova, THSD
#----------------------------------------------------------------------------

# community (whole)
community <- dm
fl_freq_community <- my_fl_freq(dm)
fl_stats_community <- my_fl_stats(dm)
anova_forklength <- my_fl_aov(dm)
THSD_forklength <- my_fl_Tukey(dm) # semi-arid is smaller than both

# centrarchid
centrarchid <- dm %>%  filter(family=='Centrarchidae') 
fl_freq_centrarchid <- my_fl_freq(centrarchid)
fl_stats_centrarchid <- my_fl_stats(centrarchid)
fl_aov_centrarchid <- my_fl_aov(centrarchid)
fl_Tukey_centrarchid <- my_fl_Tukey(centrarchid) # ns

# minnow
minnow <- dm %>%  filter(family=='Leuciscidae') 
fl_freq_minnow <- my_fl_freq(minnow)
fl_stats_minnow <- my_fl_stats(minnow)
fl_aov_minnow <- my_fl_aov(minnow)
fl_Tukey_minnow <- my_fl_Tukey(minnow) # sub-humid is larger than both

# minnow
guppy <- dm %>%  filter(family=='Poeciliidae') 
fl_freq_guppy <- my_fl_freq(guppy)
fl_stats_guppy <- my_fl_stats(guppy)
fl_aov_guppy <- my_fl_aov(guppy)
fl_Tukey_guppy <- my_fl_Tukey(guppy) # ns

#----------------------------------------------------------------------------
# anova_forklength
#----------------------------------------------------------------------------

# community (whole): ANOVA (supplemental)
anova_forklength <- my_fl_aov(dm)

# Community (whole): TUKEY HSD posthoc
THSD_forklength <- my_fl_Tukey(dm) %>%
  rename('TukeyHSD Comparison' = comparison, Difference = diff, Lower = lwr, Upper = upr, 'p-value' = `p adj`) %>%
  mutate_if(is.numeric, funs(as.character(signif(., 3))))  # semi-arid is smaller than both

THSD_forklength_70vs50 <- filter(THSD_forklength, `TukeyHSD Comparison`=='Transition-Semi-Arid') %>% 
  pull(`p-value`)%>%
  formatC(format = "e", digits = 1)

THSD_forklength_85vs50 <- filter(THSD_forklength, `TukeyHSD Comparison`=='Sub-Humid-Semi-Arid') %>% 
  pull(`p-value`) %>%
  formatC(format = "e", digits = 1)

#----------------------------------------------------------------------------
# table_forklength
#----------------------------------------------------------------------------

# stats_fl_family (supplemental):
table_forklength <- dm %>%
  group_by(family, site) %>%
  summarize('Average' = mean(forklength),
            'se' = sd(forklength)/(sqrt(length(forklength))),
            'n' = length(forklength),
            'Median' = median(forklength),
            'Min' = min(forklength),
            'Max' = max(forklength) ) %>%
  rename(Site = site, Family = family) %>%
  filter(! is.na(Family)) 

write_csv(anova_forklength, 'Rout/ANOVA_forklength.csv')
write_csv(THSD_forklength, 'Rout/THSD_forklength.csv')
write_csv(dm, 'Rout/d_forklength.csv')

#----------------------------------------------------------------------------
# End 01_forklength_calc
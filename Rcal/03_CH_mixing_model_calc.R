# 03_CH_mixing_model_calc
# written by: sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Description
#----------------------------------------------------------------------------
# The script conducts compound-specific isotope analysis (CSIA) mixing models across different ecological groups (guilds, trophic levels, common species, fish size groups), aggregating statistics and quantile information, and exports the results.

#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')
source('Rcal/03_CH_mixing_model_function.R')

d <- read_csv('Data/isotope_data_correctedH.csv')

#----------------------------------------------------------------------------
# CH mixing model: Guilds
#----------------------------------------------------------------------------

# blank table
mmix_guild <- tibble(
  deviance = numeric(),
  Aquatic = numeric(),
  Terrestrial = numeric(),
  m_group = character(),
  site = character(),
  statistic = character() )

# sites
msites <- pull(d, site) %>% unique()

# loop mixing models across sites and aggregate statistics and quantile info
for(i in msites) {
  mmix_guild <- run_simmr(my_data = d,
                          my_location = i,
                          my_group = 'guild') %>%
    full_join(mmix_guild) }

mmix_guild <- mmix_guild %>%
  mutate(dataset = 'Guild') %>%
  mutate(m_group = fct_relevel(m_group, 'Invertebrate', 'Fish'))



#----------------------------------------------------------------------------
# CH mixing model: Trophic levels
#----------------------------------------------------------------------------

# blank table
mmix_trophic <- tibble(
  deviance = numeric(),
  Aquatic = numeric(),
  Terrestrial = numeric(),
  m_group = character(),
  site = character(),
  statistic = character() )

# sites
msites_trophic <- pull(d, site) %>% unique()

# loop mixing models across sites and aggregate statistics and quantile info
for(i in msites) {
  mmix_trophic <- run_simmr(my_data = d,
                            my_location = i,
                            my_group = 'trophic_category') %>%
    full_join(mmix_trophic) }

mmix_trophic <- mmix_trophic %>%
  mutate(dataset = 'Trophic')

#----------------------------------------------------------------------------
# CH mixing model: Common Species
#----------------------------------------------------------------------------

d %>% filter(kingdom == 'Animal') %>%
  group_by(site, species) %>% 
  summarise(count=length(species)) %>%
  pivot_wider(names_from=site, values_from=count) %>%
  print(n=35)

common_species <- c('P.latipinna', 'L.macrochirus', 'L.gulosus', 'L.cyanellus', 
                    'Corbiculidae', 'Coenagrionidae')

# blank table
mmix_species <- tibble(
  deviance = numeric(),
  Aquatic = numeric(),
  Terrestrial = numeric(),
  m_group = character(),
  site = character(),
  statistic = character() )

# sites
msites <- pull(d, site) %>% unique()

# loop mixing models across sites and aggregate statistics and quantile info
for(i in msites) {
  mmix_species <- run_simmr(my_data = d %>% filter(species %in% common_species | kingdom != 'Animal'),
                            my_location = i,
                            my_group = 'species') %>%
    full_join(mmix_species) }

mmix_species <- mmix_species %>%
  mutate(dataset = 'Species') %>%
  mutate(m_group = fct_relevel(m_group, "Corbiculidae", "Coenagrionidae", 
                               "P.latipinna", "L.macrochirus", "L.cyanellus", 
                               "L.gulosus"))

#----------------------------------------------------------------------------
# CH mixing model: fish size groups
#----------------------------------------------------------------------------

# view length data
d %>% filter(guild == 'Fish') %>% 
  ggplot() +
  geom_histogram(aes(size_max)) + facet_wrap(~site) +
  xlim(c(0,200))

# bin lengths by 50 mm
d <-  d %>%
  mutate(size_bin = ifelse(size_max < 50 , '< 50mm',
                           ifelse(size_max > 50 & size_max < 100, '< 100mm',
                                  ifelse(size_max > 100 & size_max < 150, '< 150mm', '> 150mm'))))

# blank table
mmix_size <- tibble(
  deviance = numeric(),
  Aquatic = numeric(),
  Terrestrial = numeric(),
  m_group = character(),
  site = character(),
  statistic = character() )

# sites
msites <- pull(d, site) %>% unique()

# loop mixing models across sites and aggregate statistics and quantile info
for(i in msites) {
  mmix_size <- run_simmr(my_data = d %>% filter(guild == 'Fish') %>% 
                           filter(!is.na(size_bin)) %>% full_join(filter(d, kingdom != 'Animal')),
                         my_location = i,
                         my_group = 'size_bin') %>%
    full_join(mmix_size) }

mmix_size <- mmix_size %>%
  mutate(dataset = 'Size') %>%
  mutate(m_group = fct_relevel(m_group, "< 50mm", "< 100mm", "< 150mm",  
                               "> 150mm"))

# Combine all mmix
mmix_all <- full_join(mmix_guild, mmix_trophic) %>%
  full_join(mmix_size) %>%
  full_join(mmix_species) %>%
  select(Aquatic, site, statistic, m_group, dataset) %>%
  mutate(Aquatic = 100*Aquatic) %>%
  pivot_wider(names_from = statistic, values_from = Aquatic) %>%
  mutate(site = fct_relevel(site,  'Semi-Arid', 'Transition', 'Sub-Humid')) %>%
  mutate(m_group = fct_relevel(m_group, 
                               "Fish", "F-Herbivore", "P.latipinna",
                               "F-Invertivore", "L.macrochirus",
                               "F-Piscivore", "L.cyanellus", "L.gulosus",
                               "Invertebrate", 
                               "I-Filterer", "Corbiculidae", 
                               "I-Gatherer", 
                               "I-Predator", "Coenagrionidae" ))

write_csv(mmix_all, 'Rout/CH_mix_out.csv')
#----------------------------------------------------------------------------
# End 03_CH_mixing_model_calc
# 02_adjust_d2H
# Sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Description
#----------------------------------------------------------------------------
# The script adjusts hydrogen isotope values in a dataset based on environmental water contribution, applies functional feeding groups (FFGs) to invertebrate samples, and exports the modified dataset for further analysis.

#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

#load data
d <- read_csv('Rout/isotope_clean.csv') %>%
  select(site, trophic_category, species, trophic_level_fbase, size_max, guild, carbon, hydrogen, nitrogen) %>%
  left_join(read_csv("Rdat/RAPID_CNOH_WATER.csv") %>%
              rename(hydrogen_water = hydrogen) %>%
              select(site, hydrogen_water) %>%
              mutate(site = ifelse(site == 'SFC', 'Semi-Arid',
                                   ifelse(site == 'AR', 'Transition',
                                          'Sub-Humid'))) )

#----------------------------------------------------------------------------
# Environmental Water Contribution (Solomon et al. 2009)
#----------------------------------------------------------------------------
# The total contribution of environmental water to tissue H (Wcompound) depends on the value of W and on the trophic level of the consumer. Assuming a linear food chain where each consumer eats only from the next lowest trophic level and where W is a constant for all consumers, Wcompound = 1-(1-W)^trophiclevel

# For now, the best available data suggest using a point estimate of W= 0.173 in mixing model calculations, but carrying a fairly large uncertainty in this estimate (SD = 0.122)

d <- d %>% # insert W column
  mutate(W = 0.173,           # point estimate of environmental water contribution
         W_sd = 0.122) %>%    # recommended (generous) sd for e-water contribution
  mutate(W_compound = 
           1-(1-W)^(trophic_level_fbase+.00001) ) %>% # estimate Wcompound (increases with trophic level)
  mutate(hydrogen_raw = hydrogen) %>%
  mutate(hydrogen = hydrogen - W_compound*hydrogen_water)

# Add kingdom grouping variable to allow for whole commmunity @ trophic levels
d <- mutate(d, kingdom = ifelse(guild %in% c('Fish', 'Invertebrate'), 'Animal',
                                ifelse(guild %in% c('Aquatic', 'Terrestrial'), 'Plant', 
                                       'Water')) )

#----------------------------------------------------------------------------
# add_FFG
#----------------------------------------------------------------------------
# Import functional feeding groups to invertebrate samples for aggregation and comparison

d <- d %>%
  mutate(species = ifelse(species == "Belastomatidae", "Belostomatidae",
                    ifelse(species == 'Dystiscidae', 'Dytiscidae', species)))

# Isolate invertebrate families
my_ifam <- d %>%
  filter(guild == 'Invertebrate') %>%
  rename(Family = species) 

# extract taxonomic and FFG from poff database for my invertebrate families
poff <- read_csv('Data/invertebrate_traits_Poff2010.csv') %>% 
  filter(Family %in% my_ifam$Family ) %>%
  select(Family, Trop) %>%
  mutate(Trop = str_replace(Trop, 'Trop', '')) %>%
  mutate(Trop = str_replace(Trop, "\\.", "")) %>%
  mutate(Trop = str_replace(Trop, "Collector", "")) %>%
  unique() %>%
  mutate(keep = ifelse(Family == 'Leptophlebiidae' & Trop %in% c('Herbivore',
                                                                'Filterer'), 0,
                       ifelse(Family == 'Chironomidae' & Trop == 'Predator', 0, 1))) %>%
  filter(near(keep, 1)) %>%
  select(-keep)

# add families missed by poff 2010
missing_families <- c("Annelidae" ,  "Cambaridae",   "Corbiculidae",
                      "Thiaridae",  "Dogielinotidae",  "Gyrinidae",
                        "Palaemonidae" )
missing_ffg <- c('Gatherer', 'Gatherer', 'Filterer', 
                 'Herbivore', 'Gatherer', 'Predator',
                 'Gatherer')
missing <- tibble(missing_families, missing_ffg) %>%
  rename(Family = missing_families,
         Trop = missing_ffg) 

FFGs <- full_join(missing, poff) %>%
  rename(FFG=Trop)

d <- left_join(d, FFGs, by = c('species' = 'Family')) %>%
  mutate(f_cat = ifelse(round(trophic_level_fbase, 0) < 3, 'Herbivore',
                        ifelse(round(trophic_level_fbase, 0) < 4, 'Invertivore',
                               'Piscivore'))) %>%
  mutate(trophic_category = ifelse(guild == 'Fish', paste('F', f_cat, sep='-' ),
                                   paste('I', FFG, sep='-'))) %>%
  select(-c('f_cat', 'FFG'))

write_csv(d, 'Rdat/isotope_data_correctedH.csv')
#----------------------------------------------------------------------------

# End 02_adjust_d2H
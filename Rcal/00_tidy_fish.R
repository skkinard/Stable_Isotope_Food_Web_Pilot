# 00_tidy_fish
# written by: sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Description
#----------------------------------------------------------------------------
# The script performs data cleaning and tidying on fish abundance data, addressing errors, filtering out specific taxa, replacing juvenile sunfish taxa, calculating stream area sampled, and exporting the cleaned dataset.

#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

d_f <- read_csv('Rdat/HH_TERRG_fish_abundance.csv')
d_i <- read_csv('Rdat/RAPID_invert.csv')
my_fish_names <- read_csv('Rdat/my_fish_species.csv') # ID names verified using Fishbase.se
de <- read_csv('Rdat/Site_Day_Mean_GapfillData.csv')

#----------------------------------------------------------------------------
# Tidy Datasets 
#----------------------------------------------------------------------------
# clean clerical errors in fish data set

d_f <- d_f %>%
  mutate(date = as_date(paste(year, month, day, sep = '-'))) %>%
  select(-genus, -species) %>%
  full_join(my_fish_names) %>%
  select(UID, code, date, cycle, abundance, ftot, order, family, genus, species,   lowest_taxon, comments) %>%
  rename(site = code, total_abundance = ftot)
# look at comments
filter(d_f, is.na(comments) == FALSE) %>% select(lowest_taxon, comments)
# remove blue crab
d_f <- filter(d_f, ! lowest_taxon == 'C. sapidus')

# replace juvenile sunfish with most common sunfish taxa
juvenile <- filter(d_f, species == 'spp.')

common_sf <- d_f %>%
                    group_by(site, date, order, family, genus) %>%
                    filter(genus == 'Lepomis') %>%
                    filter(! species == 'spp.') %>%
                    summarise(mode_sf = max(abundance) ) %>%
                    full_join(d_f) %>%
                    filter(mode_sf == abundance)

juv_to_common <- left_join(juvenile, common_sf, by = c('site', 'date', 'order', 'family')) %>%
  select(site, date, species.x, abundance.x, species.y, abundance.y) %>%
  rename(species = species.x, species_new = species.y)

# one site has same # of juveniles as modal species
filter(d_f, date == as_date('2020-09-08') & site == 'WMC')

juv_to_common$species_new <- str_replace(juv_to_common$species_new, 'spp.', 'megalotis')
juv_to_common$species_new <- str_replace(juv_to_common$species_new, 'cyanellus', 'megalotis')

juv_to_common <- select(juv_to_common, site, date, species, species_new)


my_rows <- c(which(d_f$species == 'spp.'))

for(i in 1:12) {
  j <- my_rows[i]
  d_f$species[j] <- juv_to_common$species_new[i] }

# merge rows of fish that had duplicate species with typos or that now have duplicates because of juvenile fish name change
d_f <-  d_f %>%
  group_by(site, date, order, family, genus, species) %>%
  summarize(abun = ceiling(sum(abundance)),
            total_abun = round(mean(total_abundance)))

#----------------------------------------------------------------------------
# calculate stream area sampled to enable calculation of fish per unit area
colnames(de) <- str_to_lower(colnames(de))

de <- de %>%
  mutate(date = as_date(paste(year, month, day, sep='-'))) 

de %>%
  filter(site == 'AR') %>%
  select(site, date, width)

(d_f <- de %>%
  group_by(site, date) %>%
  summarize(stream_area = mean(width)*75) %>%
  right_join(d_f))

#----------------------------------------------------------------------------
# Export
write_csv(d_f, 'Rdat/tidy_fish.csv')

# End 00_tidy_fish
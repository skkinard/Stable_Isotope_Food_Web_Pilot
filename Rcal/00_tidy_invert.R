# 00_tidy_invert
# written by: sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Description
#----------------------------------------------------------------------------
# The script reads and tidies invertebrate kick sampling data, calculating taxon abundances per square meter, extracting taxonomic information, and exporting the cleaned dataset.

#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')
# load packages #
library(tidyverse)
library(lubridate)
library(bio.infer)

# load data 
d <- read_csv('Rdat/rapid kick.csv')

#----------------------------------------------------------------------------
# Tidy Datasets 

# formatting
d <- d %>%  
  rename(site = Site_Code,
         date = Collection_Date,
         order = Order,
         family = Family,
         taxon = Taxon) %>%
  select(site, date, order, family, taxon) 

# taxon abundances
d <- d %>%
  group_by(site, date, order, family, taxon) %>% 
  summarize(abun = length(taxon) / 1.86) # 0.093 m2 per kick x 20 samples = 1.86 m2 so you would need to divide the total by 1.86 to get individuals/m2

# total site_date abundances
d <- d %>% 
  group_by(site, date) %>%
  summarize(total_abun = sum(abun)) %>%
  right_join(d) %>%
  select( site, date, order, family, taxon, abun, total_abun)

# Use bio.infer to extract taxonomic information based on 'Taxa' column
d <- d %>%
  ungroup() %>%
  mutate(uid = paste(site, date, sep="_")) %>%
  select(uid, taxon, abun) %>%
  as.data.frame %>%
  get.taxonomic() 

# create lowest_taxon column
d <- d %>%
  as_tibble() %>%
  mutate(lowest_taxon = ifelse(is.na(SPECIES) == F, SPECIES,
                        ifelse(is.na(GENUS) == F, GENUS,
                        ifelse(is.na(FAMILY) == FALSE, FAMILY, 
                        ifelse(is.na(SUPERFAMILY) == FALSE, SUPERFAMILY,
                        ifelse(is.na(INFRAORDER) == FALSE, INFRAORDER,
                        ifelse(is.na(SUBORDER) == FALSE, SUBORDER,
                        ifelse(is.na(ORDER) == FALSE, ORDER,
                        ifelse(is.na(CLASS) == FALSE, CLASS,
                        ifelse(is.na(PHYLUM) == FALSE, PHYLUM, taxon))))))))) )

colnames(d) <- str_to_lower(colnames(d))


d <- d %>% 
    mutate(order = ifelse(order %in% c('BASOMMATOPHORA', 
                                     'NEOTAENIOGLOSSA',
                                     'THYSANOPTERA'), 'GASTROPODA',
                        ifelse(order %in% c('COLLEMBOLA',
                                            'MYSIDA',
                                            'TROMBIDIFORMES') , 'ARTHROPODA',
                               ifelse(order == 'DIPLOSTRACA', 'CLADOCERA',
                                      ifelse(order == 'VENEROIDA', 'BIVALVA', order))))) %>%
  filter(!is.na(order))


d <- d %>%
  mutate(order = str_to_title(order),
         family = str_to_title(family),
         genus = str_to_title(genus),
         lowest_taxon = str_to_title(lowest_taxon)) %>%
  separate(uid, into=c('site', 'date'), sep="_")



# total site_date abundances
d <- d %>% 
  group_by(site, date) %>%
  summarize(total_abun = sum(abun)) %>%
  right_join(d) %>%
  select( site, date, order, family, genus, lowest_taxon, abun, total_abun)

#----------------------------------------------------------------------------
# Export
write_csv(d, 'Rdat/tidy_invert2.csv')

# End 00_tidy_invert
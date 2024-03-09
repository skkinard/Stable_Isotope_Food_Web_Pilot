# 06_HN_range_calc
# written by: sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Description
#----------------------------------------------------------------------------
# The script filters isotopic data for fish and invertebrate guilds, calculates the percentage composition of community species, extracts aquatic species and trophic categories, and estimates isotopic trophic levels for specific species and trophic categories. Then, it computes bootstrap estimates for hydrogen and nitrogen ranges across different guilds and saves the results to a CSV file.

#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

d <- read_csv("Rout/isotope_data_correctedH_correctedN.csv") %>%
  filter(guild %in% c('Fish', 'Invertebrate'))

community_percent <- read_csv('Rdat/tidy_invert2.csv') %>%
  filter(site %in% c('SFC', 'AR', 'GC')) %>%
  mutate(site = case_when(site == 'SFC' ~ 'Semi-Arid',
                          site == 'AR' ~ 'Transition',
                          site == 'GC' ~ 'Sub-Humid')) %>%
  group_by(site, family) %>%
  dplyr::summarize(prct_com = mean(abun/total_abun)*100) %>%
  rename(species=family) %>%
  full_join(read_csv('Data/tidy_fish.csv') %>%
              filter(site %in% c('SFC', 'AR', 'GC')) %>%
              mutate(site = case_when(site == 'SFC' ~ 'Semi-Arid',
                                      site == 'AR' ~ 'Transition',
                                      site == 'GC' ~ 'Sub-Humid')) %>%
              mutate(species=paste(substr(genus,1,1), species,sep='.')) %>%
              group_by(site, species) %>%
              dplyr::summarize(prct_com = mean(abun/total_abun)*100))

my_spp <- c('Corbiculidae', 'Coenagrionidae', 'P.latipinna', "L.macrochirus",
            "L.cyanellus", "L.gulosus")

Aq_spe <- read_csv("Rout/mixing_model_signif.csv") %>%
  filter(m_group %in% my_spp) %>%
  select(m_group, site, mu) %>%
  rename(species = m_group, aq_spe=mu)

Aq_tro <- read_csv("Rout/mixing_model_signif.csv") %>%
  mutate(trophic=ifelse(substr(m_group,1,2) %in% c('F-', 'I-'), 1, 0)) %>%
  filter(trophic>0) %>%
  select(m_group, site, mu) %>%
  rename(trophic_category = m_group, aq_tro=mu)

isotopic_tl_spe <- read_csv('Rout/TL_all.csv') %>%
  filter(m_group %in% my_spp) %>%
  select(m_group, site, TL_mu) %>%
  rename(species=m_group, TL_spe=TL_mu)

isotopic_tl_tro <- read_csv('Rout/TL_all.csv')  %>%
  mutate(trophic=ifelse(substr(m_group,1,2) %in% c('F-', 'I-'), 1, 0)) %>%
  filter(trophic>0) %>%
  select(m_group, site, TL_mu) %>%
  rename(trophic_category=m_group, TL_tro=TL_mu)

#------------------------------------------------------------------------------
# H and N Range bootstrap estimates
#------------------------------------------------------------------------------

niche_dimensions <- function(my_data) {
  table_ranges <- my_data %>%
    group_by(site, guild) %>%
    dplyr::summarize(Hydrogen_mu = estimate_range_mu(hydrogen),
                     Hydrogen_lower = estimate_range_lower(hydrogen),
                     Hydrogen_upper = estimate_range_upper(hydrogen),
                     Carbon_mu = estimate_range_mu(carbon),
                     Carbon_lower = estimate_range_lower(carbon),
                     Carbon_upper = estimate_range_upper(carbon),
                     Nitrogen_mu = estimate_range_mu(nitrogen),
                     Nitrogen_lower = estimate_range_lower(nitrogen),
                     Nitrogen_upper = estimate_range_upper(nitrogen)) %>%
    ungroup() %>%
    pivot_longer(Hydrogen_mu:Nitrogen_upper, names_to='x', values_to='Range') %>%
    separate(x, into=c('element', 'stat'), sep = '_') %>%
    pivot_wider(names_from = stat, values_from = Range) %>%
    fix_site_order() %>%
    arrange(element, guild, site)
  
  table_signif <- table_ranges %>% detect_signif
  
  my_output <- table_signif
  
  return(my_output) }

d_range <- d %>% niche_dimensions()

write_csv(d_range, 'Rout/HN_range.csv')

#------------------------------------------------------------------------------
# 06_HN_range_calc
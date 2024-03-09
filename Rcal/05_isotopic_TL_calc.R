# 05_isotopic_TL_calc
# Sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Description
#----------------------------------------------------------------------------
# The script calculates isotopic trophic levels from corrected nitrogen data, providing summary statistics for guilds, trophic categories, and common species, and reorganizes factor levels for better interpretation, then checks for confidence interval overlap and saves the results to CSV files.

#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

d <- read_csv("Rout/isotope_data_correctedH_correctedN.csv")

#----------------------------------------------------------------------------
# Calculate isotopic trophic level
#----------------------------------------------------------------------------

d <- d %>%
  mutate(
    trophic_level_N = ifelse(nitrogen <= 0, 0, nitrogen / 3.4),
    trophic_level_N_scaling = case_when(
      nitrogen <= 0 ~ 0,
      nitrogen <= 3.4 ~ nitrogen / 3.4,
      nitrogen <= 7 ~ nitrogen / 3.5,
      nitrogen <= 10.8 ~ nitrogen / 3.6,
      nitrogen <= 14.8 ~ nitrogen / 3.7,
      nitrogen > 14.8 ~ nitrogen / 3.8))

# create summary statistics for groups of comparison
tl_stats <- function(x) {
  n_several <- x %>%
    filter(kingdom == 'Animal') %>%
    group_by(site, m_group) %>%
    dplyr::summarize(n=length(trophic_level_N)) %>%
    ungroup() %>% 
    group_by(m_group) %>%
    dplyr::summarize(min_n = min(n)) %>%
    ungroup() %>% filter(min_n>2) %>% pull(m_group) %>% unique()
  
  x %>%
    filter(kingdom == 'Animal') %>%
    filter(m_group %in% n_several) %>%
    group_by(site, m_group) %>%
    dplyr::summarize(TL_mu = estimate_mu(trophic_level_N),
              TL_lower = estimate_mu_lower(trophic_level_N),
              TL_upper = estimate_mu_upper(trophic_level_N),
              TL_n = length(trophic_level_N)) }

# Guild
TL_guild <- d %>%
  rename(m_group = guild) %>%
  tl_stats()

# Trophic Category
TL_TC <- d %>% 
  rename(m_group = trophic_category) %>%
  tl_stats()

# Common Species
TL_species <- d %>% 
  rename(m_group = species) %>%
  tl_stats()

# join summary stats for groups of comparison
TL_all <- full_join(TL_guild, TL_TC) %>% 
  full_join(TL_species)

translate_TL_plus_one <- function(x) {
  x %>%
    mutate(TL_mu = TL_mu + 1,
           TL_lower = TL_lower + 1,
           TL_upper = TL_upper + 1) }

TL_all <- TL_all %>% translate_TL_plus_one()


# Reformat: Organize factor levels
my_groups <- c("Fish", 
               "F-Herbivore", "P.latipinna", 
               "F-Invertivore", "L.macrochirus",
               "F-Piscivore", "L.cyanellus", "L.gulosus",
               "Invertebrate", "I-Filterer",
               "I-Gatherer", "I-Predator","Corbiculidae", 
               "Coenagrionidae")

TL_trim <- TL_all %>%
  as_tibble() %>%
  filter(m_group %in% my_groups) %>%
  fix_site_order() %>%
  mutate(m_group = fct_relevel(m_group, my_groups) )

# Confidence interval overlap?
colnames(TL_trim) <- str_replace_all(colnames(TL_trim), 'TL_','')
TL_trim <- TL_trim  %>% 
  select(-n) %>% 
  detect_signif() %>% 
  left_join(TL_trim) %>%
  select(site, m_group, n, everything())

write_csv(d, 'Rout/TL_d.csv')
write_csv(TL_all, 'Rout/TL_all.csv')
write_csv(TL_trim, 'Rout/TL_signif.csv')

#----------------------------------------------------------------------------
# End 05_isotopic_TL_calc

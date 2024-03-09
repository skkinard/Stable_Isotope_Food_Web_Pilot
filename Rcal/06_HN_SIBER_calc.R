# 06_HN_SIBER_calc
# written by: sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Description
#----------------------------------------------------------------------------
# The script estimates isotopic niche area using a HN ellipse, niche height using Food Chain Length, theoretical niche volume using width * height, and additional metrics such as nearest neighbor distance. It reports metrics for guild, fish sizes, invertebrate trophic groups, and common taxa in a grid.

#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

d <- read_csv("Rout/isotope_data_correctedH_correctedN.csv")

#----------------------------------------------------------------------------
# options for running jags
#----------------------------------------------------------------------------
parms <- list()
parms$n.iter <- 2 * 10^4   # number of iterations to run the model for
parms$n.burnin <- 1 * 10^3 # discard the first set of values
parms$n.thin <- 10     # thin the posterior by this many
parms$n.chains <- 2        # run this many chains

# define the priors
priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3

#----------------------------------------------------------------------------
# Custom SIBER functions:
#----------------------------------------------------------------------------
# Calculate SEA for each group within data (groups must contain 3 or more obs)
my_SEA_function <- function(SEA_data, SEA_group) {
  # Data prep
  siber.example <- SEA_data %>%
    rename(iso1 = hydrogen, 
           iso2 = nitrogen_raw, 
           group = SEA_group, 
           community = site) %>%
    select(iso1, iso2, group, community) %>%
    mutate(iso1 = as.numeric(iso1),
           iso2 = as.numeric(iso2),
           community = as.factor(community)) %>%
    as.data.frame() %>%
    createSiberObject()
  
  # Calculate summary statistics for each group: TA, SEA and SEAc
  group.ML <- groupMetricsML(siber.example)
  ellipses.posterior <- siberMVN(siber.example, parms, priors)
  SEA.B <- siberEllipses(ellipses.posterior)
  
  SEA.columns <- colnames(as_tibble(SEA.B))
  
  for(i in 1:length(SEA.columns)) {
    
    baghold <- as_tibble(SEA.B) %>% pull(SEA.columns[i]) %>% hdr(prob = c(95, 75, 50))
    
    SEA_ci <- baghold$hdr %>% 
      as_tibble(rownames = 'percent') %>% 
      select( lower = 2, upper = 3, percent) %>%
      pivot_longer(cols=lower:upper, names_to = 'position', values_to = 'value') %>%
      mutate(metric = ifelse(percent == '95%' & position == 'lower', 'ci_2.5',
                             ifelse(percent == '95%' & position == 'upper', 'ci_97.5',
                                    ifelse(percent == '75%' & position == 'lower', 'ci_12.5',
                                           ifelse(percent == '75%' & position == 'upper', 'ci_87.5',
                                                  ifelse(percent == '50%' & position == 'lower', 'ci_25',
                                                         ifelse(percent == '50%' & position == 'upper', 'ci_75', NA))))))) %>%
      select(-percent, -position) %>%
      pivot_wider(names_from= metric) %>%
      mutate(mode = baghold$mode %>% as.numeric(),
             site.group = colnames(group.ML)[i]) %>%
      separate(site.group, into = c('site', 'm_group'), sep="\\.") %>%
      full_join(SEA_ci)
    
  }
  return(SEA_ci)
}

# create empty tibble
SEA_ci <- tibble(
  site = character(),
  m_group = character(),
  mode = numeric(),
  ci_2.5 = numeric(),
  ci_12.5 = numeric(),
  ci_25 = numeric(),
  ci_75 = numeric(),
  ci_87.5 = numeric(),
  ci_97.5 = numeric()
)

#----------------------------------------------------------------------------
# SIBER Ellipse Calculations
#----------------------------------------------------------------------------


# Guilds
SEA_guild <- my_SEA_function(d %>%
                               filter(kingdom == 'Animal'),
                             'guild')

# Trophic Groups
my_trophic_groups <- d %>%
  filter(kingdom=='Animal') %>% 
  group_by(site, trophic_category) %>% summarize(n=n()) %>%
  filter(n > 3) %>%
  pull(trophic_category) %>% unique()

# Trophic Groups
SEA_TC <- my_SEA_function(d %>%
                            filter(trophic_category %in% my_trophic_groups),
                          'trophic_category')


# Common Species
d %>%
  filter(kingdom=='Animal') %>% 
  group_by(site, species) %>% summarize(n=n()) %>%
  filter(n >= 3) %>%
  pivot_wider(names_from= 'site', values_from = 'n')

my_common_species <- c('L.cyanellus', 'L.gulosus', 'L.macrochirus', 'P.latipinna')

# Common Species
SEA_species <- my_SEA_function(d %>%
                                 filter(species %in% my_common_species) %>%
                                 mutate(species = str_replace(species, '\\.', '_')),
                               'species') %>%
  mutate(m_group = str_replace(m_group, '_', '\\.'))

# -------------------------------------------------------------------------
# Merge SEA tables
# -------------------------------------------------------------------------
SEA_all <- SEA_guild %>%
  full_join(SEA_TC) %>%
  full_join(SEA_species) %>%
  mutate(site = fct_relevel(site, 'Semi-Arid', 'Transition', 'Sub-Humid'),
         m_group = fct_relevel(m_group,
                               "Fish",
                               "F-Herbivore", 
                               "P.latipinna", 
                               "F-Invertivore",
                               "L.macrochirus",
                               "F-Piscivore",
                               "L.cyanellus", 
                               "L.gulosus",
                               'Invertebrate',
                               'I-Filterer',
                               'I-Gatherer',
                               'I-Predator'))

# detect overlap confidence intervals
SEA_all <- SEA_all %>%
  rename(lower=ci_2.5, upper=ci_97.5, mu=mode) %>%
  select(m_group, site, mu, lower, upper) %>%
  detect_signif()

write_csv(SEA_all, 'Rout/ellipse_output.csv')

#----------------------------------------------------------------------------
# SIBER Layman niche metrics
#----------------------------------------------------------------------------

# Function takes filtered data and the associated grouping label
my_layman_function <- function(my_data, my_group) {

my_siber_object <- my_data %>%
  arrange(site, species) %>%
  mutate(binary=seq(1:length(my_data %>% pull(guild))),
         binary=ifelse((binary %% 2) == 0, 'even', 'odd')) %>%
  rename(iso1 = hydrogen, 
         iso2 = nitrogen_raw, 
         group = binary, 
         community = site) %>%
  select(iso1, iso2, group, community) %>%
  mutate(iso1 = as.numeric(iso1),
         iso2 = as.numeric(iso2),
         community = as.factor(community)) %>%
  as.data.frame() %>%
  createSiberObject()

group.ML <- groupMetricsML(my_siber_object)
ellipses.posterior <- siberMVN(my_siber_object, parms, priors)

# extract the posterior means
mu.post <- extractPosteriorMeans(my_siber_object, ellipses.posterior)

# calculate the corresponding distribution of layman metrics
layman.B <- bayesianLayman(mu.post)

# create empty tibble
layman_stats <- tibble(site = character(),
                       stat = character(),
                       mu = numeric(),
                       med = numeric(),
                       ci_2.5 = numeric(),
                       ci_12.5 = numeric(),
                       ci_25 = numeric(),
                       ci_75 = numeric(),
                       ci_87.5 = numeric(),
                       ci_97.5 = numeric() )

my_sites <- my_siber_object$all.communities
my_stats <- layman.B[[1]] %>% colnames()

for(i in 1:length(my_sites)) {
  site <- my_sites[i]
  for(j in 1:length(my_stats)){
    stat <- my_stats[j]
    mu <- layman.B[[i]][,my_stats[j]] %>% mean()
    med <- layman.B[[i]][,my_stats[j]] %>% median()
    ci_2.5 <- ci(layman.B[[i]][,my_stats[j]], ci= 0.95, method = 'ETI')[2] %>% as.numeric()
    ci_12.5 <- ci(layman.B[[i]][,my_stats[j]], ci= 0.75, method = 'ETI')[2] %>% as.numeric()
    ci_25 <- ci(layman.B[[i]][,my_stats[j]], ci= 0.50, method = 'ETI')[2] %>% as.numeric()
    ci_75 <- ci(layman.B[[i]][,my_stats[j]], ci= 0.50, method = 'ETI')[3] %>% as.numeric()
    ci_87.5 <- ci(layman.B[[i]][,my_stats[j]], ci= 0.75, method = 'ETI')[3] %>% as.numeric()
    ci_97.5 <- ci(layman.B[[i]][,my_stats[j]], ci= 0.95, method = 'ETI')[3] %>% as.numeric()
    
    layman_stats <- full_join(layman_stats, tibble(site, stat, mu, med, 
                                                   ci_2.5, ci_12.5, ci_25, 
                                                   ci_75, ci_87.5, ci_97.5)) }}

# Remove TA and SDNND since # of sites is < 4
layman_stats <- layman_stats %>% filter(mu > 0) %>%
  mutate(stat = ifelse(stat == 'dY_range' , 'N Range',
                       ifelse(stat == 'dX_range', 'D Range',
                              ifelse(stat == 'CD', 'Centroid Distance',
                                     ifelse(stat == 'NND', 'Neighbor Distance', stat))))) %>%
  mutate(stat = fct_relevel(stat, 
                            'D Range','N Range',
                            'Centroid Distance', 'Neighbor Distance'),
         site=fct_relevel(site, "Semi-Arid", 'Transition', 'Sub-Humid'),
         m_group = my_group)
 
return(layman_stats) }

layman_all <- d %>% filter(kingdom=='Animal') %>% my_layman_function(., 'Fish&Invert') %>% 
  full_join(d %>% filter(guild=='Fish') %>% my_layman_function(., 'Fish')) %>%
  full_join(d %>% filter(guild == 'Invertebrate') %>% my_layman_function(., 'Invertebrate')) %>%
  mutate(m_group = fct_relevel(m_group, 'Fish&Invert', 'Fish', 'Invertebrate'))

# Overlapping confidence intervals?
layman_all <- layman_all %>%
  rename(lower=ci_2.5, upper=ci_97.5) %>%
  select(m_group, stat, site, mu, lower, upper) %>%
  detect_signif()

layman_all <- layman_all %>%
  full_join(SEA_all %>% mutate(stat='Ellipse Area'))

write_csv(layman_all, "Rout/layman_output.csv")
#----------------------------------------------------------------------------
# End 06_HN_SIBER_calc
# 01_isotope_scatter_calc
# written by: sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Description
#----------------------------------------------------------------------------
# The script preprocesses isotope data, fixing names, removing detritus and aquatic vegetation, and exploring baselines, followed by analysis and visualization of terrestrial and aquatic sources, identifying and removing outliers.

#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

d <- read_csv('Rdat/master_isotope.csv') %>%
  as_tibble()

# fix names
d <- d %>%
  mutate(site = case_when(
    site == "SFC" ~ "Semi-Arid",
    site == "AR" ~ "Transition",
    site == "GC" ~ "Sub-Humid")) %>%
  mutate(guild = str_replace_all(guild, ' primary producer', ''),
         guild = str_replace_all(guild, ' material', ''),
         guild=str_to_title(guild)) %>%
  mutate(guild = as.factor(guild)) %>%
  mutate(guild = factor(guild, levels = c('Aquatic', 
                                          'Terrestrial', 
                                          'Detritus', 
                                          'Invertebrate', 
                                          'Fish'))) %>%
  fix_site_order()

# Remove detritus and aquatic vegetation (vascular aquatic plants)
d <- d %>%
  filter(! guild == 'Detritus') %>%
  filter(! species == 'aquatic vegetation') %>%
  filter(! species == 'Bryophite')

d <- d %>%  arrange(site, species) %>%
  as.data.frame()

#----------------------------------------------------------------------------
# Explore baselines
#----------------------------------------------------------------------------
source_scatter <- function(my_data) {
  d_prepped <- my_data %>%
    fix_site_order() %>%
    filter(guild %in% c('Aquatic', 'Terrestrial')) %>%
    mutate(species = str_to_title(species)) %>%
    mutate(species=fct_relevel(species, c('Filamentous Algae', 'Periphyton',
                                          'Grass', 'Leaf'))) %>% 
    pivot_longer(cols=c(carbon, hydrogen), 
                 names_to = 'x_name', 
                 values_to = 'x_val')
  
  d_prepped %>%
    ggplot(aes(x_val, nitrogen, shape = guild, fill = species) ) +
    geom_point(size = 4, alpha=.7) +
    geom_point(data = filter(d_prepped, guild == 'Terrestrial' & 
                               x_val >-20 &
                               x_name == 'carbon'),
               shape=21, size=9, fill=NA, color='red') +
    scale_shape_manual(values=c(21, 24)) +
    scale_fill_manual(values=c('green4', 'skyblue1', 'orange4', 'yellow1')) +
    guides(fill = guide_legend(override.aes=list(shape=22))) +
    labs(shape = NULL) +
    labs(fill = NULL) +
    xlab(element_blank()) +
    ylab(expression(paste(delta)^15*N~('\u2030'))) +
    theme_bw(base_size=14) +
    facet_grid(site~x_name, labeller = iso_facet_lab, scales='free_x') }

# raw data with outlier
source_scatterplot_raw <- source_scatter(d) 

# Terrestiral sources are consistent across sites (1 outlier)
# Aquatic sources vary in C at drier sites
#----------------------------------------------------------------------------
# Terrestrial sources
#----------------------------------------------------------------------------
# outlier
filter(d, guild == 'Terrestrial' & carbon >-20) # Semi-Arid grass sample

# grass samples:
filter(d, species == 'grass') %>% select(site, carbon, nitrogen, hydrogen) 
# grass outlier is 15 permil lower than all other grass samples (n=7)

# remove grass outlier (analytical error)
d <- d[-which(d$guild == 'Terrestrial' & d$carbon > -20),]

# terrestrial sources
source_scatterplot <- source_scatter(d)

# generate UID column
d <- d %>%
  mutate(UID = 1:length(d$site))

# Differentiate labels of C3 and C4 grasses
# C3
d$species <- str_replace(d$species, 
                         'grass', 
                         noquote('C3 grass'))
# C4
d[which(d$species == 'C3 grass' & d$carbon > -16), 3] <- 'C4 grass'

#----------------------------------------------------------------------------
# Export isotope data
#----------------------------------------------------------------------------
write_csv(d, 'Rout/isotope_clean.csv')

#----------------------------------------------------------------------------
# End 01_isotope_scatter_calc
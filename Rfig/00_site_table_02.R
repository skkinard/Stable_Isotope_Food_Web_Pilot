# 00_site_table_02
# Goal: export table containing mean, se, and n for canopy, nitrate, phosphate, and phosphate
# Sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

# data extracted from RAPID 2017-2018 site-transect data
# removed transects 75 from SFC, 75 from AR, and 25 from GC due to proximity to bridge

d <- read_csv('Rdat/Site_Day_Transect_GapfillData.csv')
site_info <- read_csv('Rdat/site_lat_lon.csv')

my_evars <- c('canopy_mid',
              'conductivity','dissolved_oxygen',
              'depth_max', 'channel_width',
              'algae_green','algae_cyano','algae_diatom',
              'nitrate','phosphate')

# tidy and filter collection dates
d_trim <- d %>%
  mutate(date = as_date(paste(Year, Month, Day, sep = '-'))) %>%
  rename(site = Site, 
         transect = Transect, 
         canopy_mid = Canopy.Density.Mid,
         conductivity = Conductivity,
         dissolved_oxygen = DO_mg.L,
         channel_width = Width,
         depth_max = Depth.Mx,
         algae_green = Green.Algae,
         algae_diatom = Diatoms,
         algae_cyano = Bluegreen.cyano,
         nitrate = NO3N,
         phosphate = Ortho.P) %>%
  select(site, date, transect, any_of(my_evars)) %>%
  filter(date > as_date('2018-04-20') & 
           date < as_date('2018-07-20')) %>% 
  mutate(site = case_when(site == 'SFC' ~ 'Semi-Arid',
                          site == 'AR' ~ 'Transition',
                          site == 'GC' ~ 'Sub-Humid')) %>%
  fix_site_order()

# Flow Data
d_flow <- read_csv('Rdat/flow_record_allsites_2017_to_2020.csv') %>%
  filter(site_code %in% c('SF', 'AR', 'GC')) %>%
  filter(year == 2018) %>%
  filter(q>0) %>%
  fix_site_code() 

# summary stats
d_stats <- d_trim %>%
  rename(cyanobacteria=algae_cyano,
         diatom=algae_diatom,
         green_algae=algae_green,
         Max_depth=depth_max,
         Canopy=canopy_mid) %>%
  pivot_longer(cols=Canopy:phosphate, names_to='Variable', values_to='E_val') %>%
  group_by(site, Variable) %>%
  dplyr::summarize(mu=estimate_mu(E_val),
                   lower=estimate_mu_lower(E_val),
                   upper=estimate_mu_upper(E_val))

flow_stats <- d_flow %>%
  group_by(site_code) %>%
  dplyr::summarize(mu=estimate_mu(q),
                   lower=estimate_mu_lower(q),
                   upper=estimate_mu_upper(q)) %>%
  rename(site=site_code) %>%
  mutate('Variable' = 'Discharge')

site_stats <- site_info %>% 
  select(-site, -`Site Name`) %>%
  rename(site=`Site Category`) %>%
  pivot_longer(cols=Latitude:Elevation, names_to='Variable', values_to='mu')

# table
variable_order<- c(
  'Latitude','Longitude','Rainfall','Temperature','Elevation',
  'Canopy','Channel Width','Max Depth', 
  'Dissolved Oxygen', 'Nitrate','Phosphate',
  'Diatom','Green Algae', 'Cyanobacteria', 'Conductivity','Discharge')

full_join(site_stats, flow_stats) %>%
  full_join(d_stats) %>%
  mutate(smmry=ifelse(
    is.na(lower), tormat(mu, 2),
           paste(tormat(mu, 1), ' (',
                 tormat(lower, 0), ":",
                 tormat(upper, 0), ")",
                 sep=''))) %>%
  mutate(smmry=str_replace_all(smmry, "  ", "")) %>%
  select(site, Variable, smmry) %>%
  pivot_wider(names_from=site, values_from=smmry) %>%
  mutate(Variable=str_replace_all(Variable, '_', ' '),
         Variable=str_to_title(Variable),
         Variable=fct_relevel(Variable, variable_order)) %>%
  arrange(Variable) %>%
  rename(E.Variable =Variable)

# plots
annual_flows <- d_flow %>% 
  ggplot(aes(q, fill=site_code, color = site_code)) +
  geom_density(alpha=.1) +
  scale_x_log10() +
  scale_color_manual(values=c('goldenrod3', 'skyblue2','blue')) +
  scale_fill_manual(values=c('goldenrod3', 'skyblue2','blue')) +
  theme_bw(base_size=14) +
  ylab('Frequency') +
  xlab('Discharge (l/s)') +
  theme(legend.position=c(.72,.84)) +
  theme(legend.title=element_blank())
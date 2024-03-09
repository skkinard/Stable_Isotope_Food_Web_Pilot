# 01_site_table
# Sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Description
#----------------------------------------------------------------------------
# The script generates a table containing mean, standard error, and sample size for environmental variables including canopy, nitrate, phosphate, channel width, depth, conductivity, and discharge, exported for further analysis.

#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

# data extracted from RAPID 2017-2018 site-transect data
# removed transects 75 from SFC, 75 from AR, and 25 from GC due to proximity to bridge

# load data
d <- read_csv('Rdat/Site_Day_Transect_GapfillData.csv')
site_info <- read_csv('Rdat/site_lat_lon.csv')

# Trim and rename variables
d_trim <- d %>%
          mutate(date = as_date(paste(Year, Month, Day, sep = '-'))) %>%
          rename(site = Site, 
            transect = Transect, 
            canopy_left = Canopy.Density.Left,
            canopy_right = Canopy.Density.Right,
            canopy_mid = Canopy.Density.Mid,
            conductivity = Conductivity,
            channel_width = Width,
            depth_max = Depth.Mx,
            algae_green = Green.Algae,
            algae_diatom = Diatoms,
            algae_cyano = Bluegreen.cyano,
            nitrate = NO3N,
            phosphate = Ortho.P) %>%
          select(site, date, transect, canopy_left, canopy_right, canopy_mid, channel_width, depth_max, algae_green, algae_diatom, algae_cyano, conductivity, nitrate, phosphate)

# -----------------------------------------------------------------------------
# Calculations: Summary statistics (mean + standard error)
# -----------------------------------------------------------------------------

# Canopy and Algae
my_se <- function(x) { 
  sd(x)/sqrt(length(x)*3) } # 3 replicates per sample event

ca_table <- d_trim %>%
            filter(date > as_date('2018-04-20') & 
                     date < as_date('2018-07-20')) %>%
            group_by(site, date, transect) %>%
            summarise(canopy_tr = mean(canopy_left, 
                                       canopy_right, 
                                       canopy_mid),
                      algae_tr = sum(algae_green, 
                                     algae_diatom, 
                                     algae_cyano)) %>%
            group_by(site, date) %>%
            summarise(canopy_dt = mean(canopy_tr),
                      algae_dt = mean(algae_tr)) %>%
            group_by(site) %>%
            summarise('In-Situ n' = length(algae_dt)*3,
                      canopy_m = mean(canopy_dt),
                      canopy_se = my_se(canopy_dt),
                      algae_m = mean(algae_dt),
                      algae_se = my_se(algae_dt) ) 

ca_table_short <- ca_table %>%
            mutate(canopy = paste(substr(canopy_m, 1, 4), 
                                  expression('\u00B1'),
                                  substr(canopy_se, 1, 4)),
                   algae = paste(substr(algae_m, 1, 4), 
                                  expression('\u00B1'),
                                  substr(algae_se, 1, 4)) ) %>%
            select(site, 'In-Situ n', canopy, algae)
            
# Channel width, depth, nitrates, phosphates       

other_means <- d_trim %>%
  filter(date > as_date('2018-04-20') & 
           date < as_date('2018-07-20')) %>%
  group_by(site) %>%
  summarise(
    channel_width_m = mean(channel_width),
    channel_width_se = my_se(channel_width),
    max_depth_m = mean(depth_max),
    max_depth_se = my_se(depth_max),
    conductivity_m = mean(conductivity),
    conductivity_se = my_se(conductivity),
    nitrate_m = mean(nitrate),
    nitrate_se = my_se(nitrate),
    phosphate_m = mean(phosphate),
    phosphate_se = my_se(phosphate))

other_means_short <- other_means %>%
  mutate(channel_width = paste(substr(channel_width_m, 1, 4), 
                                  expression('\u00B1'),
                                  substr(channel_width_se, 1, 4)),
         depth_max = paste(substr(max_depth_m, 1, 4), 
                                  expression('\u00B1'),
                                  substr(max_depth_se, 1, 4)),
         conductivity = paste(substr(conductivity_m, 1, 4), 
                                  expression('\u00B1'),
                                  substr(conductivity_se, 1, 4)),
         nitrate = paste(substr(nitrate_m, 1, 4), 
                                  expression('\u00B1'),
                                  substr(nitrate_se, 1, 4)),
         phosphate = paste(substr(phosphate_m, 1, 4), 
                                  expression('\u00B1'),
                                  substr(phosphate_se, 1, 4))) %>%
            select(site, channel_width, depth_max, conductivity, nitrate, phosphate )

# Flow Data
d_flow <- read_csv("Rdat/RDA_environment.csv") %>% # FLOW DATA
  filter(collection_date > ymd('2018-04-20') & 
           collection_date < ymd('2018-07-20')) %>%
  rename(site=site_code) %>%
  group_by(site) %>%
  summarise(
    flow_2wk_mean_m = mean(flow_2wk_mean),
    flow_2wk_mean_se = my_se(flow_2wk_mean)) %>%
  ungroup() %>%
  mutate("Discharge" = paste(substr(flow_2wk_mean_m, 1, 4), 
                              expression('\u00B1'),
                              substr(flow_2wk_mean_se, 1, 4))) %>%
  select(-flow_2wk_mean_m, -flow_2wk_mean_se) %>%
  mutate(site = ifelse(site == 'Semi-Arid', 'SFC',
                       ifelse(site == 'Transition', 'AR', 'GC')))
  
# -----------------------------------------------------------------------------
# Index
# -----------------------------------------------------------------------------

# site category, site name, latitude, longitude
site_table_index <- site_info %>%
              left_join(ca_table) %>%
              left_join(other_means) %>%
              mutate( Rainfall = round(Rainfall, 2),
                      Temperature = round(Temperature, 2) ) %>%
              select(-site)

write_csv(site_table_index, 'Rout/site_table_index.csv')
# -----------------------------------------------------------------------------
# Table
# -----------------------------------------------------------------------------

site_table <- site_info %>%
  left_join(d_flow) %>%
  left_join(ca_table_short) %>%
  left_join(other_means_short) %>%
  mutate( Rainfall = round(Rainfall, 2),
          Temperature = round(Temperature, 2) ) %>%
  select(-site, -`Site Category`) %>%
  t()
colnames(site_table) <- c('Semi-Arid', 'Transition', 'Sub-Humid')

site_table <- site_table %>%
  as_tibble(rownames = 'E.Variable') %>%
  mutate('E.Variable' = str_replace_all(`E.Variable`, 
                                        pattern='_', replacement=" "),
         'E.Variable' = str_to_title(`E.Variable`),
         'E.Variable' = str_replace_all(`E.Variable`, "In-Situ N",
                                        "Measurements")) %>%
  mutate(Units = c('', 'DD', 'DD', 'cm/yr', '°C', 'm', 'cfs', '',
                   '%', 'µg/cm^2', 'm', 'm', 'µcm/S', 'mg/L', 'mg/L')) %>%
  select(E.Variable, Units, `Semi-Arid`, `Transition`, `Sub-Humid`)

write_csv(site_table, 'Rout/site_table.csv')
write_csv(site_table, 'Rfig/site_table.csv')

# -----------------------------------------------------------------------------
# End 01_site_table
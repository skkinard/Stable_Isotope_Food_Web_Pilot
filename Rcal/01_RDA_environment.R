# 01_RDA_environment
# written by: sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Description
#----------------------------------------------------------------------------
# The script prepares and tidies environmental data including flow, transect measurements, and rainfall, merging them by site and date for analysis, filtering for a specific time period, and selecting relevant environmental variables.

#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

gauge <- read_csv('Rdat/site_table.csv') %>%
  rename(site_code = `Site Category`)

flow <- read_csv("Rdat/Site_Day_Flow.csv") %>%
  mutate(site_code = substr(Site, 1,2)) %>%
  unite(col = collection_date, "Year":"Day", sep = "-") %>%
  mutate(collection_date = ymd(collection_date)) %>%
  select(-c(UID, Site, Cycle)) %>%
  select(site_code, collection_date, everything())

transect <- read_csv("Rdat/Site_Day_TransectData.csv") %>%
  mutate(Site_Code = substr(Site, 1,2)) %>%
  unite("Collection_Date", c("Year", "Month", "Day"), sep='-') %>%
  mutate(Collection_Date = ymd(Collection_Date))

colnames(transect) <- str_to_lower(colnames(transect))

#----------------------------------------------------------------------------
# Tidy environmental data
#----------------------------------------------------------------------------

# Calculate mean and sd for transect measurements (grouped by site and sampling date)
# filter transect data to May-July 2018
transect <- transect %>%
  pivot_longer(cols = bluegreen.cyano : ortho.p,
               names_to = "measure",
               values_to = "value") %>%
  group_by(site_code, collection_date, measure) %>%
  summarize(xmean = mean(value, na.rm=T)) %>%
  mutate(measure = paste(measure, 'mean', sep='_')) %>%
  pivot_wider(names_from = measure, values_from = xmean) %>%
  left_join( transect %>%
               pivot_longer(cols = bluegreen.cyano : ortho.p,
                            names_to = "measure",
                            values_to = "value") %>%
               group_by(site_code, collection_date, measure) %>%
               summarize(xsd = sd(value) ) %>%
               mutate(measure = paste(measure, 'sd', sep = '_')) %>%
               pivot_wider(names_from = measure, values_from = xsd) ) %>%
  filter(site_code == 'SF' | site_code == 'AR' | site_code == 'GC') %>%
  filter(collection_date > as_date('2018-04-29') & collection_date < as_date('2018-08-01')) %>%
  mutate(site_code = ifelse(site_code=='SF', 'Semi-Arid',
                            ifelse(site_code == 'AR', 'Transition', 'Sub-Humid')))

# filter flow metrics to May-July 2018
flow <- flow %>%
  filter(site_code == 'SF' | site_code == 'AR' | site_code == 'GC') %>%
  filter(collection_date > as_date('2018-04-29') & collection_date < as_date('2018-08-01')) %>%
  mutate(site_code = ifelse(site_code=='SF', 'Semi-Arid',
                            ifelse(site_code == 'AR', 'Transition', 'Sub-Humid')))

#----------------------------------------------------------------------------
# select environmental variables. Merge to community
#----------------------------------------------------------------------------

de <- gauge %>%
  rename(annual_rainfall = Rainfall) %>%
  select(site_code, annual_rainfall) %>%
  right_join( flow %>%
  rename(flow_2wk_mean = Q.2w.mn) %>%
  select(site_code, collection_date, flow_2wk_mean) ) %>%
  left_join( transect %>%
  rename(conductivity = conductivity_mean, 
         depth_max = depth.mx_mean,
         nitrate = no3n_mean,
         canopy = canopy.total_mean) %>%
    mutate(algae = sum(diatoms_mean, green.algae_mean, bluegreen.cyano_mean)) %>%
  select(site_code, collection_date, conductivity, depth_max, nitrate, algae, canopy) ) %>%
  unique()

write_csv(de, "Rdat/RDA_environment.csv")

#----------------------------------------------------------------------------
# End 01_RDA_environment
# 08_autotrophy
# written by: sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Description
#----------------------------------------------------------------------------
# The script computes flow statistics and autotrophic predictors for different sites, including median flow, maximum flow deviation from the median, standard deviation of flow, and the percentage of days with flow exceeding three times the median flow. Additionally, it calculates autotrophic predictors such as algal plate measurements, chlorophyll levels, nitrate, and phosphate concentrations, considering factors like rainfall and canopy density.

# Resuls summary: 
# The sub-tropical coastal streams in Texas have sandy substrates that can easily by disturbed by flood events. In the arid region, base flows are infrequently interrupted by intense, short-lived floods. In the humid region, floods are less intense and occur more frequently. I hypothesize that disturbance frequency influences benthic algal production more than intensity because the threshold for bed movement low for sand substrates. So, the stable base flows during drought periods should facilitate algal growth and that frequent low-intensity floods in the humid region hinder algal growth. Our results are consistent with this theory with declines in algal growth in wetter climate. Additionally, there were field observations of flooding and sand movement which may have scoured growth plate tiles. In this script, I will use daily flow data to calculate base flow, number of floods or proportion of time spent in a state of flooding, and the maximum flood strength during May and June 2019. These variables will then be examined as explanatory mechanisms for the observed algal patterns.
#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

d_flow <- read_csv('Rdat/flow_record_allsites_2017_to_2020.csv') %>%
  filter(site_code %in% c('SF', 'AR', 'GC')) %>%
  filter(year == 2018) %>%
  filter(month %in% c(5,6,7,8)) %>%
  fix_site_code() 
  
d_ex <- read_csv('Rout/exclosure_stats.csv')
colnames(d_ex) <- str_to_lower(colnames(d_ex))
d_ex <- d_ex %>%
  rename(site_code = site)

d_en <- read_csv('Rdat/Site_Day_Mean_GapfillData.csv')
colnames(d_en) <- str_to_lower(colnames(d_en))
colnames(d_en) <- str_replace_all(colnames(d_en), "\\.", '_')

REU_dates <- d_en %>%
  rename(site_code = site) %>%
  mutate(site_code = substr(site_code, 1,2)) %>%
  filter(site_code %in% c('SF', 'AR', 'GC')) %>%
  filter(year == 2018) %>%
  filter(month %in% c(7,8)) %>%
  mutate(collection_date = ymd(paste(year, month, day, sep='-'))) %>%
  select(site_code, collection_date) %>%
  mutate(x=case_when(collection_date<ymd('2018-07-15') ~ 'Start',
                     collection_date>ymd('2018-07-15') ~ 'Stop')) %>%
  pivot_wider(names_from=x, values_from=collection_date) %>%
  fix_site_code() 

REU_dates <- REU_dates %>%
  mutate(Start_mu = mean(REU_dates$Start),
         Stop_mu = mean(REU_dates$Stop)) 

write_csv(REU_dates,'Rdat/REU_dates.csv')

d_en <- d_en %>%
  rename(site_code = site) %>%
  mutate(site_code = substr(site_code, 1,2)) %>%
  filter(site_code %in% c('SF', 'AR', 'GC')) %>%
  filter(year == 2018) %>%
  filter(month %in% c(5,6,7,8)) %>%
  select(site_code, year, month, contains('canopy'), 
         contains('green'), diatoms, no3n, ortho_p) %>%
  fix_site_code() 

d_rain <- read_csv('Data/site_data.csv') %>%
  select(Site, AnnualRain) %>%
  rename(site_code = Site,
         rain=AnnualRain) %>%
  mutate(site_code = substr(site_code, 1,2)) %>%
  filter(site_code %in% c('SF', 'AR', 'GC')) %>%
  fix_site_code() 

#d_turb <- read_csv('Data/Pre-TERRG Field Data.csv') %>%
#    rename(site_code = Site) %>%
#    mutate(site_code = substr(site_code, 1,2)) %>%
#    filter(site_code %in% c('SF', 'AR', 'GC')) %>%
#    select(site_code, Year, Month, Transect, 
#           Turbidity) %>%
#    filter(Month %in% c(5,6)) %>% 
#    group_by(site_code, Month) %>%
#    dplyr::summarize(turbidity_mu = mean(Turbidity, na.rm=T),
#                     turbidity_sd = sd(Turbidity, na.rm=T))
# It doesn't make sense to assess turbidity since sampling always took place at low flows. The field measurement using an YSI probe is problematic in my experience and the measurement would be more informative if we had turbidity readings at the various flow rates during the experiment. 
  
#----------------------------------------------------------------------------
# Flow Statistics
#----------------------------------------------------------------------------

# flow_base = median flow
# HFPP3 = % of days >= 3x flow_base
# HFPP9 = % of days >= 9x flow_base
# flow_max = (max - median) / median

flow_stats <- d_flow %>%
  left_join(REU_dates) %>%
  filter(collection_date > Start_mu) %>%
  filter(collection_date < Stop_mu) %>%
  group_by(site_code) %>%
  dplyr:: summarize(flow_base = median(q),
                    flow_max = (max(q) - flow_base) / flow_base,
                    flow_sd = sd(q)) %>%
  right_join(d_flow) %>%
  ungroup() %>%
  mutate(flow_3x = ifelse(q >= 3*flow_base, 1, 0))

flow_stats <- flow_stats %>% 
  group_by(site_code) %>%
  dplyr::summarise(HFPP3 = sum(flow_3x)/length(flow_3x)*100) %>%
  right_join(flow_stats) %>%
  select(site_code, flow_base, flow_sd, flow_max, HFPP3) %>%
  unique()

write_csv(flow_stats, 'Rout/flow_stats.csv')


#----------------------------------------------------------------------------
# Autotrophic Predictors
#----------------------------------------------------------------------------

d_algae <- d_ex %>% # Algal plates
  group_by(site_code) %>%
  dplyr::summarize(tile_chl = mean(algae_mean)) %>%
  left_join(d_rain) %>%
  full_join(d_en %>% # Benthotorch
              mutate(chlorophyll_total = 
                       green_algae + bluegreen_cyano + diatoms) %>%
              group_by(site_code) %>%
              dplyr::summarise(canopy = mean(canopy_density_mid),
                               nitrate = mean(no3n),
                               phosphate = mean(ortho_p))  ) %>%
  full_join(flow_stats) %>%
  mutate(site_code = fct_relevel(site_code, c('Semi-Arid', 'Transition',
                                              'Sub-Humid'))) %>%
  arrange(site_code) %>%
  mutate(canopy=canopy*100)

write_csv(d_algae, 'Rout/d_autorophy.csv')

#----------------------------------------------------------------------------
# End 08_autotrophy_calc
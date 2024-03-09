# fig_autotrophy
# written by: Sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

REU_dates <- read_csv('Rdat/REU_dates.csv') %>%
  mutate(site_code=fct_relevel(site_code, c('Semi-Arid', 'Transition', 'Sub-Humid')))

flow_stats <- read_csv('Rout/flow_stats.csv') %>%
  mutate(site_code=fct_relevel(site_code, c('Semi-Arid', 'Transition', 'Sub-Humid')))

d_algae <- read_csv('Rout/d_autorophy.csv')  %>%
  mutate(site_code=fct_relevel(site_code, c('Semi-Arid', 'Transition', 'Sub-Humid')))

d_flow <- read_csv('Rdat/flow_record_allsites_2017_to_2020.csv') %>%
  filter(site_code %in% c('SF', 'AR', 'GC')) %>%
  filter(year == 2018) %>%
  filter(month %in% c(5,6,7,8)) %>%
  fix_site_code() %>%
  mutate(site_code=fct_relevel(site_code, c('Semi-Arid', 'Transition', 'Sub-Humid')))

#----------------------------------------------------------------------------
# Plot Hydro during REU
#----------------------------------------------------------------------------
REU_flow_timeseries <- d_flow %>%
  left_join(REU_dates) %>%
  filter(collection_date > Start_mu) %>%
  filter(collection_date < Stop_mu) %>%
  ggplot(aes(collection_date, q, fill=site_code, color = site_code)) +
  geom_line(show.legend = F) +
  scale_y_log10() +
  scale_color_manual(values=c('goldenrod3', 'skyblue2','blue')) +
  scale_fill_manual(values=c('goldenrod3', 'skyblue2','blue')) +
  theme_bw(base_size=14) +
  ylab(element_blank()) +
  ylab('Discharge (l/s)') +
  xlab('Time') +
  theme(legend.title=element_blank())

hist_base <- function(my_data) {
  my_data %>%
    ggplot(aes(q, fill=site_code, color = site_code)) +
    geom_density(alpha=.1) +
    scale_x_log10() +
    scale_color_manual(values=c('goldenrod3', 'skyblue2','blue')) +
    scale_fill_manual(values=c('goldenrod3', 'skyblue2','blue')) +
    theme_bw(base_size=14) +
    ylab('Frequency') +
    xlab('Discharge (l/s)')
}

flow_density <- d_flow %>%
  hist_base() +
  theme(legend.title=element_blank())

flow_density_sites <- d_flow %>%
  hist_base() +
  facet_wrap(~site_code, ncol=3) +
  theme(legend.position = 'none')

# Throughout the exclosure experiment, flow variability was higher at sites with wetter climates. The Semi-Arid site had the most stable flows with 4.07% of daily discharges exceeding three times the base flow conditions. Discharge narrowly clustered around the base flow (median discharge) of 35.4 l/s. The Transition site experienced slightly more flooding in the experimental period, exceeding three times the base flow 17.1% of the time. Transition site flows remained highly consistent around the base flow of 58.6 l/s. The Sub-Humid site's flows were highly variable during the experimental period. Sub-Humid flows exceeded three times the base flow estimate 22.0% of the time and the histogram shows little evidence for a consistent 'base-flow' (6.65 l/s). Unexpectedly, the Sub-Humid stream had the highest density of extremely low discharge. Flow stability at arid sites may be related to waste-water or reservoir outflows.

#----------------------------------------------------------------------------
# Plot Algae
#----------------------------------------------------------------------------
algae_labels <- as_labeller(
  c(tile_chl="Chlorophyll~(μg/cm)",
    flow_max="Flood~Max~(Ratio)", 
    HFPP3="Flooding~('%')",
    canopy="Canopy~('%')",
    nitrate='NO[3]~(mg/L)',
    phosphate='PO[4]~(mg/L)'),
  default = label_parsed)

x_variables <- c('tile_chl', 'flow_max', 'HFPP3', 'canopy', 'nitrate', 'phosphate')

p_algae <- d_algae %>%
  select(site_code, rain, any_of(x_variables)) %>%
  pivot_longer(cols = x_variables, 
               names_to='xvar',
               values_to='xval') %>%
  mutate(xvar=fct_relevel(xvar, x_variables)) %>%
  ggplot(aes(rain, xval)) +
  geom_path(linetype=2, linewidth=.2, color='grey80') +
  geom_point(aes(fill=site_code),
             size = 4, shape = 21, color='black') +
  facet_wrap(~xvar, scales='free', nrow=2, labeller=algae_labels) +
  scale_fill_manual('Site', values=c('yellow2', 'green3', 'skyblue2')) +
  dark_theme_grey(base_size=18) +
  ylab(element_blank()) +
  xlab('Annual Rainfall (cm)')

x_slim <- c('flow_max', 'HFPP3', 'nitrate', 'phosphate')

p_algae_slim <- d_algae %>%
  select(site_code, rain, any_of(x_slim)) %>%
  pivot_longer(cols = x_slim, 
               names_to='xvar',
               values_to='xval') %>%
  mutate(xvar=fct_relevel(xvar, x_slim)) %>%
  ggplot(aes(rain, xval)) +
  geom_path(linetype=2, linewidth=.2, color='grey20') +
  geom_point(size = 2, shape = 21, color='black', fill='darkgreen') +
  facet_wrap(~xvar, scales='free', nrow=2, labeller=algae_labels) +
  theme_classic(base_size=11) +
  ylab(element_blank()) +
  xlab('Annual Rainfall (cm)')

#----------------------------------------------------------------------------
# Table
#----------------------------------------------------------------------------
table_algae <- d_algae  %>%
  mutate(canopy = canopy*100) %>%
  mutate(tile_chl = tormat(tile_chl,2),
         rain = tormat(rain,1),
         canopy = tormat(canopy, 1),
         nitrate = tormat(nitrate, 2),
         phosphate = tormat(phosphate, 2),
         flow_base = tormat(flow_base,1),
         flow_sd = tormat(flow_sd,1),
         flow_max = tormat(flow_max, 2),
         HFPP3 = tormat(HFPP3, 1) )

colnames(table_algae) <- c('Site', 'Chlorophyll', 'Rain', 'Canopy', 
                           'Nitrate', 'Phosphate', 
                           'Q median', 'Q sd', 'Max Flood', 'Flooding')

write_csv(table_algae,'Rfig/table_autotrophy.csv')

#----------------------------------------------------------------------------
# End fig_autotrophy

#----------------------------------------------------------------------------
# Summary
#----------------------------------------------------------------------------

# Average chlorophyll concentrations from all ceramic plates (exclosure and controls) decreased exponentially with rainfall; Chlorophyll concentrations were greatest at the Semi-Arid site (2.00), low at the Transition site (0.30) and extremely low at the Sub-Humid site (0.06). We expected increased canopy in more humid watersheds would hinder in-stream algal growth. However, riparian canopy coverage varied little among sample sites (77%, 82%, and 81%) making it an unlikely driver of observed algal growth patterns. Suspended sediments and dissolved compounds also regulate the quantity and quality of radiation reaching stream benthos but were not assessed in this study. Therefore, light-limitation along the rainfall gradient would be more thoroughly assessed with light sensors and frequent turbidity measurements. 

# We expected nutrients to be higher in streams within drier climates because waste-water outfalls provide nutrient-rich base flows that are rarely diluted by rainfall but are often concentrated by evaporation. We found elevated nitrate and phosphate concentrations at the Semi-Arid (12.4 and 2.1 mg/l) and Transition streams (4.1 and 2.2 mg/l). So nutrient concentrations coincide with observed algal patterns. We also hypothesized that floods can disrupt algal establishment and growth on loose, sandy substrates. During the month of the experiment, floods exceeding three times the median discharge were least frequent at the Semi-Arid site (4.07%), moderately frequent at the Transition site (11.5%), and most frequent at the Sub-Humid site (22.0%). Flood strength were also highest at the Sub-Humid site. Floods at the Sub-Humid site are more likely to disrupt substrates which could cover growth tiles with sand or scour the surfaces clean. Nutrients and flow data provide reasonable explanations for observed algal patterns.
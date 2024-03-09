# 04_adjust_d15N_calc
# Sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Description
#----------------------------------------------------------------------------
# The script conducts nitrogen calibration for compound-specific isotope analysis (CSIA) data, adjusting d15N values based on the proportion of nitrogen sources and their isotopic compositions. It then visualizes the transformed nitrogen data using boxplots and density plots, finally saving the adjusted data to a CSV file.

#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

d_raw <-  read_csv("Rdat/isotope_data_correctedH.csv")

# extract %Aq and % Te
src_percent <- read_csv("Rout/mixing_model_signif.csv") %>% 
  rename(trophic_category = m_group, percent_AQ_mu = mu) %>%
  select(site, trophic_category, percent_AQ_mu) %>%
  mutate(percent_AQ_mu = .01*percent_AQ_mu,
         percent_TE_mu = 1 - percent_AQ_mu)

boxplot_nitrogen <- d_raw %>%
  fix_site_order() %>%
  ggplot(aes(site, nitrogen, fill=guild)) +
  geom_boxplot() +
  scale_fill_manual(values=my_colors2)

# extract Aq and Te average d15N
src_d15n <- d_raw %>% 
  filter(kingdom != 'Animal') %>% group_by(site, guild) %>%
  summarize(source_N_mu = estimate_mu(nitrogen)) %>%
  pivot_wider(names_from=guild, values_from=source_N_mu) %>%
  rename(src_d15N_AQ_mu = Aquatic,
         src_d15N_TE_mu=Terrestrial)

# combine with original dataframe (fix sources)
d <- d_raw %>% left_join(src_d15n) %>% left_join(src_percent) %>%
  mutate(percent_AQ_mu= case_when(
    guild=='Aquatic' ~ 1,
    guild=='Terrestrial' ~ 0,
    guild %in% c('Fish', 'Invertebrate') ~ percent_AQ_mu)  ) %>%
  mutate( percent_TE_mu = 1 - percent_AQ_mu ) 


#----------------------------------------------------------------------------
# Nitrogen Calibration
#----------------------------------------------------------------------------
# Orient d15N to site resource basal values
# subtract proportions of each resource multiplied by the source mean d15N to obtain the N value of the sample due to fractionation

d <- d %>% mutate(
  nitrogen_raw = nitrogen,
  nitrogen = case_when(
  guild == 'Aquatic' ~ nitrogen_raw - src_d15N_AQ_mu,
  guild == 'Terrestrial' ~ nitrogen_raw - src_d15N_TE_mu,
  kingdom == 'Animal' ~ nitrogen_raw - percent_AQ_mu*src_d15N_AQ_mu -
      percent_TE_mu*src_d15N_TE_mu),
  nitrogen=ifelse(is.na(nitrogen), nitrogen_raw, nitrogen)) %>%
  fix_site_order()

write_csv(d, "Rout/isotope_data_correctedH_correctedN.csv")

#----------------------------------------------------------------------------
# visualize transformed N data
#----------------------------------------------------------------------------
plot_n_v_nraw <- d %>% 
  filter(kingdom=='Animal') %>%
  ggplot(aes(nitrogen_raw, nitrogen, color = site, shape = guild)) +
  geom_point(size=3, alpha=.5) +
  facet_grid(~site) +
  theme(legend.position='none') +
  theme_bw(base_size=14)

hist_n_v_nraw <- d %>% 
  filter(species %in% common_species) %>%
  mutate(species=substr(species, 1,4)) %>%
  pivot_longer(cols=contains('nitrogen'), names_to='type', values_to = 'n_val') %>%
  ggplot(aes(n_val, fill=type)) +
  geom_density(alpha=.5) +
  facet_grid(site~ trophic_category) +
  theme(legend.position='none') +
  theme_bw(base_size=14)

#----------------------------------------------------------------------------
# End 04_adjust_d15N_calc
# 02_algae_boot_calc
# written by: sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Description
#----------------------------------------------------------------------------
# The script performs bootstrapping to estimate means and 95% confidence intervals (CI) for total algae across different treatments and sites, detects non-overlapping 95% CI, and exports the results.

#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

d <- read_csv('Rdat/2018_REU_pecan_algae.csv')

#----------------------------------------------------------------------------
# Bootstrap mean and 95% CI
#----------------------------------------------------------------------------

# tidy data
d <- d %>%
  rename(site=location) %>%
  select(-cyanobacteria, -green_algae, -diatoms) %>%
  mutate(treatment=case_when(
    treatment =='C' ~ 'Open',
    treatment == 'E' ~ 'Exclosure' ))

# Bootstrap estimate means and 95% CI
algae_boot <- d %>%
  filter(site != 'WMC') %>%
  mutate(site = case_when(
    site == 'SFC' ~ 'Semi-Arid',
    site == 'AR' ~ 'Transition',
    site == 'GC' ~ 'Sub-Humid' )) %>%
  fix_site_order() %>%
  group_by(site, treatment) %>%
  dplyr::summarize(
    mu = estimate_mu(total_algae),
    lower = estimate_mu_lower(total_algae),
    upper = estimate_mu_upper(total_algae) )

# Detect non-overlapping 95% CI
algae_boot <- algae_boot %>%
  detect_signif() %>%
  rename(Sit.Sig=signif) %>%
  arrange(treatment, site) %>%
  left_join(algae_boot %>%
              detect_signif2() %>%
              rename(Tre.Sig=signif) )

write_csv(algae_boot, 'Rout/algae_boot.csv')

#----------------------------------------------------------------------------
# End 02_algae_boot_calc
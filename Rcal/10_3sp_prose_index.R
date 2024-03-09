# 10_3SP_index
# Contains abbreviations for values extracted from results
# Sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Description
#----------------------------------------------------------------------------
# The script contains abbreviations for values extracted from results, including aquatic assimilation, trophic level, exclosure statistics, community niche metrics, density, diversity, autotrophy, and RDA analysis for both fish and invertebrates.

#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')
#------------------------------------------------------------------------------
# mixing model (aquatic assimilation)
#------------------------------------------------------------------------------
table_aq_assimilated <- read_csv('Rfig/table_aq_assimilated.csv')

aq_f_A <- table_aq_assimilated %>%
  filter(Group == 'Fish') %>%
  pull(`Semi-Arid`) %>%
  substr(1,2) %>% paste(., '%', sep='')
aq_f_T <- table_aq_assimilated %>%
  filter(Group == 'Fish') %>%
  pull(`Transition`) %>%
  substr(1,2) %>% paste(., '%', sep='')
aq_f_H <- table_aq_assimilated %>%
  filter(Group == 'Fish') %>%
  pull(`Sub-Humid`) %>%
  substr(1,1) %>% paste(., '%', sep='')

aq_i_A <- table_aq_assimilated %>%
  filter(Group == 'Invertebrate') %>%
  pull(`Semi-Arid`) %>%
  substr(1,2) %>% paste(., '%', sep='')
aq_i_T <- table_aq_assimilated %>%
  filter(Group == 'Invertebrate') %>%
  pull(`Transition`) %>%
  substr(1,2) %>% paste(., '%', sep='')
aq_i_H <- table_aq_assimilated %>%
  filter(Group == 'Invertebrate') %>%
  pull(`Sub-Humid`) %>%
  substr(1,2) %>% paste(., '%', sep='')

aq_ifilt_A <- table_aq_assimilated %>%
  filter(Group == 'I-Filterer') %>%
  pull(`Semi-Arid`) %>%
  substr(1,2) %>% paste(., '%', sep='')
aq_ifilt_T <- table_aq_assimilated %>%
  filter(Group == 'I-Filterer') %>%
  pull(`Transition`) %>%
  substr(1,2) %>% paste(., '%', sep='')
aq_ifilt_H <- table_aq_assimilated %>%
  filter(Group == 'I-Filterer') %>%
  pull(`Sub-Humid`) %>%
  substr(1,2) %>% paste(., '%', sep='')

#------------------------------------------------------------------------------
# Trophic Level
#------------------------------------------------------------------------------
table_TL <- read_csv('Rfig/table_TL.csv')

TL_f_A <- table_TL %>% 
  filter(Group == 'Fish') %>%
  pull(`Semi-Arid`) %>%
  substr(1,4)

TL_f_T <- table_TL %>% 
  filter(Group == 'Fish') %>%
  pull(`Transition`) %>%
  substr(1,4)

TL_f_H <- table_TL %>% 
  filter(Group == 'Fish') %>%
  pull(`Sub-Humid`) %>%
  substr(1,4)

TL_Fherb_A <- table_TL %>% 
  filter(Group == 'F-Herbivore') %>%
  pull(`Semi-Arid`) %>%
  substr(1,4)

TL_Fherb_T <- table_TL %>% 
  filter(Group == 'F-Herbivore') %>%
  pull(`Transition`) %>%
  substr(1,4)

TL_Fherb_H <- table_TL %>% 
  filter(Group == 'F-Herbivore') %>%
  pull(`Sub-Humid`) %>%
  substr(1,4)

TL_i_A <- table_TL %>% 
  filter(Group == 'Invertebrate') %>%
  pull(`Semi-Arid`) %>%
  substr(1,4)

TL_i_T <- table_TL %>% 
  filter(Group == 'Invertebrate') %>%
  pull(`Transition`) %>%
  substr(1,4)

TL_i_H <- table_TL %>% 
  filter(Group == 'Invertebrate') %>%
  pull(`Sub-Humid`) %>%
  substr(1,4)

#------------------------------------------------------------------------------
# Exclosure
#------------------------------------------------------------------------------
exclosure_stats <- read_csv('Rout/exclosure_stats.csv')
table_THSD_exclosure <- read_csv('Rfig/table_algae_boot.csv')

ex_filter <- function(my_site_code, my_treatment, my_stat) {
  exclosure_stats%>%
    pivot_longer(cols=Algae_mean:LowPoint, 
                 names_to = 'Statistic', 
                 values_to = 'Value') %>%
    filter(Site == my_site_code) %>%
    filter(Statistic == my_stat) %>%
    filter(Treatment == my_treatment) %>%
    pull(Value) %>%
    tormat(.,1) }

ex_A_mu <- ex_filter('Semi-Arid', 'Exclosure', 'Algae_mean')
ex_A_se <- ex_filter('Semi-Arid', 'Exclosure', 'Algae_se')

ex_T_mu <- ex_filter('Transition', 'Exclosure', 'Algae_mean')
ex_T_se <- ex_filter('Transition', 'Exclosure', 'Algae_se')

ex_H_mu <- ex_filter('Sub-Humid', 'Exclosure', 'Algae_mean')
ex_H_se <- ex_filter('Sub-Humid', 'Exclosure', 'Algae_se')

op_A_mu <- ex_filter('Semi-Arid', 'Open', 'Algae_mean')
op_A_se <- ex_filter('Semi-Arid', 'Open', 'Algae_se')

op_T_mu <- ex_filter('Transition', 'Open', 'Algae_mean')
op_T_se <- ex_filter('Transition', 'Open', 'Algae_se')

op_H_mu <- ex_filter('Sub-Humid', 'Open', 'Algae_mean')
op_H_se <- ex_filter('Sub-Humid', 'Open', 'Algae_se')

#------------------------------------------------------------------------------
# Community Niche
#------------------------------------------------------------------------------
table_niche <- read_csv('Rfig/table_siber_and_bstrap_range.csv')

niche_index <- function(my_stat, my_com, my_site_code) {
  table_niche %>%
    pivot_longer(cols=c(`Semi-Arid`, Transition, `Sub-Humid`),
                 names_to = "site_code_code",
                 values_to = "x") %>%
    mutate(x=str_replace(x, '  ' ,' ')) %>%
    separate(x, into=c('x1', 'x2', 'x3'), sep = ' ') %>%
    mutate(x1=as.numeric(x1)) %>%
    filter(Estimate == my_stat) %>% 
    filter(Guild == my_com) %>%
    filter(site_code_code == my_site_code) %>%
    pull(x1)  }

SEA_f_A <- niche_index('Ellipse Area', 'Fish', 'Semi-Arid')
SEA_f_T <- niche_index('Ellipse Area', 'Fish', 'Transition')
SEA_f_H <-  niche_index('Ellipse Area', 'Fish', 'Sub-Humid')

Nrange_f_A <- niche_index('N-Range', 'Fish', 'Semi-Arid')
Nrange_f_T <- niche_index('N-Range', 'Fish', 'Transition')
Nrange_f_H <- niche_index('N-Range', 'Fish', 'Sub-Humid')

Drange_f_A <- niche_index('H-Range', 'Fish', 'Semi-Arid')
Drange_f_T <- niche_index('H-Range', 'Fish', 'Transition')
Drange_f_H <- niche_index('H-Range', 'Fish', 'Sub-Humid')

SEA_i_A <- niche_index('Ellipse Area', 'Invertebrate', 'Semi-Arid')
SEA_i_T <- niche_index('Ellipse Area', 'Invertebrate', 'Transition')
SEA_i_H <- niche_index('Ellipse Area', 'Invertebrate', 'Sub-Humid')

Nrange_i_A <- niche_index('N-Range', 'Invertebrate', 'Semi-Arid')
Nrange_i_T <- niche_index('N-Range', 'Invertebrate', 'Transition')
Nrange_i_H <-  niche_index('N-Range', 'Invertebrate', 'Sub-Humid')

Drange_i_A <- niche_index('H-Range', 'Invertebrate', 'Semi-Arid')
Drange_i_T <- niche_index('H-Range', 'Invertebrate', 'Transition')
Drange_i_H <- niche_index('H-Range', 'Invertebrate', 'Sub-Humid')

#------------------------------------------------------------------------------
# Niche width
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Niche height
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Carvallo et al 2022 background
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Density
#------------------------------------------------------------------------------

table_density <- read_csv('Rfig/density_table.csv')

density_f_A <- table_density$`Semi-Arid`[1] %>% substr(1,3)
density_i_H <- table_density$`Sub-Humid`[4] %>% substr(1,4)
density_i_A <- table_density$`Transition`[4] %>% substr(1,3)
density_i_T <- table_density$`Semi-Arid`[4] %>% substr(1,3)


THSD_density_f <- read_csv('Rout/THSD_density_f.csv')

# Index p-values for easy recall
THSD_density_f_85vs70 <- filter(
  THSD_density_f, Comparison=='Sub-Humid-Transition') %>% 
  pull(`p adj`) %>%
  formatC(format = "e", digits = 1)

THSD_density_f_85vs55 <- filter(
  THSD_density_f, Comparison=='Sub-Humid-Semi-Arid') %>% 
  pull(`p adj`) %>%
  formatC(format = "e", digits = 1)

THSD_density_f_70vs55 <- filter(
  THSD_density_f, Comparison=='Transition-Semi-Arid') %>% 
  pull(`p adj`) %>%
  formatC(format = "e", digits = 1)

THSD_density_i <- read_csv('Rout/THSD_density_i.csv')

# Index p-values for easy recall
THSD_density_i_85vs70 <- filter(
  THSD_density_i, Comparison=='Sub-Humid-Transition') %>% 
  pull(`p adj`) %>%
  signif(2)

THSD_density_i_85vs55 <- filter(
  THSD_density_i, Comparison=='Sub-Humid-Semi-Arid') %>% 
  pull(`p adj`) %>%
  signif(2)

THSD_density_i_70vs55 <- filter(
  THSD_density_i, Comparison=='Transition-Semi-Arid') %>% 
  pull(`p adj`) %>%
  signif(2)

#------------------------------------------------------------------------------
# Diversity
#------------------------------------------------------------------------------

THSD_shannon_f <- read_csv('Rout/THSD_shannon_f.csv')

# Index P-values for easier recall in figures
THSD_shannon_f_85vs70 <- filter(THSD_shannon_f, 
                                Comparison=='Sub-Humid-Transition') %>% 
  pull(`p adj`) %>%
  signif(2)

THSD_shannon_f_85vs55 <- filter(THSD_shannon_f, 
                                Comparison=='Sub-Humid-Semi-Arid') %>% 
  pull(`p adj`) %>%
  signif(2)

THSD_shannon_f_70vs55 <- filter(THSD_shannon_f, 
                                Comparison=='Transition-Semi-Arid') %>% 
  pull(`p adj`) %>%
  signif(2)


THSD_shannon_i <- read_csv('Rout/THSD_shannon_i.csv')

# Index P-values for easier recall in figures
THSD_shannon_i_85vs70 <- filter(THSD_shannon_i, 
                                Comparison=='Sub-Humid-Transition') %>% 
  pull(`p adj`) %>%
  formatC(format = "e", digits = 1)

THSD_shannon_i_85vs55 <- filter(THSD_shannon_i, 
                                Comparison=='Sub-Humid-Semi-Arid') %>% 
  pull(`p adj`) %>%
  formatC(format = "e", digits = 1)
THSD_shannon_i_70vs55 <- filter(THSD_shannon_i, 
                                Comparison=='Transition-Semi-Arid') %>% 
  pull(`p adj`) %>%
  formatC(format = "e", digits = 1)
#------------------------------------------------------------------------------
# autotrophy
#------------------------------------------------------------------------------
t_algae <- read_csv('Rout/d_autorophy.csv')

bf_A <- t_algae %>% filter(site_code == 'Semi-Arid') %>% 
  pull(flow_base) %>% round(.,0)
bf_T <- t_algae %>% filter(site_code == 'Transition') %>% 
  pull(flow_base)%>% round(.,0)
bf_H <- t_algae %>% filter(site_code == 'Sub-Humid') %>% 
  pull(flow_base)%>% round(.,0)

F3x_A <- t_algae %>% filter(site_code == 'Semi-Arid') %>%
  pull(HFPP3)%>% round(.,0)
F3x_T <- t_algae %>% filter(site_code == 'Transition') %>% 
  pull(HFPP3)%>% round(.,0)
F3x_H <- t_algae %>% filter(site_code == 'Sub-Humid') %>% 
  pull(HFPP3)%>% round(.,0)

No3_A <- t_algae %>% filter(site_code == 'Semi-Arid') %>% 
  pull(nitrate)%>% round(.,1)
No3_T <- t_algae %>% filter(site_code == 'Transition') %>% 
  pull(nitrate)%>% round(.,1)
No3_H <- t_algae %>% filter(site_code == 'Sub-Humid') %>% 
  pull(nitrate)%>% round(.,1)

Po4_A <- t_algae %>% filter(site_code == 'Semi-Arid') %>% 
  pull(phosphate)%>% round(.,1)
Po4_T <- t_algae %>% filter(site_code == 'Transition') %>% 
  pull(phosphate)%>% round(.,1)
Po4_H <- t_algae %>% filter(site_code == 'Sub-Humid') %>% 
  pull(phosphate)%>% round(.,1)

dcanopy <- round( max(t_algae$canopy) - min(t_algae$canopy) ,  1 )

#------------------------------------------------------------------------------
# RDA fish
#------------------------------------------------------------------------------

RDA_F_axes <- read_csv('Rout/RDA_fish_axis.csv') %>%
  mutate(variance = substr(label, 7,10)) %>%
  mutate(variance = str_replace_all(variance, "%", "")) %>%
  mutate(variance = str_replace_all(variance, '\\)', ''))

rda_f_axis_1 <- RDA_F_axes$variance[1] %>% as.numeric() %>% round(0)
rda_f_axis_2 <- RDA_F_axes$variance[2] %>% as.numeric() %>% round(0)

#------------------------------------------------------------------------------
# RDA invertebrate
#------------------------------------------------------------------------------

RDA_i_axes <- read_csv('Rout/RDA_axis_invertebrate.csv') %>%
  mutate(variance = substr(label, 7,10)) %>%
  mutate(variance = str_replace_all(variance, "%", "")) %>%
  mutate(variance = str_replace_all(variance, '\\)', ''))

rda_i_axis_1 <- RDA_i_axes$variance[1] %>% as.numeric() %>% round(0)
rda_i_axis_2 <- RDA_i_axes$variance[2] %>% as.numeric() %>% round(0)

#------------------------------------------------------------------------------
# End 10_3sp_prose_index


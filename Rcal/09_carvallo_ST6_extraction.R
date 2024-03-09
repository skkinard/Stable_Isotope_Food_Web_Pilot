# 09_carvallo_2022_extraction
# written by: Sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Description
#----------------------------------------------------------------------------
# The script extracts functional feeding group data on invertebrate communities as analyzed in Carvallo et al. 2022. It includes the creation of tables and figures displaying the proportions of herbivore, predator, and other invertebrate functional feeding groups in Spring 2018 and each season of 2018, along with abundance and diversity metrics.

#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

#setwd('/home/kinard/Documents/Research/Dissertation/03_Autochthony')
d <- read_csv('Rdat/Carvallo_2018_TS6_invert_FG_prcnt.csv')

# ---------------------------------------------------------------------------
# FFG Table Spring 2018
# ---------------------------------------------------------------------------
d_carvallo_ffg <- d %>%
  filter(Season %in% c('Summer')) %>%
  filter(Site %in% c('San Fernando Creek', 'Aransas River', 'Garcitas Creek')) %>%
  mutate(site_code = case_when(
    Site == 'San Fernando Creek' ~ 'Semi-Arid',
    Site == 'Aransas River' ~ 'Transition',
    Site == 'Garcitas Creek' ~ 'Sub-Humid')) %>%
  mutate(site_code = fct_relevel(site_code, c('Semi-Arid', 'Transition', 'Sub-Humid'))) %>%
  select( -Site, -Season, -Year) %>%
  select(site_code, everything())

table_carvallo_ffg <- d_carvallo_ffg %>%
  rename(Site=site_code, Filterer=`Filter Feeder`, Gatherer=`Collector-Gatherer`) %>%
  mutate(Filterer=tormat(Filterer,2),
         Gatherer=tormat(Gatherer,2),
         Herbivore=tormat(Herbivore,2),
         Predator=tormat(Predator,2),
         Shredder=tormat(Shredder,0))

write_csv(table_carvallo_ffg, 'Rfig/table_carvallo_ffg.csv')
  
# ---------------------------------------------------------------------------
# FFG Figures
# ---------------------------------------------------------------------------
p_invert_ffg <- d_carvallo_ffg %>%
  pivot_longer(cols=c('Filter Feeder', 'Collector-Gatherer', 'Herbivore',
                      'Predator', 'Shredder'),
               names_to='FFG',
               values_to='prcnt_com') %>%
  mutate(FFG = case_when(
    FFG == 'Herbivore' ~ 'Herbivore', 
    FFG == 'Predator' ~ 'Predator', 
    FFG %in% c('Collector-Gatherer',
               'Filter Feeder', 
               'Shredder') ~ 'Other' )) %>%
  group_by(site_code, FFG) %>%
  dplyr::summarize(prcnt_com=sum(prcnt_com)) %>%
  mutate(FFG = fct_relevel(FFG, c('Herbivore', 'Other', 'Predator'))) %>%
  arrange(desc(FFG)) %>%
  mutate(pos = cumsum(prcnt_com) - prcnt_com/2) %>% 
  ggplot(aes(x=site_code, y=prcnt_com, fill=FFG)) +
  geom_col(position = 'fill', fill = NA, aes(color = FFG), linewidth=1.4) +
  geom_col(position = 'fill', alpha = .7) +
  geom_text(aes(y=pos, label = paste(round(prcnt_com*100,0), '%', sep=' ')), color = 'black') +
  theme_bw(base_size = 14) +
  scale_fill_manual(values = c('deepskyblue3', 'grey50', 'darkorange3')) +
  scale_color_manual(values = c('deepskyblue3', 'grey50', 'darkorange3')) +
  xlab(element_blank()) +
  ylab('% of Community') +
  labs(fill= element_blank(),
       color = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

p_invert_ffg2 <- d %>%
  filter(Site %in% c('San Fernando Creek', 'Aransas River', 'Garcitas Creek')) %>%
  mutate(site_code = case_when(
    Site == 'San Fernando Creek' ~ 'Semi-Arid',
    Site == 'Aransas River' ~ 'Transition',
    Site == 'Garcitas Creek' ~ 'Sub-Humid')) %>%
  mutate(site_code = fct_relevel(site_code, c('Semi-Arid', 'Transition', 'Sub-Humid'))) %>%
  select( -Site, -Year) %>%
  select(site_code, everything()) %>%
  pivot_longer(cols=c('Filter Feeder', 'Collector-Gatherer', 'Herbivore',
                      'Predator', 'Shredder'),
               names_to='FFG',
               values_to='prcnt_com') %>%
  mutate(FFG = case_when(
    FFG == 'Herbivore' ~ 'Herbivore', 
    FFG == 'Predator' ~ 'Predator', 
    FFG %in% c('Collector-Gatherer',
               'Filter Feeder', 
               'Shredder') ~ 'Other' )) %>%
  group_by(site_code, Season, FFG) %>%
  dplyr::summarize(prcnt_com=sum(prcnt_com)) %>%
  mutate(FFG = fct_relevel(FFG, c('Herbivore', 'Other', 'Predator')),
         Season = fct_relevel(Season, c('Spring', 'Summer', 'Fall', 'Winter'))) %>%
  arrange(desc(FFG)) %>%
  mutate(pos = cumsum(prcnt_com) - prcnt_com/2) %>% 
  ggplot(aes(x=site_code, y=prcnt_com, fill=FFG)) +
  geom_col(position = 'fill', fill = NA, aes(color = FFG), linewidth=1.4) +
  geom_col(position = 'fill', alpha = .7) +
  geom_text(aes(y=pos, label = paste(round(prcnt_com*100,0), '%', sep=' ')), color = 'black') +
  theme_bw(base_size = 14) +
  scale_fill_manual(values = c('deepskyblue3', 'grey50', 'darkorange3')) +
  scale_color_manual(values = c('deepskyblue3', 'grey50', 'darkorange3')) +
  xlab(element_blank()) +
  ylab('% of Community') +
  labs(fill= element_blank(),
       color = element_blank()) +
  facet_wrap(~Season) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

caption_table_invert_ffg <- 'Proportions of herbivore, predator, and other invertebrate functional feeding groups in Spring 2018. Data extracted from supplemental table 6 in Carvallo et al 2022.'
caption_p_invert_ffg <- 'Proportions of herbivore, predator, and other invertebrate functional feeding groups in Spring 2018. Data extracted from supplemental table 6 in Carvallo et al 2022.'
caption_p_invert_ffg <- 'Proportions of herbivore, predator, and other invertebrate functional feeding groups in each season of 2018. Data extracted from supplemental table 6 in Carvallo et al 2022.'

# ---------------------------------------------------------------------------
# Abundance, diversity Table
# ---------------------------------------------------------------------------
ts4 <- read_csv('Rdat/Fernando_etal_2022_Table4.csv') %>%
  filter(Site %in% c('Tranquitas Creek', 'Aransas River', 'Garcitas Creek')) %>%
  mutate(Site = case_when(
    Site == 'Tranquitas Creek' ~ 'Semi-Arid',
    Site == 'Aransas River' ~ 'Transition',
    Site == 'Garcitas Creek' ~ 'Sub-Humid')) %>%
  mutate(Site = fct_relevel(Site, c('Semi-Arid', 'Transition', 'Sub-Humid')))  %>%
  pivot_longer(cols=c('Functional richness', 'RaoQ', 'Richness', 'Shannon diversity', 'Abundance'),
               names_to='Variable',
               values_to = 'x') %>%
  mutate(x=str_replace_all(x, ' ', '')) %>%
  separate(x, into=c('Mean', 'SE'), sep='±') %>%
  filter(Variable %in% c('Abundance', 'Richness', 'Functional richness')) %>%
  mutate( # some kind of translation error -> had to enter values manually
    Variable = c(
      "Functional Richness",    "Species Richness",     "Abundance",
      "Functional Richness",    "Species Richness",    "Abundance",
      "Functional Richness",    "Species Richness",    "Abundance")) %>%
  mutate(Variable = fct_relevel(Variable, c('Abundance', 'Species Richness', 
                                            'Functional Richness'))) %>%
  mutate(# some kind of translation error -> had to enter values manually
    Mean = c( 8.4, 12.8, 11.8, 133.4, 24.2, 17.8, 45.9,  16.7, 11.7),
    SE = c(3.2, 1.4, 3.9, 7.6, 0.9, 5.3, 8.1, 0.6, 2.7)) %>%
  mutate(# convert to individuals per square meter
    Mean = ifelse(Variable == 'Abundance', Mean*127.323954474, Mean), 
    SE = ifelse(Variable == 'Abundance', SE*127.323954474, SE)) %>%
  mutate(lower = Mean - SE,
         upper = Mean + SE)

p_table_ts4 <-  ts4 %>%
  ggplot(aes(x=Site, y=Mean, fill=Site)) +
  geom_errorbar(aes(x = Site, ymin=lower, ymax=upper),
                color='black') +
  geom_point(size=5, shape = 23) +
  facet_wrap(~Variable, scales='free') +
  theme_bw(base_size = 14) +
  scale_fill_manual(values = c("white", "skyblue", "royalblue1")) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

table_ts4 <- ts4 %>%
  mutate(X=paste(round(Mean,0), "±", round(SE, 0), sep = ' ')) %>%
  select(Site, Variable, X) %>%
  pivot_wider(names_from=Variable, values_from=X)

write_csv(table_ts4, 'Rfig/table_carvallo_comstat.csv')

table_ts4
caption_table_ts4 <- 'Annual mean ± standard error Abundance (invertebrates per m^2^) and Diversity metrics from 2018 invertebrate kicknet sampling and sediment cores from San Fernando Creek (Semi-Arid), Aransas River (Transition), and Garcitas Creek (Sub-Humid) (Carvallo *et al.* 2022).'

p_table_ts4
caption_p_table_ts4 <- 'Annual Mean and standard errors, reported in Carvallo *et al.* 2022, from 2018 invertebrate kicknet sampling and sediment cores from Fernando Creek (Semi-Arid), Aransas River (Transition), and Garcitas Creek (Sub-Humid). Abundance is reported in invertebrates per square meter.'

# ---------------------------------------------------------------------------
# FFG commentary
# ---------------------------------------------------------------------------
# In the Spring of 2018, herbivorous invertebrates constituted incrementally larger proportions of the community from Semi-Arid (4%) to Transition (18%) to Sub-Humid (31%) sites. As the proportion of herbivores increased in the wetter climates, predators declined 17% and other groups (collector-gatherer and filterer) declined 11%. The pattern of prevalent herbivorous invertebrates at the wetter sites persisted for the majority of the year (Fall, Winter, and Spring). In summer however, herbivores constituted 28% of the Semi-Arid community, 9% at the Transition, and 14% at the Sub-Humid site. 

# The prevalence of herbivores at the Sub-Humid site was consistent with the results of the tile experiment, where exclosures revealed fish likely regulate grazing pressure by invertebrate herbivores. There are numerous explanations for why the proportion of herbivores was inversely related to the standing algal stocks on all plates. Competitive exclusion by fish would manifest as depleted algae on exposed tiles, but exposed tiles at the Semi-Arid site had more algae at the than all of the tiles at the other sites. It is also unlikely that Semi-Arid herbivorous fish consumed invertebrate herbivores since their isotopic trophic levels (0.4 and 0.12 respectively) were both very low. 

# We suspected herbivory emerged from arid-adapted species with generalist diets and found greater dietary plasticity in fish than invertebrates. Fish within all feeding groups consumed more autochthonous resources as shown by lower isotopic trophic levels and greater autochthonous sourcing in drier climates. Dramatic examples of opportunistic herbivores include *P.latipinna* which fell 1.96 trophic levels and increased autochthony 62% as well as *C.lutrensis* which extended the Transition site's fish community niche width by 126% towards autochthonous resources. Similar dietary shifts were observed in insectivorous (56%) and even piscivorous fish (60%). Unilateral declines in isotopic trophic level among fish groups were not matched by invertebrates, implying that fish autochthony was not passed up the food chain through invertebrates. Since herbivorous invertebrates are scarce at the Semi-Arid site, we expected aridity-induced dietary shifts towards autochthonous sources within invertebrate collector-gatherers and filterers. Examined as functional feeding groups (and families in rare cases), autochthony increased ~20% and isotopic trophic levels declined 0.7 in both filterers (Corbiculidae) and invertebrate predators (Coenagrionidae) . However, collector-gatherer autochthony only increased 15% and isotopic trophic level remained similar across sites. It is possible that several families or genuses within collector-gatherers had drastic dietary shifts that were subdued during aggregation into functional feeding groups. Overall, the opportunistic herbivory among invertebrates was modest compared to fish.  

# ---------------------------------------------------------------------------
# comstat commentary
# ---------------------------------------------------------------------------
# Invertebrate abundance and species richness and functional richness peak in the middle of the rainfall gradient. This is likely caused by overlap between the distribution of species that thrive at either extremes of the study region. Semi-Arid sites contain lower species and functional richness which might be attributed to strong environmental filtering by the harsh physical conditions imposed by drought conditions throughout the year.

# ---------------------------------------------------------------------------
# End 09_carvallo_2022_extraction
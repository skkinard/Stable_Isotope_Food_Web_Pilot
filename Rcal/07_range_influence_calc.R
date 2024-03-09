# 07_range_influence
# written by: sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Description
#----------------------------------------------------------------------------
# The script identifies species influencing signature ranges by calculating the influence of each species on hydrogen, carbon, and nitrogen isotopic ranges. It generates a table showing the extent of influence, relative abundance, aquatic trophic level, and isotopic trophic level for each species at different sites and trophic categories.

#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

d <- read_csv("Rout/isotope_data_correctedH_correctedN.csv") %>%
  filter(guild %in% c('Fish', 'Invertebrate'))

community_percent <- read_csv('Rdat/tidy_invert2.csv') %>%
  filter(site %in% c('SFC', 'AR', 'GC')) %>%
  mutate(site = case_when(site == 'SFC' ~ 'Semi-Arid',
                          site == 'AR' ~ 'Transition',
                          site == 'GC' ~ 'Sub-Humid')) %>%
  group_by(site, family) %>%
  dplyr::summarize(prct_com = mean(abun/total_abun)*100) %>%
  rename(species=family) %>%
  full_join(read_csv('Data/tidy_fish.csv') %>%
              filter(site %in% c('SFC', 'AR', 'GC')) %>%
              mutate(site = case_when(site == 'SFC' ~ 'Semi-Arid',
                                      site == 'AR' ~ 'Transition',
                                      site == 'GC' ~ 'Sub-Humid')) %>%
              mutate(species=paste(substr(genus,1,1), species,sep='.')) %>%
              group_by(site, species) %>%
              dplyr::summarize(prct_com = mean(abun/total_abun)*100))

my_spp <- c('Corbiculidae', 'Coenagrionidae', 'P.latipinna', "L.macrochirus",
            "L.cyanellus", "L.gulosus")

Aq_spe <- read_csv("Rout/mixing_model_signif.csv") %>%
  filter(m_group %in% my_spp) %>%
  select(m_group, site, mu) %>%
  rename(species = m_group, aq_spe=mu)

Aq_tro <- read_csv("Rout/mixing_model_signif.csv") %>%
  mutate(trophic=ifelse(substr(m_group,1,2) %in% c('F-', 'I-'), 1, 0)) %>%
  filter(trophic>0) %>%
  select(m_group, site, mu) %>%
  rename(trophic_category = m_group, aq_tro=mu)

isotopic_tl_spe <- read_csv('Rout/TL_all.csv') %>%
  filter(m_group %in% my_spp) %>%
  select(m_group, site, TL_mu) %>%
  rename(species=m_group, TL_spe=TL_mu)

isotopic_tl_tro <- read_csv('Rout/TL_all.csv')  %>%
  mutate(trophic=ifelse(substr(m_group,1,2) %in% c('F-', 'I-'), 1, 0)) %>%
  filter(trophic>0) %>%
  select(m_group, site, TL_mu) %>%
  rename(trophic_category=m_group, TL_tro=TL_mu)
#------------------------------------------------------------------------------
# Species influencing Signature Ranges
#------------------------------------------------------------------------------

find_influencers <- function(my_data) {
  my_species <- my_data$species %>% unique()
  
  x_out <- tibble(
    guild = as.character(),
    species = as.character(),
    site = as.character(),
    HR_inf = as.numeric(),
    CR_inf = as.numeric(),
    NR_inf = as.numeric())
  
  for(i in 1:length(my_species)) {
    x_ranges <- my_data %>%
      group_by(site, guild) %>%
      dplyr::summarize(HR = estimate_range_mu(hydrogen),
                       CR = estimate_range_mu(carbon),
                       NR = estimate_range_mu(nitrogen)) %>%
      ungroup() %>%
      right_join(my_data%>%select(site, guild, species) %>% unique())
    
    x_out <- my_data %>%
      filter(species != my_species[i]) %>%
      group_by(site, guild) %>%
      dplyr::summarize(HR_spGone = estimate_range_mu(hydrogen),
                       CR_spGone = estimate_range_mu(carbon),
                       NR_spGone = estimate_range_mu(nitrogen)) %>%
      ungroup() %>%
      right_join(x_ranges) %>%
      filter(species == my_species[i]) %>%
      mutate(HR_inf = (HR-HR_spGone)/HR*100,
             CR_inf = (CR-CR_spGone)/CR*100,
             NR_inf = (NR-NR_spGone)/NR*100) %>%
      select(guild, species, site, contains('inf')) %>%
      full_join(x_out) }
  
  compile_influence_table <- function(my_variable) {
    
    colnames(x_out) <- str_replace_all(
      colnames(x_out), my_variable, 'X')
    
    d_trophic <- my_data %>% select(site, guild, species, trophic_category) %>%
      unique()
    
    my_output <- x_out %>%
      filter(X > 0) %>%
      arrange(guild, desc(X)) %>%
      select(site, guild, species, X) %>%
      left_join(community_percent) %>%
      left_join(Aq_spe) %>%
      left_join(isotopic_tl_spe) %>%
      right_join(d_trophic) %>%
      left_join(Aq_tro) %>%
      left_join(isotopic_tl_tro) %>%
      mutate(Aquatic = ifelse(is.na(aq_spe), paste(tormat(aq_tro,0), '†', sep=' '),
                              tormat(aq_spe,0)),
             I.T.L= ifelse(is.na(TL_spe), paste(tormat(TL_tro,1), '†', sep=' '),
                           tormat(TL_spe,1))) %>%
      mutate(Element=substr(my_variable,1,1)) %>%
      filter( ! is.na(X)) %>%
      mutate(prct_com=round(prct_com,0)) %>%
      select(site, trophic_category, species, Element, X, prct_com, Aquatic, 
             I.T.L) %>%
      rename(Site = site, T.Group=trophic_category, Taxa=species, 
             "Range Ext."=X , R.Abundance=prct_com) 
    
    return(my_output)
  }
  
  # Make table of influencers
  table_influencers <- full_join(compile_influence_table('HR_inf'), 
                                 compile_influence_table('CR_inf') ) %>%
    full_join(compile_influence_table('NR_inf')) %>%
    select(Element, Taxa, `Range Ext.`, Site, R.Abundance, Aquatic, I.T.L, 
           T.Group) %>%
    arrange(Element, desc(`Range Ext.`))
  
  d_inf <- table_influencers %>%
    mutate(Aquatic = str_replace_all(Aquatic, ' †', ''),
           Aquatic = as.numeric(Aquatic),
           I.T.L =  str_replace_all(I.T.L, ' †', ''),
           I.T.L = as.numeric(I.T.L),
           Site = fct_relevel(Site, c(
             'Semi-Arid', 'Transition', 'Sub-Humid')))
  
  my_output <- list( d_inf, table_influencers)
  
  return(my_output) }

influencers_out <- find_influencers(d)
d_inf <- influencers_out[[1]]
table_inf <- influencers_out[[2]]

write_csv(d_inf, 'Rout/d_range_influence.csv')
write_csv(table_inf, 'Rfig/table_range_influence.csv')

#------------------------------------------------------------------------------
# End 07_range_influence_calc
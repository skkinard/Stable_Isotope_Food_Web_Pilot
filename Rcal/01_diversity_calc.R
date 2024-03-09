# 01_diversity_calc
# written by: sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Description
#----------------------------------------------------------------------------
# The script estimates Hill numbers for fish and invertebrate communities, conducts ANOVA tests for Shannon-Wiener diversity among different sites for both guilds, and exports the results along with the diversity estimates.

#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

d <- read_csv('Rdat/communities.csv')

# format dataframe with species rows and column names as UID (site-month)
di <- d %>%
  filter(guild=='invertebrate') %>%
  group_by(site, month, lowest_taxon) %>%
  summarize(abundance=sum(ceiling(abun))) %>%
  ungroup() %>%
  mutate(UID=paste(site, month, sep ='_')) %>%
  select(-site, -month) %>%
  pivot_wider(names_from = UID, values_from = abundance, values_fill = 0) %>%
  column_to_rownames(var = "lowest_taxon") %>%
  as.data.frame()

df <- d %>%
  filter(guild == 'fish') %>%
  group_by(site, month, lowest_taxon) %>%
  summarize(abundance=sum(ceiling(abun/stream_area *100))) %>%
  ungroup() %>%
  mutate(UID=paste(site, month, sep ='_')) %>%
  select(-site, -month) %>%
  pivot_wider(names_from = UID, values_from = abundance, values_fill = 0) %>%
  column_to_rownames(var = "lowest_taxon") %>%
  as.data.frame()

#----------------------------------------------------------------------------
# Calculate Hill numbers
#----------------------------------------------------------------------------
# Format data for iNEXT: Invertebrate from April-June 2018

# supported data formats (abundance, incidence_raw, incidence_frequency)

# 'abundance' -> Individual‐based abundance data (datatype="abundance"): Input data for each assemblage/site include species abundances in an empirical sample of n individuals (“reference sample”). When there are N assemblages, input data consist of an S by N abundance matrix, or N lists of species abundances.

# Create iNEXt objects:
f_next <- iNEXT(df)

i_next <- iNEXT(di)

# Extract Hill numbers from iNEXT objects
f_hill <- f_next$AsyEst %>%
  separate(Assemblage, c('Site', 'Month'), sep = '_') %>%
  mutate(Guild = 'Fish')

i_hill <- i_next$AsyEst %>%
  separate(Assemblage, c('Site', 'Month'), sep = '_') %>%
  mutate(Guild = 'Invertebrate')

# Combine taxonomic guilds into mega-table
# Prepare hill data for tables and plots
hill <- full_join(f_hill, i_hill) %>% 
  as_tibble() %>%
  group_by(Site,Guild, Diversity) %>%
  summarize(average=mean(Observed),
            se=sd(Observed)/sqrt(length(Observed)),
            n=length(Observed)) %>%
  ungroup() %>%
  right_join(full_join(f_hill, i_hill)) %>%
  mutate(Site = fct_relevel(Site, 'Semi-Arid', 'Transition', 'Sub-Humid')) %>%
  mutate(Diversity = as.factor(Diversity)) %>%
  mutate(Diversity = ifelse(Diversity == 'Species richness', 'Richness',
                            ifelse(Diversity == 'Shannon diversity', 'Shannon-Wiener',
                                   'Simpson'))) %>%
  mutate(Diversity = fct_relevel(Diversity, 'Richness', 'Shannon-Wiener', 'Simpson'))

#----------------------------------------------------------------------------
# anova_shannon_f
#----------------------------------------------------------------------------
# ANOVA helper function:
my_shannon_aov <- function(my_data) {
  # Anova community
  my_aov <- aov(Estimator~Site, data = my_data)
  
  table_my_aov <- as_tibble(summary(my_aov)[[1]]) %>%
    mutate('T-test' = noquote(rownames(summary(my_aov)[[1]]))) %>%
    rename(df = Df, 'F-stat'= 'F value', 'p-value' = 'Pr(>F)')%>%
    select('T-test', 'F-stat', df, 'p-value') %>%
    as.data.frame()
  
  table_my_aov$`T-test` <- str_to_title(table_my_aov$`T-test`)
  
  table_my_aov <- table_my_aov[1:2,]
  
  return(table_my_aov) }

## Tukey HSD
my_shannon_Tukey <- function(my_data) {
  
  my_aov <- aov(Estimator~Site, data = my_data)
  
  tukey_shannon_v_site <- TukeyHSD(my_aov)$Site %>%
    as_tibble(rownames='Comparison')
  
  return(tukey_shannon_v_site)}

# ANOVA: fish shannon
anova_shannon_f <- hill %>% 
  filter(Guild == 'Fish') %>%
  filter(Diversity == 'Shannon-Wiener') %>%
  my_shannon_aov()

# TukeyHSD: fish shannon
THSD_shannon_f <- hill %>% 
  filter(Guild == 'Fish') %>%
  filter(Diversity == 'Shannon-Wiener') %>%
  my_shannon_Tukey() %>%
  mutate(Guild = 'Fish')

#----------------------------------------------------------------------------
# anova_shannon_i
#----------------------------------------------------------------------------
# ANOVA: invertebrate shannon
anova_shannon_i <- hill %>% 
  filter(Guild == 'Invertebrate') %>%
  filter(Diversity == 'Shannon-Wiener') %>%
  my_shannon_aov()

# TukeyHSD: invertebrate shannon
THSD_shannon_i <- hill %>% 
  filter(Guild == 'Invertebrate') %>%
  filter(Diversity == 'Shannon-Wiener') %>%
  my_shannon_Tukey() %>%
  mutate(Guild = 'Invertebrate')

write_csv(hill, 'Rout/diversity_estimates.csv')
write_csv(THSD_shannon_f, 'Rout/THSD_shannon_f.csv')
write_csv(THSD_shannon_i, 'Rout/THSD_shannon_i.csv')

#----------------------------------------------------------------------------
# End 01_diversity_calc
# fig_CH_mixing_model
# Sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

mmix_all <- read_csv('Rout/CH_mix_out.csv')
#----------------------------------------------------------------------------
# Plots
#----------------------------------------------------------------------------

# functionally derive within groups of comparison whether there is no overlap
mmix_all <- mmix_all %>%
  rename(mu=mean, lower=`2.5%`, upper=`97.5%`) %>%
  select(m_group, site, mu, lower, upper) %>%
  detect_signif() %>%
  mutate(guild=case_when(
    m_group %in% c("Fish", 
                   "F-Herbivore", "P.latipinna", 
                   "F-Invertivore", "L.macrochirus",
                   "F-Piscivore", "L.cyanellus", "L.gulosus") ~ 'Fish',
    m_group %in% c("Invertebrate", "I-Filterer", "I-Gatherer", "I-Predator",
                   "Corbiculidae", "Coenagrionidae") ~ "Invertebrate" )) %>%
  mutate(m_group = fct_relevel(m_group, 
                               "Fish", "F-Herbivore", "P.latipinna",
                               "F-Invertivore", "L.macrochirus",
                               "F-Piscivore", "L.cyanellus", "L.gulosus",
                               "Invertebrate", 
                               "I-Filterer", "Corbiculidae", 
                               "I-Gatherer", 
                               "I-Predator", "Coenagrionidae" ))

mmix_signif <- mmix_all %>%
  mutate(signif = case_when(
    signif == "∗∗" ~ 2,
    signif == "∗" ~ 1)) %>%
  group_by(m_group) %>%
  dplyr::summarize(signif=max(signif, na.rm=T)) %>%
  mutate(signif = ifelse(is.infinite(signif), NA, signif)) %>%
  right_join(mmix_all%>%select(-signif)) %>%
  mutate(signif = case_when(
    signif == 2 ~ "∗∗",
    signif == 1 ~ "∗"))

# plot function
mmix_boxplot <- function(my_data) {
  my_groups <- my_data %>% pull(m_group) %>% unique()
  
  my_data %>%
    ggplot(aes(x=m_group, y=mu, fill = site)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), position=position_dodge(width = .6),
                  width = .8, linewidth=.15) +
    geom_point(size=4, shape = 23, aes(fill=site), position=position_dodge(width = .6)) +
    #geom_text(data=mmix_signif%>%filter(m_group %in% my_groups),
    #          aes(x=m_group, y = -5, label = signif), color = 'red', size = 8) +
    scale_fill_manual(breaks=c('Sub-Humid', 'Transition', 'Semi-Arid'),
                      values = rev(my_colors)) +
    ylab('Autochthonous Assimilation (%)') +
    xlab(element_blank()) +
    theme_bw(base_size = 14) +
    theme(legend.title = element_blank(),
          legend.box.background = element_rect(colour = "black")) +
    ylim(-0,100) +
    scale_x_discrete(limits = rev) }

# generate individual plots
mix_box_fish <- mmix_boxplot(mmix_all%>%filter(guild=='Fish')) 
mix_box_invertebrate <- mmix_boxplot(mmix_all%>%filter(guild=='Invertebrate'))
mix_box_guild <- mmix_boxplot(mmix_all%>%
                                filter(m_group%in%c('Fish', 'Invertebrate')) %>%
                                fix_site_order())
mix_box_ffg <- mmix_boxplot(mmix_all%>%
                                filter(substr(m_group,2,2) == '-') %>%
                              fix_site_order()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))





# Combine Plots
plot_simmr <- mmix_all %>% 
  filter(m_group %in% c("Fish", "F-Herbivore", "F-Invertivore", "F-Piscivore",
                        "Invertebrate", "I-Filterer", "I-Gatherer", "I-Predator")) %>%
  mmix_boxplot()

plot_simmr_caption <- "Autochthonous (% Aquatic Source) assimilation comparisons among Semi-Arid, Transition, and Sub-Humid sites for (A) fish communities, size groups, and common species and (B) invertebrate communities, functional feeding groups, and common species. Diamonds represent mean values with bars extending to the 95% confidence interval. Red stars highlight instances where confidence intervals do not overlap within each group of comparison. Autochthonous assimilation is estimated using d13C and d2H in a bayesian mixing model for each group of comparison, calibrated to local aquatic and terrestrial source signatures."

#----------------------------------------------------------------------------
# Table
#----------------------------------------------------------------------------

table_simmr_caption <- "Mean plus or minus standard deviations of % Aquatic Source assimilation in Semi-Arid, Transition, and Sub-Humid sites. Each row contains the group of comparison, statistical significance (Signif) as determined by 95% confidence intervals as well as the mean plus or minus the standard deviation of % Aquatic Source assimilated. Autochthonous assimilation is estimated using d13C and d2H in a bayesian mixing model for each group of comparison, calibrated to local aquatic and terrestrial source signatures."

table_aq_assimilated <- mmix_all %>%
  mutate(signif = ifelse(is.na(signif), "",signif)) %>%
  mutate(Autochthony = paste(tormat(mu, 0), ' (', 
                         tormat(lower, 0), ':',
                         tormat(upper, 0), ")",
                         format(signif, nsmall=2), sep='')) %>%
  select(site, m_group, Autochthony) %>%
  pivot_wider(names_from = site, values_from = Autochthony) %>%
  rename(Group = m_group) %>%
  select(Group, `Semi-Arid`, 'Transition', `Sub-Humid`) %>%
  arrange(Group) %>%
  mutate(`Sub-Humid` = ifelse(is.na(`Sub-Humid`), '', `Sub-Humid`)) 

write_csv(mmix_all, 'Rout/mixing_model_signif.csv')

write_csv(table_aq_assimilated, 'Rfig/table_aq_assimilated.csv')
#----------------------------------------------------------------------------
# End fig_CH_mixing_model
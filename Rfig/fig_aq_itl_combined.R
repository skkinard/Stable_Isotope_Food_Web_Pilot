# aq_itl_combined
# My goal is to create a figure and table that display measures of aquatic assimilation and trophic level simultaneously. There are 2 levels of comparison: trophic category and species.
# written by: sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

Aq_spe <-  read_csv("Rout/CH_mix_out.csv") %>%
  filter(dataset=='Species') %>%
  select(m_group, site, mean, `2.5%`, `97.5%`) %>%
  rename(taxa = m_group, Aq_mu=mean, Aq_bottom=`2.5%`, Aq_top=`97.5%`) %>%
  mutate(comparison='Species')

Aq_tro <- read_csv("Data/mixing_model_results.csv") %>%
  filter(dataset=='Trophic') %>%
  select(m_group, site, mean, `2.5%`, `97.5%`) %>%
  rename(taxa = m_group, Aq_mu=mean, Aq_bottom=`2.5%`, Aq_top=`97.5%`) %>%
  mutate(comparison='Trophic Category')

isotopic_tl_spe <- read_csv('Rout/TL_all.csv') %>%
  filter(m_group %in% c("P.latipinna", "L.macrochirus", "L.cyanellus", 
                        "L.gulosus", "Corbiculidae", "Coenagrionidae")) %>%
  select(m_group, site, TL_mu, TL_lower, TL_upper) %>%
  rename(taxa = m_group, TL_bottom=TL_lower, TL_top=TL_upper) %>%
  mutate(comparison='Species')

isotopic_tl_tro <- read_csv('Rout/TL_all.csv') %>%
  filter(m_group %in% c("F-Herbivore","F-Invertivore", "F-Piscivore", 
                        "I-Filterer", "I-Gatherer", "I-Predator")) %>%
  select(m_group, site, TL_mu, TL_lower, TL_upper) %>%
  rename(taxa = m_group, TL_bottom=TL_lower, TL_top=TL_upper) %>%
  mutate(comparison='Trophic Category')

d <- left_join(isotopic_tl_spe, Aq_spe) %>%
  full_join( left_join(isotopic_tl_tro, Aq_tro) ) %>%
  mutate(guild = ifelse(taxa %in% c("P.latipinna", "L.macrochirus", 
                                    "L.cyanellus","L.gulosus") |
                          substr(taxa,1,1) == 'F', 'Fish', 'Invertebrate')) %>%
  fix_site_order()
  
#------------------------------------------------------------------------------
# Figure
#------------------------------------------------------------------------------

p_aq_itl_facet_ <- d %>%
  ggplot(aes(x=Aq_mu, y=TL_mu, fill=site)) +
  facet_grid(guild~comparison) +
  geom_text_repel(aes(label=taxa), show.legend=F) +
  #geom_errorbar(aes(ymin=TL_bottom, ymax=TL_top)) +
  #geom_errorbarh(aes(xmin=Aq_bottom, xmax=Aq_top)) +
  geom_point(shape=22, size = 4) +
  theme_bw(base_size = 14) +
  scale_fill_manual(values=my_colors) +
  xlim(c(-5,100)) +
  ylim(c(-0.5, 3.5))
  

plot_aq_tl <-  function(x) {
    
  myout <- x %>%
      ggplot(aes(x=Aq_mu, y=TL_mu)) +
      theme_bw(base_size = 11) +
      scale_shape_manual(values=c(21:27)) +
      scale_fill_manual(values=my_colors) +
      guides(fill = guide_legend(override.aes=list(shape=22))) +
    xlab('Autochthonous Assimilation (%)') +
    ylab('Isotopic Trophic Position') +
    xlim(c(-5, 105)) +
    ylim(c(0.5, 3.5)) +
    theme(legend.title = element_blank(),
            legend.box.background = element_rect(colour = "black")) +
    geom_smooth(method='lm', se=F, color = 'black', linetype=2,
                linewidth=.3) +
    geom_jitter(aes(fill=site, shape=taxa),
                size=5, alpha=.8,
                position = position_jitter(width = 0.5, height = 0.5)) +
    stat_poly_eq(use_label(c("R2", "P")),
                 label.y = "bottom", label.x = "left",
                 size=3.5)
  
  return(myout) }


p_aq_itl_i_spe <- d %>% filter(taxa %in% c('Coenagrionidae', 
                                           'Corbiculidae')) %>%
  plot_aq_tl() 

p_aq_itl_i_tro <- d %>% filter(substr(taxa,1,2) == 'I-') %>%
  ggplot(aes(x=Aq_mu, y=TL_mu)) +
  theme_bw(base_size = 11) +
  scale_shape_manual(values=c(21:27)) +
  scale_fill_manual(values=my_colors) +
  guides(fill = guide_legend(override.aes=list(shape=22))) +
  xlab('Autochthonous Assimilation (%)') +
  ylab('Isotopic Trophic Position') +
  xlim(c(-5, 105)) +
  ylim(c(0.5, 3.5)) +
  theme(legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  geom_jitter(aes(fill=site, shape=taxa),
              size=5, alpha=.8,
              position = position_jitter(width = 0.5, height = 0.5)) +
  stat_poly_eq(use_label(c("R2", "P")),
               label.y = "bottom", label.x = "left",
               size=3.5)

p_aq_itl_f_spe <- d %>% filter(taxa %in% c("P.latipinna", "L.macrochirus", 
                                           "L.cyanellus", "L.gulosus")) %>%
  plot_aq_tl() 

p_aq_itl_f_tro <- d %>% filter(substr(taxa,1,2) == 'F-') %>%
  plot_aq_tl() 

#------------------------------------------------------------------------------
# Table
#------------------------------------------------------------------------------

table_aq_itl <- d %>%
  mutate(`ITL CI` = paste('(', tormat(TL_bottom,1), ', ',
                          tormat(TL_top,1), ')', sep='')) %>%
  mutate(`Aquatic CI` = paste('(', tormat(Aq_bottom,0), ', ',
                              tormat(Aq_top,0), ')', sep=''),
         TL_mu=tormat(TL_mu,1)) %>%
  rename(Aquatic = Aq_mu, ITL = TL_mu,
         Group=taxa, Site = site) %>%
  select(Group, Site, Aquatic, `Aquatic CI`, ITL, `ITL CI`) %>%
  arrange(Group, Site, Aquatic, ITL) 

table_aq_itl_spe <- table_aq_itl %>%
  filter(Group %in% c("P.latipinna", "L.macrochirus", "L.cyanellus", 
                     "L.gulosus", "Corbiculidae", "Coenagrionidae"))

table_aq_itl_tro <- table_aq_itl %>%
  filter(substr(Group,2,2) == '-')

write_csv(d, 'Rout/data_aq_itl.csv')
write_csv(table_aq_itl, 'Rout/table_aq_itl_all.csv')
write_csv(table_aq_itl_spe, 'Rout/table_aq_itl_spe.csv')
write_csv(table_aq_itl_tro, 'Rout/table_aq_itl_tro.csv')

#------------------------------------------------------------------------------
# End aq_itl_combined
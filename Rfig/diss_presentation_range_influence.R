# fig_range_influence
# Species influencing H and N ranges
# Trophic groups influencing H and N ranges
# written by: sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

d_inf <- read_csv('Rout/d_range_influence.csv')
table_inf <- read_csv('Rfig/table_range_influence.csv')

# Prepare df for plotting
my_labeller <- as_labeller(c(C="delta~C^13", 
                             H="delta~H^2", 
                             N="delta~N^15"),
                           default = label_parsed)
#------------------------------------------------------------------------------
# plot influencers
#------------------------------------------------------------------------------
p_inf_H <- d_inf %>%
  mutate(Site = str_replace_all(Site, 'Transition', 'Mesic')) %>%
  mutate(Site = fct_relevel(Site, 'Semi-Arid', 'Mesic', 'Sub-Humid')) %>%
  filter(Element %in% c('H')) %>%
  ggplot(aes(x=Aquatic, y=`Range Ext.`, fill=Site, label=Taxa)) +
  geom_text_repel(aes(x=Aquatic, y=`Range Ext.`, fill=Site, label=Taxa), 
                  size = 5, box.padding = 0.5, max.overlaps=4,
                  show.legend = F) +
  geom_point(aes(color=Site), shape=22, size=4) +
  scale_color_manual(values=my_colors2) +
  scale_fill_manual(values=my_colors) +
  facet_wrap(~Element, labeller = my_labeller) +
  xlim(c(-10,100)) +
  ylim(c(-5,55)) +
  ylab('Range Extension (%)') +
  xlab('% Autochthonous Assimilation') +
  dark_theme_grey(base_size = 18) +
  theme(legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black"))

p_inf_N <- d_inf %>%
  filter(Element %in% c('N')) %>%
  ggplot(aes(x=I.T.L, y=`Range Ext.`, fill=Site, label=Taxa)) +
  geom_text_repel(aes(x=I.T.L, y=`Range Ext.`, fill=Site, label=Taxa), 
                  size = 5, box.padding = 0.5, max.overlaps=4,
                  show.legend = F) +
  geom_point(aes(color=Site), shape=22, size=4) +
  scale_color_manual(values=my_colors2) +
  scale_fill_manual(values=my_colors) +
  facet_wrap(~Element, labeller = my_labeller) +
  xlim(c(0, 3)) +
  ylim(c(-5,55)) +
  ylab('Range Extension (%)') +
  xlab('Isotopic Trophic Level') +
  dark_theme_grey(base_size = 18) +
  theme(legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black")) 

p_inf_combo <- p_inf_H + 
  theme(legend.position='none') +
  p_inf_N + ylab(element_blank())

#------------------------------------------------------------------------------
# influence Table
#------------------------------------------------------------------------------
my_ffg_order <- c("I-Herbivore", "I-Gatherer", "I-Filterer", "I-Predator", 
                  "F-Herbivore", "F-Invertivore", "F-Piscivore")

table_inf <- table_inf %>%
  filter(Element != 'C') %>%
  mutate(T.Group=factor(T.Group, levels=my_ffg_order)) %>%
  arrange(Element, T.Group) %>%
  mutate(R.Abundance=ifelse(is.na(R.Abundance), "", tormat(R.Abundance, 0)),
         Aquatic=str_replace_all(Aquatic, 'NA', '   '),
         I.T.L=str_replace_all(I.T.L, 'NA' , '   '))


write_csv(table_inf, 'Rfig/table_inf_2.csv')
#------------------------------------------------------------------------------
# End fig_range_influence
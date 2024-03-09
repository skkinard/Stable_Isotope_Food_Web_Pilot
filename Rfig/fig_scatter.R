# fig_scatter
# Detailed fish and invertebrate biplots
# written by: sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

d <- read_csv("Rout/isotope_data_correctedH_correctedN.csv") %>%
  filter(guild %in% c('Fish', 'Invertebrate'))
#------------------------------------------------------------------------------
# biplot ranges for each trophic group facet by site
#------------------------------------------------------------------------------

# bootstrap mu and ci for each trophic category
d_boot_tro_mu <- d %>%
  group_by(site, guild, trophic_category) %>%
  dplyr::summarize(hydrogen = estimate_mu(hydrogen),
                   carbon = estimate_mu(carbon),
                   nitrogen = estimate_mu(nitrogen))

# Source mean and CI
d_source <- read_csv("Data/isotope_data_correctedH_correctedN.csv") %>%
  filter(guild %in% c('Aquatic', 'Terrestrial')) %>%
  group_by(site,guild) %>%
  dplyr::summarize(H_mu=mean(hydrogen),
                   H_min=min(hydrogen),
                   H_max=max(hydrogen),
                   N_mu=mean(nitrogen),
                   N_min=min(nitrogen),
                   N_max=max(nitrogen))%>%
  mutate(site=fct_relevel(site, c('Semi-Arid','Transition','Sub-Humid')))

#------------------------------------------------------------------------------
# Prep fish groups
#------------------------------------------------------------------------------
d_fish_order <- d %>%
  filter(guild=='Fish') %>%
  add_taxonomic() %>%
  mutate(size_group = case_when(
    size_max <=40 ~ "< 40mm",
    size_max >40 & size_max <=80 ~ "< 80mm",
    size_max >80 & size_max <=120 ~ "< 120mm",
    size_max>120 ~ "> 120mm")) %>%
  filter(!is.na(size_group)) %>%
  mutate(size_group = fct_relevel(size_group,
                                  c("< 40mm", "< 80mm",
                                    "< 120mm", "> 120mm"))) %>%
  ungroup() %>%
  mutate(site=fct_relevel(site, c('Semi-Arid','Transition','Sub-Humid'))) %>%
  mutate(order2=case_when(
    order == 'Centrachiformes' ~ 'R',
    order == 'Cyprinidontiformes' ~ 'D',
    order == 'Siluriformes' ~ 'S',
    order == 'Cypriniformes' ~ 'N',
    order == 'Perciformes' ~ 'P',
    order == 'Lepisosteiformes' ~ "L",
    order == 'Cichliformes' ~ "H")) %>%
  mutate(order=str_replace_all(order, 'formes', 'd')) %>%
  mutate(order = fct_relevel(order, c('Centrachid',
                                      'Cyprinidontid',
                                      'Silurid',
                                      'Cyprinid',
                                      'Percid',
                                      'Lepisosteid',
                                      'Cichlid')))
d_invert <- d %>%
  filter(guild == 'Invertebrate') %>%
  mutate(trophic_category = fct_relevel(trophic_category, 
                                        c('Herbivore','Gatherer',
                                          'Filterer', 'Predator'))) %>%
  mutate(site = fct_relevel(site, c('Semi-Arid','Transition','Sub-Humid')))



#------------------------------------------------------------------------------
# Biplots
#------------------------------------------------------------------------------
biplot_invert <-  d_invert %>%
  ggplot() +
  geom_rect(data = d_source %>% filter(guild == 'Aquatic'),
            aes(x = H_mu, y = N_mu,
                xmin = H_min,
                xmax = H_max,
                ymin = N_min,
                ymax = N_max),
            fill = 'cyan', alpha = .3) +
  geom_rect(data = d_source %>% filter(guild == 'Terrestrial'),
            aes(x = H_mu, y = N_mu,
                xmin = H_min,
                xmax = H_max,
                ymin = N_min,
                ymax = N_max),
            fill = 'red', alpha = .3) +
  geom_label(data=d_source%>%filter(guild=='Aquatic'),
            aes(label=substr(guild,1,2), x=H_mu, y=N_mu),
            fill='cyan', color='black') +
  geom_label(data=d_source%>%filter(guild=='Terrestrial'),
            aes(label=substr(guild,1,2), x=H_mu, y=N_mu),
            fill='red', color='black') +
  geom_text_repel(aes(x = hydrogen, y = nitrogen, fill = trophic_category, 
                      shape = trophic_category, label = substr(species, 1, 4)),
                  box.padding = .5 , force = 20) +
  geom_point(aes(x = hydrogen, y = nitrogen, 
                 fill = trophic_category, shape = trophic_category),
             size = 3) +
  facet_wrap(~site) +
  theme_bw(base_size = 10) +
  scale_fill_manual(values = c('yellow', 'palegreen1', 'skyblue2', 'purple3')) +
  scale_shape_manual(values = c(21:24)) +
  ylab(expression(paste(delta)^15*N~('\u2030'))) +
  xlab(expression(paste(delta)^2*H~('\u2030'))) +
  theme(legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  guides(fill = guide_legend(override.aes = list(shape = c(21:24))))

biplot_fish <- d_fish_order %>%
  ggplot() +
  geom_rect(data=d_source%>%filter(guild=='Aquatic'),
            aes(xmin= H_min,xmax=H_max,ymin=N_min,ymax=N_max), 
            fill='cyan', alpha=.3) +
  geom_rect(data=d_source%>%filter(guild=='Terrestrial'),
            aes(xmin= H_min,xmax=H_max,ymin=N_min,ymax=N_max), 
            fill='red', alpha=.3) +
  geom_label(data=d_source%>%filter(guild=='Aquatic'),fill='cyan',
             aes(label=substr(guild,1,2), x=H_mu, y=N_mu), color='black') +
  geom_label(data=d_source%>%filter(guild=='Terrestrial'), fill='red',
             aes(label=substr(guild,1,2), x=H_mu, y=N_mu), color='black') +
  geom_point(aes(x = hydrogen, y = nitrogen, fill=size_group),
             data= d %>% mutate(site=fct_relevel(site, c('Semi-Arid',
                                                         'Transition',
                                                         'Sub-Humid'))) %>%
               filter(guild=='Invertebrate'),
             size=2, shape = 4, fill=NA, color='grey25') +
  geom_point(aes(x = hydrogen, y = nitrogen, fill=size_group, shape=order), 
             size = 4) +
  geom_label(data=d_fish_order%>%filter(size_group %in% c("< 120mm", "> 120mm")),
             aes(label=order2, x = hydrogen, y = nitrogen, fill=size_group),
             size=3, show.legend=F) +
  geom_label(data=d_fish_order%>%filter(size_group %in% c("< 120mm", "> 120mm")),
            aes(label=order2, x = hydrogen, y = nitrogen, fill=size_group),
            color='white', size=3, show.legend=F) +
  geom_label(data=d_fish_order%>%filter(size_group %in% c("< 40mm", "< 80mm")),
             aes(label=order2, x = hydrogen, y = nitrogen, fill=size_group),
             size=3, show.legend=F) +
  # geom_text_repel(data= d_invert,
  #                 aes(x = hydrogen, y = nitrogen, 
  #                     label = species),
  #                 max.overlaps = 1) +
  facet_wrap(.~site, ncol=1) +
  theme_bw(base_size = 10) +
  scale_fill_viridis_d(direction=-1) +
  scale_shape_manual(values=c(82, 68, 83, 78, 80, 76, 72)) +
  guides(fill = guide_legend(override.aes=list(shape=22))) +
  #theme(legend.position = c(0.4, 0.1),legend.background = element_rect(colour = 'grey50', fill = 'white', linetype='solid'), legend.title = element_blank(), legend.direction="horizontal") +
  ylab(expression(paste(delta)^15*N~('\u2030'))) +
  xlab(expression(paste(delta)^2*H~('\u2030'))) +
  theme(legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black"))

#------------------------------------------------------------------------------
# End fig_scatter
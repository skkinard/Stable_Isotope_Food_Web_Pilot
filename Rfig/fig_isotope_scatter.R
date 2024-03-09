# fig_isotope_scatter
# Project: summer 2019 3-sites
# stable isotopes: carbon, nitrogen, hydrogen
# written by: sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

d <- read_csv('Rdat/isotope_data.csv') %>%
  as_tibble()

#----------------------------------------------------------------------------
# C H N plots
#----------------------------------------------------------------------------

my_scatter <- function(my_data, xvar, yvar) {
  colnames(my_data) <- str_replace_all(colnames(my_data), xvar, 'XXX')
  colnames(my_data) <- str_replace_all(colnames(my_data), yvar, "YYY")
  
  xvar1 <- xvar%>%str_to_title()%>%substr(1,1)
  yvar1 <- yvar%>%str_to_title()%>%substr(1,1)
  
  my_data %>%
    fix_site_order() %>%
    ggplot(aes(x = XXX, y = YYY)) +
    #stat_ellipse(aes(fill = Category), geom = 'polygon', alpha = .3) +
    stat_ellipse(aes(color = guild), 
                 linetype =1,
                 level = .9, show.legend = F) +
    geom_point(aes(shape = guild, fill = guild),
               size = 2, alpha = .6) +
    facet_wrap(vars(site), nrow=3) +
    scale_color_manual(values = my_colors2) +
    scale_fill_manual(values = my_colors2) +
    theme_classic(base_size = 14) +
    scale_shape_manual(values = c(21:25)) +
    #theme(legend.position = c(0.4, 0.1),legend.background = element_rect(colour = 'grey50', fill = 'white', linetype='solid'), legend.title = element_blank(), legend.direction="horizontal")
    theme(legend.title=element_blank(),
          legend.box.background = element_rect(colour = "black")) +
    xlab(xvar1)+
    ylab(yvar1) }

biplot_NH <- my_scatter(d, xvar='hydrogen', yvar='nitrogen') +
  ylab(expression(paste(delta)^15*N~('\u2030'))) +
  xlab(expression(paste(delta)^2*H~('\u2030'))) 

biplot_CH <- my_scatter(d, xvar='hydrogen', yvar='carbon') +
  ylab(expression(paste(delta)^13*C~('\u2030'))) +
  xlab(expression(paste(delta)^2*H~('\u2030')))

biplot_NC <- my_scatter(d, xvar='hydrogen', yvar='nitrogen') +
  ylab(expression(paste(delta)^15*N~('\u2030'))) +
  xlab(expression(paste(delta)^13*C~('\u2030')))

boxplot_CHN <- d %>%
  pivot_longer(cols=c('carbon', 'nitrogen', 'hydrogen'), names_to = 'isotope', values_to = 'value') %>%
  mutate(isotope = str_to_title(isotope)) %>%
  ggplot(aes(value)) +
  geom_boxplot(aes(fill = guild)) +
  facet_grid(site ~ isotope, scales="free", labeller = iso_facet_lab) +
  scale_fill_manual(values = my_colors2) +
  theme_bw(base_size = 12) +
  scale_shape_manual(values = c(21:25)) +
  guides(fill = guide_legend(reverse=TRUE)) +
  xlab(element_blank())+
  theme(legend.title=element_blank(),
        legend.box.background = element_rect(colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

#----------------------------------------------------------------------------
# C H N Table
#----------------------------------------------------------------------------

# generate species summary stats:
table_CHN_raw <- d %>%
  group_by(guild, trophic_category, species, site) %>%
  summarise(hydrogen_mean = mean(hydrogen, na.rm=TRUE),
            hydrogen_n = length(hydrogen),
            hydrogen_sd = sd(hydrogen, na.rm=TRUE),
            hydrogen_se = sd(hydrogen, na.rm=TRUE) / sqrt(sum(!is.na(hydrogen))),
            carbon_mean = mean(carbon, na.rm=TRUE),
            carbon_n = length(carbon),
            carbon_sd = sd(carbon, na.rm=TRUE),
            carbon_se = sd(carbon, na.rm=TRUE) / sqrt(sum(!is.na(carbon))),
            nitrogen_mean = mean(nitrogen, na.rm=TRUE),
            nitrogen_n = length(nitrogen),
            nitrogen_sd = sd(nitrogen, na.rm=TRUE),
            nitrogen_se = sd(nitrogen, na.rm=TRUE) / sqrt(sum(!is.na(nitrogen)))) %>%
  mutate(species = str_to_title(species))

table_CHN_combined <- table_CHN_raw %>%
  mutate(H2 = paste(round(hydrogen_mean, 2), '±', round(hydrogen_se, 2)),
         C13 = paste(round(carbon_mean, 2), '±', round(carbon_se, 2)),
         N15 = paste(round(nitrogen_mean, 2), '±', round(nitrogen_se, 2))) %>%
  rename(Category = guild, 'Trophic Group'=trophic_category, Sample = species, Rainfall = site, n = nitrogen_n) %>%
  select(Category, 'Trophic Group', Sample, Rainfall, H2, C13, N15, n)

#----------------------------------------------------------------------------
# Table
#----------------------------------------------------------------------------
d_species <- read_csv('Rdat/my_fish_species.csv') %>%
  mutate(lowest_taxon = str_replace_all(lowest_taxon, 'cyanoguttatum', 'cyanoguttatus'),
         species = str_replace_all(species, 'cyanoguttatum', 'cyanoguttatus'),
         lowest_taxon = str_replace_all(lowest_taxon, ' ', '')) %>%
  select(lowest_taxon, species, genus) %>%
  unique()



d %>% as_tibble() %>% filter(str_detect(species, 'H')) %>%
  select(species)

table_HCN <- d %>% as_tibble() %>%
  rename(lowest_taxon=species) %>%
  left_join(d_species) %>%
  mutate(species = ifelse(guild == 'Fish', 
                          paste(genus, str_to_lower(species), sep = ' '),
                          lowest_taxon)) %>%
  mutate(d2H = ifelse(is.na(s.hydrogen_sd), 
                      paste(tormat(s.hydrogen_mean, 1), 
                            " (", s.hydrogen_n, ')', sep=''),
                      paste(tormat(s.hydrogen_mean, 1), 
                            " \u00b1 ", round(s.hydrogen_sd, 1), 
                            " (", s.hydrogen_n, ')', sep='')),
         d13C = ifelse(is.na(s.carbon_sd), 
                       paste(tormat(s.carbon_mean, 1), 
                             " (", s.carbon_n, ')', sep=''),
                       paste(tormat(s.carbon_mean, 1), 
                             " \u00b1 ", tormat(s.carbon_sd, 1), 
                             " (", s.carbon_n, ')', sep='')),
         d15N = ifelse(is.na(s.nitrogen_sd), 
                       paste(tormat(s.nitrogen_mean, 1), 
                             " (", s.nitrogen_n, ')', sep=''),
                       paste(tormat(s.nitrogen_mean, 1), 
                             " \u00b1 ", tormat(s.nitrogen_sd, 1), 
                             " (", s.nitrogen_n, ')', sep='')) ) %>%
  select(guild, species, site,  d2H, d13C, d15N) %>%
  mutate(d2H = ifelse(is.na(d2H), '', d2H),
         d13C = ifelse(is.na(d13C), '', d13C),
         d15N = ifelse(is.na(d15N), '', d15N)) %>%
  mutate(guild = fct_relevel(guild, 'Aquatic', 'Terrestrial', 'Fish', 'Invertebrate'),
         site = fct_relevel(site, 'Semi-Arid', 'Transition', 'Sub-Humid')) %>%
  mutate(species = str_to_title(species),
         species = ifelse(species == 'Leaf', 'Green Leaves', species)) %>%
  rename(Guild = guild,
         Species = species,
         Site = site) %>%
  arrange(Guild, Species, Site) %>%
  unique()


write_csv(table_HCN,'Rfig/table_HCN.csv')
#----------------------------------------------------------------------------
# End fig_isotope_scatter
# fig_HN_niche_metrics
# written by: sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

SEA_all <- read_csv('Data/ellipse_output.csv')
layman_all <- read_csv("Rout/layman_output.csv")

#----------------------------------------------------------------------------
# Table SEA
#----------------------------------------------------------------------------
my_order <- c('Invertebrate', 'Fish',
             "I-Herbivore","I-Gatherer", "I-Filterer", "I-Predator", 
             "F-Herbivore", "F-Invertivore", "F-Piscivore",
             "P.latipinna", "L.macrochirus",  "L.cyanellus", "L.gulosus")

table_siber <- layman_all %>%
  filter(m_group %in% c('Fish', 'Invertebrate')) %>%
    arrange(stat, m_group) %>%
    mutate(signif=case_when(
      is.na(signif) ~ "  ",
      signif == "∗" ~ "∗ ",
      signif == "∗∗" ~ "∗∗"),
      smy=paste(tormat(mu,1), " (",
                tormat(lower,1), ", ",
                tormat(upper,1), ")", signif, sep='')) %>%
  select(stat, m_group, site, smy) %>%
  pivot_wider(names_from=site, values_from = smy) %>%
  rename(Group=m_group, Metric=stat)

table_SEA_caption <- 'Standard Ellipse Area (\u2030^2) for fish, invertebrates, trophic categories, and common species. Summary statistics represent the highest density mode and the associated 95% credible interval from the bayesian estimates of Standard Ellipse Area using d2H and d15N. Significance marks groups where there is no overlap between confidence intervals between a pair of sites.'

write_csv(table_siber, 'Rfig/table_siber.csv')

#----------------------------------------------------------------------------
# plot SIBER niche metrics
#----------------------------------------------------------------------------

siber_plot <- function(x) {
  x %>%
    fix_site_order() %>%
    ggplot(aes(x=m_group, y=mu, fill = site)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), 
                  position=position_dodge(width = .6),
                  width = .8, linewidth=.3) +
    geom_point(size=4, shape = 23, 
               position=position_dodge(width = .6)) +
    scale_fill_manual(breaks=c('Sub-Humid', 'Transition', 'Semi-Arid'),
                      values = rev(my_colors)) +
    theme_bw(base_size = 14) +
    theme(legend.title = element_blank(),
          legend.box.background = element_rect(colour = "black")) +
    coord_flip() +
    scale_x_discrete(limits = rev) +
    xlab(element_blank()) +
    ylab(element_blank()) }

siber_label<- as_labeller(
  c('Ellipse Area'="paste(delta)^2*H~x~paste(delta)^15*N~Ellipse~Area~('\u2030'^2)",
    'Neighbor Distance'="Nearest~Neighbor~Distance~('\u2030')"),
  default = label_parsed)

plot_nearest <- layman_all %>%
  filter(m_group %in% c('Fish', 'Invertebrate')) %>%
  filter(stat %in% c('Neighbor Distance')) %>%
  siber_plot() +
  facet_wrap(~stat, scales='free', labeller=siber_label)

plot_SEA <- layman_all %>%
  filter(m_group %in% c('Fish', 'Invertebrate')) %>%
  filter(stat %in% c('Ellipse Area')) %>%
  siber_plot() +
  facet_wrap(~stat, scales='free', labeller=siber_label)

plot_area_caption <- 'Standard ellipse area (permil^2), the range of deuterium (scaled) values, and the range of (scaled) nitrogen values for fish or invertebrate communities. Diamonds, colored by site (darkening with increasing precipitation), represent the density mode with bars extending to 95% credible intervals of the bayesian estimates using d2H and d15N. Red asterisks highlight instances where there is no overlap between credible intervals between a pair of sites for a given community.'

#----------------------------------------------------------------------------
# End fig_HN_SIBER


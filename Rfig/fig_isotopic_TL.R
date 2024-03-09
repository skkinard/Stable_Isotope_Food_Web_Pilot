# fig_Isotopic_TL
# Sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

d <- read_csv('Rout/TL_signif.csv')

my_ffgs <- c("F-Herbivore","F-Invertivore", "F-Piscivore",
             "I-Filterer", "I-Predator")
my_guilds <- c('Fish', 'Invertebrate')

#----------------------------------------------------------------------------
# Figure
#----------------------------------------------------------------------------

# plot function
trophic_plot <- function(my_data) {
  my_data %>%
    ggplot(aes(m_group, y=mu, fill = site)) +
    geom_errorbar(aes(ymin=lower, ymax=upper), 
                  position=position_dodge(width = .5),
                  width = 0.4, linewidth=.15) +
    geom_point(size=4, shape = 23, aes(fill=site), 
               position=position_dodge(width = .5)) +
    #geom_point(aes(x=m_group, y = m_group_sig), 
    #           color = 'red', shape = 8, size = 3, show.legend =F) +
    scale_fill_manual(breaks= c('Sub-Humid', 'Transition','Semi-Arid'),
                      values = rev(my_colors)) +
    ylab('Isotopic Trophic Level') +
    xlab(element_blank()) +
    theme_bw(base_size = 14) +
    ylim(c(-.2, 2.7)) +
    theme(legend.title = element_blank(),
          legend.box.background = element_rect(colour = "black")) +
    scale_x_discrete(limits = rev)
  }

# plot guild , ffg, and common species
#plot_TL_f <- trophic_plot(filter(TL_all,
#m_group %in% c("Fish", 
#"F-Herbivore", "P.latipinna", 
#"F-Invertivore", "L.macrochirus",
#"F-Piscivore", "L.cyanellus", "L.gulosus")))

#plot_TL_i <- trophic_plot(filter(TL_all,
#m_group %in% c("Invertebrate", "I-Filterer",
#"I-Gatherer", "I-Predator","Corbiculidae", 
#"Coenagrionidae")))

#plot_TL_f + plot_TL_i + plot_layout(guides = 'collect') +
#plot_annotation(tag_levels = "A")

# plot guild and ffg
plot_TL_guild <- d %>%
  filter(m_group %in% my_guilds) %>%
  mutate(m_group=fct_relevel(m_group, my_guilds)) %>%
  fix_site_order() %>%
  trophic_plot() 

plot_TL_ffg <- d %>%
  filter(m_group %in% my_ffgs) %>%
  mutate(m_group=fct_relevel(m_group, my_ffgs)) %>%
  fix_site_order() %>%
  trophic_plot() 

plot_TL_caption <- "Isotopic trophic levels for fish and invertebrate communities as well as functional feeding groups. Diamonds represent mean values and are colored according to sample site (darkening with increased precipitation) with bars extending to the 95% confidence interval."

#----------------------------------------------------------------------------
# Table
#----------------------------------------------------------------------------
table_TL <- d %>%
  mutate(m_group=fct_relevel(
    m_group, c("Fish", 
               "F-Herbivore", "P.latipinna", 
               "F-Invertivore", "L.macrochirus",
               "F-Piscivore", "L.cyanellus", "L.gulosus",
               "Invertebrate", "I-Filterer", "Corbiculidae",
               "I-Predator"))) %>%
  arrange(m_group,site) %>%
  mutate(n=format(n, nsmall=),
         mu = tormat(mu, 1),
         CI.95 = paste("(", tormat(lower,1), ', ', 
                       tormat(upper,1), ")", sep=''),
         CI.95=str_replace_all(CI.95, " ", ""),
         CI.95=str_replace_all(CI.95, ",", ", "),
         signif=case_when(is.na(signif) ~ '  ',
                          signif == "∗" ~ "∗ ",
                          signif == "∗∗" ~ "∗∗"),
         smmry = paste(mu, " ", CI.95, signif, sep='')) %>%
  select(site, m_group, smmry) %>%
  pivot_wider(names_from=site, values_from=smmry) %>%
  rename(Group=m_group)
  
write_csv(table_TL, 'Rfig/table_TL.csv')

#----------------------------------------------------------------------------
# End fig_Isotopic_trophic_level
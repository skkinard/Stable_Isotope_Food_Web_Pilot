# fig_HN_range
# written by: Sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

d_range <- read_csv('Rout/HN_range.csv') %>%
  fix_site_order() %>%
  mutate(guild = fct_relevel(guild, 'Invertebrate', 'Fish'))
layman_all <- read_csv("Rout/layman_output.csv") %>%
  fix_site_order() %>%
  mutate(m_group = fct_relevel(m_group, 'Invertebrate', 'Fish'))

my_labeller <- as_labeller(c(Carbon="delta~C^13~Range~('\u2030')", 
                             Hydrogen="delta~H^2~Range~('\u2030')", 
                             Nitrogen="delta~N^15~Range~('\u2030')"),
                           default = label_parsed)
#------------------------------------------------------------------------------
# Plot boostrap H and N range
#------------------------------------------------------------------------------
base_plot <- function(x) {
  x %>%
    ggplot(aes(x=guild, y=upper +10, fill = site)) +
    geom_errorbar(aes(ymin=lower, ymax=upper), 
                  position=position_dodge(width = .6),
                  width = 0.6, linewidth=.3) +
    geom_point(aes(y=mu), size=4, shape = 23, 
               position=position_dodge(width = .6)) +
    #geom_text(aes(y=upper+5, label=signif), color = 'red',
    #         position=position_dodge(width = .6),
    #        size=8) +
    scale_fill_manual(breaks=c('Sub-Humid', 'Transition', 'Semi-Arid'),
                      values = rev(my_colors)) +
    theme_bw(base_size = 14) +
    ylab(element_blank()) +
    theme(legend.title = element_blank(),
          legend.box.background = element_rect(colour = "black")) +
    xlab(element_blank()) +
    coord_flip() }

p_niche_dimenensions <- d_range %>%
  base_plot() +
  facet_wrap(~element, scales = 'free', labeller = my_labeller)

p_h_range <- d_range %>%
  filter(element == 'Hydrogen') %>%
  base_plot() +
  facet_wrap(~element, scales = 'free', labeller = my_labeller)

p_n_range <- d_range %>%
  filter(element == 'Nitrogen') %>%
  base_plot() +
  facet_wrap(~element, scales = 'free', labeller = my_labeller)

#------------------------------------------------------------------------------
# Table H & N bootstrapped Range Estimates
#------------------------------------------------------------------------------

table_HN_ranges <- d_range %>%
  filter(element != 'Carbnon') %>%
  mutate(signif = case_when(is.na(signif) ~ '  ',
                            signif == '∗'~ "∗ ",
                            signif == '∗∗' ~ "∗∗") ) %>%
  mutate(Range = paste(tormat(mu,1), ' (', 
                       tormat(lower, 1), ',', 
                       tormat(upper,1),
                       ')', signif, sep='')) %>%
  mutate(element=paste(element, 'Range', sep=' ')) %>%
  select(site, guild, element, Range) %>%
  pivot_wider(names_from=element, values_from=Range) %>%
  select(guild, everything()) %>%
  rename(Site = site, Guild = guild)

write_csv(table_HN_ranges, 'Rfig/table_HN_ranges.csv')

#------------------------------------------------------------------------------
# Table SEA, NND, HR, and NR
#------------------------------------------------------------------------------

table_siber_range <- d_range %>%
  rename(m_group=guild) %>%
  mutate(stat = paste(substr(element,1,1), 'Range', sep="-")) %>%
  select(-element) %>%
  full_join(layman_all %>%
              filter(stat %in% c('Ellipse Area', 'Neighbor Distance')))  %>% 
  filter(m_group %in% c('Invertebrate', 'Fish')) %>%
  mutate(signif=ifelse(is.na(signif), '', signif)) %>%
  mutate(smmry=paste(tormat(mu, 1),
                     ' (', tormat(lower, 1),
                     ', ', tormat(upper, 1),
                     ')', signif, sep='')) %>%
  rename(guild=m_group) %>%
  select(site, guild, stat, smmry) %>%
  pivot_wider(names_from = site, values_from = smmry) %>%
  mutate(units = ifelse(stat == 'Ellipse Area', permil2, "‰" ) ) %>%
  select(guild, stat, units, everything()) %>%
  arrange(guild, stat) %>%
  rename(Guild=guild, Estimate=stat, Unit=units)

write_csv(table_siber_range, 'Rfig/table_siber_and_bstrap_range.csv')

#------------------------------------------------------------------------------
# End fig_HN_range
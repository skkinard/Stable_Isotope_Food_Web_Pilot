# fig_algae_boot
# written by: sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

algae_boot <- read_csv('Rout/algae_boot.csv')
library(ggdark)
#----------------------------------------------------------------------------
# Figure
#----------------------------------------------------------------------------
plot_algae_boot <- algae_boot %>%
  mutate(treatment = str_replace_all(treatment, 'Open', 'Control')) %>%
  mutate(
    site=fct_relevel(site, c('Sub-Humid', 'Transition', 'Semi-Arid'))) %>%
  ggplot(aes(x=site, y=mu, color=treatment, fill=treatment, shape=treatment)) +
  #geom_line(linetype = 9, aes(group = treatment)) +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                position = position_dodge(width=0.2),
                width=.18) +
  geom_point(size = 4, color='black',
             position = position_dodge(width=0.2)) +
  dark_theme_grey(base_size=18) +
  scale_y_log10() +
  scale_fill_manual(values=c('red', 'cyan')) +
  scale_color_manual(values=c('red', 'cyan')) +
  scale_shape_manual(values=c(21, 23)) +
  xlab(element_blank())+
  ylab(expression(paste('Total Algae ( ', mu, 'g/cm)'))) +
  theme(legend.position = c(0.2, 0.82)) +
  theme(legend.background = element_rect(colour = 'grey50', fill = 'black', linetype='solid')) +
  theme(legend.title = element_blank())

#----------------------------------------------------------------------------
# Table
#----------------------------------------------------------------------------
table_algae_boot <- algae_boot %>%
  mutate(Tre.Sig = ifelse(is.na(Tre.Sig), '', Tre.Sig)) %>%
  mutate(Algae=tormat(mu, 2), 
         CI.95=paste('(', tormat(lower, 2), ', ',
         tormat(upper, 2), ')', sep='' )) %>%
  select(site, treatment, Algae, CI.95, Sit.Sig, Tre.Sig) %>%
  rename(Site=site, Treatment=treatment)

write_csv(table_algae_boot, 'Rfig/table_algae_boot.csv')
#----------------------------------------------------------------------------
# End fig_algae_boot

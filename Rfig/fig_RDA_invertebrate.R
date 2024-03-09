# fig_RDA_invertebrate
# written by: sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

coordinates_i <- read_csv('Rout_RDA_coordinates_invertebrate.csv') 

axis_i <- read_csv('Rout/RDA_axis_invertebrate.csv') 

vectors_i <- read_csv('Rout/RDA_vectors_invertebrate.csv') 

# Extract site, environment, and species data frames
site_coor <- coordinates_i %>% filter(c_type=='Site') %>% 
  rename(site=labels) %>% 
  fix_site_order()

spe_coor <- coordinates_i %>% filter(c_type=='Species') %>% 
  rename(species=labels) 

env_vec <- vectors_i %>% 
  filter(v_type=='Physical') %>%
  mutate(my_label=vector%>%str_to_title())

spe_vec <- vectors_i %>% 
  filter(v_type=='Species')

#----------------------------------------------------------------------------
# base ordination plot
#----------------------------------------------------------------------------
# Step 3: Generate plot adding information on sites and species similar to the ordiplot:

RDA_i <- ggplot() +
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab(axis_i[1, "label"]) +
  ylab(axis_i[2, "label"]) +
  scale_x_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  geom_point(data=site_coor,
             aes(x=axis1, y=axis2, fill=site),
             color="black",
             shape = 21,
             size=7) +
  geom_text(data=site_coor %>% filter(site %in% c('Semi-Arid', 'Transition')),
            aes(x=axis1, y=axis2, label=month),
            color="black") +
  geom_text(data=site_coor %>% filter(site == 'Sub-Humid'),
            aes(x=axis1, y=axis2, label=month),
            color="white") +
  #geom_point(data=species.long2,
  #aes(x=axis1, y=axis2), color = 'red', shape = 3) +
  scale_fill_manual(values=my_colors) +
  coord_fixed(ratio=1) +
  theme_classic(base_size = 13) +
  theme(legend.title= element_blank()) +
  theme(legend.title = element_blank(),
        legend.spacing.y = unit(0, "mm"), 
        panel.border = element_rect(colour = "black", fill=NA),
        aspect.ratio = 1, axis.text = element_text(colour = 1, size = 10),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

# With an ordination via redundancy analysis, the positions of species should be interpreted as arrows. It is straightforward to show the positions of the species as arrows. However, with a large number of species, it is better to reduce the number to those that explain more of the variation among the sites. This information is obtained with the envfit function of vegan.

#----------------------------------------------------------------------------
# RDA_i_vectors_e (ordination with environmental vectors)
#----------------------------------------------------------------------------

RDA_i_e_caption <- "Ordintaion of invertebrate communities at Semi-Arid, Transition, and Sub-Humid sites collected in April-June of 2018. Redundancy Analysis axes are labeled with the proportion of variation within the community matrix is explained. The ordination is constrained by six environmental variables represented by  arrows (only those with *p*<0.1 are plotted). Red crosses represent species with grey labels. Circles represent communities (darkening with increasing annual precipitation)."

RDA_i_e <- RDA_i +
  geom_segment(data=env_vec%>%filter(p<0.05),
               aes(x=0, y=0, xend=r2^2*axis1, yend=r2^2*axis2),
               colour="black", size=1, linewidth=.5,
               arrow=arrow(length=unit(0.1, "inches"),
                           type='closed')) +
  geom_text_repel(data=env_vec%>%filter(p<0.05),
               aes(x=r2^2*axis1, y=r2^2*axis2, label=vector),
               colour="black", box.padding=1) +
  geom_segment(data=env_vec%>%filter(p<0.1 & p > 0.05),
               aes(x=0, y=0, xend=r2^2*axis1, yend=r2^2*axis2),
               colour="grey50", size=1, linewidth=.5,
               arrow=arrow(length=unit(0.1, "inches"),
                           type='closed')) +
  geom_text_repel(data=env_vec%>%filter(p<0.1 & p > 0.05),
                  aes(x=r2^2*axis1, y=r2^2*axis2, label=vector),
                  colour="grey50", box.padding=1, max.overlaps=1)

#----------------------------------------------------------------------------
# RDA_i_s (ordination with species vectors)
#----------------------------------------------------------------------------

RDA_i_s_caption <- "Ordintaion of invertebrate communities at Semi-Arid, Transition, and Sub-Humid sites collected in April-June of 2018. Redundancy Analysis axes are labeled with the proportion of variation within the community matrix is explained. Red crosses represent species; common species are in the middle and species that distinguish one community from another are labeled. Circles represent communities, darkening with increasing annual precipitation."

# species vectors (invertebrate Community Composition FIGURE)
RDA_i_s <- RDA_i +
  geom_text_repel(data=spe_vec,
                  aes(x=axis1, y=axis2, label=vector),
                  colour="red",
                  box.padding = .1, max.overlaps = 7)

#----------------------------------------------------------------------------
# RDA_i_se (ordination with environmental vectors and species labels)
#----------------------------------------------------------------------------

RDA_i_se_caption <- "Ordintaion of invertebrate kicknet communities at Semi-Arid, Transition, and Sub-Humid sites collected in May-July of 2018. Redundancy Analysis axes are labeled with the proportion of variation within the community matrix is explained. The ordination is constrained by 6 environmental variables represented by labeled, arrows (only those with *p*<0.1 are plotted). Circles represent communities with numeric labels for the month sampled and shaded by annual precipitation. Red crosses with spread-out labels represent taxonomic families; rare and region-wide species ordinate in the center while site-specific and populous species ordinate next to their community."

  RDA_i_se <- RDA_i +
  geom_point(data=spe_coor, aes(x=axis1, axis2),color='red', shape=4) +
  geom_text_repel(data=spe_vec,
                  aes(x=axis1, y=axis2, label=vector),
                  colour="red",
                  segment.color = "pink2",
                  force=100,
                  max.overlaps =15) +
  geom_point(data=site_coor,
             aes(x=axis1, y=axis2, fill=site),
             color="black",
             shape = 21,
             size=7) +
  geom_text(data=site_coor %>% filter(site %in% c('Semi-Arid', 'Transition')),
            aes(x=axis1, y=axis2, label=month),
            color="black") +
  geom_text(data=site_coor %>% filter(site == 'Sub-Humid'),
            aes(x=axis1, y=axis2, label=month),
            color="white") +
  geom_segment(data=env_vec%>%filter(p<0.05),
               aes(x=0, y=0, xend=r2^2*axis1, yend=r2^2*axis2),
               colour="black", size=1, linewidth=.5,
               arrow=arrow(length=unit(0.1, "inches"),
                           type='closed')) +
  geom_label(data=env_vec%>%filter(p<0.05),
             aes(x=r2^2*axis1, y=r2^2*axis2, label=my_label),
             colour="black") +
  scale_x_continuous(expand = expansion(mult = c(0.2, 0.2)))
  
#----------------------------------------------------------------------------
# table_RDA_i
#----------------------------------------------------------------------------

table_RDA_i_caption <- "Axes values (in radians), correlation coefficients and p vlaues for fitted vectors of environmental variables and influential taxa in the Redundancy Analysis (RDA) of invertebrate communities."

# Fitted species and environmental Vector outputs (For Supplemental Table)
table_RDA_i <- vectors_i %>% 
  arrange(v_type, vector) %>%
  select(vector, axis1, axis2, r2, p) %>%
  mutate(axis1=tormat(axis1, 2),
         axis2=tormat(axis2, 2),
         r2 = tormat(r2, 2),
         p = tormat(p, 2) %>% starmat )

write_csv(table_RDA_i, 'Rfig/table_RDA_i.csv')

#----------------------------------------------------------------------------
# End fig_RDA_invertebrate
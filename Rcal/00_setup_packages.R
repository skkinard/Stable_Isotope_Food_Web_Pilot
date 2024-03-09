# 00_setup_packages
# Sean Kinard
# Last Edit: 2023-05-21
#------------------------------------------------------------------------------
# Description
#------------------------------------------------------------------------------
# The script loads various packages for data analysis and visualization, defines objects including color schemes and taxonomic labels, and contains several functions for data manipulation, including fixing site codes, reordering site factors, rounding numbers, adding significance asterisks to numbers, detecting non-overlapping confidence intervals, adding taxonomic information, and estimating bootstrapped statistics.

#------------------------------------------------------------------------------
# Set Directory and Load Packages
#------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggsignif)
library(patchwork)
library(knitr)
library(iNEXT)
library("ggpmisc")
library(ggrepel)
library(BiodiversityR)
library(car)
library(SIBER)
library(bayestestR)
library(simmr)
library(hdrcde)
library(kableExtra)
library(boot)
library(broom)
library(reporter)
library(ggdark)
library(purrr)

#------------------------------------------------------------------------------
# Objects
#------------------------------------------------------------------------------

# Color sites
my_colors <- c("white", "skyblue", "royalblue1")
my_colors2 <- c('#EF7C12FF', '#F4B95AFF', '#1BB6AFFF', '#8785B2FF')

# Isotope facet labels with special characters
iso_facet_lab <- as_labeller(c(C="delta~C^13", 
                               H="delta~H^2", 
                               N="delta~N^15",
                               Carbon="delta~C^13", 
                               Hydrogen="delta~H^2", 
                               Nitrogen="delta~N^15",
                               carbon="delta~C^13", 
                               hydrogen="delta~H^2", 
                               nitrogen="delta~N^15",
                               "Semi-Arid"="Semi-Arid",
                               "Transition"="Transition",
                               "Sub-Humid"="Sub-Humid"),
                             default = label_parsed)

common_species <- c('P.latipinna', 'L.macrochirus', 'L.gulosus', 'L.cyanellus', 
                    'Corbiculidae', 'Coenagrionidae')

permil2 <- "‰" %p% supsc("2")

#------------------------------------------------------------------------------
# Functions
#------------------------------------------------------------------------------

# Convert site codes to 3-site-pilot names
fix_site_code <- function(x) {
  x %>%
    mutate(site_code = case_when(site_code == 'SF' ~ 'Semi-Arid',
                                 site_code == 'AR' ~ 'Transition',
                                 site_code == 'GC' ~ 'Sub-Humid')) %>%
    mutate(site_code = 
             fct_relevel(site_code, c('Semi-Arid', 
                                      'Transition', 
                                      'Sub-Humid'))) }

# reorder site factor according to rainfall
fix_site_order <- function(x) { 
  x %>% mutate(
    site=fct_relevel(site, c('Semi-Arid', 'Transition', 'Sub-Humid'))) }

# round # and format standard length chr.
tormat <- function(xvar, digits) { 
  format(round(xvar, digits), nsmall=digits) }

# add signif asteriks, return standard length chr.
starmat <- function(xvar) { 
  xnum <- as.numeric(xvar)
  signif = case_when(xnum < 0.01 ~ "**",
                     xnum >= .01 & xvar < 0.05 ~ "* ",
                     xnum >= 0.05 ~ "  ")
  output=paste(xvar, signif, sep="")
  
  return(output) }


# detect non-overlapping confidence intervals 
# (must contain 'mu', 'lower', 'upper', 'site')
detect_signif <- function(my_data) {
  
  table_LCI <- my_data %>%
    select(-mu, -upper) %>%
    mutate(site = case_when(site=='Semi-Arid' ~ 'A',
                            site=='Transition' ~ 'T',
                            site=='Sub-Humid' ~ 'H')) %>%
    pivot_wider(names_from=site, values_from = lower, names_prefix='L_') 
  
  table_UCI <- my_data %>%
    select(-mu, -lower) %>%
    mutate(site = case_when(site=='Semi-Arid' ~ 'A',
                            site=='Transition' ~ 'T',
                            site=='Sub-Humid' ~ 'H')) %>%
    pivot_wider(names_from=site, values_from = upper, names_prefix='U_')
  
  table_signif <- my_data %>%
    left_join(table_LCI) %>%
    left_join(table_UCI) %>%
    mutate(
      s.abrv = case_when(site=='Semi-Arid' ~ 'A',
                         site=='Transition' ~ 'T',
                         site=='Sub-Humid' ~ 'H'),
      signif = case_when(
        upper < L_A & upper < L_H ~ '∗∗',
        upper < L_A & upper < L_T ~ '∗∗',
        upper < L_T & upper < L_H ~ '∗∗',
        lower > U_A & upper < L_H ~ '∗∗',
        lower > U_H & upper < L_A ~ '∗∗',
        upper < L_A ~ '∗',
        upper < L_H ~ '∗',
        upper < L_T ~ '∗',
        lower > U_A & lower > U_H ~ '∗∗',
        lower > U_A & lower > U_T ~ '∗∗',
        lower > U_T & lower > U_H ~ '∗∗',
        lower > U_A ~ '∗', 
        lower > U_T ~ '∗', 
        lower > U_H ~ '∗') ) %>%
    select(-contains('L_'), -contains('U_'), -s.abrv)
  
  return(table_signif) }

detect_signif2 <- function(my_data) {
  
  table_LCI <- my_data %>%
    select(-mu, -upper) %>%
    mutate(treatment = case_when(treatment=='Open' ~ 'C',
                                 treatment=='Exclosure' ~ 'T')) %>%
    pivot_wider(names_from=treatment, values_from = lower, names_prefix='L_') 
  
  table_UCI <- my_data %>%
    select(-mu, -lower) %>%
    mutate(treatment = case_when(treatment=='Open' ~ 'C',
                                 treatment=='Exclosure' ~ 'T')) %>%
    pivot_wider(names_from=treatment, values_from = upper, names_prefix='U_')
  
  table_signif <- my_data %>%
    left_join(table_LCI) %>%
    left_join(table_UCI) %>%
    mutate(
      t.abrv = case_when(treatment=='Open' ~ 'C',
                         treatment=='Exclosure' ~ 'T'),
      signif = case_when(
        upper < L_C ~ '∗',
        upper < L_T ~ '∗',
        lower > U_C ~ '∗', 
        lower > U_T ~ '∗') ) %>%
    select(-contains('L_'), -contains('U_'), -t.abrv)
  
  return(table_signif) }


add_taxonomic <- function(my_data) {
  my_taxonomic <- c('order', 'family', 'genus', 
                    'species', 'lowest_taxon')
  d_spp <- read_csv('Data/my_fish_species.csv') %>%
    mutate(species = str_replace_all(lowest_taxon, ' ', ''),
           species = str_replace_all(species, 
                                     'cyanoguttatum', 'cyanoguttatus')) %>%
    select(any_of(my_taxonomic)) %>%
    unique()
  my_data <- my_data %>% left_join(d_spp)
  return(my_data) }

boot_range <- function(x, resample) {
  max(x[resample]) - min(x[resample]) }

estimate_range_mu <- function(my_x) {
  my_output <- boot(my_x, boot_range, R=2000) %>%
    tidy() %>%
    pull(statistic)
  return(my_output) }

estimate_range_lower <- function(my_x) {
  my_ci <- boot(my_x, boot_range, R=2000) %>%
    boot.ci(type="norm")
  my_output <- my_ci[[4]][2]
  return(my_output) }

estimate_range_upper <- function(my_x) {
  my_ci <- boot(my_x, boot_range, R=2000) %>%
    boot.ci(type="norm")
  my_output <- my_ci[[4]][3]
  return(my_output) }

boot_mu <- function(x, resample) {
  mean(x[resample]) }

estimate_mu <- function(my_x) {
  my_output <- boot(my_x, boot_mu, R=2000) %>%
    tidy() %>%
    pull(statistic)
  return(my_output) }

estimate_mu_lower <- function(my_x) {
  my_ci <- boot(my_x, boot_mu, R=2000) %>%
    boot.ci(type="norm")
  my_output <- my_ci[[4]][2]
  return(my_output) }

estimate_mu_upper <- function(my_x) {
  my_ci <- boot(my_x, boot_mu, R=2000) %>%
    boot.ci(type="norm")
  my_output <- my_ci[[4]][3]
  return(my_output) }

#------------------------------------------------------------------------------
# End 00_setup_packages
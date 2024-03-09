# 00_tidy_isotope
# written by: sean Kinard
# last edit: 2023-05-19
#----------------------------------------------------------------------------
# Description
#----------------------------------------------------------------------------
# The script loads and tidies isotope data, merging datasets, handling missing values, calculating summary statistics, identifying and removing outliers, and exporting the cleaned dataset for further analysis.

#----------------------------------------------------------------------------
# Setup
#----------------------------------------------------------------------------
source('Rcal/00_setup_packages.R')

#load data
data_CN <- read_csv("Rdat/2019_summer_SIA_CN.csv")
data_HO <- read_csv("Rdat/2019_summer_SIA_HO.csv")

#----------------------------------------------------------------------------
# tidy data: merge CN and OH sheets, create group columns without an NAs

# note: CNH values still contain NAs where a sample UID was not run for both analyses

my_data <- merge(data_CN,data_HO, by = "UID", all=TRUE) %>%
  mutate(site = ifelse(is.na(site.x) == TRUE, 
                       site.y, 
                       site.x)) %>%
  mutate(size_max = ifelse(is.na(size_max.x) == TRUE, 
                           size_max.y, 
                           size_max.x)) %>%
  mutate(category = ifelse(is.na(category.x) == TRUE, 
                           category.y, 
                           category.x)) %>%
  mutate(Rainfall = ifelse(site == 'SFC', 50,
                           ifelse(site =='AR', 70,
                                  ifelse(site == 'GC', 85, NA)))) %>%
  mutate(trophic_level_fbase.y = case_when(
    is.na(trophic_level_fbase) == FALSE ~ trophic_level_fbase,
    species.y %in% c('aquatic vegetation', 
                     'Bryophite', 
                     'Filamentous Algae',
                     'FPOM', 
                     'grass', 
                     'leaf', 
                     'periphyton') ~ 1,
    species.y %in% c('Annelidae', 
                     'Cambaridae', 
                     'Chironomidae', 
                     'Corbiculidae', 
                     'Corduliidae', 
                     'Dogielinotidae', 
                     'Elmidae', 
                     'Hydropsychidae', 
                     'Leptophlebiidae', 
                     'P.latipinna', 
                     'Palaemonidae', 
                     'Thiaridae', 
                     'Veliidae') ~ 2,
    species.y %in% c('C.lutrensis', 
                     'H.cyanoguttatus') ~ 2.9,
    species.y %in% c('Aeshnidae',
                     'Belastomatidae',
                     'Coenagrionidae',
                     'Corydalidae',
                     'Dystiscidae',
                     'Gomphidae',
                     'Gyrinidae',
                     'Nepidae') ~ 3 ,
    species.y %in% c('E.gracile',
                     'L.auritus',
                     'L.macrochirus') ~ 3.2,
    species.y == 'A.natalis' ~ 3.33,
    species.y == 'L.gulosus' ~ 3.4,
    species.y %in% c('L.cyanellus', 'L.microlophus') ~ 3.5,
    species.y == 'L.megalotis' ~ 3.6,
    species.y %in% c('M.salmoides', 'P.olivaris') ~ 3.8,
    species.y == 'L.oculatus' ~ 4,
    species.y == 'I.punctatus' ~ 4.2 ) )  %>%
  mutate(TROPHIC_LEVEL = 
           ifelse(is.na(trophic_level_fbase) == TRUE, 
                  trophic_level_fbase.y, 
                  trophic_level_fbase)) %>%
  mutate(trophic_category.y = case_when(
    species.y %in% c('aquatic vegetation',
                     'Bryophite',
                     'Filamentous Algae',
                     'periphyton') ~ "aquatic_primary_producer",
    species.y %in% c('C.lutrensis',
                     'H.cyanoguttatus',
                     'P.latipinna') ~ "fish_herbivore",
    species.y %in% c('A.natalis',
                     'E.gracile',
                     'L.auritus',
                     'L.gulosus',
                     'L.macrochirus') ~ 'fish_invertivore',
    species.y %in% c('I.punctatus',
                     'L.cyanellus',
                     'L.microlophus',
                     'L.oculatus',
                     'M.salmoides',
                     'P.olivaris') ~ 'fish_piscivore',
    species.y %in% c('Annelidae',
                     'Cambaridae',
                     'Chironomidae',
                     'Corduliidae',
                     'Dogielinotidae',
                     'Elmidae',
                     'Hydropsychidae',
                     'Leptophlebiidae',
                     'Palaemonidae',
                     'Thiaridae') ~ 'invertebrate_primary_consumers',
    species.y %in% c('Aeshnidae',
                     'Belastomatidae',
                     'Coenagrionidae',
                     'Corydalidae',
                     'Dystiscidae',
                     'Gomphidae',
                     'Gyrinidae',
                     'Nepidae',
                     'Veliidae') ~ 'invertebrate_predator' ,
    species.y %in% c( 'CPOM',
                      'FPOM') ~ 'organic_material' ,
    species.y %in% c('grass', 'leaf') ~ 'terrestrial_primary_producer') ) %>%
  mutate(Trophic_Category = 
           ifelse(is.na(trophic_category) == TRUE, 
                  trophic_category.y, 
                  trophic_category)) %>%
  mutate(species = ifelse(is.na(species.x) == TRUE, species.y, species.x)) %>%
  select(C13_raw, N15_raw, H2, site, Trophic_Category, species, TROPHIC_LEVEL, size_max, category) %>%
  rename(carbon = C13_raw, nitrogen = N15_raw, hydrogen = H2, trophic_category = Trophic_Category, guild = category, trophic_level_fbase = TROPHIC_LEVEL)

#Rename trophic categories
my_data$trophic_category <- 
  str_replace_all(my_data$trophic_category, 
                  pattern = "invertebrate_", 
                  replacement = "I-")
my_data$trophic_category <- 
  str_replace_all(my_data$trophic_category, 
                  pattern = "fish_", 
                  replacement = "F-")
my_data$trophic_category <-   
  str_replace_all(my_data$trophic_category, 
                  pattern = "aquatic_", 
                  replacement = "A-")
my_data$trophic_category <-   
  str_replace_all(my_data$trophic_category, 
                  pattern = "terrestrial_", 
                  replacement = "T-")
my_data$trophic_category <-   
  str_replace_all(my_data$trophic_category, 
                  pattern = "primary_producer", 
                  replacement = "1")
my_data$trophic_category <-   
  str_replace_all(my_data$trophic_category, 
                  pattern = "grazer", 
                  replacement = "2")
my_data$trophic_category <-   
  str_replace_all(my_data$trophic_category, 
                  pattern = "herbivore", 
                  replacement = "2")
my_data$trophic_category <-   
  str_replace_all(my_data$trophic_category, 
                  pattern = "predator", 
                  replacement = "3")
my_data$trophic_category <-   
  str_replace_all(my_data$trophic_category, 
                  pattern = "invertivore", 
                  replacement = "3")
my_data$trophic_category <-   
  str_replace_all(my_data$trophic_category, 
                  pattern = "piscivore", 
                  replacement = "4") 

# size_max has NA for missing and non-fish categorical rows

# Identify rows containing stable isotope values = NA
filter(my_data, is.na(carbon) == TRUE) # 17 carbon 
filter(my_data, is.na(nitrogen) == TRUE) # 17 nitrogen
filter(my_data, is.na(hydrogen) == TRUE) # 22 hydrogen missing

#----------------------------------------------------------------------------
# create summary statistics for C N H
si_summary <- my_data %>%
  group_by(site, species) %>%
  summarize(s.hydrogen_mean = mean(hydrogen, na.rm=TRUE),
            s.hydrogen_n = length(hydrogen),
            s.hydrogen_sd = sd(hydrogen, na.rm=TRUE),
            s.hydrogen_se = sd(hydrogen, na.rm=TRUE) / sqrt(sum(!is.na(hydrogen))),
            s.carbon_mean = mean(carbon, na.rm=TRUE),
            s.carbon_n = length(carbon),
            s.carbon_sd = sd(carbon, na.rm=TRUE),
            s.carbon_se = sd(carbon, na.rm=TRUE) / sqrt(sum(!is.na(carbon))),
            s.nitrogen_mean = mean(nitrogen, na.rm=TRUE),
            s.nitrogen_n = length(nitrogen),
            s.nitrogen_sd = sd(nitrogen, na.rm=TRUE),
            s.nitrogen_se = sd(nitrogen, na.rm=TRUE) / sqrt(sum(!is.na(nitrogen))),
  )
# add summary data to df (joined by site, species)
my_data <- left_join(my_data, si_summary)

# fill missing CNH values with mean CNH values from group = site:species
my_data <- mutate(my_data,
                  carbon_fill = ifelse(is.na(carbon) == FALSE,
                                       carbon,
                                       s.carbon_mean),
                  nitrogen_fill = ifelse(is.na(nitrogen) == FALSE,
                                         nitrogen,
                                         s.nitrogen_mean),
                  hydrogen_fill = ifelse(is.na(hydrogen) == FALSE,
                                         hydrogen,
                                         s.hydrogen_mean)
)
colnames(my_data)
sum(is.na(my_data$carbon_fill) == TRUE)   # Zero NA in carbon_fill           
sum(is.na(my_data$nitrogen_fill) == TRUE) # Zero NA in nitrogen_fill
sum(is.na(my_data$hydrogen_fill) == TRUE) # Two NA remain in Hydrogen because no average could be obtained for the site:species

# remove 2 instances where no hydrogen data was obtained or could be substituted for missing hydrogen values
my_data <- my_data %>%
  filter(is.na(my_data$hydrogen_fill) == FALSE) %>%
  select(-carbon, -nitrogen, -hydrogen) %>%
  rename(carbon = carbon_fill, nitrogen = nitrogen_fill, hydrogen = hydrogen_fill)


# add columns with stable isotope summary stat values for each trophic category
my_data <- my_data %>%
  group_by(site, trophic_category) %>%
  mutate(t.hydrogen_mean = mean(hydrogen, na.rm=TRUE),
         t.hydrogen_n = length(hydrogen),
         t.hydrogen_sd = sd(hydrogen, na.rm=TRUE),
         t.hydrogen_se = sd(hydrogen, na.rm=TRUE) / sqrt(sum(!is.na(hydrogen))),
         t.carbon_mean = mean(carbon, na.rm=TRUE),
         t.carbon_n = length(carbon),
         t.carbon_sd = sd(carbon, na.rm=TRUE),
         t.carbon_se = sd(carbon, na.rm=TRUE) / sqrt(sum(!is.na(carbon))),
         t.nitrogen_mean = mean(nitrogen, na.rm=TRUE),
         t.nitrogen_n = length(nitrogen),
         t.nitrogen_sd = sd(nitrogen, na.rm=TRUE),
         t.nitrogen_se = sd(nitrogen, na.rm=TRUE) / sqrt(sum(!is.na(nitrogen))))


# outlier analysis
outlier <- my_data %>%
  filter(guild %in% c('fish', 'invertebrate')) %>%
  group_by(site, guild) %>%
  dplyr::summarize(H_mu = mean(hydrogen),
                   H_sd = sd(hydrogen),
                   N_mu = mean(nitrogen),
                   N_sd = mean(nitrogen),
                   C_mu = mean(carbon),
                   C_sd = sd(carbon)) %>%
  right_join(select(my_data, site, guild, species, hydrogen, nitrogen, carbon)) %>%
  mutate(H_outlier = ifelse(hydrogen > H_mu + 4*H_sd | 
                              hydrogen < H_mu - 4*H_sd,
                            'Y','N'),
         N_outlier = ifelse(nitrogen > N_mu + 2*N_sd | 
                              nitrogen < N_mu - 2*N_sd,
                            'Y','N'),
         C_outlier = ifelse(carbon > C_mu + 4*C_sd |
                              carbon < C_mu - 4*C_sd,
                            'Y','N'),
         
  ) %>%
  filter(H_outlier == 'Y' | C_outlier == 'Y' | N_outlier == 'Y')

# one C.lutrensis sample was more than 4 standard deviations from the community mean, leaving 3 C.lutrensis samples that still work

# remove outlier
my_data <- outlier %>% 
  select(site, guild, species, hydrogen) %>%
  mutate(outlier='Y') %>%
  right_join(my_data) %>%
  filter(is.na(outlier)) %>%
  select(-outlier)

# Analytical N error in AR fish sample resulting in extreme low d15N
my_data %>%
  filter(site == 'AR') %>%
  filter(guild=='fish') %>%
  ggplot(aes(nitrogen)) +
  geom_density()

outlier2 <- my_data %>%
  filter(site == 'AR') %>%
  filter(guild=='fish') %>%
  filter(nitrogen<12)

# remove L.cyanellus sample from AR with extreme low d15N and an extremely low length record
my_data <- outlier2 %>% 
  select(site, guild, species, nitrogen) %>%
  mutate(outlier='Y') %>%
  right_join(my_data) %>%
  filter(is.na(outlier)) %>%
  select(-outlier)

# Remove Cambaridae (Not evenly sampled by error, and distorts N range)
my_data <- my_data %>% filter(species != 'Cambaridae')

#----------------------------------------------------------------------------
# summary: 
# merged CN and H data frames
# removed NA from categorical variables
# created summary statistics of CNH values for site:species groups
# replaced missing CN or H values with mean values from site:species groups
# removed 2 rows missing CNH values.

#----------------------------------------------------------------------------
# Export
#my_data is now suitable for mixing model software
write_csv(my_data, 'Rdat/tidy_isotope.csv')

# End 00_tidy_isotope


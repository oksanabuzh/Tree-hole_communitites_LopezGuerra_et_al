# Script: Data Preparation for Tree Hole Community Analysis

# Purpose: Import, clean, and combine tree hole community data from 2023-2024
#          sampling campaigns for biodiversity analysis

# Packages ---------------------------------------------------------------------

library(tidyverse)
library(conflicted)
# Prefer dplyr's select whenever there is a conflict
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")


# 2023 DATA --------------------------------------------------------------------

# Import 2023 data and calculate species abundance per tree hole
community2023 <- read_csv("data/raw_data/sampling_2023_2024/Species_and_trees_from_the_sampling_SEW2023.csv") %>% 
  pivot_longer(Clogmia_sp:Phaonia_subventa, 
               names_to = "Sp_ID", 
               values_to = "Abundance",
               values_drop_na = TRUE)


str(community2023)


# Check for duplicates (should be 0 rows)
community2023 %>% 
  group_by(Plot, Tree_ID, Treehole_number, Type_of_tree, 
           Tree_hole_type, Sampling_date, Sp_ID) %>%
  count() %>% 
  filter(n > 1)

# 2024 DATA --------------------------------------------------------------------

# Import 2024 data and sum abundance
community2024 <- read_csv("data/raw_data/sampling_2023_2024/Sampling_2024_indiviudlas.csv") %>% 
  select(-Nickname_2014, -Nickname_2015, -"ID for the sampling", 
         -"Label", -"Notes", -"Pictures") %>% 
  mutate(Abundance = ifelse(is.na(Abundance), 1, Abundance)) %>%
  summarise(Abundance = sum(Abundance), 
            .by = c("Plot", "Tree_ID", "Treehole_number", "Type_of_tree", 
                    "Outside", "Tree_hole_type", "Sampling_date", "Sp_ID")) 
community2024 %>% 
  filter(is.na(Abundance)) 

# Check for duplicates (should be 0 rows)
community2024 %>% 
  group_by(Plot, Tree_ID, Treehole_number, Type_of_tree, 
           Tree_hole_type, Sampling_date, Sp_ID) %>%
  count() %>% 
  filter(n > 1)

# COMBINE DATA -----------------------------------------------------------------

# Merge 2023 and 2024, standardize dates, extract month
Community_2023_2024 <- bind_rows(
  "2023" = community2023, 
  "2024" = community2024, 
  .id = "Year"
) %>% 
  relocate(Year, .after = Treehole_number) %>% 
  filter(!is.na(Sp_ID)) %>%  # Remove rows with missing species ID as those were pupae unidentified
# Standardize date format to YYYY-MM-DD
  mutate(Sampling_date = case_when(
    Sampling_date == "06/11/2023" ~ "2023-11-06",
    Sampling_date == "07/11/2023" ~ "2023-11-07",
    Sampling_date == "13/06/2024" ~ "2024-06-13",
    Sampling_date %in% c("16/05/2024", "16/5/2024") ~ "2024-05-16",
    Sampling_date %in% c("16/06/2024", "16/6/2024") ~ "2024-06-16",
    Sampling_date == "17/05/2024" ~ "2024-05-17",
    Sampling_date %in% c("17/6/2024", "17/06/2024") ~ "2024-06-17",
    Sampling_date == "22/05/2024" ~ "2024-05-22",
    Sampling_date == "22/07/2024" ~ "2024-07-22",
    Sampling_date == "27/05/2024" ~ "2024-05-27",
    Sampling_date == "21/5/2024" ~ "2024-05-21",
    Sampling_date == "23/07/2024" ~ "2024-07-23",
    Sampling_date == "19/06/2024" ~ "2024-06-19")) %>% 
# Extract month name for seasonal analysis
  mutate(Month = case_when(
    Sampling_date == "2023-11-06" ~ "November",
    Sampling_date == "2023-11-07" ~ "November",
    Sampling_date == "2024-06-13" ~ "June",
    Sampling_date == "2024-05-16" ~ "May",
    Sampling_date == "2024-06-16" ~ "June",
    Sampling_date == "2024-05-17" ~ "May",
    Sampling_date == "2024-06-17" ~ "June",
    Sampling_date == "2024-05-22" ~ "May",
    Sampling_date == "2024-07-22" ~ "July",
    Sampling_date == "2024-05-27" ~ "May",
    Sampling_date == "2024-05-21" ~ "May",
    Sampling_date == "2024-06-19" ~ "June",
    Sampling_date == "2024-07-23" ~ "July"), 
    .after = Year) %>% 
  mutate(Tree_hole_type_coarse = case_when(
    Tree_hole_type %in% c("Branch", "Cut tree", "Trunk", "Root") ~ "rot",
    Tree_hole_type %in% c("Division") ~ "pan",
    .default = Tree_hole_type), .after = Tree_hole_type)%>% 
  mutate(Tree_hole_opening = case_when(
    Tree_hole_type %in% c("Trunk", "Root") ~ "side_opening",
    Tree_hole_type %in% c("Division","Branch", "Cut tree") ~ "top_opening",
    .default = Tree_hole_type), .after = Tree_hole_type_coarse) %>% 
  relocate(Sampling_date, .after = Month) %>% 
# Correct Tree_ID based on known issues from missing_tree_species.csv
  left_join(read_csv("data/processed_data/missing_tree_species.csv") %>% 
              group_by(Plot, Tree_ID, Reason_missing, Tree_ID_Bexis, Tree_ID_Bexis_devision_2) %>% 
              summarise(count = n(), .groups = "drop") %>% 
              select(-count),
            by = c("Plot", "Tree_ID")) %>%
  mutate(Tree_ID=case_when(Reason_missing=="Misspelling" ~ Tree_ID_Bexis,
                           Reason_missing=="Idenfitication/GPS_problem" ~ Tree_ID_Bexis,
                           Reason_missing=="Division" ~ Tree_ID_Bexis, # tree a in tree division is always larger than tree b
                           .default=Tree_ID)) %>% 
  select(-Reason_missing, -Tree_ID_Bexis, -Tree_ID_Bexis_devision_2) %>% 
  mutate(Sp_ID=ifelse(Sp_ID=="Syprh", "Syrph", Sp_ID)) # mistake  
  
Community_2023_2024 %>% 
  filter(str_detect(Sp_ID, "Syrph")) 

# EXPLORATORY CHECKS -----------------------------------------------------------

names(Community_2023_2024)

Community_2023_2024 %>% 
  group_by(Sp_ID) %>%
  count() 


Community_2023_2024 %>% 
  group_by(Tree_hole_type) %>%
  count()


Community_2023_2024 %>% print(n = Inf)

# Check for missing species IDs
Community_2023_2024 %>% filter(is.na(Sp_ID))

# Sampling summary by year and date
Community_2023_2024 %>% 
  group_by(Year, Sampling_date) %>%
  count() 

Community_2023_2024 %>% 
  select(Plot, Tree_ID, Treehole_number, Year, Sampling_date) %>%
  filter(is.na(Sampling_date))


Community_2023_2024 %>% filter(is.na(Sampling_date))




# COMMUNITY DATA (DNA corrected) ---------------------------------------------------------------

names(Community_2023_2024)

Community_2023_2024 %>% 
  #select(Plot, Tree_ID, Treehole_number, Tree_hole_type, Tree_hole_opening,
  select(Plot, Plot, Tree_ID, Treehole_number, Year, Month, Sampling_date, 
         Tree_hole_type, Tree_hole_type_coarse, Tree_hole_opening, Outside,
         Year, Month, Sp_ID, Abundance) %>% 
  write_csv("data/processed_data/Community_2023_2024.csv")



# Correct species given the DNA data:
# in new data ....DNA corrected:
# new columns:
# Sp_ID_DNAcorrected: corrected species ID based on DNA data
# species - full species name
# Tree_ID_Bexis - Tree ID in  database submitted to Bexis to match with our Tree_ID
# Treehole_ID_Bexis - Treehole ID in database submitted to Bexis to match with our Treehole_number
#
# DNA data:
DNA_dat <- read_csv("data/raw_data/Community_2023_2024_Sp_ID_DNAcorrected.csv") %>% 
  select(-Abundance) %>% 
  mutate(Sampling_date=lubridate::dmy(Sampling_date)) %>% 
#  mutate(# Sp_ID_DNAcorrected=ifelse(Sp_ID_DNAcorrected=="Syrph2", "Syrph", Sp_ID_DNAcorrected), 
#         Species=ifelse(Species=="Syrphidae sp.2", "Syrphidae sp.", Species)) %>% 
  mutate(Species=ifelse(Sp_ID_DNAcorrected=="Ceratopogonidae", "Ceratopogonidae sp.", Species)) %>% 
  mutate(Species=ifelse(Species=="Poecilobothrus nobilitatus20", "Poecilobothrus nobilitatus", Species)) 

DNA_dat %>% 
  filter(Species=="Syrphidae sp.")

DNA_dat %>% 
  filter(str_detect(Sp_ID_DNAcorrected, "Syrph")) 
# Syrph and Syrph2 (Sp_ID=="Fagi") were identifyed as morphologically different species. No DNA analysis was performed on them.
# Syrph has body size directly identified 


# Merge with community data:

Community_final <- read_csv("data/processed_data/Community_2023_2024.csv") %>% 
  left_join(DNA_dat, by = c("Plot", "Tree_ID", "Treehole_number", "Year", "Month", 
                            "Sampling_date", "Tree_hole_type", "Tree_hole_type_coarse",
                            "Tree_hole_opening", "Outside", "Sp_ID")) %>% 
  #  left_join(traits_final %>% 
  #              select(Sp_ID, dry_weight_mg), by = c("Sp_ID")) %>% 
  group_by(Plot, Tree_ID, Treehole_number, Year, Month, Sampling_date, 
           Tree_hole_type, Tree_hole_type_coarse, Tree_hole_opening, Outside, 
           Sp_ID_DNAcorrected, Species) %>% 
  summarise(Abundance = sum(Abundance), .groups = "drop")  

## Write data -----------------------------------------------------------------------
Community_final %>% 
  write_csv("data/processed_data/Community_2023_2024_DNAcorrected.csv")


# TRAITS DATA --------------------------------------------------------------------

# traits for all species (not used in this study)
# traits <- read_csv("data/raw_data/traits/summary_traits.csv")%>% 
#  rename(Sp_ID=Species_ID) %>% 
#  rename(Full_name = `Full name`) %>% 
#  mutate(Sp_ID = case_when(
#    Sp_ID == "Anopholes_plumbeus" ~"Anopheles_plumbeus",
#    .default = Sp_ID)) 

# taxonomy includes taxonomic information for Sp_ID (but not for the DNA corrected)
# taxonomy <- traits %>% 
#  group_by( Order, Family , Full_name,  Sp_ID) %>%
#  count() %>%  ungroup() %>% 
#  drop_na(Sp_ID) %>%
#  select(-n) %>% 
#  mutate(Genus=word(Full_name, 1), .before=Full_name) 

# Traits for this study
traits_2023_2024 <- read_csv("data/raw_data/traits/Traits_Community_2023_2024.csv") %>% 
  mutate(predator=ifelse(is.na(predator), 0, predator),
         decomposer=ifelse(is.na(decomposer), 0, decomposer)) %>% 
  rename(Order_DNA_corrected = "Order",
         Sp_ID_DNAcorrected="Sp_ID_correctedDNA") %>% 
  mutate(Genus_DNA_corrected = ifelse(Genus_DNA_corrected=="Dasyhelea" & Sp_ID_DNAcorrected=="Ceratopogonidae", 
                                    "Ceratopogonidae", Genus_DNA_corrected)) %>% 
  mutate(Genus_DNA_corrected = ifelse(Genus_DNA_corrected=="Syrphidae sp.2", "Syrphidae", Genus_DNA_corrected)) 
  


traits_2023_2024 %>% 
  group_by(Sp_ID_DNAcorrected) %>% 
  count() %>% 
  print(n=Inf)

traits_2023_2024 %>% 
  group_by(Sp_ID_DNAcorrected, Genus_DNA_corrected, Family_DNA_corrected, Order_DNA_corrected,
           predator, decomposer, Treehole_specialist) %>% 
  count() %>% 
  print(n=Inf)

traits_2023_2024 %>% 
  group_by(Sp_ID, Sp_ID_DNAcorrected) %>% 
  count() %>%
  print(n=Inf)

DNA_ID_unique <- DNA_dat %>% 
  group_by(Sp_ID, Sp_ID_DNAcorrected, Species) %>%
  count() %>% 
  print(n=Inf)

## Check if Sp_ID match in community data and in trait data----------------------------
DNA_ID_unique %>% 
  left_join(traits_2023_2024 %>% 
              group_by(Sp_ID, Sp_ID_DNAcorrected, predator) %>%  count() %>% select(-n), 
            by = c("Sp_ID")) %>% 
  print(n=Inf)



## Join with body size measurements ----------------------------------------------

# check if there are repetitions in Sp_ID 
traits_2023_2024 %>% 
  count(Sp_ID) %>% 
  arrange(desc(n)) %>% 
  print(n=Inf)

# All body size data (own measurements) 
body_size_all <- read_csv("data/raw_data/traits/body_measurements_merged_final.csv") %>%
  rename(Sp_ID=Species_ID) %>% 
  rename(dry_weight_mg = "dry_weight _mg") %>% 
  dplyr::select(Sp_ID, wet_weight_mg, length_mm, dry_weight_mg)

# Summarised body size data by Sp_ID
BodySize_mean_Sp_ID <- body_size_all %>% 
  summarise(Sp_ID_wet_weight = mean(wet_weight_mg, na.rm = TRUE), 
            Sp_ID_length = mean(length_mm, na.rm = TRUE),
            Sp_ID_dry_weight = mean(dry_weight_mg, na.rm = TRUE),
             Sp_ID_Indiv_n = n(),
             Body_size_level = "Sp_ID",
            .by = c("Sp_ID")) %>% 
  left_join(traits_2023_2024 %>% 
              dplyr::select(Sp_ID, Sp_ID_DNAcorrected), by = c("Sp_ID")) %>% 
  relocate(Sp_ID_DNAcorrected, .after = Sp_ID) %>% 
  print(n=Inf)
   
# Summarised body size data by Sp_ID corrected with DNA data (for some species we have more than one Sp_ID but only one Sp_ID_DNAcorrected, so we will average the body size for those species)
BodySize_mean_SpID_DNA_Corr <-  body_size_all %>% 
  left_join(traits_2023_2024, by = c("Sp_ID"))%>% 
  relocate(Sp_ID_DNAcorrected, .after = Sp_ID) %>%
  summarise(wet_weight_mg = mean(wet_weight_mg, na.rm = TRUE), 
            length_mm = mean(length_mm, na.rm = TRUE),
            dry_weight_mg = mean(dry_weight_mg, na.rm = TRUE),
            Indiv_n = n(),
            Body_size_level = "SpID_DNA_Corr",
            .by = c("Sp_ID_DNAcorrected"))  

BodySize_mean_SpID_DNA_Corr %>% 
  print(n=Inf)

# Summarised body size data by Family_DNA_corrected
BodySize_mean_Family_DNA_corrected <- body_size_all %>% 
  left_join(traits_2023_2024 %>% 
              select(Sp_ID, Family_DNA_corrected), 
            by = "Sp_ID") %>% 
  relocate(Family_DNA_corrected, .after = Sp_ID) %>% 
  summarise(
    Family_DNA_wet_weight_mg = mean(wet_weight_mg, na.rm = TRUE),
    Family_DNA_length_mm = mean(length_mm, na.rm = TRUE),
    Family_DNA_dry_weight_mg = mean(dry_weight_mg, na.rm = TRUE),
    Family_DNA_Indiv_n = n(),
    .by = "Family_DNA_corrected")


# Merge summerised body size data with traits data, first by Sp_ID_DNAcorrected and then by Family_DNA_corrected, and fill NA with family mean if Sp_ID mean is not available, and then fill remaining NA with literature data for missing families (Tipulidae and Tabanidae)
traits_final <- traits_2023_2024 %>% 
  mutate(Genus_DNA_corrected = ifelse(is.na(Genus_DNA_corrected), Family_DNA_corrected, Genus_DNA_corrected)) %>%
  left_join(BodySize_mean_SpID_DNA_Corr, by = c("Sp_ID_DNAcorrected")) %>% 
  relocate(c(Sp_ID_DNAcorrected, Sp_ID, wet_weight_mg, length_mm, dry_weight_mg, Indiv_n, Body_size_level),  
           .before = Order_DNA_corrected) %>% 
 left_join(BodySize_mean_Family_DNA_corrected, by = c("Family_DNA_corrected")) %>% 
  mutate(Body_size_level = ifelse(is.na(dry_weight_mg), "Family_DNA_corr", Body_size_level),
         Indiv_n = ifelse(is.na(dry_weight_mg), Family_DNA_Indiv_n, Indiv_n),
         wet_weight_mg = ifelse(is.na(wet_weight_mg), Family_DNA_wet_weight_mg, wet_weight_mg),
         length_mm = ifelse(is.na(length_mm), Family_DNA_length_mm, length_mm),
         dry_weight_mg = ifelse(is.na(dry_weight_mg), Family_DNA_dry_weight_mg, dry_weight_mg)) %>% 
  # Fill NA with the literature data:
  mutate(dry_weight_mg = ifelse(Family_DNA_corrected=="Tipulidae", 1.41, dry_weight_mg), # Table 3 in https://doi.org/10.3390/ijerph19063240 (Estimate based on the literature allometric relationship)
         length_mm = ifelse(Family_DNA_corrected=="Tipulidae", 10, length_mm), # Table 3 in https://doi.org/10.3390/ijerph19063240 (Estimate based on the literature allometric relationship)
         Indiv_n = ifelse(Family_DNA_corrected=="Tipulidae", 53, Indiv_n), # Table 2  in https://doi.org/10.3390/ijerph19063240
         Body_size_level = ifelse(Family_DNA_corrected=="Tipulidae", "Literature", Body_size_level)) %>%          
  mutate(dry_weight_mg = ifelse(Family_DNA_corrected=="Tabanidae", 0.0001*(10^4.2208), dry_weight_mg), # DM <- a * L^b with a=0.0001, b=4.2208, L=10mm from Poepperl, R., 1998. Biomass determination of aquatic invertebrates in the Northern German lowland using the relationship between body length and dry mass. Faunistisch-Ökologische Mitteilungen, 7, pp.379-386. https://www.zobodat.at/pdf/Faun-Oekol-Mitt_7_0379-0386.pdf
         length_mm = ifelse(Family_DNA_corrected=="Tabanidae", 10, length_mm), # range 5.0-15.0 mm (Tab. 1 in https://www.zobodat.at/pdf/Faun-Oekol-Mitt_7_0379-0386.pdf)
         Indiv_n = ifelse(Family_DNA_corrected=="Tabanidae", 19, Indiv_n), # n=19 individuals (Tab. 1 in https://www.zobodat.at/pdf/Faun-Oekol-Mitt_7_0379-0386.pdf)
         Body_size_level = ifelse(Family_DNA_corrected=="Tabanidae", "Literature", Body_size_level)) %>% 
  select(-Family_DNA_wet_weight_mg, -Family_DNA_length_mm, -Family_DNA_dry_weight_mg, -Family_DNA_Indiv_n) %>% 
  rename(Indiv_number_for_body_size_estimation=Indiv_n,
         Level_of_aggregation_for_body_size_estimation=Body_size_level) %>% 
  left_join(DNA_dat %>% 
              select(Sp_ID_DNAcorrected, Species) %>%
              summarise(Species = unique(Species), .by = c("Sp_ID_DNAcorrected")),
            by = "Sp_ID_DNAcorrected") %>% 
  relocate(Species, .after = Sp_ID_DNAcorrected) 

traits_final%>% 
  print(n=Inf)



write_csv(traits_final, "data/processed_data/Traits_2023_2024_final.csv")

traits_final %>% 
  group_by(Sp_ID_DNAcorrected) %>% 
  count() %>% ungroup() %>% 
  arrange(desc(n)) %>%
  print(n=Inf)

# are traits repetitive for the same Sp_ID_DNAcorrected? 
traits_final %>% 
  group_by(Sp_ID_DNAcorrected, wet_weight_mg,	length_mm,	dry_weight_mg) %>% 
  count() %>% ungroup() %>% 
  arrange(desc(n)) %>%
  print(n=Inf)
# Yes, same as above


# grouped traits by Sp_ID_DNAcorrected
traits_final_DNA_grouped <- traits_final %>%
  summarise(length_mm = unique(length_mm),
            dry_weight_mg = unique(dry_weight_mg),
            Indiv_number_for_body_size_estimation = unique(Indiv_number_for_body_size_estimation),
            Level_of_aggregation_for_body_size_estimation = unique(Level_of_aggregation_for_body_size_estimation),
            predator = unique(predator),
            decomposer = unique(decomposer),
            Treehole_specialist = unique(Treehole_specialist),
            .by = c("Sp_ID_DNAcorrected", "Species", 
                    "Family_DNA_corrected", "Genus_DNA_corrected"))


traits_final_DNA_grouped %>%
#  mutate(length_mm=round(length_mm, 2),
#         dry_weight_mg=round(dry_weight_mg, 3)) %>%
  write_csv("data/processed_data/Traits_2023_2024_final_DNA_corrected.csv")


# ENVIRONMENTAL DATA  --------------------------
Community_final %>% 
  select(-Sp_ID_DNAcorrected, -Species, -Abundance) %>% 
  distinct() %>%
  write_csv("data/processed_data/Environment_2023_2024.csv")

# Check unique tree holes in environmental data
Community_final %>% 
  select(-Sp_ID_DNAcorrected, -Species, -Abundance) %>% 
  group_by(Plot, Tree_ID, Treehole_number, Year, Month, Sampling_date, 
           Tree_hole_type, Tree_hole_type_coarse, Tree_hole_opening, Outside) %>%
  count()


# EXPLORATORIES DATA -----------------------------------------------------------

# Import own environmental data
Environment_2023_2024 <- read_csv("data/processed_data/Environment_2023_2024.csv")


# Tree data --------------------------------------------------------------------
# 31487_7
# single tree data, on all forest EPs, 2020 - 2023 (year is not a group, as different trees measured in different years)
# original data on species, diameter at breast height, cm (d), tree heigth, m (h), 
# wood volume, m3 (v) and geographical location (caliper limit: dbh >= 7 cm).
# tree -tree ID
# species - Tree species (name shortened to 16 characters)

tree_data <- read_csv("data/raw_data/BiodExpl/31487_7_data.csv") %>% 
  filter(Exploratory == "SCH") %>% 
  rename(tree_heigth = h,
         DBH = d,
         wood_volume = v) %>% 
  select(EP, tree, species, DBH, tree_heigth, wood_volume, year)  

tree_data


tree_data %>% 
  filter(EP=="SEW45")

# MERGE COMMUNITY AND TREE DATA 
merged_tree_data <- Environment_2023_2024 %>% 
  left_join(tree_data, by = c("Tree_ID" = "tree")) %>% 
  mutate(tree_species = ifelse(is.na(species), "Fagus_sylvatica", species),
        .after=Tree_ID, 
        .keep = "all") 

names(merged_tree_data)

# Check for missing species in merged data
merged_tree_data %>% 
  pull(tree_species) %>% 
  unique()

merged_tree_data %>% 
  filter(!Outside==TRUE) %>% 
  filter(is.na(species)) %>% 
  print(n=Inf) %>% 
  dplyr::select(Plot, Tree_ID, tree_species, species)

# two Tree_IDs are not having coordinates in our data
# Plot  Tree_ID  tree_species    Treehole_number  Year Month Sampling_date Tree_hole_type Tree_hole_type_coarse
#   1 SEW07 SEW07_T3 Fagus_sylvatica SEW07_T3_1       2024 June  2024-06-17    Trunk          rot                  
#   2 SEW07 SEW07_T5 Fagus_sylvatica SEW07_T5_1       2024 June  

# write_csv(merged_data, "data/processed_data/Community_2023_2024_with_tree_data.csv")

# Tree diversity ------

Tree_composition <- tree_data %>% 
  group_by(EP, species, year) %>% 
  count() %>% 
  ungroup() %>% 
  summarise(Abundance=mean(n),
            .by=c("EP", "species")) %>% 
  pivot_wider(names_from=species, values_from=Abundance, values_fill=0) %>% 
  ungroup() %>%
#  mutate(Total_trees = rowSums(across(-EP)), .after = "EP") %>%
#  mutate(Pinus_sylvestris_perc=Pinus_sylvestris* 100/Total_trees,
#         .after="Pinus_sylvestris") %>%
  # calculate % for each tree species
  mutate(across(-EP, ~ .x * 100/ rowSums(across(-EP)), .names = "perc_{.col}"),
         .keep="unused")


names(Tree_composition)


Tree_diversity <- tree_data %>% 
 group_by(EP, species, year) %>% 
  count() %>% 
  ungroup() %>% 
  summarise(Abundance=mean(n),
            .by=c("EP", "species")) %>%
  summarise(
    # Total number of individuals across all species in each plot
    Tree_abundance = sum(Abundance, na.rm = TRUE),
    
    # Number of unique species (species richness) in each tree hole
    Tree_sp_richness = n_distinct(species),
    Tree_Shannon = vegan::diversity(Abundance, index = "shannon"),
    .by=c("EP"))  %>% 
  left_join(Tree_composition, by = c("EP"))

# Silvicultural Management Intensity (SIM) ------------------------------
# Dynamics on all forest EPs, 2008 - 2020
# SMId (ratio) - Density component of silvicultural management intensity - relative deviance between actual basal area and basal area carrying capacity
# SMIr (value) - Risk component of silvicultural management intensity - probability of stand loss before the age of 180 years
# SMI	(index) -	Silvicultural management intensity - mean of risk and density component
# 31217_9_data.csv

SIM_all <- read_csv("data/raw_data/BiodExpl/31217_9_data.csv") %>% 
  filter(Exploratory == "SCH")

# 2008_2020
SIM_2008_2020 <- SIM_all %>% 
 # mutate(year = substr(year, 7, 10)) %>%
  summarise(across(where(is.numeric), 
                   list(mean_2008_2020 = mean, 
                        sd_2008_2020=sd), na.rm = TRUE),
            .by=c("EP"))
    
# 2018_2020
SIM_2018_2020 <- SIM_all %>% 
  mutate(year = substr(year, 7, 10)) %>%
  filter(year %in% c("2018", "2019", "2020")) %>%
  summarise(across(where(is.numeric), 
                   list(mean_2018_2020 = mean, sd_2018_2020=sd), na.rm = TRUE),
            .by=c("EP"))

SIM2020 <- SIM_all %>% 
  mutate(year = substr(year, 7, 10)) %>%
  filter(year %in% c("2020")) %>%
  summarise(across(where(is.numeric), 
                   list("2020" = mean), na.rm = TRUE),
            .by=c("EP"))

# Why sd in some cases are ==0? check if there are measurements for all year?
SIM_all %>% 
  mutate(year = substr(year, 7, 10)) %>%
  filter(year %in% c("2018", "2019", "2020")) %>%
  filter(EP=="SEW09")
# OR
SIM_all %>% 
  mutate(year = substr(year, 7, 10)) %>%
  filter(year %in% c("2018", "2019", "2020")) %>%
  filter(EP=="SEW01")
# The land use stays the same over 3 years that is why sd =0

# merge both SIM datasets
SIM_data <- SIM_2008_2020 %>% 
  left_join(SIM_2018_2020, by = "EP") %>% 
  left_join(SIM2020, by = "EP")
  

# check for missing data if merged
merged_tree_data %>% 
  left_join(SIM_data, by = c("Plot" = "EP")) %>% 
  filter(!Outside==TRUE) %>% 
  filter(is.na(SMIr_sd_2008_2020)) %>% 
  print(n=Inf)

names(SIM_data)

#  Forest Management Intensity Index (ForMI) ------------------------------
# Dynamics of all forest EPs between 2008 and 2018
# Inonat (fraction) -	Proportion of non-natural tree species
# Iharv	(fraction) - Proportion of harvested tree biomass	
# Idwcut(fraction) - Proportion of dead wood showing signs of saw cuts	
# Formi (indexv) - Index of Forest Management Intensity
# 24646_4_data.csv

ForMI_all <- read_csv("data/raw_data/BiodExpl/24646_4_data.csv") %>% 
  filter(Exploratory == "SCH") %>% 
  mutate(DWi_year = substr(DWi_year, 7, 10)) %>% 
  mutate(DWi_year = ifelse(is.na(DWi_year), "2012", DWi_year))  %>% 
  select(EP, DWi_year, Inonat, Iharv, Idwcut, Formi)

ForMI_all %>% 
  group_by(DWi_year) %>%
    count()

# 2012_2018
ForMI_2012_2018 <- ForMI_all %>% 
  summarise(across(where(is.numeric), 
                   list(mean_2012_2018 = mean, sd_2012_2018=sd), na.rm = TRUE),
            .by=c("EP"))

# 2018 only
ForMI_2018 <- ForMI_all %>%
  filter(DWi_year %in% c("2018")) %>%
  summarise(across(where(is.numeric), 
                   list("2018" = mean), na.rm = TRUE),
            .by=c("EP"))


# merge both ForMI datasets
ForMI_data <- ForMI_2018 %>% 
  left_join(ForMI_2012_2018, by = "EP")

names(ForMI_data)
# check for missing data if merged
merged_tree_data %>% 
  left_join(ForMI_data, by = c("Plot" = "EP")) %>% 
  filter(!Outside==TRUE) %>% 
  filter(is.na(Formi_2018) | is.na(Inonat_mean_2012_2018) ) %>% 
  print(n=Inf)


# Plot Biodiversity Potential -------------------------------------------------
# Index of Biodiversity Potential developed by Larrieu and Gonin (2008)
# 31873_7_data

biodiv_data <- read_csv("data/raw_data/BiodExpl/31873_7_data.csv") %>% 
  # filter ID that consists "SCH"
  filter(str_detect(ID, "SEW"))
biodiv_data

biodiv_data %>% 
  select(Wet_macrohabitats) %>% 
  print(n=Inf)

# check missing data if merged
merged_tree_data %>% 
  left_join(biodiv_data, by = c("Plot" = "EP")) %>% 
  filter(!Outside==TRUE) %>% 
  filter(is.na(Tree_richness)) %>% 
  print(n=Inf)

names(biodiv_data)
# Laser scan data -------------------------------------------------
# ENL: The effective number of layers quantifies vertical stand structure 
# SSCI: The index quantifies stand structural complexity 
# Canopy Openness: The index quantifies canopy openness as percentage of sky pixels of a simulated hemisperical image for an opening angle of 60°. 
# 32085_6_data.csv

Laser_data <- read_csv("data/raw_data/BiodExpl/32085_6_data.csv") %>% 
  filter(exploratory=="SCH" & season=="summer23") %>% 
  # correct plot_id numbers: when plots have 4 characters (SEW1), insert 0 before the last character
  mutate(plot.id = if_else(str_length(plot.id) == 4, 
                           # insert "0" before the 4th character
                           str_replace(plot.id, "^(.{3})(.)$", "\\10\\2"),
                           plot.id))


# check missing data if merged
merged_tree_data %>% 
  left_join(Laser_data, by = c("Plot" = "plot.id")) %>% 
  filter(!Outside==TRUE) %>% 
  filter(is.na(enl) | is.na(ssci) | is.na(canopy.openness)) %>% 
  print(n=Inf)


# Stand structural attributes -------------------------------------------------
# 2014 - 2018 # not grouped by year, as different plots are sampled in different years
# 22766_4_data.csv
Stand_str_data <- read_csv("data/raw_data/BiodExpl/22766_4_data.csv") %>% 
  filter(Exploratory=="SCH") 

names(Stand_str_data)

# check missing data if merged
merged_tree_data %>% 
  left_join(Stand_str_data, by = c("Plot" = "EP")) %>% 
  filter(!Outside==TRUE) %>% 
  filter(is.na(ssm_N )) %>% 
  print(n=Inf)


# Climate Data -------------------------------------------------

climate2024 <- read_csv("data/raw_data/BiodExpl/climate_data_May_June_July_2024.csv") %>% 
  select(plotID, datetime ,
         precipitation_radolan_rain_days,
         precipitation_radolan, precipitation_radolan_acc,
         Ta_10, Ta_10_max, rH_200_DMR,
         Ta_200, Ta_200_heat_index, Ta_200_humidex,
         # binary variables
         "Ta_200_extremely hot days", Ta_200_extremely_cold_days,
         Ta_200_heating_degree_days) %>% 
  rename(
    Ta_200_extremely_hot_days = "Ta_200_extremely hot days") %>%
  mutate(Year = 2024, .after = plotID) %>%
  mutate(
    Month = factor(month.name[as.integer(format(datetime, "%m"))],
                   levels = month.name, ordered = TRUE), 
    .after = Year) %>% 
  summarise(
    across(
      c(precipitation_radolan_rain_days, precipitation_radolan, precipitation_radolan_acc,
        Ta_10, Ta_10_max, rH_200_DMR,
        Ta_200, Ta_200_heat_index, Ta_200_humidex),
      ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"),
    across(
      c(Ta_200_extremely_hot_days, Ta_200_extremely_cold_days,
        Ta_200_heating_degree_days),
      ~ sum(.x, na.rm = TRUE), .names = "{.col}_sum"),

      .by = c("plotID", "Year", "Month")
  )
    
    
climate2024
names(climate2024)

climate2024 %>% 
  filter(is.na(precipitation_radolan_rain_days_mean))

climate2023 <- read_csv("data/raw_data/BiodExpl/climate_data_November_2023.csv") %>% 
  select(plotID, datetime ,
         precipitation_radolan_rain_days,
         precipitation_radolan, precipitation_radolan_acc,
         Ta_10, Ta_10_max, rH_200_DMR,
         Ta_200, Ta_200_heat_index, Ta_200_humidex,
         # binary variables
         "Ta_200_extremely hot days", Ta_200_extremely_cold_days,
         Ta_200_heating_degree_days) %>% 
  rename(
    Ta_200_extremely_hot_days = "Ta_200_extremely hot days") %>%
  mutate(Year = 2023, .after = plotID) %>%
  mutate(
    Month = factor(month.name[as.integer(format(datetime, "%m"))],
                   levels = month.name, ordered = TRUE), 
    .after = Year) %>% 
  summarise(
    across(
      c(precipitation_radolan_rain_days,
        precipitation_radolan, precipitation_radolan_acc,
        Ta_10, Ta_10_max, rH_200_DMR,
        Ta_200, Ta_200_heat_index, Ta_200_humidex),
      ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"),
    across(
      c(Ta_200_extremely_hot_days, Ta_200_extremely_cold_days,
        Ta_200_heating_degree_days),
      ~ sum(.x, na.rm = TRUE), .names = "{.col}_sum"),
    
    .by = c("plotID", "Year", "Month")
  )


climate2023 %>% 
  arrange(plotID, Year, Month)

climate2023 %>% 
  filter(is.na(precipitation_radolan_rain_days_mean))


# Merge climate data for 2023 and 2024
climate_data <- bind_rows(climate2023, climate2024)
climate_data%>% 
  filter(is.na(Ta_10_mean))


# check missing data if merged
merged_tree_data %>% 
  left_join(climate_data, by = c("Plot" = "plotID" , "Year", "Month")) %>% 
  filter(!Outside==TRUE) %>% 
  select(Plot, Year, Month, Ta_200_mean) %>%
  filter(is.na(Ta_200_mean)) %>% 
  print(n=Inf)

# Tree hole mapping data-----------------------------------------------

Tree_mapping <- read_csv("data/raw_data/BiodExpl/BExIS_20966_v2_data.csv") %>% 
  filter(Exploratory == "SEW") %>% 
  # add 0 in plot numbers after "SEW" with 1 digit characters (e.g., SEW1 -> SEW01)
  mutate(Plot = if_else(str_length(Plot) == 4, 
                           str_replace(Plot, "^(.{3})(.)$", "\\10\\2"),
                           Plot)) %>% 
  rename(Total_hole_number_mapping = Total)


Tree_mapping

# check for missing data if merged
merged_tree_data %>% 
  left_join(Tree_mapping, by = c("Plot")) %>% 
#  filter(!Outside==TRUE) %>% 
  filter(is.na(Total_hole_number_mapping)) %>% 
  print(n=Inf)




# MERGE ALL ENVIRONMENTAL DATA ---------------------------------------------------------------

merged_all_envir_data <- merged_tree_data %>% 
  left_join(Tree_mapping, by = c("Plot")) %>% 
  left_join(Tree_diversity, by = c("Plot" = "EP")) %>% 
  left_join(SIM_data, by = c("Plot" = "EP")) %>% 
  left_join(ForMI_data, by = c("Plot" = "EP")) %>% 
  left_join(biodiv_data, by = c("Plot" = "EP")) %>% 
  left_join(Laser_data, by = c("Plot" = "plot.id")) %>% 
  left_join(Stand_str_data, by = c("Plot" = "EP")) %>% 
  left_join(climate_data, by = c("Plot" = "plotID", "Year", "Month")) 



merged_all_envir_data %>% 
  pull(Month) %>%
  unique()

merged_all_envir_data %>% 
  filter(is.na(Total_hole_number_mapping))


merged_all_envir_data %>% 
  select(Plot, Total_hole_number_mapping) %>%
  print(n=Inf)


write_csv(merged_all_envir_data, "data/processed_data/Environment_ALL.csv")


# LANDSCAPE DATA -------------------------------------------------
# class level of land type is the resolution of land cover classification
# class_0: coarse level (e.g., forest, agriculture, urban)
# class_1: intermediate level (e.g., deciduous forest, cropland, residential area)
# class_2: fine level (e.g., beech forest, wheat field, high-density residential area)
# area_km2: area covered by each land cover class within the buffer
# buffer sizes: 250m and 500m around each plot

files_250m <- list.files("data/Raster_measurements/raster_measurement_250m", 
                    pattern = "^SEW.*\\.csv$", full.names = TRUE) %>%
  set_names(basename(.)) %>%  # name the list elements by filename
  map_dfr(~ readr::read_csv(.x, show_col_types = FALSE), .id = "file") %>% 
  # add buffer size
  mutate(buffer_size_m = 250) %>%
  # add a plotID extracted from filename 
  mutate(plot = tools::file_path_sans_ext(file)) %>% 
  select(-file, -plot, -fid, -id) %>% 
  mutate(class_2=ifelse(is.na(class_2), class_1, class_2)) %>% 
  # sum area for each land cover classes within each plotID 
  summarise(
    class2_area = sum(area_km2),
    .by = c("plotID", "buffer_size_m", "class_0", "class_1", "class_2")) %>% 
  mutate(class1_area = sum(class2_area), 
         .by = c("plotID", "class_1"), .before=class2_area) %>%
  mutate(class0_area = sum(class2_area), 
         .by = c("plotID", "class_0"), .before=class1_area) %>% 
  # calculate total plot area
  mutate(plot_area_km2 = sum(class2_area), .by = c("plotID"), .after=buffer_size_m) %>% 
  rename(class0_LandType=class_0,
         class1_LandType=class_1,
         class2_LandType=class_2) %>%
  pivot_longer(-c(plotID, buffer_size_m, plot_area_km2),
               names_to = c("LandType_level", ".value"),
               names_sep="_") %>% 
  rename(LandType_area_km2=area,
         LandType_code=LandType)%>% 
  # at resolution class_0 there are repetitions of land types within same plotID
  summarise(LandType_area_km2 = mean(LandType_area_km2),
            .by = c("plotID", "buffer_size_m", "plot_area_km2", "LandType_level", "LandType_code")
  )

files_250m



files_500m <- list.files("data/Raster_measurements/raster_measurement_500m", 
                         pattern = "^SEW.*\\.csv$", full.names = TRUE)%>%
  set_names(basename(.)) %>%  # name the list elements by filename
  map_dfr(~ readr::read_csv(.x, show_col_types = FALSE), .id = "file") %>% 
  # add buffer size
  mutate(buffer_size_m = 500)%>%
  # add a plotID extracted from filename 
  mutate(plot = tools::file_path_sans_ext(file)) %>% 
  select(-file, -plot, -fid, -id) %>% 
  mutate(class_2=ifelse(is.na(class_2), class_1, class_2)) %>% 
  # sum area for each land cover classes within each plotID 
  summarise(
    class2_area = sum(area_km2),
    .by = c("plotID", "buffer_size_m", "class_0", "class_1", "class_2")) %>% 
  mutate(class1_area = sum(class2_area), 
         .by = c("plotID", "class_1"), .before=class2_area) %>%
  mutate(class0_area = sum(class2_area), 
         .by = c("plotID", "class_0"), .before=class1_area) %>% 
  # calculate total plot area
  mutate(plot_area_km2 = sum(class2_area), .by = c("plotID"), .after=buffer_size_m) %>% 
  rename(class0_LandType=class_0,
         class1_LandType=class_1,
         class2_LandType=class_2) %>%
  pivot_longer(-c(plotID, buffer_size_m, plot_area_km2),
               names_to = c("LandType_level", ".value"),
               names_sep="_") %>% 
  rename(LandType_area_km2=area,
         LandType_code=LandType) %>% 
  # at resolution class_0 there are repetitions of land types within same plotID
  summarise(LandType_area_km2 = mean(LandType_area_km2),
            .by = c("plotID", "buffer_size_m", "plot_area_km2", "LandType_level", "LandType_code")
            )

files_500m

# Import Land type ID mapping
Land_type_ID <- read_csv("data/Raster_measurements/Land_type_ID.csv")

# Combine 250m and 500m landscape data
landscape_data <- files_250m %>%
  bind_rows(files_500m) %>%
  mutate(LandType_percent=LandType_area_km2/plot_area_km2*100) %>% 
   mutate(LandType_level = case_when(
    LandType_level == "class0" ~ "class_0",
    LandType_level == "class1" ~ "class_1",
    LandType_level == "class2" ~ "class_2")) %>% 
  left_join(Land_type_ID, by = c("LandType_code")) %>% 
  relocate(LandType_name, .after=LandType_code)


# check if percent adds up to 100 for each resolution of land-use type
landscape_data %>% 
  group_by(plotID, buffer_size_m, LandType_level) %>% 
  summarise(total_percent = sum(LandType_percent)) %>% 
  print(n=Inf)


write_csv(landscape_data, "data/Raster_measurements/Landscape_Type_composition_ALL_OB.csv")

# check unique land types
landscape_data %>% 
  filter(LandType_level=="class_0") %>% 
  pull(LandType_name) %>% 
  unique()


landscape_data %>% 
  filter(LandType_level=="class_1") %>%
  pull(LandType_name) %>% 
  unique()


# Main Land types - proportions per plot
# for all resolution classes of land-use type
land_types_proportions_all <- landscape_data %>% 
  mutate(LandType_name = word(LandType_name, 1)) 

write_csv(land_types_proportions_all, "data/processed_data/Landscape_types_all.csv")

# for class_0:
land_types_proportions_class_0 <-  landscape_data %>% 
  filter(LandType_level=="class_0") %>%
  # in LandType_name keep only first word
  mutate(LandType_name = word(LandType_name, 1)) %>% 
  select(plotID, buffer_size_m, LandType_name, LandType_percent) %>%
  pivot_wider(names_from = LandType_name, 
              values_from = LandType_percent,
              values_fill = 0
  ) %>%
  mutate(Forest_percent = Forest,
         Agricultural_percent=Agricultural+Bare,
         Water_bodies_percent=Mire+Water, 
         Urban_percent=Transportation + Settlements, 
         LandType_level="class_0",  
         .keep ="unused") 

# Calculate landscape heterogeneity metrics
landscape_heterogeneity <- landscape_data %>%
summarise(
  LandType_richness = n_distinct(LandType_name),
  LandType_Shannon = vegan::diversity(LandType_percent, index = "shannon"),
  LandType_even = vegan::diversity(LandType_percent, index = "invsimpson"),
  .by = c(plotID, buffer_size_m, LandType_level)) %>% 
  pivot_wider(names_from = LandType_level, 
              values_from = c(LandType_richness, LandType_Shannon, LandType_even)) %>%
  left_join(land_types_proportions_class_0, 
            by = c("plotID", "buffer_size_m"))
  


# check missing data if merged
merged_tree_data %>% 
  left_join(landscape_heterogeneity %>% 
              filter(LandType_level=="class_0" & buffer_size_m==500),
            by = c("Plot" = "plotID")) %>% 
#  filter(!Outside==TRUE) %>% 
  filter(is.na(LandType_richness_class_0)) %>% 
  print(n=Inf)



write_csv(landscape_heterogeneity, "data/processed_data/Landscape_heterogeneity.csv")

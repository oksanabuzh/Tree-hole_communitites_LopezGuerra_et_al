# Script: Data Preparation for Tree Hole Community Analysis

# Purpose: Import, clean, and combine tree hole community data from 2023-2024
#          sampling campaigns for biodiversity analysis

# Packages ---------------------------------------------------------------------

library(tidyverse)

# 2023 DATA --------------------------------------------------------------------

# Import 2023 data and calculate species abundance per tree hole
community2023 <- read_csv("data/raw_data/sampling_2023_2024/Species_and_trees_from_the_sampling_SEW2023.csv") %>% 
  pivot_longer(Clogmia_sp:Phaonia_subventa, 
               names_to = "Sp_ID", 
               values_to = "Abundance",
               values_drop_na = TRUE)


str(community2023)
  

names(community2023)
community2023 %>% pull(Tree_ID) %>% unique()

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
  summarise(Abundance = sum(Abundance), 
            .by = c("Plot", "Tree_ID", "Treehole_number", "Type_of_tree", 
                    "Outside", "Tree_hole_type", "Sampling_date", "Sp_ID")) 

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
    Sampling_date == "16/6/2024" ~ "2024-06-16",
    Sampling_date == "17/05/2024" ~ "2024-05-17",
    Sampling_date %in% c("17/6/2024", "17/06/2024") ~ "2024-06-17",
    Sampling_date == "22/05/2024" ~ "2024-05-22",
    Sampling_date == "22/07/2024" ~ "2024-07-22",
    Sampling_date == "27/05/2024" ~ "2024-05-27"
  )) %>%
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
    Sampling_date == "2024-05-27" ~ "May"
  ), .after = Year) 

# EXPLORATORY CHECKS -----------------------------------------------------------

names(Community_2023_2024)
Community_2023_2024 %>% print(n = Inf)

# Check for missing species IDs
Community_2023_2024 %>% filter(is.na(Sp_ID))

# Sampling summary by year and date
Community_2023_2024 %>% 
  group_by(Year, Sampling_date) %>%
  count() 


# write_csv(Community_2023_2024, "data/processed_data/Community_2023_2024.csv")


# EXPLORATORIES DATA -----------------------------------------------------------

# TREE DATA --------------------------------------------------------------------
# 31487_7
# single tree data, on all forest EPs, 2020 - 2023
# original data on species, diameter at breast height, cm (d), tree heigth, m (h), 
# wood volume, m3 (v) and geographical location (caliper limit: dbh >= 7 cm).
# tree -tree ID
# species - Tree species (name shortened to 16 characters)


tree_data <- read_csv("data/raw_data/BiodExpl/31487_7_data.csv") %>% 
  filter(Exploratory == "SCH") 
tree_data

tree_data %>% 
  filter(str_detect(tree, "SEW45_0038")) %>% 
  print(n=Inf)




merged_data <- Community_2023_2024 %>% 
  left_join(tree_data, by = c("Tree_ID" = "tree")) 

write_csv(merged_data, "data/processed_data/Community_2023_2024_with_tree_data.csv")


merged_data %>% 
  pull(species) %>% 
  unique()

  
  


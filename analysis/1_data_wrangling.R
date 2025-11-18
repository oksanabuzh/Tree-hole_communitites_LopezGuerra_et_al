# Purpose: prepare data for analysis of community composition and biodiversity in tree holes

# packages
library(tidyverse)

# data  ------

# 2023 ----
community2023 <- read_csv("data/raw_data/sampling_2023_2024/Species_and_trees_from_the_sampling_SEW2023.csv") %>% 
  select(-Individual_ID, -ID_metadatadoc, -"Length in mm", -"Weight in mg (unless stated differently)") %>% 
  summarise(Abundance=n_distinct(Sp_ID), 
            .by=c("Plot", "Tree_ID", "Treehole_number", "Type_of_tree", "Tree_hole_type", "Sampling_date", "Sp_ID")) %>% 
  mutate(Outside=FALSE) 

names(community2023)

# check data
community2023 %>% 
  group_by(Plot, Tree_ID, Treehole_number, Type_of_tree, Tree_hole_type, Sampling_date, Sp_ID) %>%
  count() %>% 
  filter(n>1)

# 2024 ----
community2024 <- read_csv("data/raw_data/sampling_2023_2024/Sampling_2024_indiviudlas.csv") %>% 
  select(-Nickname_2014, -Nickname_2015, -"ID for the sampling", -"Label", -"Notes", -"Pictures") %>% 
  summarise(Abundance=sum(Abundance), 
            .by=c("Plot", "Tree_ID", "Treehole_number", "Type_of_tree", 
                  "Outside" ,"Tree_hole_type", "Sampling_date", "Sp_ID") 
            ) %>% 
  mutate(Sampling_date=case_when(
    Sampling_date=="06/11/2023" ~ "233-11-06",
    Sampling_date=="07/11/2023" ~ "2023-11-07",
    
    
    TRUE ~ Sampling_date
  )) %>%
  mutate(Month=case_when(
    Sampling_date %in% c("06/11/2023", "07/11/2023") ~ "November",
    Sampling_date %in% c("13/06/2024", "16/6/2024", "17/06/2024", "17/6/2024") ~ "June",
    Sampling_date %in% c("2024-07-09", "2024-07-10") ~ "July",
    Sampling_date %in% c("2024-08-13", "2024-08-14") ~ "August",
    TRUE ~ NA_character_
  )))
names(community2024)


# check data
community2024 %>% 
  group_by(Plot, Tree_ID, Treehole_number, Type_of_tree, Tree_hole_type, Sampling_date, Sp_ID) %>%
  count() %>% 
  filter(n>1)

community2024 %>% 
  group_by(Plot, Tree_ID, Treehole_number, Type_of_tree, Tree_hole_type, Sampling_date, Sp_ID) %>%
  count() %>% 
  filter(n>1)

# combine data
Community_2023_2024 <- bind_rows(
  "2023" = community2023, 
  "2024" = community2024, 
  .id="Year") %>% 
  relocate(Outside, .after = Treehole_number)

Community_2023_2024


Community_2023_2024 %>% 
  filter(is.na(Sp_ID))

Community_2023_2024 %>% 
  group_by(Year, Sampling_date) %>%
  count() 

# write_csv(Community_2023_2024, "data/processed_data/Community_2023_2024.csv")

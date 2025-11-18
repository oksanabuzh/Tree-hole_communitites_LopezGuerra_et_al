# Purpose: Calculate diversity measures for community data

# packages
library(tidyverse)

# data  ------

# 2023 ----
Diversity_2023_2024 <- read_csv("data/processed_data/Community_2023_2024.csv") %>% 
  summarise(
    abundance = sum(Abundance, na.rm = TRUE),
    sp_richness = n_distinct(Sp_ID),
    evenness = vegan::diversity(Abundance, index = "invsimpson"),
    hill_shannon = exp(vegan::diversity(Abundance, index = "shannon")),
    .by=c("Year", "Plot", "Tree_ID", "Treehole_number", "Type_of_tree", 
          "Outside" ,"Tree_hole_type", "Sampling_date")
  )

Diversity_2023_2024
# write_csv(Diversity_2023_2024, "data/processed_data/Diversity_2023_2024.csv")

Diversity_2023_2024 %>% 
  group_by(Year, Plot, Tree_ID, Type_of_tree, 
           Outside , Sampling_date) %>% 
  count(Treehole_number) %>% 
  arrange(desc(n))


Diversity_2023_2024 %>% 
  group_by(Year, Plot#, Sampling_date
           ) %>% 
  count(Treehole_number) %>% 
  arrange(desc(n))

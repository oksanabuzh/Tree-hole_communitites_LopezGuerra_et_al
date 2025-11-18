# Script: Calculate diversity measures for tree hole community data
# Purpose: Calculate abundance, species richness, evenness (inverse Simpson), and Hill Shannon
#          for tree hole communities sampled in 2023 and 2024

# Load packages ----------------------------------------------------------------
library(tidyverse)  
library(vegan)      

# Load and process data --------------------------------------------------------

# Read community data from 2023 and 2024
# Calculate diversity metrics for each unique tree hole
Diversity_2023_2024 <- read_csv("data/processed_data/Community_2023_2024.csv") %>% 
  summarise(
    # Total number of individuals across all species in each tree hole
    abundance = sum(Abundance, na.rm = TRUE),
    
    # Number of unique species (species richness) in each tree hole
    sp_richness = n_distinct(Sp_ID),
    
    # Evenness using inverse Simpson index
    # Higher values = more even distribution of individuals across species
    evenness = vegan::diversity(Abundance, index = "invsimpson"),
    
    # Hill number based on Shannon diversity (effective number of species)
    # Exponential of Shannon index = number of equally common species
    hill_shannon = exp(vegan::diversity(Abundance, index = "shannon")),
    
    # Group by all identifying variables for each tree hole
    .by=c("Plot", "Tree_ID", "Treehole_number", "Year", "Month", 
          "Type_of_tree", 
          "Outside" ,"Tree_hole_type", "Sampling_date")
  )

# View all diversity results
Diversity_2023_2024 %>% 
  print(n = Inf)


# write_csv(Diversity_2023_2024, "data/processed_data/Diversity_2023_2024.csv")

# Exploratory data checks ------------------------------------------------------

# Count number of tree holes per tree
# Grouped by year, plot, tree ID, tree type, location, and sampling date
# Shows which trees have the most tree holes 
Diversity_2023_2024 %>% 
  group_by(Year, Plot, Tree_ID, Type_of_tree, 
           Outside, Sampling_date) %>% 
  count(Treehole_number) %>%  
  arrange(desc(n))            

# Count tree holes per plot
# Grouped only by year and plot
# Shows distribution of tree holes across plots 
Diversity_2023_2024 %>% 
  group_by(Year, Plot  
  ) %>% 
  count(Treehole_number) %>%  
  arrange(desc(n))             


# Summary statistics for diversity metrics ------------------------------------
Diversity_2023_2024 %>% 
  names()

Diversity_2023_2024 %>% 
  ggplot(aes(x=Tree_hole_type, y = abundance, fill=Month)) +
  geom_boxplot() +
  #  facet_wrap(~Year) +
  theme_bw()

Diversity_2023_2024 %>% 
  ggplot(aes(x=Tree_hole_type, y = sp_richness, fill=Month)) +
  geom_boxplot() +
  #  facet_wrap(~Year) +
  theme_bw()

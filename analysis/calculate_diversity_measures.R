# Script: Calculate diversity measures for tree hole community data
# Purpose: Calculate abundance, species richness, evenness (inverse Simpson), and Hill Shannon
#          for tree hole communities sampled in 2023 and 2024

# Load packages ----------------------------------------------------------------
library(tidyverse)  
library(vegan)      

# Load and process data --------------------------------------------------------

# Read community data from 2023 and 2024
Community <- read_csv("data/processed_data/Community_2023_2024.csv") %>% 
  mutate(Abundance=ifelse(is.na(Abundance), 1, Abundance))
  
names(Community)


# Calculate diversity metrics for each unique tree hole
Diversity_2023_2024 <- Community %>% 
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


??diversity
# View all diversity results
Diversity_2023_2024 %>% 
  filter(Outside == "FALSE") %>%
    print(n = Inf)

Diversity_2023_2024 %>% 
  filter(Outside == "FALSE") %>%
  print(n = Inf)

view(Diversity_2023_2024)

Diversity_2023_2024 %>% 
  select(Plot, Tree_ID, Treehole_number, Year, Month, 
         Outside,
         abundance, sp_richness, evenness, hill_shannon) %>% 
  # write_csv("data/processed_data/Diversity_2023_2024.csv")

# Exploratory data checks ------------------------------------------------------

# Count number of tree holes per tree
# Grouped by year, plot, tree ID, tree type, location, and sampling date
# Shows which trees have the most tree holes 
Diversity_2023_2024 %>% 
  group_by(Year, Plot, Tree_ID, Outside, Sampling_date, Treehole_number) %>% 
  count() %>%  
  arrange(desc(n))            

# Count tree holes per plot
# Grouped only by year and plot
# Shows distribution of tree holes across plots 
Diversity_2023_2024 %>% 
  group_by(Year, Plot  
  ) %>% 
  count(Treehole_number) %>%  
  arrange(desc(n))             

Diversity_2023_2024 %>% 
  select(Year, Month, Plot, Treehole_number, abundance, sp_richness) %>% 
  arrange(abundance) %>% 
  print(n = Inf)


Diversity_2023_2024 %>% 
  ggplot(aes(y=abundance, x=Month)) +
  geom_boxplot() +
  geom_jitter(aes(color=factor(Year)),
              width=0.2, height=0, alpha=0.5) 

Diversity_2023_2024 %>% 
  ggplot(aes(y=sp_richness, x=Month)) +
  geom_boxplot() +
  geom_jitter(aes(color=factor(Year)),
              width=0.2, height=0, alpha=0.5) 


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


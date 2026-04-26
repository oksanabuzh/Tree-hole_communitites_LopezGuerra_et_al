# Script: Calculate diversity measures for tree hole community data
# Purpose: Calculate abundance, species richness, evenness (inverse Simpson), and Hill Shannon
#          for tree hole communities sampled in 2023 and 2024

# Load packages ----------------------------------------------------------------
library(tidyverse)  
library(vegan)      

# Load and process data --------------------------------------------------------

# Read community data from 2023 and 2024
Community <- read_csv("data/processed_data/Community_2023_2024_DNAcorrected.csv") %>% 
  # merge with body-mass data:
  left_join(read_csv("data/processed_data/Traits_2023_2024_final_DNA_corrected.csv") %>% 
              select(Sp_ID_DNAcorrected, dry_weight_mg), 
            by = c("Sp_ID_DNAcorrected"))

# check for any missing body mass values
Community %>% 
  filter(is.na(dry_weight_mg))
  

# Calculate diversity and biomass metrics for each unique tree hole
Diversity_2023_2024 <- Community %>% 
  summarise(
    # Total number of individuals across all species in each tree hole
    abundance = sum(Abundance, na.rm = TRUE),
    biomass_dry_mg = sum(dry_weight_mg, na.rm = TRUE), 
    # Number of unique species (species richness) in each tree hole
    sp_richness = n_distinct(Sp_ID_DNAcorrected),
    
    # Evenness using inverse Simpson index
    # Higher values = more even distribution of individuals across species
    Hill_Simpson = vegan::diversity(Abundance, index = "invsimpson"),
    
    # Hill number based on Shannon diversity (effective number of species)
    # Exponential of Shannon index = number of equally common species
    Hill_Shannon = exp(vegan::diversity(Abundance, index = "shannon")),
    
    # Group by all identifying variables for each tree hole
    .by=c("Plot", "Tree_ID", "Treehole_number", "Tree_hole_type", 
          "Tree_hole_type_coarse", "Tree_hole_opening",
          "Year", "Month", "Outside", "Sampling_date"))


# View all diversity results
Diversity_2023_2024 %>% 
  print(n = Inf)

write_csv(Diversity_2023_2024, "data/processed_data/Diversity_2023_2024.csv")


names(Diversity_2023_2024)

corl1 <- round(cor(Diversity_2023_2024 %>% 
                     dplyr::select(abundance, biomass_dry_mg,"sp_richness",
                                   "Hill_Simpson","Hill_Shannon"),
                   method = c("pearson"), use = "pairwise.complete.obs"), 2)

corl1

ggcorrplot::ggcorrplot(corl1,
                       hc.order = F, type = "lower",
                       lab = TRUE, lab_size = 4,
                       colors = c("red", "white", "blue"))





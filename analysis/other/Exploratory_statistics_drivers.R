#   Analysis of the drivers of tree-hole insect abundance in 2023-2024f 

library(tidyverse)
library(car)
library(performance)
library(dplyr)
library(ggplot2)
library(purrr)
library(ggpubr)
library(multcomp)
library(emmeans)
library(conflicted)

# Prefer dplyr's select whenever there is a conflict
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

# data -------------------------------------------------------
environm <- read_csv("data/processed_data/Environment_ALL.csv") %>% 
  mutate(Month=factor(Month, levels=c("May", "June", "July", "November"))) %>% 
  mutate(Tree_hole_type_coarse=factor(Tree_hole_type_coarse, levels=c("pan", "rot"))) %>% 
  mutate(Tree_hole_type=factor(Tree_hole_type, 
                               levels=c("Division", "Branch", "Trunk", 
                                        "Cut tree", "Root",  NA)))
str(environm)



landscape_heterogeneity <- read_csv("data/processed_data/Landscape_heterogeneity.csv")



Diversity_2023_2024 <- read_csv("data/processed_data/Diversity_2023_2024.csv") %>% 
  mutate(Month=factor(Month, levels=c("May", "June", "July", "November"))) %>% 
  mutate(Tree_hole_type_coarse=factor(Tree_hole_type_coarse, levels=c("pan", "rot"))) %>% 
  mutate(Tree_hole_type=factor(Tree_hole_type, 
                               levels=c("Division", "Branch", "Trunk", 
                                        "Cut tree", "Root",  NA))) %>% 
  select(Plot, Tree_ID, Treehole_number, Year, Month,
         abundance,	sp_richness,	biomass_dry_mg) %>% 
  left_join(environm,
            by=c("Plot", "Tree_ID", "Treehole_number", "Year", "Month")) %>% 
  #% per ha of open areas (clearings, edges and other areas with a well-developed herb layer composed of flowering plants): 0 = 0%, 2 = < 1% or > 5%, 5 = 1 to 5%
  mutate(Openness = case_when(
    Openness == 0 ~ "0%",
    Openness == 2 ~ "1-5%",
    Openness == 5 ~ ">5%"),
    Openness = factor(Openness, levels = c("0%", "1-5%", ">5%"))
  ) %>% 
  mutate(Vertical_structure = case_when(
    Vertical_structure == 1 ~ "2 layers",
    Vertical_structure == 2 ~ "3-4 layers",
    Vertical_structure == 5 ~ "5 layers"),
    Vertical_structure = factor(Vertical_structure, 
                                levels = c("2 layers", "3-4 layers", "5 layers"))
  )

str(Diversity_2023_2024)



Diversity_2023_2024_log <- Diversity_2023_2024 %>% 
  mutate(biomass_log = log(biomass_dry_mg)) %>% 
  mutate(Inonat_mean_2012_2018_log = log1p(Inonat_mean_2012_2018),
         Inonat_2018_log = log1p(Inonat_2018),
         SMId_mean_2018_2020_log = log1p(SMId_mean_2018_2020),
         SMIr_mean_2018_2020_log = log1p(SMIr_mean_2008_2020),
         SMI_mean_2018_2020_log = log1p(SMI_mean_2018_2020),
         Tree_sp_richness_log = log1p(Tree_sp_richness), 
         Tree_abundance_log = log1p(Tree_abundance)) %>% 
  rename(Vert_stand_struct = enl,
         Stand_struct_complex = ssci)


# Selected predictors: -----------------------------------------------------

## abundance: ------
# Tree_abundance_log
# Openness + Vertical_structure + 
# LandType_richness_class_2 (500 m) +
# Forest_percent +Agricultural_percent + Urban_percent +
# precipitation_radolan_mean +
# perc_Carpinus_betulus + 

# biomass_dry_mg:
# # Tree_abundance_log + Tree_sp_richness_log +
# ssci # "Stand structural complexity" +
# Vertical_structure +  
# precipitation_radolan_mean
# perc_Carpinus_betulus 

# LandType_richness_class_2 (500 m) +
# Forest_percent, Agricultural_percent, Urban_percent


# sp_richness:
# Openness  + Vertical_structure + Standing_deadwood + 
# Inonat_mean_2012_2018_log + Iharv_mean_2012_2018 + Idwcut_mean_2012_2018 + Formi_mean_2012_2018 +
# OR "Inonat_2018_log", "Iharv_2018", "Idwcut_2018", "Formi_2018"           
# Agricultural_percent


# All selected predictors: ----------

# Land Use:
## Inonat_mean_2012_2018_log + Iharv_mean_2012_2018 + Idwcut_mean_2012_2018 + 
## Formi_mean_2012_2018 +

# Landscape: 
## LandType_richness_class_2 (500 m) + Forest_percent + Agricultural_percent + Urban_percent

# Tree community:
## Tree_abundance_log + Tree_sp_richness_log

# Structural complexity
##ssci # "Stand structural complexity"

# Plot biodiversity potential 
### Openness  + Vertical_structure + Standing_deadwood +

# Climate:
## precipitation_radolan_mean




names(Diversity_2023_2024)




# SMI -------------------------------
# "SMI_mean_2018_2020_log", "SMIr_mean_2018_2020_log", "SMId_mean_2018_2020_log"
# "SMI_mean_2008_2020", "SMIr_mean_2008_2020", "SMId_mean_2008_2020"

## Formi ----------------------------
# "Inonat_2018_log", "Iharv_2018", "Idwcut_2018", "Formi_2018"           
# "Inonat_mean_2012_2018_log", "Iharv_mean_2012_2018", "Idwcut_mean_2012_2018", "Formi_mean_2012_2018"


# Tree properties -------------------
# c( "DBH", "tree_heigth")

# Tree diversity -------------------------------------------------------------
#    "Tree_sp_richness_log", "Tree_abundance_log" 


## Plot biodiversity potential -------------------------------------------------------------
preds1 <- c("Vertical_structure", 
            "Standing_deadwood", "Lying_deadwood", 
            "Very_large_trees", "Habitat_trees",
            "Openness",  "IBPscore")

# Structural complexity: ---------------------------------------------
# enl  - "Vertical stand structure"
# ssci - "Stand structural complexity"
# canopy.openness -"Canopy openness"
# "enl", "ssci", "canopy.openness"

preds1 <- c("Vert_stand_struct", "Stand_struct_complex", "canopy.openness")

## Tree composition -----
preds1 <- c("perc_Betula_pendula","perc_Fagus_sylvatica","perc_Pinus_sylvestris",
            "perc_Quercus_spec","perc_Carpinus_betulus",
            "perc_Tilia_cordata","perc_Prunus_avium",
            "perc_Acer_pseudoplatanus"
          # no variability:
           # "perc_Prunus_serotina",  "perc_Picea_abies", "perc_Acer_platanoides", ,"perc_Fraxinus_excelsior"
            )

preds2 <- c("perc_Pyrus_pyraster","perc_Alnus_spec","perc_Robinia_pseudoacacia",
            "perc_Ulmus_spec","perc_Carya_ovata","perc_Malus_sylvestris",
            "perc_Populus_tremula","perc_Salix_caprea","perc_Populus_nigra",
            "perc_Betula_spec","perc_Ulmus_glabra","perc_Aesculus_hippocastanum")


# tree hole mapping (Petermann et al.) ----------------------------------------
# "Total_hole_number_mapping"

# Stand structural attributes -------------------------------------------------
## Exploration plots --------------

preds0 <- c("ssm_N","ssm_Vol","ssm_CPA","sp_N_1D", "sp_BA_1D", "sp_N_2D", "sp_BA_2D", "spat_Pielou")

preds1 <- c("ssm_N","ssm_SDI","ssm_BA","ssm_Vol","ssm_CPA","ssm_con_BA","ssm_con_CPA",
            "ssm_Pa_CPA","ssm_Ps_CPA","ssm_Qs_CPA","ssm_Fs_CPA","sp_0D","sp_N_1D",
            "sp_N_2D","sp_BA_1D","sp_BA_2D","d_qm","d_m","d_SD","d_CV","d_50")

preds2 <- c("d_max", "d_gini","dc_0D","dc_1D","dc_1D_BA","dc_sp_1D","hc_0D","hc_1D","hc_1D_BA",
            "hc_sp_1D","r20_SD_BA","r20_CV_BA","r20_SD_N","r20_CV_N","r20_VMR")

preds3 <- c("r20_Morisita","spat_clarkevans","spat_Pielou","spat_spM","spat_TD",
            "spat_SCI_d","spat_Th","spat_SCI_h")


# Climate -------------------------------------------------

preds0 <- c("precipitation_radolan_rain_days_mean","precipitation_radolan_acc_mean", "rH_200_DMR_mean",
            "Ta_10_mean","Ta_10_max_mean")

preds1 <- c("precipitation_radolan_mean","precipitation_radolan_acc_mean", "Ta_200_mean","Ta_200_heat_index_mean",
            "Ta_200_humidex_mean")

preds2 <- c(#"Ta_200_extremely_hot_days_sum", 
  "Ta_200_extremely_cold_days_sum","Ta_200_heating_degree_days_sum")


# Landscape heterogeneity (biotops) ------------------------------------------

df1 <- Diversity_2023_2024_log %>%
  left_join(landscape_heterogeneity %>% 
              filter(buffer_size_m==250),
            by = c("Plot" = "plotID"))

df2 <- Diversity_2023_2024_log %>%
  left_join(landscape_heterogeneity %>% 
              filter(buffer_size_m==500),
            by = c("Plot" = "plotID"))


names(df1)
preds1 <- c("Forest_percent", "Agricultural_percent", 
             "Water_bodies_percent", "Urban_percent")

preds2 <- c("LandType_richness_class_0", "LandType_Shannon_class_0", "LandType_even_class_0",
           "LandType_richness_class_0", "LandType_Shannon_class_0", "LandType_even_class_0",
           "LandType_richness_class_2", "LandType_Shannon_class_2", "LandType_even_class_2")

# Define predictors: ----------------------------------------------------


preds <- c("Forest_percent", "Agricultural_percent", 
           "Water_bodies_percent", "Urban_percent")


# abundance  ---------------------------------

response <- c("abundance")

plots <- map(preds, function(var) {
  datp <- Diversity_2023_2024_log %>% 
    select(all_of(c(response, var))) %>%
    na.omit()
  ggplot(datp, aes_string(x = var, y = response)) +
    geom_jitter(width=0, height=0, pch=21, 
                color="brown", fill="#FFA55B") +
    geom_smooth(
      method = "glm",
      formula = y ~ x,
      method.args = list(family = quasipoisson),
      color = "#086096",fill  = "#86BBD8",
      se = TRUE
    ) +
    theme_minimal() +
    labs( x = var, y = response)
})

ncol <- 4
nrow <- ceiling(length(plots) / ncol)
ggarrange(plotlist = plots, ncol = ncol, nrow = nrow)


# Biomass -------
response2 <- c("biomass_log") 

plots <- map(preds, function(var) {
  datp <- Diversity_2023_2024_log %>% 
    select(all_of(c(response2, var))) %>%
    na.omit()
  ggplot(datp, aes_string(x = var, y = response2)) +
    geom_jitter(width=0, height=0, pch=21, 
                color="brown", fill="#FFA55B") +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      color = "#086096",fill  = "#86BBD8",
      se = TRUE
    ) +
    theme_minimal() +
    labs(x = var, y = response2)
})

ggarrange(plotlist = plots, ncol = ncol, nrow = nrow)



# sp_richness ---------------------------------

response3 <- c("sp_richness")


plots <- map(preds, function(var) {
  datp <- Diversity_2023_2024_log %>% 
    #filter(sp_richness<9) %>% # outlier in forest management plots
    select(all_of(c(response3, var))) %>%
    na.omit()
  ggplot(datp, aes_string(x = var, y = response3)) +
    geom_jitter(width=0, height=0, pch=21, 
                color="brown", fill="#FFA55B") +
    geom_smooth(
      method = "glm",
      formula = y ~ x,
      method.args = list(family = poisson),
      color = "#086096",fill  = "#86BBD8",
      se = TRUE
    ) +
    theme_minimal() +
    labs(x = var, y = response3)
})

ggarrange(plotlist = plots, ncol = ncol, nrow = nrow)


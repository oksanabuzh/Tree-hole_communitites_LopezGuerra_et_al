#  

library(tidyverse)
library(car)
library(performance)

# data -------------------------------------------------------
environm <- read_csv("data/processed_data/Environment_ALL.csv") %>% 
  mutate(Month=factor(Month, levels=c("May", "June", "July", "November"))) %>% 
  mutate(Tree_hole_type_coarse=factor(Tree_hole_type_coarse, levels=c("pan", "rot"))) %>% 
  mutate(Tree_hole_type=factor(Tree_hole_type, 
                               levels=c("Division", "Branch", "Trunk", 
                                        "Cut tree", "Root",  NA)))
str(environm)

Diversity_2023_2024 <- read_csv("data/processed_data/Diversity_2023_2024.csv") %>% 
  mutate(Month=factor(Month, levels=c("May", "June", "July", "November"))) %>% 
  mutate(Tree_hole_type_coarse=factor(Tree_hole_type_coarse, levels=c("pan", "rot"))) %>% 
  mutate(Tree_hole_type=factor(Tree_hole_type, 
                               levels=c("Division", "Branch", "Trunk", 
                                        "Cut tree", "Root",  NA))) %>% 
  select(Plot, Tree_ID, Treehole_number, Year, Month,
         abundance,	sp_richness,	Hill_Simpson,	Hill_Shannon) %>% 
  left_join(environm,
            by=c("Plot", "Tree_ID", "Treehole_number", "Year", "Month"))

str(Diversity_2023_2024)


# Tree properties------------------------------------------------------

# diameter at breast height

m1 <- glm(sp_richness ~ #SMI_mean_2018_2020 + 
          # DBH , 
            Month + tree_heigth, 
           family = poisson,
          data=Diversity_2023_2024)

check_overdispersion(m1)
summary(m1)
Anova(m1)

Diversity_2023_2024 %>% 
  ggplot(aes(x=DBH, y=sp_richness)) +
  geom_jitter(width=0.01, height=0.01, pch=21, 
              color="brown", fill="#FFA55B") +
  geom_smooth(method="glm", method.args = list(family = "poisson"),
              color = "#086096",fill  = "#86BBD8") + 
  labs(x="DBH, cm", y="Species richness") +
  theme_bw()


Diversity_2023_2024 %>% 
  ggplot(aes(x=tree_heigth, y=sp_richness)) +
  geom_jitter(width=0.01, height=0.01, pch=21, 
              color="brown", fill="#FFA55B") +
  geom_smooth(method="glm", method.args = list(family = "poisson"),
              color = "#086096",fill  = "#86BBD8") + 
  labs(x="Tree height, m", y="Species richness") +
  theme_bw()


# Land use -------------------------------------------------------------
## SMI ----------------------------
#SMI_mean_2018_2020 
# SMIr_mean_2018_2020, SMId_mean_2018_2020

# SMI_mean_2008_2020
# SMIr_mean_2008_2020, SMId_mean_2008_2020

m2 <- glm(sp_richness ~  SMI_mean_2018_2020, family = poisson,
          data=Diversity_2023_2024)

check_overdispersion(m2)
summary(m2)
Anova(m2)

Diversity_2023_2024 %>% 
  ggplot(aes(x=log(SMI_mean_2018_2020), y=sp_richness)) +
  geom_jitter(width=0.01, height=0.01, pch=21, 
              color="brown", fill="#FFA55B") +
  geom_smooth(method="glm", method.args = list(family = "poisson"),
              color = "#086096",fill  = "#86BBD8") + 
  labs(x="SMI", y="Species richness") +
  theme_bw()


#  SMIr_mean_2018_2020
m1.1 <- glm(sp_richness ~ SMIr_mean_2018_2020 + SMId_mean_2018_2020, family = poisson,
          data=Diversity_2023_2024)

check_overdispersion(m1.1)
summary(m1.1)
Anova(m1.1)


Diversity_2023_2024 %>% 
  ggplot(aes(x=log(SMIr_mean_2018_2020), y=sp_richness)) +
  geom_jitter(width=0.01, height=0.01, pch=21, 
              color="brown", fill="#FFA55B") +
  geom_smooth(method="glm", method.args = list(family = "poisson"),
              color = "#086096",fill  = "#86BBD8") + 
  labs(x="Risk component of SMI", y="Species richness") +
  theme_bw()


Diversity_2023_2024 %>% 
  ggplot(aes(x=log(SMId_mean_2018_2020), y=sp_richness)) +
  geom_jitter(width=0.01, height=0.01, pch=21, 
              color="brown", fill="#FFA55B") +
  geom_smooth(method="glm", method.args = list(family = "poisson"),
              color = "#086096",fill  = "#86BBD8") + 
  labs(x="Density component of SMI", y="Species richness") +
  theme_bw()



## Formi ----------------------------
# "Inonat_2018", "Iharv_2018", "Idwcut_2018"          
# "Formi_2018"           
# "Inonat_mean_2012_2018", "Iharv_mean_2012_2018", "Idwcut_mean_2012_2018"
# "Formi_mean_2012_2018" 


# Formi_2018
m2 <- glm(sp_richness ~ Formi_2018, family = poisson,
          data=Diversity_2023_2024)

check_overdispersion(m2)
summary(m2)
Anova(m2)

Diversity_2023_2024 %>% 
  ggplot(aes(x=Formi_2018, y=sp_richness)) +
  geom_jitter(width=0.01, height=0.01, pch=21, 
              color="brown", fill="#FFA55B") +
  geom_smooth(method="glm", method.args = list(family = "poisson"),
              color = "#086096",fill  = "#86BBD8") + 
  labs(x="Forest Management Intensity", y="Species richness") +
  theme_bw()

# Components:
m2.1 <- glm(sp_richness ~ Inonat_2018 + Idwcut_2018  + Iharv_2018, family = poisson,
          data=Diversity_2023_2024)

check_overdispersion(m2.1)
summary(m2.1)
Anova(m2.1)


Diversity_2023_2024 %>% 
  ggplot(aes(x=(Inonat_2018), y=sp_richness)) +
  geom_jitter(width=0.01, height=0.01, pch=21, 
              color="brown", fill="#FFA55B") +
  geom_smooth(method="glm", method.args = list(family = "poisson"),
              color = "#086096",fill  = "#86BBD8") + 
  labs(x="Non-natural tree species", y="Species richness") +
  theme_bw()


Diversity_2023_2024 %>% 
  ggplot(aes(x=(Idwcut_2018), y=sp_richness)) +
  geom_jitter(width=0.01, height=0.01, pch=21, 
              color="brown", fill="#FFA55B") +
  geom_smooth(method="glm", method.args = list(family = "poisson"),
              color = "#086096",fill  = "#86BBD8") + 
  labs(x="Dead wood with saw cuts", y="Species richness") +
  theme_bw()


Diversity_2023_2024 %>% 
  ggplot(aes(x=(Iharv_2018), y=sp_richness)) +
  geom_jitter(width=0.01, height=0.01, pch=21, 
              color="brown", fill="#FFA55B") +
  geom_smooth(method="glm", method.args = list(family = "poisson"),
              color = "#086096",fill  = "#86BBD8") + 
  labs(x="Harvested tree biomass", y="Species richness") +
  theme_bw()







# Plot biodiversity potential -------------------------------------------------------------

Diversity_2023_2024 %>% 
  select(Wet_macrohabitats, Rocky_macrohabitats, Temporal_continuity_of_the_woody_state) %>% 
  print(n=Inf)

names(Diversity_2023_2024)


response <- "sp_richness"
preds <- c("Tree_sp_richness", "Tree_abundance",
           "Tree_richness", "Vertical_structure",
           "Standing_deadwood", "Lying_deadwood",
           "Very_large_trees", "Habitat_trees",
           "Openness",  "IBPscore")

plots <- map(preds, function(var) {
  datp <- Diversity_2023_2024 %>% 
    select(all_of(c(response, var))) %>%
    na.omit()
  ggplot(datp, aes_string(x = var, y = response)) +
    geom_jitter(width=0.05, height=0.05, pch=21, 
                color="brown", fill="#FFA55B") +
    geom_smooth(
      method = "glm",
      formula = y ~ x,
      method.args = list(family = quasipoisson),
      color = "#086096",fill  = "#86BBD8",
      se = TRUE
    ) +
    theme_minimal() +
    labs(title = var, x = var, y = response)
})

ncol <- 3
nrow <- ceiling(length(plots) / ncol)
ggarrange(plotlist = plots, ncol = ncol, nrow = nrow)


## selected predictors ---------------

m1 <- glm(sp_richness ~ 
            #Openness + 
         #   log1p(Tree_sp_richness) ,
           log1p(Tree_abundance) , 
          # log1p(Vertical_structure), 
           # IBPscore, 
            family = poisson,
          data=Diversity_2023_2024)

check_overdispersion(m1)
summary(m1)
Anova(m1)


Diversity_2023_2024 %>% 
  ggplot(aes(x=log1p(Tree_sp_richness), y=sp_richness)) +
  geom_jitter(width=0.05, height=0.05, size=2,
              pch=21, color="brown", fill="#FFA55B") +
  geom_smooth(method="glm", method.args = list(family = "poisson"),
                            color = "#086096",fill  = "#86BBD8") + 
  labs(x="Tree richness", y="Species richness") +
  theme_bw()


Diversity_2023_2024 %>% 
  ggplot(aes(x=log1p(Tree_abundance), y=sp_richness)) +
  geom_jitter(width=0.03, height=0.05, size=2,
              pch=21, color="brown", fill="#FFA55B") +
  geom_smooth(method="glm", method.args = list(family = "poisson"),
              color = "#086096",fill  = "#86BBD8") + 
  labs(x="Tree abundance", y="Species richness") +
  theme_bw()



Diversity_2023_2024 %>% 
  ggplot(aes(x=log1p(Vertical_structure), y=sp_richness)) +
  geom_jitter(width=0.03, height=0.05, size=2,
              pch=21, color="brown", fill="#FFA55B") +
  geom_smooth(method="glm", method.args = list(family = "poisson"),
              color = "#086096",fill  = "#86BBD8") + 
  labs(x="Canopy vertical strucure", y="Species richness") +
  theme_bw()






## Tree composition -----

response <- "sp_richness"
preds1 <- c("perc_Betula_pendula","perc_Fagus_sylvatica","perc_Pinus_sylvestris",
            "perc_Prunus_serotina","perc_Quercus_spec","perc_Carpinus_betulus",
            "perc_Picea_abies","perc_Tilia_cordata","perc_Prunus_avium",
            "perc_Acer_platanoides","perc_Acer_pseudoplatanus","perc_Fraxinus_excelsior")

preds2 <- c("perc_Pyrus_pyraster","perc_Alnus_spec","perc_Robinia_pseudoacacia",
            "perc_Ulmus_spec","perc_Carya_ovata","perc_Malus_sylvestris",
            "perc_Populus_tremula","perc_Salix_caprea","perc_Populus_nigra",
            "perc_Betula_spec","perc_Ulmus_glabra","perc_Aesculus_hippocastanum")



df1 <- Diversity_2023_2024 %>%
  mutate(across(all_of(preds1), ~ if (is.factor(.x)) as.numeric(as.character(.x)) else .x)) %>%
  mutate(across(all_of(preds1), ~ log1p(.x), .names = "{.col}"))

df2 <- Diversity_2023_2024 %>%
  mutate(across(all_of(preds2), ~ if (is.factor(.x)) as.numeric(as.character(.x)) else .x)) %>%
  mutate(across(all_of(preds2), ~ log1p(.x), .names = "{.col}"))


# 2) make plots using the transformed predictors
plots <- map(preds1, function(var) {
  datp <- df1 %>% select(all_of(c(response, var))) %>% na.omit()
  
  ggplot(datp, aes_string(x = var, y = response)) +
    geom_jitter(width = 0.01, height = 0.01, pch = 21,
                color = "brown", fill = "#FFA55B") +
    geom_smooth(
      method = "glm",
      formula = y ~ x,
      method.args = list(family = poisson),   # or quasipoisson if overdispersed
      color = "#086096", fill = "#86BBD8",
      se = TRUE
    ) +
    theme_minimal() +
    labs(title = var, x = paste0(var, " (log1p)"), y = response)
})

# arrange & display
ncol <- 3
nrow <- ceiling(length(plots) / ncol)
ggarrange(plotlist = plots, ncol = ncol, nrow = nrow)

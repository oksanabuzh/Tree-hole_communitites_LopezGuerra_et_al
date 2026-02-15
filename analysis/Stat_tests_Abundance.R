#  

library(tidyverse)
library(car)
library(performance)

library(dplyr)
library(ggplot2)
library(purrr)
library(ggpubr)


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

m1 <- glm(abundance ~ #SMI_mean_2018_2020 + 
            DBH, 
            #   tree_heigth, 
            family = quasipoisson,
            data=Diversity_2023_2024)

check_overdispersion(m1)
summary(m1)
Anova(m1)

Diversity_2023_2024 %>% 
  ggplot(aes(x=DBH, y=abundance)) +
  geom_jitter(width=0.00, height=0.03) +
  geom_smooth(method="glm", method.args = list(family = "poisson")) + 
  #labs(x="SFOrest management", y="Abundance") +
  theme_bw()



# Land use -------------------------------------------------------------
## SMI ----------------------------
#SMI_mean_2018_2020 
# SMIr_mean_2018_2020, SMId_mean_2018_2020

# SMI_mean_2008_2020
# SMIr_mean_2008_2020, SMId_mean_2008_2020

m1 <- glm(abundance ~ SMId_mean_2008_2020, family = quasipoisson,
          data=Diversity_2023_2024)

check_overdispersion(m1)
summary(m1)
Anova(m1)

Diversity_2023_2024 %>% 
  ggplot(aes(x=SMId_mean_2008_2020, y=abundance)) +
  geom_jitter(width=0.01, height=0.01) +
  geom_smooth(method="glm", method.args = list(family = "quasipoisson")) + 
  #labs(x="SFOrest management", y="Abundance") +
  theme_bw()

## Formi ----------------------------
# "Inonat_2018", "Iharv_2018", "Idwcut_2018"          
# "Formi_2018"           
# "Inonat_mean_2012_2018", "Iharv_mean_2012_2018", "Idwcut_mean_2012_2018"
# "Formi_mean_2012_2018" 

m2 <- glm(abundance ~ Idwcut_mean_2012_2018, family = quasipoisson,
          data=Diversity_2023_2024)

check_overdispersion(m2)
summary(m2)
Anova(m2)

Diversity_2023_2024 %>% 
  ggplot(aes(x=Idwcut_mean_2012_2018, y=abundance)) +
  geom_jitter(width=0.01, height=0.01) +
  geom_smooth(method="glm", method.args = list(family = "quasipoisson")) + 
 # labs(x="Formi_2018", y="Abundance") +
  theme_bw()



# -> Species Richness  ------------

m2 <- glm(sp_richness ~ poly(Formi_2018,2), family = poisson,
          data=Diversity_2023_2024)

m2 <- lm(log1p(sp_richness) ~ log1p(Formi_2018),
          data=Diversity_2023_2024)

check_overdispersion(m2)
summary(m2)
60.332/53
Anova(m2)


Diversity_2023_2024 %>% 
  ggplot(aes(x=log1p(Formi_2018), y=log1p(sp_richness))) +
  geom_jitter(width=0.05, height=0.05) +
    geom_smooth(method="lm") +   
    labs(x="Formi_2018", y="Species richness") +
  theme_bw()


Diversity_2023_2024 %>% 
  ggplot(aes(x=log1p(Inonat_mean_2012_2018), y=abundance)) +
  geom_point() +
  geom_smooth(method="lm") + 
  labs(x="Formi_2018", y="Abundance") +
  theme_bw()



# Plot biodiversity potential -------------------------------------------------------------

names(Diversity_2023_2024)


response <- "abundance"
preds <- c("Tree_richness", "Vertical_structure",
           "Standing_deadwood", "Lying_deadwood",
           "Very_large_trees", "Habitat_trees",
           "Openness", "Temporal_continuity_of_the_woody_state",
           "Wet_macrohabitats", "Rocky_macrohabitats", "IBPscore")

plots <- map(preds, function(var) {
  datp <- Diversity_2023_2024 %>% 
    select(all_of(c(response, var))) %>%
    na.omit()
  ggplot(datp, aes_string(x = var, y = response)) +
    geom_jitter(width=0.01, height=0.01, pch=21, 
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


m1 <- glm(abundance ~ #factor(Openness) + 
           # Tree_richness + 
            IBPscore, family = quasipoisson,
          data=Diversity_2023_2024)
check_overdispersion(m1)
summary(m1)
Anova(m1)

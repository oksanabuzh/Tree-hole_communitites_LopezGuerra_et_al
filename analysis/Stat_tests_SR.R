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


m1 <- glm(sp_richness ~ #SMI_mean_2018_2020 + 
           DBH + 
         #   tree_heigth, family = poisson,
          data=Diversity_2023_2024)

check_overdispersion(m1)
summary(m1)
Anova(m1)

Diversity_2023_2024 %>% 
  ggplot(aes(x=wood_volume, y=sp_richness)) +
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

m1 <- glm(sp_richness ~ SMI_mean_2018_2020, family = poisson,
          data=Diversity_2023_2024)

check_overdispersion(m1)
summary(m1)
Anova(m1)

Diversity_2023_2024 %>% 
  ggplot(aes(x=SMI_mean_2018_2020, y=sp_richness)) +
  geom_jitter(width=0.01, height=0.01) +
  geom_smooth(method="glm", method.args = list(family = "poisson")) + 
  #labs(x="SFOrest management", y="Abundance") +
  theme_bw()


#  SMIr_mean_2018_2020
m1.1 <- glm(sp_richness ~ SMIr_mean_2018_2020, family = poisson,
          data=Diversity_2023_2024)

check_overdispersion(m1.1)
summary(m1.1)
Anova(m1.1)

Diversity_2023_2024 %>% 
  ggplot(aes(x=SMIr_mean_2018_2020, y=sp_richness)) +
  geom_jitter(width=0.01, height=0.01) +
  geom_smooth(method="glm", method.args = list(family = "poisson")) + 
  #labs(x="SFOrest management", y="Abundance") +
  theme_bw()


#  SMId_mean_2018_2020
m1.2 <- glm(sp_richness ~ SMId_mean_2018_2020, family = poisson,
            data=Diversity_2023_2024)

check_overdispersion(m1.2)
summary(m1.2)
Anova(m1.2)

Diversity_2023_2024 %>% 
  ggplot(aes(x=SMId_mean_2018_2020, y=sp_richness)) +
  geom_jitter(width=0.01, height=0.01) +
  geom_smooth(method="glm", method.args = list(family = "poisson")) + 
  #labs(x="SFOrest management", y="Abundance") +
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
  geom_jitter(width=0.01, height=0.01) +
  geom_smooth(method="glm", method.args = list(family = "poisson")) + 
 # labs(x="Formi_2018", y="Abundance") +
  theme_bw()



# Inonat_2018
m2.1 <- glm(sp_richness ~ Inonat_2018, family = poisson,
          data=Diversity_2023_2024)

check_overdispersion(m2.1)
summary(m2.1)
Anova(m2.1)

Diversity_2023_2024 %>% 
  ggplot(aes(x=Inonat_2018, y=sp_richness)) +
  geom_jitter(width=0.01, height=0.01) +
  geom_smooth(method="glm", method.args = list(family = "poisson")) + 
  # labs(x="Formi_2018", y="Abundance") +
  theme_bw()


# Idwcut_2018
m2.3 <- glm(sp_richness ~ Idwcut_2018, family = poisson,
            data=Diversity_2023_2024)

check_overdispersion(m2.3)
summary(m2.3)
Anova(m2.3)

Diversity_2023_2024 %>% 
  ggplot(aes(x=Idwcut_2018, y=sp_richness)) +
  geom_jitter(width=0.01, height=0.01) +
  geom_smooth(method="glm", method.args = list(family = "poisson")) + 
  # labs(x="Formi_2018", y="Abundance") +
  theme_bw()


# Iharv_2018
m2.2 <- glm(sp_richness ~ Iharv_2018, family = poisson,
            data=Diversity_2023_2024)

check_overdispersion(m2.2)
summary(m2.2)
Anova(m2.2)

Diversity_2023_2024 %>% 
  ggplot(aes(x=Iharv_2018, y=sp_richness)) +
  geom_jitter(width=0.01, height=0.01) +
  geom_smooth(method="glm", method.args = list(family = "poisson")) + 
  # labs(x="Formi_2018", y="Abundance") +
  theme_bw()

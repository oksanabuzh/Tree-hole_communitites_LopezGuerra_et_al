#   Analysis of the drivers of tree-hole insect biomass_dry_mg in 2023-2024f 

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
library(lme4)
library(lmerTest)
library(effects)

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
  ) %>% 
  left_join(landscape_heterogeneity %>% 
              filter(buffer_size_m==500),
            by = c("Plot" = "plotID")) %>% 
  #  mutate(Inonat_mean_2012_2018 =Inonat_2018,
  #    Iharv_mean_2012_2018 = Iharv_2018,
  #      Idwcut_mean_2012_2018 = Idwcut_2018,
  #     Formi_mean_2012_2018 = Formi_2018) %>%
  mutate(Inonat_mean_tr =Inonat_mean_2012_2018^0.3) %>% 
  mutate(biomass_dry_mg_log = log1p(biomass_dry_mg)) 



str(Diversity_2023_2024)



hole_type_color <- (c("rot"="brown", 
                      "pan"="#14724C"))



# 3) Biomass --------------------------------------------------------------------

## Test random effects: --------------------------------------------------------

m1a <- lmerTest::lmer(biomass_dry_mg_log ~  
                        Formi_mean_2012_2018 + 
                        Forest_percent + 
                        Tree_sp_richness + 
                        precipitation_radolan_mean + (1|Plot),
                      data=Diversity_2023_2024)

ranova(m1a)

# remove random effects

## Test fixed effects: ---------------------------------------------------------


m1_mass <- lm(biomass_dry_mg_log ~  
             Formi_mean_2012_2018 + 
               Forest_percent + 
             Tree_sp_richness + 
               precipitation_radolan_mean,
           data=Diversity_2023_2024)

par(mfrow=c(2,2))
plot(m1_mass)
par(mfrow=c(1,1))

check_collinearity(m1_mass)
Anova(m1_mass)


m2_mass <- lm(biomass_dry_mg_log ~ 
                Inonat_mean_tr + 
                Iharv_mean_2012_2018 + 
                Idwcut_mean_2012_2018 +
                Forest_percent + 
                precipitation_radolan_mean,
              data=Diversity_2023_2024)


check_collinearity(m2_mass)
Anova(m2_mass)

m2b_mass <- lm(biomass_dry_mg_log ~ 
              #   Inonat_mean_tr + 
                 Iharv_mean_2012_2018 + 
                 Idwcut_mean_2012_2018 +
                 Forest_percent + 
                 precipitation_radolan_mean,
               data=Diversity_2023_2024)


anova(m2_mass, m2b_mass)
# keep Inonat_mean_tr

# test other land use types
m3_mass <- lm(biomass_dry_mg_log ~  
                Formi_mean_2012_2018 + 
               Forest_percent + Urban_percent + Agricultural_percent,
           data=Diversity_2023_2024)

check_collinearity(m3_mass)
Anova(m3_mass)

# test forest structure
m4_mass <- lm(biomass_dry_mg_log ~  
                Formi_mean_2012_2018 + 
                ssci +  
                Openness +
                Vertical_structure,
              data=Diversity_2023_2024)

check_collinearity(m4_mass)
Anova(m4_mass)


# test landscape heterogeneity 
m5_mass <- lm(biomass_dry_mg_log ~   
               Formi_mean_2012_2018 +
               log1p(LandType_richness_class_2),
             data=Diversity_2023_2024)

check_collinearity(m5_mass)
Anova(m5_mass)


## Plots: --------------------------
### Forest management: ------------------------------------
#### Harvested tree biomass: Iharv_mean_2012_2018 ----------------------------------------------------------
rng_Iharv <- range(Diversity_2023_2024$Iharv_mean_2012_2018, na.rm = TRUE)
rng_Iharv

m1a_SR_Iharv_perc <- Effect("Iharv_mean_2012_2018", m2_mass,
                            xlevels = list(Iharv_mean_2012_2018 = seq(rng_Iharv[1]-0.01, 
                                                                      rng_Iharv[2], 
                                                                      by=0.00001))) %>% 
  as.data.frame()



ggplot(m1a_SR_Iharv_perc, aes(x = Iharv_mean_2012_2018, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill  = "#86BBD8") +
  geom_point(data=Diversity_2023_2024, 
             aes(y=biomass_dry_mg_log), 
             pch=21, size=1.5, alpha=0.6, stroke = 0.8, color="brown", fill="#FFA55B",
             position = position_jitter(width = 0.005, height = 0)) +
  geom_line(linewidth = 1,  color = "#086096") +
  theme_bw() +
  scale_y_continuous(breaks = c(0, 1, 2), 
                     labels =  function(x) paste0(round(exp(x) - 1, 1))) +
 # scale_y_continuous(breaks = seq(0, 16, by = 4)) +
  labs(x = "Harvested tree biomass",  y = "Biomass, g") 




#### Dead wood with saw cuts ----------------------------------------------------------
rng_Iharv <- range(Diversity_2023_2024$Idwcut_mean_2012_2018, na.rm = TRUE)
rng_Iharv

m1a_SR_Idwcut_perc <- Effect("Idwcut_mean_2012_2018", m2_mass,
                             xlevels = list(Idwcut_mean_2012_2018 = seq(rng_Iharv[1]-0.01, 
                                                                        rng_Iharv[2],
                                                                        by = 0.001))) %>% 
  as.data.frame()


ggplot(m1a_SR_Idwcut_perc, aes(x = Idwcut_mean_2012_2018, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill  = "#86BBD8") +
  geom_point(data=Diversity_2023_2024, 
             aes(y=biomass_dry_mg_log), 
             pch=21, size=1.5, alpha=0.6, stroke = 0.8, color="brown", fill="#FFA55B",
             position = position_jitter(width = 0.05, height = 0)) +
  geom_line(linewidth = 1,  color = "#086096") +
  theme_bw() +
  scale_y_continuous(breaks = c(0, 1, 2), 
                     labels =  function(x) paste0(round(exp(x) - 1, 1))) +
  labs(x = "Dead wood with saw cuts",  y = "Biomass, g") 


#### Non-natural tree species ----------------------------------------------------------
rng_Inonat <- range(Diversity_2023_2024$Inonat_mean_tr, na.rm = TRUE)
rng_Inonat

m1_2_SR_Inonat_perc <- Effect("Inonat_mean_tr", m2_mass,
                              xlevels = list(Inonat_mean_tr = seq(rng_Inonat[1]-0.01, 
                                                                  rng_Inonat[2]+0.1, 
                                                                  by = 0.001))) %>% 
  as.data.frame()


ggplot(m1_2_SR_Inonat_perc, aes(x = Inonat_mean_tr, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill  = "#86BBD8") +
  geom_point(data=Diversity_2023_2024%>%
               mutate(Inonat_mean_tr =Inonat_mean_2012_2018^0.2), 
             aes(y=biomass_dry_mg_log), 
             pch=21, size=1.5, alpha=0.6, stroke = 0.8, color="brown", fill="#FFA55B",
             position = position_jitter(width = 0.1, height = 0.05)) +
  geom_line(linewidth = 1,  color = "#086096") +
  theme_bw() +
    scale_y_continuous(breaks = seq(0, 3, by = 1),
                       labels =  function(x) paste0(round(exp(x) - 1, 1))) +
  labs(x = "Non-natural tree species",  y = "Biomass, g") 



#### Forest Management Intensity ----------------------------------------------------------
rng_FMI <- range(Diversity_2023_2024$Formi_mean_2012_2018, na.rm = TRUE)
rng_FMI

m1_3_SR_FMI_perc <- Effect("Formi_mean_2012_2018", m1_mass,
                           xlevels=list(Formi_mean_2012_2018=seq(0.09, 1.6, 
                                                                 by=0.001))) %>% 
  as.data.frame()


ggplot(m1_3_SR_FMI_perc, aes(x = Formi_mean_2012_2018, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill  = "#86BBD8") +
  geom_point(data=Diversity_2023_2024, 
             aes(y=biomass_dry_mg_log), 
             pch=21, size=1.5, alpha=0.6, stroke = 0.8, color="brown", fill="#FFA55B",
             position = position_jitter(width = 0.01, height = 0.1)) +
  geom_line(linewidth = 1,  color = "#086096") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 3, by = 1),
                     labels =  function(x) paste0(round(exp(x) - 1, 1))) +
  labs(x = "Forest management intensity",  y = "Biomass, g") 


### Landscape: ----------------------------------------------------
#### Landscape heterogeneity: -----------------------------------------------------

rng_Land <- range(Diversity_2023_2024$LandType_richness_class_2, na.rm = TRUE)
rng_Land

m1a_SR_Land_perc <- Effect("LandType_richness_class_2", m1_mass,
                           xlevels = list(LandType_richness_class_2  = 
                                            seq(rng_Land[1]-1, 
                                                rng_Land[2]+1, 
                                                by = 0.001))) %>% 
  as.data.frame()


ggplot(m1a_SR_Land_perc, aes(x = LandType_richness_class_2, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill  = "#86BBD8") +
  geom_point(data=Diversity_2023_2024, # %>% filter(biomass_dry_mg_log<9),
             aes(y=biomass_dry_mg_log), 
             pch=21, size=1.5, alpha=0.6, stroke = 0.8, color="brown", fill="#FFA55B",
             position = position_jitter(width = 0.3, height = 0)) +
  geom_line(linewidth = 1,  color = "#086096") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 3, by = 1),
                     labels =  function(x) paste0(round(exp(x) - 1, 1))) +
  labs(x = "Landscape heterogeneity",  y = "Biomass, g") 



#### Forest cover ----------------------------------------------------------
rng_Forest <- range(Diversity_2023_2024$Forest_percent, na.rm = TRUE)
rng_Forest

m1a_SR_Forest_perc <- Effect("Forest_percent", m3_mass,
                             xlevels = list(Forest_percent = seq(rng_Forest[1]-1, rng_Forest[2]+1, by = 0.001))) %>% 
  as.data.frame()


ggplot(m1a_SR_Forest_perc, aes(x = Forest_percent, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill  = "#86BBD8") +
  geom_point(data=Diversity_2023_2024, # %>% filter(biomass_dry_mg_log<9),
             aes(y=biomass_dry_mg_log), 
             pch=21, size=1.5, alpha=0.6, stroke = 0.8, color="brown", fill="#FFA55B",
             position = position_jitter(width = 0.4, height = 0)) +
  geom_line(linewidth = 1,  color = "#086096") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 3, by = 1),
                     labels =  function(x) paste0(round(exp(x) - 1, 1))) +
  labs(x = "Forest cover, %",  y = "Biomass, g") 



#### Agricultural lands cover ----------------------------------------------------------
rng_Agric <- range(Diversity_2023_2024$Agricultural_percent, na.rm = TRUE)
rng_Agric

m1_SR_Agric_perc <- Effect("Agricultural_percent", m3_mass,
                           xlevels = list(Agricultural_percent = 
                                            seq(rng_Agric[1]-0.2, 
                                                rng_Agric[2]+0.4, 
                                                by = 0.001))) %>% 
  as.data.frame()


ggplot(m1_SR_Agric_perc, aes(x = log1p(Agricultural_percent), y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill  = "#86BBD8") +
  geom_point(data=Diversity_2023_2024, # %>% filter(biomass_dry_mg_log<9),
             aes(y=biomass_dry_mg_log), 
             pch=21, size=1.5, alpha=0.6, stroke = 0.8, color="brown", fill="#FFA55B",
             position = position_jitter(width = 0.2, height = 0)) +
  geom_line(linewidth = 1,  color = "#086096") +
  theme_bw() +
  scale_x_continuous(breaks = c(0, 1, 2, 3), 
                     labels =  function(x) paste0(round(exp(x) - 1), "")) +
  scale_y_continuous(breaks = seq(0, 3, by = 1),
                     labels =  function(x) paste0(round(exp(x) - 1, 1))) +
  labs(x = "Agricultural lands cover",  y = "Biomass, g") 



#### Urban lands cover ----------------------------------------------------------
rng_Urb <- range(Diversity_2023_2024$Urban_percent, na.rm = TRUE)
rng_Urb

m1_SR_Urb_perc <- Effect("Urban_percent", m3_mass,
                           xlevels = list(Urban_percent = 
                                            seq(rng_Urb[1]-0.2, 
                                                rng_Urb[2]+0.4, 
                                                by = 0.001))) %>% 
  as.data.frame()


ggplot(m1_SR_Urb_perc, aes(x = log1p(Urban_percent), y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill  = "#86BBD8") +
  geom_point(data=Diversity_2023_2024, # %>% filter(biomass_dry_mg_log<9),
             aes(y=biomass_dry_mg_log), 
             pch=21, size=1.5, alpha=0.6, stroke = 0.8, color="brown", fill="#FFA55B",
             position = position_jitter(width = 0.2, height = 0)) +
  geom_line(linewidth = 1,  color = "#086096") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 3, by = 1),
                     labels =  function(x) paste0(round(exp(x) - 1, 1))) +
  labs(x = "Urban lands cover, %",  y = "Biomass, g") 



### Forest structure -------------------------------------------------
#### Stand structural complexity  --------------

rng_ssci <- range(Diversity_2023_2024$ssci, na.rm = TRUE)
rng_ssci

m1_SR_ssci_perc <- Effect("ssci", m4_mass,
                         xlevels = list(ssci = 
                                          seq(rng_ssci[1]-0.1, 
                                              rng_ssci[2]+0.4, 
                                              by = 0.001))) %>% 
  as.data.frame()


ggplot(m1_SR_ssci_perc, aes(x = log1p(ssci), y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill  = "#86BBD8") +
  geom_point(data=Diversity_2023_2024, # %>% filter(biomass_dry_mg_log<9),
             aes(y=biomass_dry_mg_log), 
             pch=21, size=1.5, alpha=0.6, stroke = 0.8, color="brown", fill="#FFA55B",
             position = position_jitter(width = 0.1, height = 0)) +
  geom_line(linewidth = 1,  color = "#086096") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 3, by = 1),
                     labels =  function(x) paste0(round(exp(x) - 1, 1))) +
  labs(x = "Stand structural complexity",  y = "Biomass, g") 



#### Oppenness -------------------------------

emmeans_oppenness<- cld(emmeans(m4_mass, list(pairwise ~ Openness)), 
                        Letters = letters) %>% 
  arrange(Openness)



Diversity_2023_2024 %>%
  ggplot(aes(x=Openness, y=biomass_dry_mg_log)) +
  geom_boxplot(outlier.shape = NA,  color = "#086096") +
  geom_jitter(width=0.1, height=0.05, size=2,
              pch=21, color="brown", fill="#FFA55B") +
  #  geom_smooth(method="glm", method.args = list(family = "poisson"),
  #              color = "#086096",fill  = "#86BBD8") + 
  geom_text(data=emmeans_oppenness,
            aes(x=Openness, y=c(3, 3, 2.5),
                label=emmeans_oppenness$.group),
            size=4, col="black") +
  scale_y_continuous(breaks = seq(0, 3, by = 1),
                     labels =  function(x) paste0(round(exp(x) - 1, 1))) +
  labs(
    x = "Open areas, % ha⁻¹",
    y="Biomass, g") +
  theme_bw()


#### Number of vegetation layers ------------------------------------------------------------
emmeans_veg_leyers<- cld(emmeans(m4_mass, list(pairwise ~ Vertical_structure)), 
                         Letters = letters) %>% 
  arrange(Vertical_structure)

Diversity_2023_2024 %>%
  select(Plot, Tree_ID, Habitat_trees) %>% 
  print(n=Inf)

Diversity_2023_2024 %>%
  ggplot(aes(x=Vertical_structure, y=biomass_dry_mg_log)) +
  geom_boxplot(outlier.shape = NA,  color = "#086096") +
  geom_jitter(width=0.1, height=0, size=2,
              pch=21, color="brown", fill="#FFA55B") +
  geom_smooth(method="glm", method.args = list(family = "poisson"),
              color = "#086096",fill  = "#86BBD8") + 
  geom_text(data=emmeans_veg_leyers,
            aes(x=Vertical_structure , y=c(0.5, 2.7, 2.97),
                label=emmeans_veg_leyers$.group),
            size=4, col="black") +
  scale_y_continuous(breaks = seq(0, 3, by = 1),
                     labels =  function(x) paste0(round(exp(x) - 1, 1))) +
  labs(
    x = "Number of vegetation layers",
    y="Biomass, g") +
  theme_bw()


### Tree species richness  ----------------------------------------------------------
rng_Tree_SR <- range(Diversity_2023_2024$Tree_sp_richness, na.rm = TRUE)
rng_Tree_SR

m1a_SR_Tree_SR_perc <- Effect("Tree_sp_richness", m1_mass,
                              xlevels = list(Tree_sp_richness = seq(rng_Tree_SR[1]-1, 
                                                                    rng_Tree_SR[2], 
                                                                    by=0.01))) %>% 
  as.data.frame()



ggplot(m1a_SR_Tree_SR_perc, aes(x = Tree_sp_richness, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill  = "#86BBD8") +
  geom_point(data=Diversity_2023_2024, 
             aes(y=biomass_dry_mg_log), 
             pch=21, size=1.5, alpha=0.6, stroke = 0.8, color="brown", fill="#FFA55B",
             position = position_jitter(width = 0.3, height = 0)) +
  geom_line(linewidth = 1,  color = "#086096") +
  theme_bw() +
  scale_y_continuous(breaks = c(0, 1, 2), 
                     labels =  function(x) paste0(round(exp(x) - 1, 1))) +
  # scale_y_continuous(breaks = seq(0, 16, by = 4)) +
  labs(x = "Tree species richness",  y = "Biomass, g") 





# END --------------------------------------------------------------------------


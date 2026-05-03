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
  mutate(Inonat_mean_tr =Inonat_mean_2012_2018^0.5) %>% 
  mutate(abundance_log = log1p(abundance)) 



str(Diversity_2023_2024)


hole_type_color <- (c("rot"="brown", 
                      "pan"="#14724C"))



# Abundance --------------------------------------------------------------------

## Test random effects: --------------------------------------------------------


m0_Abund <-  glmer(abundance ~   
                   Inonat_mean_tr + 
                   Iharv_mean_2012_2018 + 
                   Idwcut_mean_2012_2018 +
                   Forest_percent + 
                   precipitation_radolan_mean+
                     (1|Plot),
                 family = poisson, 
                 data=Diversity_2023_2024)
check_convergence(m0_Abund)

m0b_Abund <-  glm(abundance ~   
                    Inonat_mean_tr + 
                    Iharv_mean_2012_2018 + 
                    Idwcut_mean_2012_2018 +
                    Forest_percent + 
                    precipitation_radolan_mean,
                  family = poisson, 
                  data=Diversity_2023_2024)


anova(m0_Abund, m0b_Abund)
# remove random effects

check_overdispersion(m0b_Abund)
# overdispersed



## Test fixed effects: ---------------------------------------------------------

m1_Abund <- glm(abundance ~  
               log1p(Formi_mean_2012_2018) + 
               Forest_percent +
               Tree_sp_richness,
             family = quasipoisson, 
             data=Diversity_2023_2024%>% 
               filter(sp_richness<9)) # remove one outlier

check_collinearity(m1_Abund)
Anova(m1_Abund)

# test components of Formi
m2_Abund <- glm(abundance ~   
               Inonat_mean_tr + 
               Iharv_mean_2012_2018 + 
               Idwcut_mean_2012_2018 +
               Forest_percent + 
               precipitation_radolan_mean,
             family = quasipoisson, 
             data=Diversity_2023_2024)


check_collinearity(m2_Abund)
Anova(m2_Abund)



# test other land use types
m3_Abund <- glm(abundance ~  
               Formi_mean_2012_2018 + 
               Forest_percent + Urban_percent + log1p(Agricultural_percent),
             family = quasipoisson,
             data=Diversity_2023_2024)

check_collinearity(m3_Abund)
Anova(m3_Abund)


# test forest structure
m4_Abund <- glm(abundance ~  
               Formi_mean_2012_2018 + 
               ssci +  
               Openness +
                 precipitation_radolan_mean, # + Vertical_structure,
             family = quasipoisson,
             data=Diversity_2023_2024)

check_collinearity(m4_Abund)
Anova(m4_Abund)

# test landscape heterogeneity 
m5_Abund <- glm(abundance ~   
               Formi_mean_2012_2018 +
               log1p(LandType_richness_class_2),
             family = quasipoisson, 
             data=Diversity_2023_2024)

check_collinearity(m5_Abund)
Anova(m5_Abund)




## Plots: --------------------------
### Forest management: ------------------------------------
#### Harvested tree biomass: Iharv_mean_2012_2018 ----------------------------------------------------------
rng_Iharv <- range(Diversity_2023_2024$Iharv_mean_2012_2018, na.rm = TRUE)
rng_Iharv

m1a_Abund_Iharv_perc <- Effect("Iharv_mean_2012_2018", m2_Abund,
                            xlevels = list(Iharv_mean_2012_2018 = seq(rng_Iharv[1]-0.01, 
                                                                      rng_Iharv[2], 
                                                                      by=0.00001))) %>% 
  as.data.frame()



ggplot(m1a_Abund_Iharv_perc, aes(x = Iharv_mean_2012_2018, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill  = "#86BBD8") +
  geom_point(data=Diversity_2023_2024, 
             aes(y=abundance), 
             pch=21, size=1.5, alpha=0.6, stroke = 0.8, color="brown", fill="#FFA55B",
             position = position_jitter(width = 0.005, height = 0.4)) +
  geom_line(linewidth = 1,  color = "#086096") +
  theme_bw() +
 # scale_y_continuous(breaks = seq(0, 16, by = 4)) +
  labs(x = "Harvested tree biomass",  y = "Abundances") 




#### Dead wood with saw cuts ----------------------------------------------------------
rng_Iharv <- range(Diversity_2023_2024$Idwcut_mean_2012_2018, na.rm = TRUE)
rng_Iharv

m1a_Abund_Idwcut_perc <- Effect("Idwcut_mean_2012_2018", m2_Abund,
                             xlevels = list(Idwcut_mean_2012_2018 = seq(rng_Iharv[1]-0.01, 
                                                                        rng_Iharv[2],
                                                                        by = 0.001))) %>% 
  as.data.frame()


ggplot(m1a_Abund_Idwcut_perc, aes(x = Idwcut_mean_2012_2018, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill  = "#86BBD8") +
  geom_point(data=Diversity_2023_2024, 
             aes(y=abundance), 
             pch=21, size=1.5, alpha=0.6, stroke = 0.8, color="brown", fill="#FFA55B",
             position = position_jitter(width = 0.01, height = 0.4)) +
  geom_line(linewidth = 1,  color = "#086096") +
  theme_bw() +
  labs(x = "Dead wood with saw cuts",  y = "Abundances") 


#### Non-natural tree species ----------------------------------------------------------
rng_Inonat <- range(Diversity_2023_2024$Inonat_mean_tr, na.rm = TRUE)
rng_Inonat

m1_2_Abund_Inonat_perc <- Effect("Inonat_mean_tr", m2_Abund,
                              xlevels = list(Inonat_mean_tr = seq(rng_Inonat[1]-0.01, 
                                                                  rng_Inonat[2]+0.1, 
                                                                  by = 0.001))) %>% 
  as.data.frame()


ggplot(m1_2_Abund_Inonat_perc, aes(x = Inonat_mean_tr, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill  = "#86BBD8") +
  geom_point(data=Diversity_2023_2024, 
             aes(y=abundance), 
             pch=21, size=1.5, alpha=0.6, stroke = 0.8, color="brown", fill="#FFA55B",
             position = position_jitter(width = 0.01, height = 0.4)) +
  geom_line(linewidth = 1,  color = "#086096") +
  theme_bw() +
  labs(x = "Non-natural tree species",  y = "Abundances") 


#### Forest Management Intensity ----------------------------------------------------------
rng_FMI <- range(Diversity_2023_2024$Formi_mean_2012_2018, na.rm = TRUE)
rng_FMI

m1_3_Abund_FMI_perc <- Effect("Formi_mean_2012_2018", m1_Abund,
                           xlevels=list(Formi_mean_2012_2018=seq(0.09, 1.6, 
                                                                 by=0.001))) %>% 
  as.data.frame()


ggplot(m1_3_Abund_FMI_perc, aes(x = Formi_mean_2012_2018, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill  = "#86BBD8") +
  geom_point(data=Diversity_2023_2024, 
             aes(y=abundance), 
             pch=21, size=1.5, alpha=0.6, stroke = 0.8, color="brown", fill="#FFA55B",
             position = position_jitter(width = 0.01, height = 0.4)) +
  geom_line(linewidth = 1,  color = "#086096") +
  theme_bw() +
  labs(x = "Forest management intensity",  y = "Abundances") 


### Landscape: ----------------------------------------------------
#### Landscape heterogeneity: -----------------------------------------------------

rng_Land <- range(Diversity_2023_2024$LandType_richness_class_2, na.rm = TRUE)
rng_Land

m1a_Abund_Land_perc <- Effect("LandType_richness_class_2", m5_Abund,
                           xlevels = list(LandType_richness_class_2  = 
                                            seq(rng_Land[1]-1, 
                                                rng_Land[2]+1, 
                                                by = 0.001))) %>% 
  as.data.frame()


ggplot(m1a_Abund_Land_perc, aes(x = LandType_richness_class_2, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill  = "#86BBD8") +
  geom_point(data=Diversity_2023_2024, # %>% filter(abundance<9),
             aes(y=abundance), 
             pch=21, size=1.5, alpha=0.6, stroke = 0.8, color="brown", fill="#FFA55B",
             position = position_jitter(width = 0.3, height = 0.3)) +
  geom_line(linewidth = 1,  color = "#086096") +
  theme_bw() +
  labs(x = "Landscape heterogeneity",  y = "Abundances") 



#### Forest cover ----------------------------------------------------------
rng_Forest <- range(Diversity_2023_2024$Forest_percent, na.rm = TRUE)
rng_Forest

m1a_Abund_Forest_perc <- Effect("Forest_percent", m2_Abund,
                             xlevels = list(Forest_percent = seq(rng_Forest[1]-1, rng_Forest[2]+1, by = 0.001))) %>% 
  as.data.frame()


ggplot(m1a_Abund_Forest_perc, aes(x = Forest_percent, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill  = "#86BBD8") +
  geom_point(data=Diversity_2023_2024, # %>% filter(abundance<9),
             aes(y=abundance), 
             pch=21, size=1.5, alpha=0.6, stroke = 0.8, color="brown", fill="#FFA55B",
             position = position_jitter(width = 0.3, height = 0.3)) +
  geom_line(linewidth = 1,  color = "#086096") +
  theme_bw() +
  labs(x = "Forest cover, %",  y = "Abundances") 



#### Agricultural lands cover ----------------------------------------------------------
rng_Agric <- range(Diversity_2023_2024$Agricultural_percent, na.rm = TRUE)
rng_Agric

m1_Abund_Agric_perc <- Effect("Agricultural_percent", m3_Abund,
                           xlevels = list(Agricultural_percent = 
                                            seq(rng_Agric[1]-0.2, 
                                                rng_Agric[2]+0.4, 
                                                by = 0.001))) %>% 
  as.data.frame()


ggplot(m1_Abund_Agric_perc, aes(x = log1p(Agricultural_percent), y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill  = "#86BBD8") +
  geom_point(data=Diversity_2023_2024, # %>% filter(abundance<9),
             aes(y=abundance), 
             pch=21, size=1.5, alpha=0.6, stroke = 0.8, color="brown", fill="#FFA55B",
             position = position_jitter(width = 0.2, height = 0.3)) +
  geom_line(linewidth = 1,  color = "#086096") +
  theme_bw() +
  scale_x_continuous(breaks = c(0, 1, 2, 3), 
                     labels =  function(x) paste0(round(exp(x) - 1), "")) +
  labs(x = "Agricultural lands cover",  y = "Abundances") 



#### Urban lands cover ----------------------------------------------------------
rng_Urb <- range(Diversity_2023_2024$Urban_percent, na.rm = TRUE)
rng_Urb

m1_Abund_Urb_perc <- Effect("Urban_percent", m3_Abund,
                           xlevels = list(Urban_percent = 
                                            seq(rng_Urb[1]-0.2, 
                                                rng_Urb[2]+0.4, 
                                                by = 0.001))) %>% 
  as.data.frame()


ggplot(m1_Abund_Urb_perc, aes(x = log1p(Urban_percent), y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill  = "#86BBD8") +
  geom_point(data=Diversity_2023_2024, # %>% filter(abundance<9),
             aes(y=abundance), 
             pch=21, size=1.5, alpha=0.6, stroke = 0.8, color="brown", fill="#FFA55B",
             position = position_jitter(width = 0.2, height = 0.3)) +
  geom_line(linewidth = 1,  color = "#086096") +
  theme_bw() +
 # scale_x_continuous(breaks = c(0, 1, 2, 3), 
 #                    labels =  function(x) paste0(round(exp(x) - 1), "%")) +
  labs(x = "Urban lands cover, %",  y = "Abundances") 



### Forest structure -------------------------------------------------
#### Stand structural complexity  --------------

rng_ssci <- range(Diversity_2023_2024$ssci, na.rm = TRUE)
rng_ssci

m1_Abund_ssci_perc <- Effect("ssci", m4_Abund,
                         xlevels = list(ssci = 
                                          seq(rng_ssci[1]-0.1, 
                                              rng_ssci[2]+0.4, 
                                              by = 0.001))) %>% 
  as.data.frame()


ggplot(m1_Abund_ssci_perc, aes(x = log1p(ssci), y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill  = "#86BBD8") +
  geom_point(data=Diversity_2023_2024, # %>% filter(abundance<9),
             aes(y=abundance), 
             pch=21, size=1.5, alpha=0.6, stroke = 0.8, color="brown", fill="#FFA55B",
             position = position_jitter(width = 0, height = 0)) +
  geom_line(linewidth = 1,  color = "#086096") +
  theme_bw() +
  # scale_x_continuous(breaks = c(0, 1, 2, 3), 
  #                    labels =  function(x) paste0(round(exp(x) - 1), "%")) +
  labs(x = "Stand structural complexity",  y = "Abundances") 



#### Oppenness -------------------------------

emmeans_oppenness<- cld(emmeans(m4_Abund, list(pairwise ~ Openness)), 
                        Letters = letters) %>% 
  arrange(Openness) %>% 
  mutate(.group = ifelse(Openness == "0%", "b", .group))# marginally significant



Diversity_2023_2024 %>%
  ggplot(aes(x=Openness, y=abundance)) +
  geom_boxplot(outlier.shape = NA,  color = "#086096") +
  geom_jitter(width=0.1, height=0.05, size=2,
              pch=21, color="brown", fill="#FFA55B") +
  #  geom_smooth(method="glm", method.args = list(family = "poisson"),
  #              color = "#086096",fill  = "#86BBD8") + 
  geom_text(data=emmeans_oppenness,
            aes(x=Openness, y=c(65, 60, 25),
                label=emmeans_oppenness$.group),
            size=4, col="black") +
  labs(
    x = "Open areas, % ha⁻¹",
    y="Abundance") +
  theme_bw()


### Tree species richness  ----------------------------------------------------------
rng_Tree_SR <- range(Diversity_2023_2024$Tree_sp_richness, na.rm = TRUE)
rng_Tree_SR

m1a_abund_Tree_SR_perc <- Effect("Tree_sp_richness", m1_Abund,
                              xlevels = list(Tree_sp_richness = seq(rng_Tree_SR[1]-1, 
                                                                    rng_Tree_SR[2], 
                                                                    by=0.01))) %>% 
  as.data.frame()



ggplot(m1a_abund_Tree_SR_perc, aes(x = Tree_sp_richness, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill  = "#86BBD8") +
  geom_point(data=Diversity_2023_2024, 
             aes(y=abundance), 
             pch=21, size=1.5, alpha=0.6, stroke = 0.8, color="brown", fill="#FFA55B",
             position = position_jitter(width = 0.3, height = 0)) +
  geom_line(linewidth = 1,  color = "#086096") +
  theme_bw() +
  labs(x = "Tree species richness",  y = "Abundance") 




# END --------------------------------------------------------------------------

################################################################################





m1 <- glmer(abundance ~  
           # Inonat_mean_2012_2018 +
            Iharv_mean_2012_2018 + 
            Idwcut_mean_2012_2018 + 
            #Formi_mean_2012_2018 +
           # LandType_richness_class_2 , 
           Forest_percent + #Agricultural_percent  +   Urban_percent +
            # Tree_abundance + 
          #  Tree_abundance +
          #  ssci + # +  Openness  , 
         #   Vertical_structure + Standing_deadwood +
          precipitation_radolan_mean +
           (1|Plot),
            family = poisson,
          data=Diversity_2023_2024)


check_collinearity(m1)
Anova(m1)
check_overdispersion(m1)
 #check_model(m1)


anova(m1, m1a)






m1 <- lm(log1p(abundance) ~ 
           Formi_mean_2012_2018 + Agricultural_percent + ssci + Openness + 
            Vertical_structure,
       #   family = quasipoisson,
          data=Diversity_2023_2024)

check_collinearity(m1)
# check_overdispersion(m1)

check_model(m1)


Anova(m1)

m1 <- lm(log1p(abundance) ~ 
         #  Inonat_mean_2012_2018 + Iharv_mean_2012_2018 + Idwcut_mean_2012_2018 + 
         #   Formi_mean_2012_2018 +
         #  LandType_richness_class_2 + 
            Forest_percent + Agricultural_percent  + #  Urban_percent +
           # Tree_abundance + 
           Tree_abundance +
            ssci + # +  Openness  , 
            Vertical_structure + Standing_deadwood +
            precipitation_radolan_mean,
      #    family = poisson,
          data=Diversity_2023_2024)

m1 <- lm(log1p(abundance) ~ 
           #  Inonat_mean_2012_2018 + Iharv_mean_2012_2018 + Idwcut_mean_2012_2018 + 
            #  Formi_mean_2012_2018 +
           #  LandType_richness_class_2 + 
         #  Forest_percent + Agricultural_percent  + #  Urban_percent +
           # Tree_abundance + 
          # Tree_abundance ,
        #  ssci + 
           Openness , 
        #  Vertical_structure + Standing_deadwood +
        #   precipitation_radolan_mean,
         #    family = poisson,
         data=Diversity_2023_2024)

step(m1)

check_collinearity(m1)
# check_overdispersion(m1)
# check_model(m1)


Anova(m1)

m2 <- lm(log1p(abundance) ~ 
           Inonat_mean_2012_2018 + Iharv_mean_2012_2018 + Idwcut_mean_2012_2018 + 
           #  Formi_mean_2012_2018 +
           # LandType_richness_class_2 + 
           Forest_percent + # Agricultural_percent  + # Urban_percent +
           # Tree_abundance + 
           Tree_abundance +
           #  ssci + #+  Openness  , 
           #   Vertical_structure + Standing_deadwood +
           precipitation_radolan_mean,
         #    family = poisson,
         data=Diversity_2023_2024)


m1 <- lm(biomass_dry_mg^0.3 ~ 
           #  Inonat_mean_2012_2018 + Iharv_mean_2012_2018 + Idwcut_mean_2012_2018 , 
             Formi_mean_2012_2018 ,
           # LandType_richness_class_2 + 
        #   Forest_percent + # Agricultural_percent  + # Urban_percent +
           # Tree_abundance + 
        #   Tree_abundance +
           #  ssci + #+  Openness  , 
           #   Vertical_structure + Standing_deadwood +
       #    precipitation_radolan_mean,
         #    family = poisson,
         data=Diversity_2023_2024)

step(m1)

check_collinearity(m1)
# check_overdispersion(m1)

check_model(m1)
par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(1,1))  

Anova(m1)

# Tree properties------------------------------------------------------

m1 <- glm(abundance ~ #SMI_mean_2018_2020 + 
           # DBH, 
           tree_heigth, 
            family = quasipoisson,
            data=Diversity_2023_2024)

check_overdispersion(m1)
summary(m1)
Anova(m1)

Diversity_2023_2024 %>% 
  ggplot(aes(x=DBH, y=abundance)) +
  geom_jitter(width=0.01, height=0.01, pch=21, 
              color="brown", fill="#FFA55B") +
  geom_smooth(method="glm", method.args = list(family = "quasipoisson"),
              color = "#086096",fill  = "#86BBD8") + 
  labs(x="DBH, cm", y="Abundance") +
  theme_bw()



Diversity_2023_2024 %>% 
  ggplot(aes(x=tree_heigth, y=abundance)) +
  geom_jitter(width=0.01, height=0.01, pch=21, 
              color="brown", fill="#FFA55B") +
  geom_smooth(method="glm", method.args = list(family = "quasipoisson"),
              color = "#086096",fill  = "#86BBD8") + 
  labs(x="Tree height, m", y="Abundance") +
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


# Formi_2018
m2 <- glm(abundance ~ Formi_2018, family = quasipoisson,
          data=Diversity_2023_2024)


check_overdispersion(m2)
summary(m2)
Anova(m2)

Diversity_2023_2024 %>% 
  ggplot(aes(x=sqrt(Formi_2018), y=abundance)) +
  geom_jitter(width=0.01, height=0.01, pch=21, 
              color="brown", fill="#FFA55B") +
  geom_smooth(method="glm", method.args = list(family = "quasipoisson"),
              color = "#086096",fill  = "#86BBD8") + 
  labs(x="Forest Management Intensity", y="Abundance") +
  theme_bw()

# Components:
m2.1 <- glm(abundance ~ Inonat_2018 + Idwcut_2018  + Iharv_2018, family = quasipoisson,
            data=Diversity_2023_2024)

check_overdispersion(m2.1)
summary(m2.1)
Anova(m2.1)


Diversity_2023_2024 %>% 
  ggplot(aes(x=(Inonat_2018), y=abundance)) +
  geom_jitter(width=0.01, height=0.01, pch=21, 
              color="brown", fill="#FFA55B") +
  geom_smooth(method="glm", method.args = list(family = "quasipoisson"),
              color = "#086096",fill  = "#86BBD8") + 
  labs(x="Non-natural tree species", y="Abundance") +
  theme_bw()


Diversity_2023_2024 %>% 
  ggplot(aes(x=(Idwcut_2018), y=abundance)) +
  geom_jitter(width=0.01, height=0.01, pch=21, 
              color="brown", fill="#FFA55B") +
  geom_smooth(method="glm", method.args = list(family = "quasipoisson"),
              color = "#086096",fill  = "#86BBD8") + 
  labs(x="Dead wood with saw cuts", y="Abundance") +
  theme_bw()


Diversity_2023_2024 %>% 
  ggplot(aes(x=(Iharv_2018), y=abundance)) +
  geom_jitter(width=0.01, height=0.01, pch=21, 
              color="brown", fill="#FFA55B") +
  geom_smooth(method="glm", method.args = list(family = "quasipoisson"),
              color = "#086096",fill  = "#86BBD8") + 
  labs(x="Harvested tree biomass", y="Abundance") +
  theme_bw()





# Plot biodiversity potential -------------------------------------------------------------

names(Diversity_2023_2024)

response <- "abundance"
preds <- c( "Vertical_structure",
           "Standing_deadwood", "Lying_deadwood",
           "Very_large_trees", "Habitat_trees",
           "Openness",  "IBPscore")

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







## selected predicors: ------------


m1 <- glm(abundance ~ #
            #    log1p(Tree_abundance) ,
          log1p(Tree_abundance) +
         Openness +
        #  log1p(IBPscore), 
        Vertical_structure,
          family = poisson,
          data=Diversity_2023_2024)
check_overdispersion(m1)
summary(m1)
Anova(m1)


emmeans_veg_leyers<- cld(emmeans(m1, list(pairwise ~ Vertical_structure)), 
                        Letters = letters) %>% 
  arrange(Vertical_structure)

Diversity_2023_2024 %>%
  select(Plot, Tree_ID, Habitat_trees) %>% 
  print(n=Inf)

Diversity_2023_2024 %>%
  ggplot(aes(x=Vertical_structure, y=abundance)) +
  geom_boxplot(outlier.shape = NA,) +
  geom_jitter(width=0.03, height=0.05, size=2,
              pch=21, color="brown", fill="#FFA55B") +
  geom_smooth(method="glm", method.args = list(family = "poisson"),
              color = "#086096",fill  = "#86BBD8") + 
  geom_text(data=emmeans_veg_leyers,
            aes(x=Vertical_structure , y=c(20, 65, 35),
                label=emmeans_veg_leyers$.group),
            size=4, col="black") +
  labs(
    x = "Number of vegetation layers",
    y="Abundance") +
  theme_bw()




# Oppenness -----------------------------------

emmeans_oppenness<- cld(emmeans(m1, list(pairwise ~ Openness)), 
                           Letters = letters) %>% 
  arrange(Openness)



Diversity_2023_2024 %>%
  ggplot(aes(x=Openness, y=abundance)) +
  geom_boxplot(outlier.shape = NA,  color = "#086096") +
  geom_jitter(width=0.03, height=0.05, size=2,
              pch=21, color="brown", fill="#FFA55B") +
#  geom_smooth(method="glm", method.args = list(family = "poisson"),
#              color = "#086096",fill  = "#86BBD8") + 
  geom_text(data=emmeans_oppenness,
            aes(x=Openness, y=c(65, 60, 25),
                label=emmeans_oppenness$.group),
            size=4, col="black") +
  labs(
    x = "Open areas, % ha⁻¹",
       y="Abundance") +
  theme_bw()



Diversity_2023_2024 %>% 
  ggplot(aes(x=log1p(Tree_abundance), y=abundance)) +
  geom_jitter(width=0.03, height=0.05, size=2,
              pch=21, color="brown", fill="#FFA55B") +
  geom_smooth(method="glm", method.args = list(family = "poisson"),
              color = "#086096",fill  = "#86BBD8") + 
    labs(x="Tree richness", y="Abundance") +
  theme_bw()




Diversity_2023_2024 %>% 
  ggplot(aes(x=log1p(Tree_abundance), y=abundance)) +
  geom_jitter(width=0.03, height=0.05, size=2,
              pch=21, color="brown", fill="#FFA55B") +
  geom_smooth(method="glm", method.args = list(family = "poisson"),
              color = "#086096",fill  = "#86BBD8") + 
  labs(x="Tree abundance", y="Abundance") +
  theme_bw()



## Tree composition -----

response <- "abundance"
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


# Selected predictors based on plots and correlations  ------------

m1 <- glm(abundance ~ #
            perc_Fagus_sylvatica +
            log1p(perc_Pinus_sylvestris) + 
            perc_Carpinus_betulus +
          perc_Quercus_spec,
            family = poisson,
          data=Diversity_2023_2024)

check_overdispersion(m1)
summary(m1)
Anova(m1)

Diversity_2023_2024 %>% 
  ggplot(aes(x = perc_Fagus_sylvatica, y = abundance)) +
  geom_jitter(width = 0.03, height = 0.05, size = 2,
              pch = 21, color = "brown", fill = "#FFA55B") +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
              color = "#086096", fill = "#86BBD8") +
  labs(x = "Fagus sylvatica, %", y = "Abundance") +
  theme_bw() +
  scale_x_continuous(labels = function(x) paste0(x, "%"))
  


Diversity_2023_2024 %>% 
  ggplot(aes(x = perc_Carpinus_betulus, y = abundance)) +
  geom_jitter(width = 0.03, height = 0.05, size = 2,
              pch = 21, color = "brown", fill = "#FFA55B") +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
              color = "#086096", fill = "#86BBD8") +
  labs(x = "Carpinus betulus, %", y = "Abundance") +
  theme_bw() +
  scale_x_continuous(labels = function(x) paste0(x, "%"))

Diversity_2023_2024 %>% 
  ggplot(aes(x = (perc_Quercus_spec), y = abundance)) +
  geom_jitter(width = 0.03, height = 0.05, size = 2,
              pch = 21, color = "brown", fill = "#FFA55B") +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
              color = "#086096", fill = "#86BBD8") +
  labs(x = "Quercus sp., %", y = "Abundance") +
  theme_bw() +
  scale_x_continuous(labels = function(x) paste0(x, "%"))


orig_breaks <- pretty(Diversity_2023_2024$perc_Pinus_sylvestris, n = 6)      # breaks on original scale

Diversity_2023_2024 %>% 
  ggplot(aes(x = log1p(perc_Pinus_sylvestris), y = abundance)) +
  geom_jitter(width = 0.03, height = 0.05, size = 2,
              pch = 21, color = "brown", fill = "#FFA55B") +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
              color = "#086096", fill = "#86BBD8") +
  labs(x = "Pinus sylvestris, %", y = "Abundance") +
  scale_x_continuous(
    breaks = log1p(orig_breaks),          # positions must be on the plotted (log1p) scale
    labels = paste0(orig_breaks, "%")     # labels in original units
  ) +
  theme_bw() +
  scale_x_continuous(breaks = (c(0, 1, 2, 3)),  
                     labels = function(x) 
                       label_number(accuracy = 0.1, , suffix = "%")(expm1(x))
  )




# tree hole mapping (Petermann et al.) -------------------------------------



m1 <- glm(abundance ~ #
            (Total_hole_number_mapping),
          family = quasipoisson,
          data=Diversity_2023_2024)

check_overdispersion(m1)
summary(m1)
Anova(m1)

Diversity_2023_2024%>% 
  ggplot(aes(x = (Total_hole_number_mapping), y = abundance)) +
  geom_jitter(width = 0.03, height = 0.05, size = 2,
              pch = 21, color = "brown", fill = "#FFA55B") +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
              color = "#086096", fill = "#86BBD8") +
  labs(x = "Tree-hole number / plot", y = "Abundance") +
  theme_bw() 
  
  
# Structural complexity: ---------------------------------------------

# enl  - "Vertical stand structure"
# ssci - "Stand structural complexity"
# canopy.openness -"Canopy openness" 

m1 <- glm(abundance ~ 
           (enl) + ssci + log(canopy.openness),
          family = quasipoisson,
          data=Diversity_2023_2024)

check_overdispersion(m1)
vif(m1)
summary(m1)
Anova(m1)

Diversity_2023_2024%>% 
  ggplot(aes(x = (enl), y = abundance)) +
  geom_jitter(width = 0.03, height = 0.05, size = 2,
              pch = 21, color = "brown", fill = "#FFA55B") +
  geom_smooth(method = "glm", method.args = list(family = "quasipoisson"),
              color = "#086096", fill = "#86BBD8") +
  labs(x = "Vertical stand structure", y = "Abundance") +
  theme_bw() 


Diversity_2023_2024%>% 
  ggplot(aes(x = (ssci), y = abundance)) +
  geom_jitter(width = 0.03, height = 0.05, size = 2,
              pch = 21, color = "brown", fill = "#FFA55B") +
  geom_smooth(method = "glm", method.args = list(family = "quasipoisson"),
              color = "#086096", fill = "#86BBD8") +
  labs(x = "Stand structural complexity", y = "Abundance") +
  theme_bw() 



Diversity_2023_2024%>% 
  ggplot(aes(x = log(canopy.openness), y = abundance)) +
  geom_jitter(width = 0.03, height = 0.05, size = 2,
              pch = 21, color = "brown", fill = "#FFA55B") +
  geom_smooth(method = "glm", method.args = list(family = "quasipoisson"),
              color = "#086096", fill = "#86BBD8") +
  labs(x = "Canopy openness", y = "Abundance") +
  theme_bw() +
  scale_x_continuous(breaks = (c(-2, 0, 2)),  
                     labels = function(x) 
                       label_number(accuracy = 0.1)(exp(x))
  )





# Stand structural attributes -------------------------------------------------
## Exploration plots --------------

response <- "abundance"

preds0 <- c("ssm_N","ssm_Vol","ssm_CPA","sp_N_1D", "sp_BA_1D", "sp_N_2D", "sp_BA_2D", "spat_Pielou")

preds1 <- c("ssm_N","ssm_SDI","ssm_BA","ssm_Vol","ssm_CPA","ssm_con_BA","ssm_con_CPA",
            "ssm_Pa_CPA","ssm_Ps_CPA","ssm_Qs_CPA","ssm_Fs_CPA","sp_0D","sp_N_1D",
            "sp_N_2D","sp_BA_1D","sp_BA_2D","d_qm","d_m","d_SD","d_CV","d_50")

preds2 <- c("d_max", "d_gini","dc_0D","dc_1D","dc_1D_BA","dc_sp_1D","hc_0D","hc_1D","hc_1D_BA",
            "hc_sp_1D","r20_SD_BA","r20_CV_BA","r20_SD_N","r20_CV_N","r20_VMR")

preds3 <- c("r20_Morisita","spat_clarkevans","spat_Pielou","spat_spM","spat_TD",
            "spat_SCI_d","spat_Th","spat_SCI_h")

df0 <- Diversity_2023_2024 %>%
  mutate(across(all_of(preds0), ~ if (is.factor(.x)) as.numeric(as.character(.x)) else .x)) %>%
  mutate(across(all_of(preds0), ~ log1p(.x), .names = "{.col}"))

df1 <- Diversity_2023_2024 %>%
  mutate(across(all_of(preds1), ~ if (is.factor(.x)) as.numeric(as.character(.x)) else .x)) %>%
  mutate(across(all_of(preds1), ~ log1p(.x), .names = "{.col}"))

df2 <- Diversity_2023_2024 %>%
  mutate(across(all_of(preds2), ~ if (is.factor(.x)) as.numeric(as.character(.x)) else .x)) %>%
  mutate(across(all_of(preds2), ~ log1p(.x), .names = "{.col}"))

df3 <- Diversity_2023_2024 %>%
  mutate(across(all_of(preds3), ~ if (is.factor(.x)) as.numeric(as.character(.x)) else .x)) %>%
  mutate(across(all_of(preds3), ~ log1p(.x), .names = "{.col}"))

# 2) make plots using the transformed predictors
plots <- map(preds0, function(var) {
  datp <- df0 %>% select(all_of(c(response, var))) %>% na.omit()
  
  ggplot(datp, aes_string(x = var, y = response)) +
    geom_jitter(width = 0.01, height = 0.01, pch = 21,
                color = "brown", fill = "#FFA55B") +
    geom_smooth(
      method = "glm",
      formula = y ~ x,
      method.args = list(family = quasipoisson),   # or quasipoisson if overdispersed
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





# Climat -------------------------------------------------
## Exploration plots --------------

response <- "abundance"

preds0 <- c("precipitation_radolan_rain_days_mean","precipitation_radolan_acc_mean", "rH_200_DMR_mean",
            "Ta_10_mean","Ta_10_max_mean")

preds1 <- c("precipitation_radolan_mean","precipitation_radolan_acc_mean", "Ta_200_mean","Ta_200_heat_index_mean",
            "Ta_200_humidex_mean")

preds2 <- c("Ta_200_extremely_hot_days_sum", "Ta_200_extremely_cold_days_sum","Ta_200_heating_degree_days_sum")

df0 <- Diversity_2023_2024 %>%
  mutate(across(all_of(preds0), ~ if (is.factor(.x)) as.numeric(as.character(.x)) else .x)) %>%
  mutate(across(all_of(preds0), ~ log1p(.x), .names = "{.col}"))

df1 <- Diversity_2023_2024 %>%
  mutate(across(all_of(preds1), ~ if (is.factor(.x)) as.numeric(as.character(.x)) else .x)) %>%
  mutate(across(all_of(preds1), ~ log1p(.x), .names = "{.col}"))

df2 <- Diversity_2023_2024 %>%
  mutate(across(all_of(preds2), ~ if (is.factor(.x)) as.numeric(as.character(.x)) else .x)) %>%
  mutate(across(all_of(preds2), ~ log1p(.x), .names = "{.col}"))

# 2) make plots using the transformed predictors
plots <- map(preds0, function(var) {
  datp <- df0 %>% select(all_of(c(response, var))) %>% na.omit()
  
  ggplot(datp, aes_string(x = var, y = response)) +
    geom_jitter(width = 0.01, height = 0.01, pch = 21,
                color = "brown", fill = "#FFA55B") +
    geom_smooth(
      method = "glm",
      formula = y ~ x,
      method.args = list(family = quasipoisson),   # or quasipoisson if overdispersed
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




# Landscape  -------------------------------------------------
## Exploration plots --------------

df1 <- Diversity_2023_2024 %>%
  left_join(landscape_heterogeneity %>% 
              filter(LandType_level=="class_2" & buffer_size_m==250),
            by = c("Plot" = "plotID"))

df2 <- Diversity_2023_2024 %>%
  left_join(landscape_heterogeneity %>% 
              filter(LandType_level=="class_2" & buffer_size_m==500),
            by = c("Plot" = "plotID"))


response <- "abundance"

preds1 <- c("LandType_richness", "LandType_Shannon", "LandType_even"#,
            # "Forest_percent", "Agricultural_percent", 
            # "Water_bodies_percent", "Urban_percent"
)


# 2) make plots using the transformed predictors
plots <- map(preds1, function(var) {
  datp <- df1 %>% select(all_of(c(response, var))) %>% na.omit()
  
  ggplot(datp, aes_string(x = var, y = response)) +
    geom_jitter(width = 0.01, height = 0.01, pch = 21,
                color = "brown", fill = "#FFA55B") +
    geom_smooth(
      method = "glm",
      formula = y ~ x,
      method.args = list(family = quasipoisson),   # or quasipoisson if overdispersed
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



# plots

df1 <- Diversity_2023_2024 %>%
  left_join(landscape_heterogeneity %>% 
              filter(LandType_level=="class_0" & buffer_size_m==500),
            by = "Plot") 




df1 %>% 
  ggplot(aes(x = Forest_percent, y = abundance)) +
  geom_jitter(width = 0.5, height = 0.3, size = 2,
              pch = 21, color = "brown", fill = "#FFA55B") +
  geom_smooth(method = "glm", method.args = list(family = "quasipoisson"),
              color = "#086096", fill = "#86BBD8",
              formula = y ~ log(x)) +
  labs(x = "Forest cover, %", y = "Abundance") +
  theme_bw()


df1 %>% 
  ggplot(aes(x = Urban_percent, y = abundance)) +
  geom_jitter(width = 0.1, height = 0.0, size = 2,
              pch = 21, color = "brown", fill = "#FFA55B") +
  geom_smooth(method = "glm", method.args = list(family = "quasipoisson"),
              color = "#086096", fill = "#86BBD8",
              formula = y ~ (x)) +
  labs(x = "Urban area, %", y = "Abundance") +
  theme_bw()

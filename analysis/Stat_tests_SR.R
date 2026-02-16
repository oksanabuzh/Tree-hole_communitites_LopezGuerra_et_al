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
  ggplot(aes(x=(SMI_mean_2018_2020), y=sp_richness)) +
  geom_jitter(width=0.01, height=0.01, pch=21, 
              color="brown", fill="#FFA55B") +
  geom_smooth(method="glm", method.args = list(family = "poisson"),
              color = "#086096",fill  = "#86BBD8") + 
  labs(x="SMI", y="Species richness") +
  theme_bw()


#  components
m1.1 <- glm(sp_richness ~ SMIr_mean_2018_2020 + SMId_mean_2018_2020, family = poisson,
          data=Diversity_2023_2024)

check_overdispersion(m1.1)
summary(m1.1)
Anova(m1.1)


Diversity_2023_2024 %>% 
  ggplot(aes(x=(SMIr_mean_2018_2020), y=sp_richness)) +
  geom_jitter(width=0.01, height=0.01, pch=21, 
              color="brown", fill="#FFA55B") +
  geom_smooth(method="glm", method.args = list(family = "poisson"),
              color = "#086096",fill  = "#86BBD8") + 
  labs(x="Risk component of SMI", y="Species richness") +
  theme_bw()


Diversity_2023_2024 %>% 
  ggplot(aes(x=(SMId_mean_2018_2020), y=sp_richness)) +
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
  ggplot(aes(x=sqrt(Formi_2018), y=sp_richness)) +
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

## Exploration plots --------------

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






# Tree composition -----
## Exploration plots --------------
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



## Selected predictors based on plots and correlations  ------------

m1 <- glm(sp_richness ~ #
            # perc_Fagus_sylvatica, 
            # log1p(perc_Pinus_sylvestris) , 
            # perc_Carpinus_betulus ,
           perc_Quercus_spec,
          family = poisson,
          data=Diversity_2023_2024)

check_overdispersion(m1)
summary(m1)
Anova(m1)

Diversity_2023_2024 %>% 
  ggplot(aes(x = perc_Fagus_sylvatica, y = sp_richness)) +
  geom_jitter(width = 0.03, height = 0.05, size = 2,
              pch = 21, color = "brown", fill = "#FFA55B") +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
              color = "#086096", fill = "#86BBD8") +
  labs(x = "Fagus sylvatica, %", y = "Species richnes") +
  theme_bw() +
  scale_x_continuous(labels = function(x) paste0(x, "%"))



Diversity_2023_2024 %>% 
  ggplot(aes(x = perc_Carpinus_betulus, y = sp_richness)) +
  geom_jitter(width = 0.03, height = 0.05, size = 2,
              pch = 21, color = "brown", fill = "#FFA55B") +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
              color = "#086096", fill = "#86BBD8") +
  labs(x = "Carpinus betulus, %", y = "Species richnes") +
  theme_bw() +
  scale_x_continuous(labels = function(x) paste0(x, "%"))

Diversity_2023_2024 %>% 
  ggplot(aes(x = (perc_Quercus_spec), y = sp_richness)) +
  geom_jitter(width = 0.03, height = 0.05, size = 2,
              pch = 21, color = "brown", fill = "#FFA55B") +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
              color = "#086096", fill = "#86BBD8") +
  labs(x = "Quercus sp., %", y = "Species richnes") +
  theme_bw() +
  scale_x_continuous(labels = function(x) paste0(x, "%"))


Diversity_2023_2024 %>% 
  ggplot(aes(x = log1p(perc_Pinus_sylvestris), y = sp_richness)) +
  geom_jitter(width = 0.03, height = 0.05, size = 2,
              pch = 21, color = "brown", fill = "#FFA55B") +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
              color = "#086096", fill = "#86BBD8") +
  labs(x = "Pinus sylvestris, %", y = "Species richnes") +
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



m1 <- glm(sp_richness ~ #
            log(Total_hole_number_mapping),
          family = poisson,
          data=Diversity_2023_2024)

check_overdispersion(m1)
summary(m1)
Anova(m1)

Diversity_2023_2024%>% 
  filter(Outside==FALSE) %>% 
  ggplot(aes(x = (Total_hole_number_mapping), y = sp_richness)) +
  geom_jitter(width = 0.03, height = 0.05, size = 2,
              pch = 21, color = "brown", fill = "#FFA55B") +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
              color = "#086096", fill = "#86BBD8") +
  labs(x = "Tree-hole number / plot", y = "Species richnes") +
  theme_bw() 



# Structural complexity: ----

# enl  - "Vertical stand structure"
# ssci - "Stand structural complexity"
# canopy.openness -"Canopy openness" 

m1 <- glm(sp_richness ~ 
            (enl) + ssci + log(canopy.openness),
          family = quasipoisson,
          data=Diversity_2023_2024)

check_overdispersion(m1)
vif(m1)
summary(m1)
Anova(m1)

Diversity_2023_2024%>% 
  ggplot(aes(x = (enl), y = sp_richness)) +
  geom_jitter(width = 0.03, height = 0.05, size = 2,
              pch = 21, color = "brown", fill = "#FFA55B") +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
              color = "#086096", fill = "#86BBD8") +
  labs(x = "Vertical stand structure", y = "Species richnes") +
  theme_bw() 


Diversity_2023_2024%>% 
  ggplot(aes(x = (ssci), y = sp_richness)) +
  geom_jitter(width = 0.03, height = 0.05, size = 2,
              pch = 21, color = "brown", fill = "#FFA55B") +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
              color = "#086096", fill = "#86BBD8") +
  labs(x = "Stand structural complexity", y = "Species richnes") +
  theme_bw() 



Diversity_2023_2024%>% 
  ggplot(aes(x = log(canopy.openness), y = sp_richness)) +
  geom_jitter(width = 0.03, height = 0.05, size = 2,
              pch = 21, color = "brown", fill = "#FFA55B") +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
              color = "#086096", fill = "#86BBD8") +
  labs(x = "Canopy openness", y = "Species richnes") +
  theme_bw() +
  scale_x_continuous(breaks = (c(-2, 0, 2)),  
                     labels = function(x) 
                       label_number(accuracy = 0.1)(exp(x))
  )




#

# Stand structural attributes -------------------------------------------------
## Exploration plots --------------
response <- "sp_richness"
preds1 <- c("ssm_N","ssm_SDI","ssm_BA","ssm_Vol","ssm_CPA","ssm_con_BA","ssm_con_CPA",
            "ssm_Pa_CPA","ssm_Ps_CPA","ssm_Qs_CPA","ssm_Fs_CPA","sp_0D","sp_N_1D",
            "sp_N_2D","sp_BA_1D","sp_BA_2D","d_qm","d_m","d_SD","d_CV","d_50")

preds2 <- c("d_max", "d_gini","dc_0D","dc_1D","dc_1D_BA","dc_sp_1D","hc_0D","hc_1D","hc_1D_BA",
            "hc_sp_1D","r20_SD_BA","r20_CV_BA","r20_SD_N","r20_CV_N","r20_VMR")

preds3 <- c("r20_Morisita","spat_clarkevans","spat_Pielou","spat_spM","spat_TD",
            "spat_SCI_d","spat_Th","spat_SCI_h")


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
plots <- map(preds3, function(var) {
  datp <- df3 %>% select(all_of(c(response, var))) %>% na.omit()
  
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

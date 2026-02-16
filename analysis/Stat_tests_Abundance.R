#   

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
            #    log1p(Tree_sp_richness) ,
       #   log1p(Tree_abundance) ,
         Openness ,
        #  log1p(IBPscore), 
       #  Vertical_structure,
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




# Oppenness

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
  ggplot(aes(x=log1p(Tree_sp_richness), y=abundance)) +
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





# Climat -------------------------------------------------
## Exploration plots --------------

response <- "abundance"
preds1 <- c("precipitation_radolan_mean","precipitation_radolan_acc_mean", "rH_200_mean","Ta_200_mean","Ta_200_heat_index_mean",
            "Ta_200_humidex_mean")

preds2 <- c("Ta_200_extremely_hot_days_sum", "Ta_200_extremely_cold_days_sum","Ta_200_heating_degree_days_sum",
            "precipitation_radolan_CV","rH_200_CV","Ta_200_CV")

df1 <- Diversity_2023_2024 %>%
  mutate(across(all_of(preds1), ~ if (is.factor(.x)) as.numeric(as.character(.x)) else .x)) %>%
  mutate(across(all_of(preds1), ~ log1p(.x), .names = "{.col}"))

df2 <- Diversity_2023_2024 %>%
  mutate(across(all_of(preds2), ~ if (is.factor(.x)) as.numeric(as.character(.x)) else .x)) %>%
  mutate(across(all_of(preds2), ~ log1p(.x), .names = "{.col}"))

# 2) make plots using the transformed predictors
plots <- map(preds2, function(var) {
  datp <- df2 %>% select(all_of(c(response, var))) %>% na.omit()
  
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
            by = "Plot") 

df2 <- Diversity_2023_2024 %>%
  left_join(landscape_heterogeneity %>% 
              filter(LandType_level=="class_2" & buffer_size_m==500),
            by = "Plot") 


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



# Summary statistics and exploratory plots 

library(tidyverse)
library(scales)
library(forcats)

# Prefer dplyr's select whenever there is a conflict
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")


# data -------------------------------------------------------

# Environmental data -----------
environm <- read_csv("data/processed_data/Environment_ALL.csv") %>% 
  mutate(Month=factor(Month, levels=c("May", "June", "July", "November"))) %>% 
  mutate(Tree_hole_type_coarse=factor(Tree_hole_type_coarse, levels=c("pan", "rot"))) %>% 
  mutate(Tree_hole_type=factor(Tree_hole_type, 
                               levels=c("Division", "Branch", "Trunk", 
                                        "Cut tree", "Root",  NA)))
str(environm)

## Diversity data -------------
Diversity_2023_2024 <- read_csv("data/processed_data/Diversity_2023_2024.csv") %>% 
  mutate(Month=factor(Month, levels=c("May", "June", "July", "November"))) %>% 
  mutate(Tree_hole_type_coarse=factor(Tree_hole_type_coarse, levels=c("pan", "rot"))) %>% 
  mutate(Tree_hole_type=factor(Tree_hole_type, 
                               levels=c("Division", "Branch", "Trunk", 
                                        "Cut tree", "Root",  NA))) 

str(Diversity_2023_2024)


## Community composition data ------------
sp_dat <- readr::read_csv("data/processed_data/Community_2023_2024_DNAcorrected.csv") %>%
  select(Treehole_number, Sp_ID_DNAcorrected, Abundance) %>%
  left_join(trait_data, by = "Sp_ID_DNAcorrected") 

## Trait data ---------------
trait_data <- read_csv("data/processed_data/Traits_2023_2024_final_DNA_corrected.csv")



# Tree and treehole data ---------
environm %>% 
  group_by(tree_species, Tree_ID) %>% 
  count() %>% 
  arrange(desc(n))

tree_repetitions <- Diversity_2023_2024 %>% 
  group_by(Year, Month, Tree_ID) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  filter(n>1)

# trees that have >1 tree holes within same years and months
environm %>%
  filter(Tree_ID %in% tree_repetitions$Tree_ID) %>% 
  select(Plot, Tree_ID, Treehole_number, Year, Month, Sampling_date, Tree_hole_type, Tree_hole_opening)


# holes that have repetitions across years and months
hole_repetitions <- environm %>% 
  group_by(tree_species, Tree_ID, Treehole_number) %>% 
  count(.drop=T) %>% 
  arrange(desc(n))

hole_repetitions 

environm %>% 
  filter(Treehole_number %in% 
           (hole_repetitions %>% filter(n>1) %>% pull(Treehole_number))) %>% 
  select(Plot, Tree_ID, Treehole_number, Year, Month, Sampling_date, Tree_hole_type, Tree_hole_opening)


# hole types:
hole_repet_2 <-environm %>% 
  group_by(Tree_ID, Year, Sampling_date, Treehole_number) %>%
  count(.drop=T) %>% 
  arrange(desc(n))

environm %>% 
  filter(Treehole_number %in% 
           (hole_repet_2 %>% filter(n>1) %>% pull(Treehole_number))) %>% 
  select(Plot, Tree_ID, Treehole_number, Year, Month, Sampling_date, Tree_hole_type, Tree_hole_opening)



# treehole counts per tree species 
treehole_counts <- environm %>%
  mutate(tree_species = str_replace_all(tree_species, "_", " ")) %>% 
  group_by(tree_species) %>%
  summarise(n = n(), .groups = "drop")

environm %>%
  # remove _ in tree_species names
  mutate(tree_species = str_replace_all(tree_species, "_", " ")) %>%
  ggplot(aes(DBH, tree_species))+
  geom_boxplot() +
  geom_jitter(aes(color=Month),size=3,
              width=0, height=0.2, alpha=0.7) + #, color="#086096") +
  theme_bw() +
  labs(y="Tree species", x="DBH (cm)") +
  # add to plot n= count of tree species
  geom_text(
    data = treehole_counts,
    aes(x = c(25, 25, 110), y = tree_species, label = paste0("n = ", n)),
    hjust = 0, size = 3) +
  xlim(0, 130)


# histogram 

# treehole counts per month
treehole_counts_month <- environm %>%
  group_by(Month) %>%
  summarise(n = n(), .groups = "drop")

environm %>%
  ggplot(aes(y=Month)) +
  geom_bar(aes(fill=factor(Year))) +
  theme_bw() +
  labs(y="Sampled month", x="Number of tree holes", fill="Year") +
  # add to plot n= count of tree species
  geom_text(
    data = treehole_counts_month,
    aes(x = 24, y = Month, label = paste0("n = ", n)),
    hjust = 0, size = 3) +
  xlim(0, 28)


# 1) Plot counts ---------------------
plot_counts <- environm %>%
  group_by(Plot) %>%
  count(Plot, .drop = T) %>% 
  arrange(desc(n))

plot_counts

environm %>% 
  filter(Plot %in% 
           (plot_counts %>% filter(n>1) %>% pull(Plot))) %>% 
  select(Plot, Tree_ID, Treehole_number, Year, Month, Sampling_date, Tree_hole_type, Tree_hole_opening) %>% 
  print(n = Inf)



# 2) Diversity metrics  --------------------------------------------

Diversity_2023_2024 %>%  pull(Tree_hole_type) %>%  unique()


hole_type_color <- (c("rot"="brown", 
                      "pan"="#14724C"))  
## Exploratory data checks ------



# Count tree holes per plot
# Grouped only by year and plot
# Shows distribution of tree holes across plots 
Diversity_2023_2024 %>% 
  group_by(Year, Month, Plot) %>% 
  count(Treehole_number) %>%  
  arrange(desc(n))             

Diversity_2023_2024 %>% 
  select(Plot, Year, Month, Treehole_number, Tree_hole_type, abundance, sp_richness) %>% 
  arrange(Plot, Treehole_number, Month) %>% 
  print(n = Inf)

Diversity_2023_2024 %>% 
  arrange(Plot, Treehole_number, Month) %>% 
  print(n = Inf)


Diversity_2023_2024 %>%
  filter(Treehole_number =="SEW29_0281_1") %>% 
  select(Plot, Tree_ID,Treehole_number, Tree_hole_type, Year, Month)

# take word of Treehole_number to get tree species

tree_ID_check <-Diversity_2023_2024 %>% 
mutate(plot_check_from_tree= str_extract(Tree_ID, "SEW\\d{2}"),
       plot_check_from_hole= str_extract(Treehole_number, "SEW\\d{2}"),
       .after = Plot)

tree_ID_check %>% 
  print(n = Inf)

# check when Plot  and plot_check_from_tree are not the same, including NA
tree_ID_check %>% 
  filter(Plot != plot_check_from_tree | is.na(plot_check_from_tree)) %>% 
  select(Plot, plot_check_from_tree, plot_check_from_hole, Tree_ID, Treehole_number) %>% 
  print(n = Inf)

# check when Plot  and plot_check_from_hole are not the same, including NA
tree_ID_check %>% 
  filter(Plot != plot_check_from_hole | is.na(plot_check_from_hole)) %>% 
  select(Plot, plot_check_from_tree, plot_check_from_hole, Tree_ID, Treehole_number) %>% 
  print(n = Inf)



## Exploratory plots -------  
names(Diversity_2023_2024)

Diversity_2023_2024 %>% 
  ggplot(aes(y=abundance, x=Month)) +
  geom_boxplot(outliers = F, notch = F) +
  geom_jitter(width=0.2, height=0, alpha=0.7, 
              color="#086096") +
  labs(y= "Abundance") +
  theme_bw()

Diversity_2023_2024 %>% 
  ggplot(aes(y=biomass_dry_mg, x=Month)) +
  geom_boxplot(outliers = F, notch = F) +
  geom_jitter(width=0.2, height=0, alpha=0.7, 
              color="#086096") +
  labs(y= "Biomass") +
  theme_bw()

Diversity_2023_2024 %>% 
  ggplot(aes(y=sp_richness, x=Month)) +
  geom_boxplot(outliers = F, notch = F) +
  geom_jitter(width=0.2, height=0, alpha=0.7, 
              color="#086096") +
  labs(y= "Species richnss") +
  theme_bw()+
  scale_y_continuous(breaks = seq(0, 10, by = 2))


Diversity_2023_2024 %>% 
  filter(!is.na(Tree_hole_type)) %>%
  ggplot(aes(x=Tree_hole_type, y = abundance, color=Tree_hole_type_coarse)) +
  geom_boxplot(outliers = F) +
  geom_jitter(width=0.2, height=0.05, alpha=0.7, size=2) +
  scale_color_manual(values=hole_type_color) +
  labs(y= "Abundance", x="Tree-hole type", color="Tree-hole type") +
  theme_bw()

Diversity_2023_2024 %>% 
  filter(!is.na(Tree_hole_type)) %>%
  ggplot(aes(x=Tree_hole_type, y = biomass_dry_mg, color=Tree_hole_type_coarse)) +
  geom_boxplot(outliers = F) +
  geom_jitter(width=0.2, height=0.05, alpha=0.7, size=2) +
  labs(y= "Biomass", x="Tree-hole type", color="Tree-hole type") +
  scale_color_manual(values=hole_type_color) +
  theme_bw() 


Diversity_2023_2024 %>% 
  filter(!is.na(Tree_hole_type)) %>%
  ggplot(aes(x=Tree_hole_type, y = sp_richness, color=Tree_hole_type_coarse)) +
  geom_boxplot(outliers = F) +
  geom_jitter(width=0.2, height=0.05, alpha=0.7, size=2) +
  labs(y= "Species richnss", x="Tree-hole type", color="Tree-hole type") +
  scale_color_manual(values=hole_type_color) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 10, by = 2))


#  Tree_hole_type_coarse

Diversity_2023_2024 %>% 
  filter(!is.na(Tree_hole_type_coarse)) %>%
  ggplot(aes(x=Tree_hole_type_coarse, y = abundance, color=Tree_hole_type_coarse)) +
  geom_boxplot(outliers = F) +
  geom_jitter(width=0.2, height=0.05, alpha=0.7, size=1) +
  labs(y= "Abundance", x="Tree-hole type", color="Tree-hole type") +
  scale_color_manual(values=hole_type_color) +
  theme_bw()

Diversity_2023_2024 %>% 
  filter(!is.na(Tree_hole_type_coarse)) %>%
  ggplot(aes(x=Tree_hole_type_coarse, y = biomass_dry_mg, color=Tree_hole_type_coarse)) +
  geom_boxplot(outliers = F) +
  geom_jitter(width=0.2, height=0.05, alpha=0.7, size=1) +
  labs(y= "Biomass", x="Tree-hole type", color="Tree-hole type") +
  scale_color_manual(values=hole_type_color) +
  theme_bw()

Diversity_2023_2024 %>% 
  filter(!is.na(Tree_hole_type_coarse)) %>%
  ggplot(aes(x=Tree_hole_type_coarse, y = sp_richness,  color=Tree_hole_type_coarse)) +
  geom_boxplot(outliers = F) +
  geom_jitter(width=0.2, height=0.05, alpha=0.7, size=1) +
  labs(y= "Species richnss", x="Tree-hole type", color="Tree-hole type") +
  scale_color_manual(values=hole_type_color) +
  theme_bw()+
  scale_y_continuous(breaks = seq(0, 10, by = 2))


# check if there are any tree holes with 1 abundance and 1 species richness
Diversity_2023_2024 %>% 
  filter(abundance == 1 & sp_richness == 1) %>% 
  select(Plot, Tree_ID, Treehole_number, Year, Month, Tree_hole_type, Tree_hole_type_coarse) %>% 
  print(n = Inf)



# 3) Species composition -------------------------------------------------------------

## Graphical data exploration -----

### 1) Frequency of occurrence per species (number of treeholes where species is present)
n_sites <- n_distinct(df$Treehole_number)

freq_tbl <- df %>%
  mutate(present = Abundance > 0) %>%
  group_by(Species, Family_DNA_corrected) %>%
  summarise(
    sites_present = n_distinct(Treehole_number[present]),
    total_abundance = sum(Abundance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(freq = sites_present / n_sites) %>%
  arrange(desc(freq), desc(total_abundance), Species) %>%
  mutate(species_rank = row_number())

print(freq_tbl, n = Inf)


# 2) Bar plot: percent occurrence, species ordered by freq (descending)
freq_tbl %>%
  # mutate(Species = fct_reorder(Species, species_rank)) %>%
  mutate(Species = fct_reorder(Species, species_rank) %>% forcats::fct_rev()) %>%
  # arrange(desc(freq)) %>%
  ggplot(aes(x = Species, y = freq, fill = Family_DNA_corrected)) +
  geom_col(width = 0.7, colour = "grey30") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = NULL, y = "Occurrence friequency",
       fill = "Family") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 13, color="black"),
        axis.text.x = element_text(size = 10, color="black"),
        text = element_text(size = 12, color="black"))

# abundance:
sp_dat %>% 
  select(Species, Sp_ID_DNAcorrected, Abundance) %>% 
  left_join(freq_tbl, by=c("Species")) %>% 
  mutate(Species = fct_reorder(Species, species_rank) %>% forcats::fct_rev()) %>%
  ggplot(aes(x = Abundance, y = Species, color=Family_DNA_corrected)) +
  geom_boxplot(alpha=0, outliers = F) +
  geom_jitter(width = 0, height = 0.3, alpha=1, size=2) +
  theme_bw() + labs(x = "Total abundance", y = "Species", color="Family")+
  theme(axis.text.y = element_text(size = 13, color="black"),
        axis.text.x = element_text(size = 10, color="black"),
        text = element_text(size = 12, color="black"))

# body size:
sp_dat %>% 
  select(Species, Sp_ID_DNAcorrected, Abundance, dry_weight_mg) %>% 
  left_join(freq_tbl, by=c("Species")) %>% 
  mutate(Species = fct_reorder(Species, species_rank) %>% forcats::fct_rev()) %>%
  ggplot(aes(x = 1, y = Species, color=Family_DNA_corrected, fill=Family_DNA_corrected,
             size = dry_weight_mg)) +
  geom_jitter(width = 0, height = 0, alpha=1, shape = 21,  colour = "black") +
  theme_bw() + labs(x = "Body mass", y = "Species", 
                    fill="Family", size="Body mass, g") +
  theme(axis.text.y = element_text(size = 13, color="black"),
        axis.text.x = element_text(size = 10, color="white"),
        #   axis.text.x = element_blank(),
        #   axis.ticks.x = element_blank(),
        text = element_text(size = 12, color="black"),
        # Legend text and title size
        legend.text  = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 12, face = "bold"),
        # Make legend keys (symbols) bigger
        legend.key.size = grid::unit(0.8, "cm"),
        # Optional: increase spacing between legend rows
        legend.spacing.y = grid::unit(0.25, "cm")) +
  scale_size_continuous(range = c(3, 13)) +
  guides(
    fill = guide_legend(override.aes = list(size = 5)), 
    size = guide_legend(override.aes = list(
      #  shape = 21,            # ensure shape uses fill+border
      stroke = 1, 
      colour = "black",     # border color in size legend
      fill = "grey80"        # interior color in size legend
    ))
  )+
  scale_x_continuous(
    limits = c(0.995, 1.005),         # narrow viewing window
    breaks = 1,                       # single tick at 1
    labels = function(x) sprintf("%.2f", x), # display as "1.00"
    expand = c(0, 0))




# community biomass:
sp_dat %>% 
  select(Species, Sp_ID_DNAcorrected, Abundance, dry_weight_mg) %>% 
  mutate(Biomass = Abundance * dry_weight_mg) %>%
  left_join(freq_tbl, by=c("Species")) %>% 
  mutate(Species = fct_reorder(Species, species_rank) %>% forcats::fct_rev()) %>%
  ggplot(aes(x = log(Biomass), y = Species, color=Family_DNA_corrected)) +
  geom_boxplot(alpha=0, outliers = F) +
  geom_jitter(width = 0, height = 0.3, alpha=1, size=2) +
  theme_bw() + labs(x = "Total biomass (log), g", y = "Species", color="Family")+
  theme(axis.text.y = element_text(size = 13, color="black"),
        axis.text.x = element_text(size = 10, color="black"),
        text = element_text(size = 12, color="black"))



# 4) Predictors (correlations)  -------------------------------------------------------
dat_envir <- environm %>% 
  mutate(Month=factor(Month, levels=c("May", "June", "July", "November"))) %>% 
  mutate(Tree_hole_type_coarse=factor(Tree_hole_type_coarse, levels=c("pan", "rot"))) %>% 
  mutate(Tree_hole_type=factor(Tree_hole_type, 
                               levels=c("Division", "Branch", "Trunk", 
                                        "Cut tree", "Root",  NA))) %>%
  summarise(
    Inonat_mean_2012_2018 = first(Inonat_mean_2012_2018),
    Iharv_mean_2012_2018 = first(Iharv_mean_2012_2018),
    Idwcut_mean_2012_2018 = first(Idwcut_mean_2012_2018),
    Formi_mean_2012_2018 = first(Formi_mean_2012_2018),
    Tree_abundance = first(Tree_abundance),
    Tree_sp_richness = first(Tree_sp_richness),
    ssci = first(ssci),
    Openness = first(Openness),
    Vertical_structure = first(Vertical_structure),
    Standing_deadwood = first(Standing_deadwood),
    .by = c("Plot")) %>% 
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
  mutate(Inonat_mean_tr =Inonat_mean_2012_2018^0.5) 

names(dat_envir)

Diversity_2023_2024 %>% 
  select(Plot, Tree_ID, Inonat_mean_tr, Iharv_mean_2012_2018, Idwcut_mean_2012_2018, Openness)


names(Diversity_2023_2024)

## Forest management types ------------------

ggplot(dat_envir, aes(x = Iharv_mean_2012_2018, y = Idwcut_mean_2012_2018)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth(method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2, se = TRUE) +
  theme_bw() + labs( x = "Harvested tree biomass", y = "Dead wood with saw cuts")

ggplot(dat_envir, aes(x = Iharv_mean_2012_2018, y = Inonat_mean_tr)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth(method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2, se = TRUE) +
  theme_bw() + labs( x = "Harvested tree biomass", y = "Non-natural tree species")

ggplot(dat_envir, aes(x = Idwcut_mean_2012_2018, y = Inonat_mean_tr)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth(method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2, se = TRUE) +
  theme_bw() + labs( x = "Dead wood with saw cuts", y = "Non-natural tree species")




## Stand structural complexity  --------------

ggplot(dat_envir, aes(x = Formi_mean_2012_2018, y = ssci)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth( method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2) +
  theme_bw()+ labs( x = "Forest Management Intensity", y = "Stand structural complexity")


ggplot(dat_envir, aes(x = Iharv_mean_2012_2018, y = ssci)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth(method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2, se = TRUE) +
  theme_bw() + labs( x = "Harvested tree biomass", y = "Stand structural complexity")
  

ggplot(dat_envir, aes(x = Idwcut_mean_2012_2018, y = ssci)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth(method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2, se = TRUE) +
  theme_bw() + labs( x = "Dead wood with saw cuts", y = "Stand structural complexity")


ggplot(dat_envir, aes(x = Inonat_mean_tr, y = ssci)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
    geom_smooth(method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2, se = TRUE) +
  theme_bw() + labs( x = "Non-natural tree species", y = "Stand structural complexity")

  
## Openness  -----------------------------

ggplot(dat_envir, aes(x = Formi_mean_2012_2018, y = Openness)) +
  geom_boxplot(outlier.shape = NA,  color = "#086096") +
  geom_jitter(width=0, height=0.2, pch=21, size=2.5, color="brown", fill="#FFA55B") +
#  geom_smooth( method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2) +
  theme_bw()+ labs( x = "Forest Management Intensity", y = "Openness, % ha⁻¹")

ggplot(dat_envir, aes(x = Iharv_mean_2012_2018, y = Openness)) +
  geom_boxplot(outlier.shape = NA,  color = "#086096") +
  geom_jitter(width=0, height=0.2, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  #  geom_smooth( method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2) +
  theme_bw()+ labs( x = "Harvested tree biomass", y = "Openness, % ha⁻¹")

ggplot(dat_envir, aes(x = Idwcut_mean_2012_2018, y = Openness)) +
  geom_boxplot(outlier.shape = NA,  color = "#086096") +
  geom_jitter(width=0, height=0.2, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  #  geom_smooth( method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2) +
  theme_bw()+ labs( x = "Dead wood with saw cuts", y = "Openness, % ha⁻¹")

ggplot(dat_envir, aes(x = Inonat_mean_tr, y = Openness)) +
  geom_boxplot(outlier.shape = NA,  color = "#086096") +
  geom_jitter(width=0, height=0.2, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  #  geom_smooth( method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2) +
  theme_bw()+ labs( x = "Non-natural tree species", y = "Openness, % ha⁻¹")



## Landscape heterogeneity -----------------------------------------------

ggplot(dat_envir, aes(x = Formi_mean_2012_2018, y = LandType_richness_class_2)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth( method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2) +
  theme_bw()+ labs( x = "Forest Management Intensity", y = "Landscape heterogeneity")


ggplot(dat_envir, aes(x = Iharv_mean_2012_2018, y = LandType_richness_class_2)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth(method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2, se = TRUE) +
  theme_bw() + labs( x = "Harvested tree biomass", y = "Landscape heterogeneity")


ggplot(dat_envir, aes(x = Idwcut_mean_2012_2018, y = LandType_richness_class_2)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth(method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2, se = TRUE) +
  theme_bw() + labs( x = "Dead wood with saw cuts", y = "Landscape heterogeneity")


ggplot(dat_envir, aes(x = Inonat_mean_tr, y = LandType_richness_class_2)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth(method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2, se = TRUE) +
  theme_bw() + labs( x = "Non-natural tree species", y = "Landscape heterogeneity")



## Forest cover ---------------------------

ggplot(dat_envir, aes(x = Formi_mean_2012_2018, y = Forest_percent)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth( method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2) +
  theme_bw()+ labs( x = "Forest Management Intensity", y = "Forest cover, %")


ggplot(dat_envir, aes(x = Iharv_mean_2012_2018, y = Forest_percent)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth(method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2, se = TRUE) +
  theme_bw() + labs( x = "Harvested tree biomass", y = "Forest cover, %")


ggplot(dat_envir, aes(x = Idwcut_mean_2012_2018, y = Forest_percent)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth(method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2, se = TRUE) +
  theme_bw() + labs( x = "Dead wood with saw cuts", y = "Forest cover, %")


ggplot(dat_envir, aes(x = Inonat_mean_tr, y = Forest_percent)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth(method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2, se = TRUE) +
  theme_bw() + labs( x = "Non-natural tree species", y = "Forest cover, %")


## Africulture cover ---------------------------

ggplot(dat_envir, aes(x = Formi_mean_2012_2018, y = Agricultural_percent)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth( method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2) +
  theme_bw()+ labs( x = "Forest Management Intensity", y = "Agricultural lands cover, %")


ggplot(dat_envir, aes(x = Iharv_mean_2012_2018, y = Agricultural_percent)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth(method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2, se = TRUE) +
  theme_bw() + labs( x = "Harvested tree biomass", y = "Agricultural lands cover, %")


ggplot(dat_envir, aes(x = Idwcut_mean_2012_2018, y = Agricultural_percent)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth(method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2, se = TRUE) +
  theme_bw() + labs( x = "Dead wood with saw cuts", y = "Agricultural lands cover, %")


ggplot(dat_envir, aes(x = Inonat_mean_tr, y = Agricultural_percent)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth(method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2, se = TRUE) +
  theme_bw() + labs( x = "Non-natural tree species", y = "Agricultural lands cover, %")


## Urban cover ---------------------------

ggplot(dat_envir, aes(x = Formi_mean_2012_2018, y = Urban_percent)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth( method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2) +
  theme_bw()+ labs( x = "Forest Management Intensity", y = "Urban lands cover, %")


ggplot(dat_envir, aes(x = Iharv_mean_2012_2018, y = Urban_percent)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth(method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2, se = TRUE) +
  theme_bw() + labs( x = "Harvested tree biomass", y = "Urban lands cover, %")


ggplot(dat_envir, aes(x = Idwcut_mean_2012_2018, y = Urban_percent)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth(method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2, se = TRUE) +
  theme_bw() + labs( x = "Dead wood with saw cuts", y = "Urban lands cover, %")


ggplot(dat_envir, aes(x = Inonat_mean_tr, y = Urban_percent)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth(method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2, se = TRUE) +
  theme_bw() + labs( x = "Non-natural tree species", y = "Urban lands cover, %")



## Tree_sp_richness -----------------------


ggplot(dat_envir, aes(x = Formi_mean_2012_2018, y = Tree_sp_richness)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth( method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2) +
  theme_bw()+ labs( x = "Forest Management Intensity", y = "Tree species richness")


ggplot(dat_envir, aes(x = Iharv_mean_2012_2018, y = Tree_sp_richness)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth(method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2, se = TRUE) +
  theme_bw() + labs( x = "Harvested tree biomass", y = "Tree species richness")


ggplot(dat_envir, aes(x = Idwcut_mean_2012_2018, y = Tree_sp_richness)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth(method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2, se = TRUE) +
  theme_bw() + labs( x = "Dead wood with saw cuts", y = "Tree species richness")


ggplot(dat_envir, aes(x = Inonat_mean_tr, y = Tree_sp_richness)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth(method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2, se = TRUE) +
  theme_bw() + labs( x = "Non-natural tree species", y = "Tree species richness")




## Tree_abundance -----------------------


ggplot(dat_envir, aes(x = Formi_mean_2012_2018, y = Tree_abundance)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth( method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2) +
  theme_bw()+ labs( x = "Forest Management Intensity", y = "Tree density")


ggplot(dat_envir, aes(x = Iharv_mean_2012_2018, y = Tree_abundance)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth(method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2, se = TRUE) +
  theme_bw() + labs( x = "Harvested tree biomass", y = "Tree density")


ggplot(dat_envir, aes(x = Idwcut_mean_2012_2018, y = Tree_abundance)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth(method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2, se = TRUE) +
  theme_bw() + labs( x = "Dead wood with saw cuts", y = "Tree density")


ggplot(dat_envir, aes(x = Inonat_mean_tr, y = Tree_abundance)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth(method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2, se = TRUE) +
  theme_bw() + labs( x = "Non-natural tree species", y = "Tree density")



## Vertical_structure -----------------------


ggplot(dat_envir, aes(x = Formi_mean_2012_2018, y = Vertical_structure)) +
  geom_boxplot(outlier.shape = NA,  color = "#086096") +
  geom_jitter(width=0, height=0.2, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  theme_bw()+ labs( x = "Forest Management Intensity", y = "Vertical structure")


ggplot(dat_envir, aes(x = Iharv_mean_2012_2018, y = Vertical_structure)) +
  geom_boxplot(outlier.shape = NA,  color = "#086096") +
  geom_jitter(width=0, height=0.2, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  theme_bw() + labs( x = "Harvested tree biomass", y = "Vertical structure")


ggplot(dat_envir, aes(x = Idwcut_mean_2012_2018, y = Vertical_structure)) +
  geom_boxplot(outlier.shape = NA,  color = "#086096") +
  geom_jitter(width=0, height=0.2, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  theme_bw() + labs( x = "Dead wood with saw cuts", y = "Vertical structure")


ggplot(dat_envir, aes(x = Inonat_mean_tr, y = Vertical_structure)) +
  geom_boxplot(outlier.shape = NA,  color = "#086096") +
  geom_jitter(width=0, height=0.2, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  theme_bw() + labs( x = "Non-natural tree species", y = "Vertical structure")


## Standing_deadwood -----------------------


ggplot(dat_envir, aes(x = Formi_mean_2012_2018, y = Standing_deadwood)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth( method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2) +
  theme_bw()+ labs( x = "Forest Management Intensity", y = "Standing deadwood")


ggplot(dat_envir, aes(x = Iharv_mean_2012_2018, y = Standing_deadwood)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth( method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2) +
  theme_bw() + labs( x = "Harvested tree biomass", y = "Standing deadwood")


ggplot(dat_envir, aes(x = Idwcut_mean_2012_2018, y = Standing_deadwood)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth( method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2) +
  theme_bw() + labs( x = "Dead wood with saw cuts", y = "Standing deadwood")


ggplot(dat_envir, aes(x = Inonat_mean_tr, y = Standing_deadwood)) +
  geom_jitter(width=0, height=0, pch=21, size=2.5, color="brown", fill="#FFA55B") +
  geom_smooth( method = "lm", color = "#086096",fill  = "#86BBD8", alpha = 0.2) +
  theme_bw() + labs( x = "Non-natural tree species", y = "Standing deadwood")






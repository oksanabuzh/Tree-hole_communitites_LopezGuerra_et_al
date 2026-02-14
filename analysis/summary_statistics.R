# Summary statistics and exploratory plots 

library(tidyverse)

# data -------------------------------------------------------
environm <- read_csv("data/processed_data/Environment_ALL.csv") %>% 
  mutate(Month=factor(Month, levels=c("May", "June", "July", "November"))) %>% 
  mutate(Tree_hole_type_coarse=factor(Tree_hole_type_coarse, levels=c("pan", "rot"))) %>% 
  mutate(Tree_hole_type=factor(Tree_hole_type, 
                               levels=c("Division", "Branch", "Trunk", 
                                        "Cut tree", "Root",  NA)))

Diversity_2023_2024 <- read_csv("data/processed_data/Diversity_2023_2024.csv") %>% 
  mutate(Month=factor(Month, levels=c("May", "June", "July", "November"))) %>% 
  mutate(Tree_hole_type_coarse=factor(Tree_hole_type_coarse, levels=c("pan", "rot"))) %>% 
  mutate(Tree_hole_type=factor(Tree_hole_type, 
                               levels=c("Division", "Branch", "Trunk", 
                                        "Cut tree", "Root",  NA)))

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
# ! -------------------
# SEW44_0084_1 and SEW44_0084_2 seems to be same tree hole or have mistake in Tree_hole_type


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
# ! -------------------
# SEW07_0029_1 seems to be same tree hole or have mistake in Tree_hole_type
# see below:
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
    aes(x = 25, y = Month, label = paste0("n = ", n)),
    hjust = 0, size = 3) +
  xlim(0, 28)


# Plot counts ---------------------
plot_counts <- environm %>%
  group_by(Plot, Month) %>%
  count(Plot, .drop = T) %>% 
  arrange(desc(n))

environm %>% 
  filter(Plot %in% 
           (plot_counts %>% filter(n>1) %>% pull(Plot))) %>% 
  select(Plot, Tree_ID, Treehole_number, Year, Month, Sampling_date, Tree_hole_type, Tree_hole_opening) %>% 
  print(n = Inf)



# Diversity metrics  --------------------------------------------

Diversity_2023_2024 %>%  pull(Tree_hole_type) %>%  unique()


hole_type_color <- (c("rot"="#D55E00", 
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
  labs(y= "Abundance", color="Year") +
  theme_bw()

Diversity_2023_2024 %>% 
  ggplot(aes(y=sp_richness, x=Month)) +
  geom_boxplot(outliers = F, notch = F) +
  geom_jitter(width=0.2, height=0, alpha=0.7, 
              color="#086096") +
  labs(y= "Species richnss", color="Year") +
  theme_bw()


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
  ggplot(aes(x=Tree_hole_type, y = sp_richness, color=Tree_hole_type_coarse)) +
  geom_boxplot(outliers = F) +
  geom_jitter(width=0.2, height=0.05, alpha=0.7, size=2) +
  labs(y= "Species richnss", x="Tree-hole type", color="Tree-hole type") +
  scale_color_manual(values=hole_type_color) +
  theme_bw()


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
  ggplot(aes(x=Tree_hole_type_coarse, y = sp_richness,  color=Tree_hole_type_coarse)) +
  geom_boxplot(outliers = F) +
  geom_jitter(width=0.2, height=0.05, alpha=0.7, size=1) +
  labs(y= "Species richnss", x="Tree-hole type", color="Tree-hole type") +
  scale_color_manual(values=hole_type_color) +
  theme_bw()


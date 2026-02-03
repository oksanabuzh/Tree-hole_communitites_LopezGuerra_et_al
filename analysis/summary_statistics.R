# Summary statistics and exploratory plots 

# 

environm <- read_csv("data/processed_data/Environment_ALL.csv")

environm %>% 
  group_by(tree_species) %>% 
  count()

environm %>%
  # remove _ in tree_species names
  mutate(tree_species = str_replace_all(tree_species, "_", " ")) %>%
  ggplot(aes(DBH, tree_species))+
  geom_boxplot() +
  geom_jitter(width=0, height=0.2, alpha=0.7, color="#086096") +
  theme_bw() +
  labs(y="Tree species", x="DBH (cm)") 


# Diversity metrics  --------------------------------------------
Diversity_2023_2024 <- read_csv("data/processed_data/Diversity_2023_2024.csv") %>% 
  mutate(Month=factor(Month, levels=c("May", "June", "July", "November"))) %>% 
  mutate(Tree_hole_type_coarse=factor(Tree_hole_type_coarse, levels=c("pan", "rot"))) %>% 
  mutate(Tree_hole_type=factor(Tree_hole_type, 
                               levels=c("Division", "Branch", "Trunk", 
                                        "Cut tree", "Root",  NA)))


Diversity_2023_2024 %>%  pull(Tree_hole_type) %>%  unique()


hole_type_color <- (c("rot"="#D55E00", 
                      "pan"="#14724C"))  
## Exploratory data checks ------

# Count number of tree holes per tree
# Grouped by year, plot, tree ID, tree type, location, and sampling date
# Shows which trees have the most tree holes 
Diversity_2023_2024 %>% 
  group_by(Year, Month, Plot, Tree_ID, Tree_hole_type, Treehole_number) %>% 
  count() %>%  
  arrange(desc(n))            


# Count tree holes per plot
# Grouped only by year and plot
# Shows distribution of tree holes across plots 
Diversity_2023_2024 %>% 
  group_by(Year, Plot  
  ) %>% 
  count(Treehole_number) %>%  
  arrange(desc(n))             

Diversity_2023_2024 %>% 
  select(Year, Month, Plot, Treehole_number, abundance, sp_richness) %>% 
  arrange(abundance) %>% 
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
  geom_jitter(width=0.2, height=0, alpha=0.7) +
  scale_color_manual(values=hole_type_color) +
  labs(y= "Abundance", x="Tree-hole type", color="Tree-hole type") +
  theme_bw()

Diversity_2023_2024 %>% 
  filter(!is.na(Tree_hole_type)) %>%
  ggplot(aes(x=Tree_hole_type, y = sp_richness, color=Tree_hole_type_coarse)) +
  geom_boxplot(outliers = F) +
  geom_jitter(width=0.2, height=0, alpha=0.7) +
  labs(y= "Species richnss", x="Tree-hole type", color="Tree-hole type") +
  scale_color_manual(values=hole_type_color) +
  theme_bw()


#  Tree_hole_type_coarse

Diversity_2023_2024 %>% 
  filter(!is.na(Tree_hole_type_coarse)) %>%
  ggplot(aes(x=Tree_hole_type_coarse, y = abundance, color=Tree_hole_type_coarse)) +
  geom_boxplot(outliers = F) +
  geom_jitter(width=0.2, height=0, alpha=0.7) +
  labs(y= "Abundance", x="Tree-hole type", color="Tree-hole type") +
  scale_color_manual(values=hole_type_color) +
  theme_bw()

Diversity_2023_2024 %>% 
  filter(!is.na(Tree_hole_type_coarse)) %>%
  ggplot(aes(x=Tree_hole_type_coarse, y = sp_richness,  color=Tree_hole_type_coarse)) +
  geom_boxplot(outliers = F) +
  geom_jitter(width=0.2, height=0, alpha=0.7) +
  labs(y= "Species richnss", x="Tree-hole type", color="Tree-hole type") +
  scale_color_manual(values=hole_type_color) +
  theme_bw()

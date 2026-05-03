# Purpose: to run ordination for species composition 
dev.off()
library(tidyverse)
library(vegan)
library(ggrepel)

# data

## Community data ---------------

Community_data <-read_csv("data/processed_data/Community_2023_2024_DNAcorrected.csv") %>% 
  select(Treehole_number, Sp_ID_DNAcorrected, Abundance) %>%
  pivot_wider(names_from = Sp_ID_DNAcorrected, values_from = Abundance, values_fill = 0) %>%
  column_to_rownames("Treehole_number") 

names(Community_data)

# is each treehole unique in the data?
read_csv("data/processed_data/Diversity_2023_2024.csv") %>% 
  count(Treehole_number) %>%  arrange(desc(n))

## Environment data ---------------

environm <- read_csv("data/processed_data/Environment_ALL.csv") %>% 
  mutate(Month=factor(Month, levels=c("May", "June", "July", "November"))) %>% 
  mutate(Tree_hole_type_coarse=factor(Tree_hole_type_coarse, levels=c("pan", "rot"))) %>% 
  mutate(Tree_hole_type=factor(Tree_hole_type, 
                               levels=c("Division", "Branch", "Trunk", 
                                        "Cut tree", "Root",  NA))) %>% 
  left_join(read_csv("data/processed_data/Landscape_heterogeneity.csv") %>% 
              filter(buffer_size_m==500) %>% 
              select(plotID, LandType_richness_class_2, Forest_percent, 
                     Agricultural_percent, Water_bodies_percent, Urban_percent),
            by=c(Plot="plotID")) 

str(environm)

# check if environment data merge with community data
read_csv("data/processed_data/Community_2023_2024_DNAcorrected.csv") %>% 
  select(Treehole_number, Sp_ID_DNAcorrected, Abundance) %>% 
  left_join(environm, by=c("Treehole_number")) %>% 
  filter(is.na(Plot))
# no NAs

## Trait data ---------------

trait_data <- read_csv("data/processed_data/Traits_2023_2024_final_DNA_corrected.csv")

# check if traits merge with community data
read_csv("data/processed_data/Community_2023_2024_DNAcorrected.csv") %>% 
  select(Treehole_number, Sp_ID_DNAcorrected, Abundance) %>% 
  left_join(trait_data, by=c("Sp_ID_DNAcorrected")) %>% 
  filter(is.na(Species))
# no NAs

# Data exploration -----
# check data sets for NA‘s
anyNA(Community_data) # no NA's

## Linear or nonlinear methods to use? ----
# check gradient length of first DCA axis (optional)
# if axis lengths for DCA1 is 
# <3 -> linear methods (PCA)
# >3 -> nonlinear methods (CCA)
# in any case non metric distance based methods can be used (NMDS or PCoA)
decorana((Community_data)) 
# we can perform NMDS 

## Graphical data exploration -----

### 1) Frequency of occurrence per species (number of treeholes where species is present)

sp_dat <- readr::read_csv("data/processed_data/Community_2023_2024_DNAcorrected.csv") %>%
  select(Treehole_number, Sp_ID_DNAcorrected, Abundance) %>%
  left_join(trait_data, by = "Sp_ID_DNAcorrected") 

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
library(scales)
library(forcats)

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



# we see large differences in abundances
# therefore the ordination can be dominated by dominant taxa

# wisconsin transformation in NMDS  removes the influence of dominant abundance, so that dominant species don't dominate the ordination.
species_data %>%
  wisconsin() %>% 
  pivot_longer(everything(), names_to = "Species", 
               values_to = "Abundance") %>% 
  ggplot(aes(x = Abundance, y = Species)) +
  geom_boxplot() 


# check also plots:
species_data %>% 
  rownames_to_column("plot_ID") %>% 
  pivot_longer(- plot_ID, values_to = "abund", names_to = "species") %>% 
  group_by( plot_ID) %>% 
  summarise(sum=sum(abund))%>% 
  ggplot(aes(x = sum, y = plot_ID)) +
  geom_bar(stat = "identity") 

# wisconsin transformation in NMDS  removes the influence of overall abundance at a plot, so that sites with higher total species counts don't dominate the ordination.

species_data %>% 
  wisconsin() %>% 
  rownames_to_column("plot_ID") %>%  
  pivot_longer(- plot_ID, values_to = "abund", names_to = "species") %>% 
  group_by( plot_ID) %>% 
  summarise(sum=sum(abund))%>% 
  ggplot(aes(x = sum, y = plot_ID)) +
  geom_bar(stat = "identity") 



set.seed(2435)
ord_mod <- metaMDS(wisconsin(species_data), 
                   scale = FALSE, distance = "bray") 

ord_mod

# NMDS fit
ord_mod$stress
# fit
stressplot(ord_mod, main = "Shepard plot")


# Permutation test:  --------------------------------------
set.seed(10)
PERM1 <- vegan::adonis2(species_data ~ 
                          MowFreq + Month + 
                          n_mow_events_befre_sampling, 
                        data=plot_data,
                        permutations = 1000, method = "bray",
                        strata=as.factor(plot_data$PlotNo),
                        by = "terms")

PERM1



# variable fitting for posthoc plotting  ------------------
set.seed(1259)
fit2 <- vegan::envfit(ord_mod   ~  
                        MowFreq + Month + 
                        n_mow_events_befre_sampling, 
                      data=plot_data,
                      #  strata=as.factor(plot_data$PlotNo),
                      perm=1000) #


fit2



# exploratory plot
plot(ord_mod, main = "NMDS plot", display = "sites")
plot(ord_mod, main = "NMDS plot", display = "species")
plot(ord_mod, main = "NMDS plot")
plot(fit2)

### Plotting NMDS results using the ggplot --------------------------------------

# extract species scores
sp.scrs <- scores(ord_mod, display = "species",
                  scaling = "species") %>% 
  as_tibble(rownames = "Taxon_EuroMed") %>% 
  left_join(Trait_data, by="Taxon_EuroMed") %>% 
  mutate(species_full_name=Taxon_EuroMed,
         Taxon_EuroMed = if_else(
           str_count(Taxon_EuroMed, "\\S+") == 1,      # If only one word (non-space sequence)
           paste(Taxon_EuroMed, "sp."),                # add "sp."
           Taxon_EuroMed                               # else keep as is
         ),
         Taxon_EuroMed = str_c(str_split_i(Taxon_EuroMed, '\\s+', 1) %>%    # splits the species_name at each empty space in the species name and extracts the first word (the genus)
                                 str_sub(.,  1, 5 ),          #  in this string (".") subtracts first 4 letters of genus (start, end 
                               str_split_i( Taxon_EuroMed, '\\s+', 2) %>%   # gets the second part of the species name after the first empty space (species)
                                 str_sub(., 1, 3),            #  subtracts first 5 letters of from that second part (species)
                               sep = '.')) %>% 
  mutate(Taxon_EuroMed = ifelse(Taxon_EuroMed=="Plant.(ro", "Plantae", Taxon_EuroMed)) %>% 
  mutate(trait=fct_relevel(status,"endangered",
                           "vulnerable", 
                           "least-concerned",
                           "neophytes", 
                           "data insufficient"))



sp.scrs %>% 
  pull(status) %>% 
  unique()


# extract plot scores 
plot.scrs <- scores(ord_mod, display = "sites",
                    scaling = "sites") %>% 
  as_tibble(rownames = "Plot") %>% 
  left_join(predictor_data, by="Plot") 

plot.scrs

names(plot.scrs)

# calculate centroid for  Grazing_season
centroid_mowing <- scores(fit1, 
                          display="cn",  
                          scaling="species") %>%   
  as_tibble(rownames = "treatment")  %>%
  filter(str_detect(treatment, "MowFreq")) %>% 
  mutate(MowFreq=stringr::str_sub(treatment, 8)) %>% 
  dplyr::select(-treatment) %>% 
  rename(NMDS1_mowing=NMDS1,
         NMDS2_mowing=NMDS2)

centroid_mowing

centroid_month <- scores(fit2, 
                         display="cn",  
                         scaling="species") %>%   
  as_tibble(rownames = "treatment")  %>%
  filter(str_detect(treatment, "Month")) %>% 
  mutate(Month=stringr::str_sub(treatment, 6)) %>% 
  dplyr::select(-treatment) %>% 
  rename(NMDS1_month=NMDS1,
         NMDS2_month=NMDS2)

centroid_month

# centroid for interaction from raw data
centroids <- plot.scrs %>% 
  group_by(MowFreq, Month) %>% 
  summarise(NMDS1_centroid=mean(NMDS1),
            NMDS2_centroid=mean(NMDS2)) %>% 
  ungroup() %>% 
  left_join(centroid_mowing, by=c("MowFreq")) %>% 
  left_join(centroid_month, by=c("Month")) %>%
  mutate(Mowing=case_when(
    MowFreq == "reduced_sown" ~ "reduced mowing & sowing",
    MowFreq == "regular" ~ "regular mowing",
    MowFreq == "reduced" ~ "reduced mowing",
    TRUE ~ as.character(MowFreq)))

centroids


# merge with site scores, order levels of categorical predictors
plot.scrs <- plot.scrs %>%
  left_join(centroids, by=c("MowFreq", "Month")) %>%
  mutate(Mowing=fct_relevel(Mowing,"regular mowing", "reduced mowing", "reduced mowing & sowing")) %>% 
  mutate(Month=fct_relevel(Month,"March", "May", "July", "September")) 


plot.scrs

# plot for plots data
set.seed(11)
# plot for plots data
plot1 <- ggplot(data=plot.scrs, 
                aes(x=NMDS1, y=NMDS2))+
  geom_hline(yintercept = 0, color="grey", lty =1) +
  geom_vline(xintercept = 0, color="grey", lty =1) +
  # spiders
  
  geom_segment(data = plot.scrs,        
               mapping = aes(xend = NMDS1_centroid, yend = NMDS2_centroid, 
                             color=Mowing),
               alpha=0.5) +
  # add plot scores as point:
  geom_point(data=plot.scrs, 
             aes(x=NMDS1, y=NMDS2, 
                 color=Mowing),
             size=1.5, pch=21) + 
  # add centroids as point:
  geom_point(data=plot.scrs, 
             aes(x=NMDS1_centroid, y=NMDS2_centroid, 
                 color=Mowing),
             size=3, pch=18) + 
  # centroids as text
  geom_text_repel(data=centroids, 
                  #geom_text(data=centroids, 
                  aes(x=NMDS1_centroid, y=NMDS2_centroid, 
                      color=Mowing, label = Month), 
                  size=5, fontface="bold", show.legend = F) +
  theme_bw()+
  scale_color_manual(values = c("#F8766D", "#00B0F6","#00BA38"))+
  labs(color="Management")

print(plot1)
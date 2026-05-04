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


species_rank <- read_csv("data/processed_data/Species_rank.csv")

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
decorana(Community_data) 
# we can perform NMDS 



Community_data %>%
  pivot_longer(everything(), names_to = "Species", 
               values_to = "Abundance") %>% 
  ggplot(aes(x = Abundance, y = Species)) +
  geom_boxplot() 

# wisconsin transformation in NMDS  removes the influence of dominant abundance, so that dominant species don't dominate the ordination.

set.seed(2435)
ord_mod <- metaMDS(wisconsin(Community_data), 
                   scale = FALSE, distance = "bray") 

ord_mod

# NMDS fit
ord_mod$stress
# fit
stressplot(ord_mod, main = "Shepard plot")


# Permutation test:  --------------------------------------
set.seed(10)

PERM1 <- vegan::adonis2(wisconsin(Community_data) ~ 
                          Inonat_mean_2012_2018 + 
                          Iharv_mean_2012_2018 + 
                          Idwcut_mean_2012_2018 +
                          LandType_richness_class_2 +
                          Tree_sp_richness +
                          Forest_percent + 
                          precipitation_radolan_mean, 
                        data=environm,
                        permutations = 1000, method = "bray",
                     #   strata=as.factor(environm$Plot),
                        by = "terms")

PERM1

set.seed(10)
PERM2 <- vegan::adonis2(wisconsin(Community_data) ~ 
                          Formi_mean_2012_2018 + ssci + 
                          Openness, 
                        data=environm,
                        permutations = 1000, method = "bray",
                        #  strata=as.factor(environm$Plot),
                        by = "terms")

PERM2




# variable fitting for posthoc plotting  ------------------
set.seed(1259)
fit1 <- vegan::envfit(ord_mod   ~  
                        Formi_mean_2012_2018 +
                        Inonat_mean_2012_2018 + 
                        Iharv_mean_2012_2018 + 
                        Idwcut_mean_2012_2018 +
                        LandType_richness_class_2 +
                        Tree_sp_richness +
                        Forest_percent + 
                        Openness +
                        precipitation_radolan_mean, 
                      data=environm,
                      #  strata=as.factor(plot_data$PlotNo),
                      perm=1000) #


fit1


# extract vector scores (NMDS axes), r and p-values
vec_scores <- as.data.frame(scores(fit1, display = "vectors")) %>%
  rownames_to_column("Term") %>% 
  mutate(NMDS1 = round(NMDS1, 3), 
         NMDS2 = round(NMDS2, 3))

# attach r and p-values from the envfit object
if (!is.null(fit1$vectors)) {
  vec_scores <- vec_scores %>%
    mutate(
      r2   = round(fit1$vectors$r[Term],3),
      pval = round(fit1$vectors$pvals[Term], 3)
    )
}

# add significance codes (optional)
vec_scores <- vec_scores %>%
  mutate(sig = case_when(
    is.na(pval) ~ NA_character_,
    pval <= 0.001 ~ "***",
    pval <= 0.01  ~ "**",
    pval <= 0.05  ~ "*",
    pval <= 0.1   ~ ".",
    TRUE ~ ""
  ))

# reorder columns
vec_scores <- vec_scores %>%
  select(Term, NMDS1, NMDS2, r2, pval, sig) %>% 
  mutate(Term=case_when(
    Term=="Formi_mean_2012_2018" ~ "Forest management intensity",
    Term=="Inonat_mean_2012_2018" ~ "Non-natural tree species",
    Term=="Iharv_mean_2012_2018" ~ "Harvested tree biomass",
    Term=="Idwcut_mean_2012_2018" ~ "Dead wood with saw cuts", 
    Term=="LandType_richness_class_2"  ~ "Landscape heterogeneity",
    Term=="Tree_sp_richness" ~ "Tree species richness",
    Term=="Forest_percent" ~ "Forest cover, %",
    Term=="Openness" ~ "Open areas, % ha⁻¹",
    Term=="precipitation_radolan_mean" ~ "Precipitation",
    TRUE ~ Term))
    
    

# inspect
vec_scores

# save
write_csv(vec_scores, "results/NMDS_envfit_table.csv")

# exploratory plot
plot(ord_mod, main = "NMDS plot", display = "sites")
plot(ord_mod, main = "NMDS plot", display = "species")
plot(ord_mod, main = "NMDS plot")
plot(fit1)

### Plotting NMDS results using the ggplot --------------------------------------

# extract species scores
sp.scrs <- scores(ord_mod, display = "species",
                  scaling = "species") %>% 
  as_tibble(rownames = "Sp_ID_DNAcorrected") %>% 
  left_join(trait_data, by="Sp_ID_DNAcorrected") %>% 
  left_join(species_rank, by="Species") 


sp.scrs


# extract plot scores   --------------------------------------------------
plot.scrs <- scores(ord_mod, display = "sites",
                    scaling = "sites") %>% 
  as_tibble(rownames = "Treehole_number") %>% 
  left_join(environm, by="Treehole_number") 

plot.scrs



# vector --------------------------------------------------
vector.scrs <- scores(fit1, display = "bp", # vector
                      scaling = "species") %>% 
  as_tibble(rownames = "Drivers")  %>% 
#  filter(Drivers %in% c("Formi_mean_2012_2018", "Inonat_mean_2012_2018",
#                        "Iharv_mean_2012_2018", "Idwcut_mean_2012_2018" )  %>% 
  mutate(NMDS1=NMDS1*2.5, NMDS2=NMDS2*2.5) %>%  # rescale for better visualization
  mutate(Drivers=case_when(
    Drivers=="Formi_mean_2012_2018" ~ "ForMI",
    Drivers=="Inonat_mean_2012_2018" ~ "Nonat.trees",
    Drivers=="Iharv_mean_2012_2018" ~ "Harvst.trees",
    Drivers=="Idwcut_mean_2012_2018" ~ "DeadwoodCuts",
    Drivers=="LandType_richness_class_2"  ~ "LandscapeHeter",
    Drivers=="Tree_sp_richness" ~ "TreeSR",
    Drivers=="Forest_percent" ~ "ForestCover",
    Drivers=="Openness" ~ "Openness",
    Drivers=="precipitation_radolan_mean" ~ "Precipit",
                      TRUE ~ Drivers))
vector.scrs




# plots -----------
set.seed(11)
# adjust label positions for vectors
text_for_vectors <- vector.scrs %>%
  mutate(len = sqrt(NMDS1^2 + NMDS2^2),
         # push label 15% further out along same direction
         lx = NMDS1 * 1.2,
         ly = NMDS2 * 1.2)


set.seed(11)

plot1 <- ggplot(data=sp.scrs %>% 
                  mutate(Species = fct_reorder(Species, species_rank) %>% forcats::fct_rev()), 
                aes(x=NMDS1, y=NMDS2))+
  geom_hline(yintercept = 0, color="grey", lty =1) +
  geom_vline(xintercept = 0, color="grey", lty =1) +
    # vectors
      geom_segment(data=vector.scrs, 
                 aes(x=0, y=0, xend=NMDS1, yend=NMDS2), 
                 arrow=arrow(length=unit(0.3,"cm")), 
                 color="gray23", linewidth=1) +
  
    geom_text(data=text_for_vectors, 
              aes(x = lx, y = ly, label=Drivers), 
              color="black", fontface="bold", 
             size=4,
             
             hjust=c(1, 1, 0.6,
                     0, 0.3, 0.4,
                     0.7, 0, 0.5), 
             vjust=c(-0.7, 0, 0.3,
                     0.3, 1, -0.2,
                     0.7, 0, 0.5)) +
  
  # species
  geom_point(aes(x=NMDS1, y=NMDS2, fill=Family_DNA_corrected, 
                 color=Family_DNA_corrected,
                 size=(dry_weight_mg)), 
             # size = 3
             pch=21,
             alpha=0.8)+
  geom_text_repel(aes(x=NMDS1, y=NMDS2, color=Family_DNA_corrected,
                      label = Species), alpha=1,
                  size=3.3, fontface="bold", show.legend = F,
                  max.overlaps=Inf) +
  theme_bw()+
 # guides(color = guide_legend(override.aes = list(size = 3)))  +
  guides(
    fill = guide_legend(override.aes = list(size = 5, 
                                            colour = "black", 
                                            alpha=1)), 
    size = guide_legend(override.aes = list(
      #  shape = 21,            # ensure shape uses fill+border
    #  stroke = 1, 
      colour = "black",     # border color in size legend
      fill = "grey80"        # interior color in size legend
    ))
  )+
  labs(x="NMDS1", y="NMDS2", fill="Family", 
       color="Family", size="Dry body mass (g)")


print(plot1)


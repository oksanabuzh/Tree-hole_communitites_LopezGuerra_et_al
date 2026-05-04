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

## Linear or nonlinear methods to use? ----
# check gradient length of first DCA axis (optional)
# if axis lengths for DCA1 is 
# <3 -> linear methods (PCA)
# >3 -> nonlinear methods (CCA)
# in any case non metric distance based methods can be used (NMDS or PCoA)
decorana((Community_data)) 

# we would use CCA as axis lengths >3

# Canonical correspondence analysis
set.seed(1)
ord_mod <- cca(Community_data, scale = FALSE) # scale data to have the same units
ord_mod


summary(eigenvals(ord_mod))


set.seed(1)
ord_mod2 <- cca(species_data ~ MowFreq + Month + 
                 n_mow_events_befre_sampling, data = predictor_data,
               scale = FALSE) # scale data to have the same units
ord_mod2
anova(ord_mod2, strata = as.factor(predictor_data$PlotNo), # random effects
                     by= "terms") # each term (sequentially from first to last),

ord_effects <- anova(ord_mod2, strata = as.factor(predictor_data$PlotNo), # random effects
                     by= "terms") # each term (sequentially from first to last), depends on the order
ord_effects

vif.cca(ord_mod2)
# proportion variance explained by CCA axes
summary(eigenvals(ord_mod2))
# adjusted R2
RsquareAdj(ord_mod2)


#-------------------------------------------------------------------------------#

# extract species scores
sp.scrs <- scores(ord_mod, display = "species",
                  scaling = "species") %>% 
  as_tibble(rownames = "Sp_ID_DNAcorrected") %>% 
  left_join(trait_data, by="Sp_ID_DNAcorrected")
  

sp.scrs


# extract plot scores   --------------------------------------------------
plot.scrs <- scores(ord_mod, display = "sites",
                    scaling = "sites") %>% 
  as_tibble(rownames = "Treehole_number") %>% 
  left_join(environm, by="Treehole_number") 

plot.scrs



# centroids --------------------------------------------------
# calculate centroid for  Grazing_season 
centroid_mowing <- scores(ord_mod, 
                          display="cn",  
                          scaling="species") %>%   
  as_tibble(rownames = "treatment")  %>%
  filter(str_detect(treatment, "MowFreq")) %>% 
  mutate(MowFreq=stringr::str_sub(treatment, 8)) %>% 
  dplyr::select(-treatment) %>% 
  rename( RDA1_mowing= RDA1,
          RDA2_mowing= RDA2)

centroid_mowing

centroid_month <- scores(ord_mod, 
                         display="cn",  
                         scaling="species") %>%   
  as_tibble(rownames = "treatment")  %>%
  filter(str_detect(treatment, "Month")) %>% 
  mutate(Month=stringr::str_sub(treatment, 6)) %>% 
  dplyr::select(-treatment) %>% 
  rename( RDA1_month= RDA1,
          RDA2_month= RDA2)

centroid_month

# centroid for interaction from raw data
centroids <- plot.scrs %>% 
  group_by(MowFreq, Month) %>% 
  summarise( RDA1_centroid=mean( RDA1),
             RDA2_centroid=mean( RDA2)) %>% 
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

# plots ----

summary(eigenvals(ord_mod)) %>% 
  as_tibble(rownames = "Axis") %>% 
  select(Axis, CA1, CA2) %>% 
  filter(Axis %in% c("Proportion Explained", "Cumulative Proportion")) %>%
  mutate(CA1=round(CA1, 3)*100,
         CA2=round(CA2, 3)*100)

## plot for site data -----

plot.scrs %>% 
  names()

set.seed(11)
plot1 <- ggplot(data=plot.scrs, 
                aes(x= CA1, y= CA2))+
  geom_hline(yintercept = 0, color="grey", lty =1) +
  geom_vline(xintercept = 0, color="grey", lty =1) +
  # spiders
  
#  geom_segment(data = plot.scrs,        
#               mapping = aes(xend =  RDA1_centroid, yend =  RDA2_centroid, 
#                             color=Mowing),
#               alpha=0.9) +
  # add plot scores as point:
  geom_point(data=plot.scrs, 
             aes(x= CA1, y= CA2, 
                 color=Tree_hole_type_coarse),  # Tree_hole_type_coarse
             size=1.5, pch=21) + 
  # add centroids as point:
#  geom_point(data=plot.scrs, 
#             aes(x= RDA1_centroid, y= RDA2_centroid, 
#                 color=Mowing),
#             size = 3,  
#             alpha=1, pch=18) + 
  # centroids as text
#  geom_text_repel(data=centroids, 
                  #geom_text(data=centroids, 
#                  aes(x= RDA1_centroid, y= RDA2_centroid, 
#                      color=Mowing, label = Month), 
#                  size=5, fontface="bold", show.legend = F) +
  theme_bw()+
#  scale_color_manual(values = c("#F8766D", "#00B0F6","#00BA38"))+
  labs(color="Type",  x="CA1 (17.1 %)", y=" CA2 (14.5 %)")

print(plot1)


# ggsave(" RDA_plot1.png", plot1, width = 6, height = 6, dpi = 350)
# ggsave(" RDA_plot1.jpeg", plot1, width = 6, height = 6, dpi = 350)

## mowing plot for species data -----

sp.scrs %>% names()

set.seed(11)
plot2 <- ggplot(data=plot.scrs, 
                aes(x= CA1, y= CA2))+
  geom_hline(yintercept = 0, color="grey", lty =1) +
  geom_vline(xintercept = 0, color="grey", lty =1) +
  # ellipse 
#  stat_ellipse(aes(fill=Mowing), alpha=0.2,
#               type='t', # type = 't' means the ellipses are calculated assuming a multivariate t-distribution (robust to outliers)
#               linewidth =0.0001, geom="polygon",
#               level=0.95, # ellipses represent a 95% confidence interval for the multivariate mean of each group) +
#               color="gray88") +
  # vector
#  geom_segment(data=vector.scrs, 
#               aes(x=0, y=0, xend=RDA1, yend=RDA2), 
#               arrow=arrow(length=unit(0.3,"cm")), 
#               color="gray23", linewidth=1) +
  
#  geom_text(data=vector.scrs, 
#            aes(RDA1, RDA2, label="Mowing"), 
#            color="black", fontface="bold", 
#           size=5, hjust=0.3, vjust=-0.3) +
  
  # species
  geom_point(data=sp.scrs, 
             aes(x= CA1, y= CA2, color=Family_DNA_corrected, 
                 size=log(dry_weight_mg)), 
            # size = 3,  
             alpha=1)+
  geom_text_repel(data=sp.scrs, 
                  aes(x= CA1, y= CA2, color=Family_DNA_corrected,
                      label = Species), 
                  size=3, fontface="bold", show.legend = F,
                  max.overlaps=Inf) +
  theme_bw()+
  guides(color = guide_legend(override.aes = list(size = 3))) + # makes legend dots large
#  scale_fill_manual(values = c(
#    "regular mowing" = "red", ##F8766D",
#    "reduced mowing" = "#00B0F8", # "#00B0F6",  #"yellow3",
#    "reduced mowing & sowing" = "green3" #"#00BA38" # "#00B0F6"
#  )) +
  #  ylim(-0.3, 0.45)+
 # labs(color="Functional category", fill="Management",
  labs(color="Family_DNA_corrected",  x="CA1 (17.1 %)", y=" CA2 (14.5 %)")


print(plot2)

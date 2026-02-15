# Script: Data Preparation for Tree Hole Community Analysis

# Purpose: Import, clean, and combine tree hole community data from 2023-2024
#          sampling campaigns for biodiversity analysis

# Packages ---------------------------------------------------------------------

library(tidyverse)
library(conflicted)
# Prefer dplyr's select whenever there is a conflict
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")


# 2023 DATA --------------------------------------------------------------------

# Import 2023 data and calculate species abundance per tree hole
community2023 <- read_csv("data/raw_data/sampling_2023_2024/Species_and_trees_from_the_sampling_SEW2023.csv") %>% 
  pivot_longer(Clogmia_sp:Phaonia_subventa, 
               names_to = "Sp_ID", 
               values_to = "Abundance",
               values_drop_na = TRUE)


str(community2023)
  
# Check for duplicates (should be 0 rows)
community2023 %>% 
  group_by(Plot, Tree_ID, Treehole_number, Type_of_tree, 
           Tree_hole_type, Sampling_date, Sp_ID) %>%
  count() %>% 
  filter(n > 1)

# 2024 DATA --------------------------------------------------------------------

# Import 2024 data and sum abundance
community2024 <- read_csv("data/raw_data/sampling_2023_2024/Sampling_2024_indiviudlas.csv") %>% 
  select(-Nickname_2014, -Nickname_2015, -"ID for the sampling", 
         -"Label", -"Notes", -"Pictures") %>% 
  summarise(Abundance = sum(Abundance), 
            .by = c("Plot", "Tree_ID", "Treehole_number", "Type_of_tree", 
                    "Outside", "Tree_hole_type", "Sampling_date", "Sp_ID")) 

# Check for duplicates (should be 0 rows)
community2024 %>% 
  group_by(Plot, Tree_ID, Treehole_number, Type_of_tree, 
           Tree_hole_type, Sampling_date, Sp_ID) %>%
  count() %>% 
  filter(n > 1)

# COMBINE DATA -----------------------------------------------------------------

# Merge 2023 and 2024, standardize dates, extract month
Community_2023_2024 <- bind_rows(
  "2023" = community2023, 
  "2024" = community2024, 
  .id = "Year"
) %>% 
  relocate(Year, .after = Treehole_number) %>% 
  filter(!is.na(Sp_ID)) %>%  # Remove rows with missing species ID as those were pupae unidentified
# Standardize date format to YYYY-MM-DD
  mutate(Sampling_date = case_when(
    Sampling_date == "06/11/2023" ~ "2023-11-06",
    Sampling_date == "07/11/2023" ~ "2023-11-07",
    Sampling_date == "13/06/2024" ~ "2024-06-13",
    Sampling_date %in% c("16/05/2024", "16/5/2024") ~ "2024-05-16",
    Sampling_date %in% c("16/06/2024", "16/6/2024") ~ "2024-06-16",
    Sampling_date == "17/05/2024" ~ "2024-05-17",
    Sampling_date %in% c("17/6/2024", "17/06/2024") ~ "2024-06-17",
    Sampling_date == "22/05/2024" ~ "2024-05-22",
    Sampling_date == "22/07/2024" ~ "2024-07-22",
    Sampling_date == "27/05/2024" ~ "2024-05-27",
    Sampling_date == "21/5/2024" ~ "2024-05-21",
    Sampling_date == "23/07/2024" ~ "2024-07-23",
    Sampling_date == "19/06/2024" ~ "2024-06-19")) %>% 
# Extract month name for seasonal analysis
  mutate(Month = case_when(
    Sampling_date == "2023-11-06" ~ "November",
    Sampling_date == "2023-11-07" ~ "November",
    Sampling_date == "2024-06-13" ~ "June",
    Sampling_date == "2024-05-16" ~ "May",
    Sampling_date == "2024-06-16" ~ "June",
    Sampling_date == "2024-05-17" ~ "May",
    Sampling_date == "2024-06-17" ~ "June",
    Sampling_date == "2024-05-22" ~ "May",
    Sampling_date == "2024-07-22" ~ "July",
    Sampling_date == "2024-05-27" ~ "May",
    Sampling_date == "2024-05-21" ~ "May",
    Sampling_date == "2024-06-19" ~ "June",
    Sampling_date == "2024-07-23" ~ "July"), 
    .after = Year) %>% 
  mutate(Tree_hole_type_coarse = case_when(
    Tree_hole_type %in% c("Branch", "Cut tree", "Trunk", "Root") ~ "rot",
    Tree_hole_type %in% c("Division") ~ "pan",
    .default = Tree_hole_type), .after = Tree_hole_type)%>% 
  mutate(Tree_hole_opening = case_when(
    Tree_hole_type %in% c("Trunk", "Root") ~ "side_opening",
    Tree_hole_type %in% c("Division","Branch", "Cut tree") ~ "top_opening",
    .default = Tree_hole_type), .after = Tree_hole_type_coarse) %>% 
  relocate(Sampling_date, .after = Month) %>% 
# Correct Tree_ID based on known issues from missing_tree_species.csv
  left_join(read_csv("data/processed_data/missing_tree_species.csv") %>% 
              group_by(Plot, Tree_ID, Reason_missing, Tree_ID_Bexis, Tree_ID_Bexis_devision_2) %>% 
              summarise(count = n(), .groups = "drop") %>% 
              select(-count),
            by = c("Plot", "Tree_ID")) %>%
  mutate(Tree_ID=case_when(Reason_missing=="Misspelling" ~ Tree_ID_Bexis,
                           Reason_missing=="Idenfitication/GPS_problem" ~ Tree_ID_Bexis,
                           Reason_missing=="Division" ~ Tree_ID_Bexis, # tree a in tree division is always larger than tree b
                           .default=Tree_ID)) %>% 
  select(-Reason_missing, -Tree_ID_Bexis, -Tree_ID_Bexis_devision_2)

# EXPLORATORY CHECKS -----------------------------------------------------------

names(Community_2023_2024)

Community_2023_2024 %>% 
  group_by(Sp_ID) %>%
  count() 


Community_2023_2024 %>% 
  group_by(Tree_hole_type) %>%
  count()


Community_2023_2024 %>% print(n = Inf)

# Check for missing species IDs
Community_2023_2024 %>% filter(is.na(Sp_ID))

# Sampling summary by year and date
Community_2023_2024 %>% 
  group_by(Year, Sampling_date) %>%
  count() 

Community_2023_2024 %>% 
  select(Plot, Tree_ID, Treehole_number, Year, Sampling_date) %>%
  filter(is.na(Sampling_date))


Community_2023_2024 %>% filter(is.na(Sampling_date))





## Write data -----------------------------------------------------------------------

names(Community_2023_2024)

### Community data ------
Community_2023_2024 %>% 
  #select(Plot, Tree_ID, Treehole_number, Tree_hole_type, Tree_hole_opening,
  select(Plot, Plot, Tree_ID, Treehole_number, Year, Month, Sampling_date, 
         Tree_hole_type, Tree_hole_type_coarse, Tree_hole_opening, Outside,
         Year, Month, Sp_ID, Abundance) %>% 
  write_csv("data/processed_data/Community_2023_2024.csv")

### Environmental data  --------------------------
Community_2023_2024 %>% 
  select(-Sp_ID, -Abundance, -Type_of_tree) %>% 
  distinct() %>%
  write_csv("data/processed_data/Environment_2023_2024.csv")

# Check unique tree holes in environmental data
Community_2023_2024 %>% 
  select(-Sp_ID, -Abundance, -Type_of_tree) %>% 
  group_by(Plot, Tree_ID, Treehole_number, Year, Month, Sampling_date, 
           Tree_hole_type, Tree_hole_type_coarse, Tree_hole_opening, Outside) %>%
  count()

# EXPLORATORIES DATA -----------------------------------------------------------

# Import own environmental data
Environment_2023_2024 <- read_csv("data/processed_data/Environment_2023_2024.csv")


# Tree data --------------------------------------------------------------------
# 31487_7
# single tree data, on all forest EPs, 2020 - 2023 (year is not a group, as different trees measured in different years)
# original data on species, diameter at breast height, cm (d), tree heigth, m (h), 
# wood volume, m3 (v) and geographical location (caliper limit: dbh >= 7 cm).
# tree -tree ID
# species - Tree species (name shortened to 16 characters)

tree_data <- read_csv("data/raw_data/BiodExpl/31487_7_data.csv") %>% 
  filter(Exploratory == "SCH") %>% 
  rename(tree_heigth = h,
         DBH = d,
         wood_volume = v) %>% 
  select(EP, tree, species, DBH, tree_heigth, wood_volume, year)  

tree_data


tree_data %>% 
  filter(EP=="SEW45")

# MERGE COMMUNITY AND TREE DATA 
merged_tree_data <- Environment_2023_2024 %>% 
  left_join(tree_data, by = c("Tree_ID" = "tree")) %>% 
  mutate(tree_species = ifelse(is.na(species), "Fagus_sylvatica", species),
        .after=Tree_ID, 
        .keep = "all") # removes species (used in mutate)

names(merged_tree_data)

# Check for missing species in merged data
merged_tree_data %>% 
  pull(tree_species) %>% 
  unique()

merged_tree_data %>% 
  filter(!Outside==TRUE) %>% 
  filter(is.na(species)) %>% 
  print(n=Inf)

# two Tree_IDs are not having coordinates in our data
# Plot  Tree_ID  tree_species    Treehole_number  Year Month Sampling_date Tree_hole_type Tree_hole_type_coarse
#   1 SEW07 SEW07_T3 Fagus_sylvatica SEW07_T3_1       2024 June  2024-06-17    Trunk          rot                  
#   2 SEW07 SEW07_T5 Fagus_sylvatica SEW07_T5_1       2024 June  

# write_csv(merged_data, "data/processed_data/Community_2023_2024_with_tree_data.csv")

# Tree diversity ------

Tree_composition <- tree_data %>% 
  group_by(EP, species, year) %>% 
  count() %>% 
  ungroup() %>% 
  summarise(Abundance=mean(n),
            .by=c("EP", "species")) %>% 
  pivot_wider(names_from=species, values_from=Abundance, values_fill=0)


Tree_diversity <- tree_data %>% 
 group_by(EP, species, year) %>% 
  count() %>% 
  ungroup() %>% 
  summarise(Abundance=mean(n),
            .by=c("EP", "species")) %>%
  summarise(
    # Total number of individuals across all species in each plot
    Tree_abundance = sum(Abundance, na.rm = TRUE),
    
    # Number of unique species (species richness) in each tree hole
    Tree_sp_richness = n_distinct(species),
    Tree_Shannon = vegan::diversity(Abundance, index = "shannon"),
    .by=c("EP"))  %>% 
  left_join(Tree_composition, by = c("EP"))

# Silvicultural Management Intensity (SIM) ------------------------------
# Dynamics on all forest EPs, 2008 - 2020
# SMId (ratio) - Density component of silvicultural management intensity - relative deviance between actual basal area and basal area carrying capacity
# SMIr (value) - Risk component of silvicultural management intensity - probability of stand loss before the age of 180 years
# SMI	(index) -	Silvicultural management intensity - mean of risk and density component
# 31217_9_data.csv

SIM_all <- read_csv("data/raw_data/BiodExpl/31217_9_data.csv") %>% 
  filter(Exploratory == "SCH")

# 2008_2020
SIM_2008_2020 <- SIM_all %>% 
 # mutate(year = substr(year, 7, 10)) %>%
  summarise(across(where(is.numeric), 
                   list(mean_2008_2020 = mean, sd_2008_2020=sd), na.rm = TRUE),
            .by=c("EP"))
    
# 2018_2020
SIM_2018_2020 <- SIM_all %>% 
  mutate(year = substr(year, 7, 10)) %>%
  filter(year %in% c("2018", "2019", "2020")) %>%
  summarise(across(where(is.numeric), 
                   list(mean_2018_2020 = mean, sd_2018_2020=sd), na.rm = TRUE),
            .by=c("EP"))

# Why sd in some cases are ==0? check if there are measurements for all year?
SIM_all %>% 
  mutate(year = substr(year, 7, 10)) %>%
  filter(year %in% c("2018", "2019", "2020")) %>%
  filter(EP=="SEW09")
# OR
SIM_all %>% 
  mutate(year = substr(year, 7, 10)) %>%
  filter(year %in% c("2018", "2019", "2020")) %>%
  filter(EP=="SEW01")
# The land use stays the same over 3 years that is why sd =0

# merge both SIM datasets
SIM_data <- SIM_2008_2020 %>% 
  left_join(SIM_2018_2020, by = "EP")

# check for missing data if merged
merged_tree_data %>% 
  left_join(SIM_data, by = c("Plot" = "EP")) %>% 
  filter(!Outside==TRUE) %>% 
  filter(is.na(SMIr_sd_2008_2020)) %>% 
  print(n=Inf)

names(SIM_data)

#  Forest Management Intensity Index (ForMI) ------------------------------
# Dynamics of all forest EPs between 2008 and 2018
# Inonat (fraction) -	Proportion of non-natural tree species
# Iharv	(fraction) - Proportion of harvested tree biomass	
# Idwcut(fraction) - Proportion of dead wood showing signs of saw cuts	
# Formi (indexv) - Index of Forest Management Intensity
# 24646_4_data.csv

ForMI_all <- read_csv("data/raw_data/BiodExpl/24646_4_data.csv") %>% 
  filter(Exploratory == "SCH") %>% 
  mutate(DWi_year = substr(DWi_year, 7, 10)) %>% 
  mutate(DWi_year = ifelse(is.na(DWi_year), "2012", DWi_year))  %>% 
  select(EP, DWi_year, Inonat, Iharv, Idwcut, Formi)

ForMI_all %>% 
  group_by(DWi_year) %>%
    count()

# 2012_2018
ForMI_2012_2018 <- ForMI_all %>% 
  summarise(across(where(is.numeric), 
                   list(mean_2012_2018 = mean, sd_2012_2018=sd), na.rm = TRUE),
            .by=c("EP"))

# 2018 only
ForMI_2018 <- ForMI_all %>%
  filter(DWi_year %in% c("2018")) %>%
  summarise(across(where(is.numeric), 
                   list("2018" = mean), na.rm = TRUE),
            .by=c("EP"))


# merge both ForMI datasets
ForMI_data <- ForMI_2018 %>% 
  left_join(ForMI_2012_2018, by = "EP")

names(ForMI_data)
# check for missing data if merged
merged_tree_data %>% 
  left_join(ForMI_data, by = c("Plot" = "EP")) %>% 
  filter(!Outside==TRUE) %>% 
  filter(is.na(Formi_2018) | is.na(Inonat_mean_2012_2018) ) %>% 
  print(n=Inf)


# Plot Biodiversity Potential -------------------------------------------------
# Index of Biodiversity Potential developed by Larrieu and Gonin (2008)
# 31873_7_data

biodiv_data <- read_csv("data/raw_data/BiodExpl/31873_7_data.csv") %>% 
  # filter ID that consists "SCH"
  filter(str_detect(ID, "SEW"))
biodiv_data

biodiv_data %>% 
  select(Wet_macrohabitats) %>% 
  print(n=Inf)

# check missing data if merged
merged_tree_data %>% 
  left_join(biodiv_data, by = c("Plot" = "EP")) %>% 
  filter(!Outside==TRUE) %>% 
  filter(is.na(Tree_richness)) %>% 
  print(n=Inf)

names(biodiv_data)
# Laser scan data -------------------------------------------------
# ENL: The effective number of layers quantifies vertical stand structure 
# SSCI: The index quantifies stand structural complexity 
# Canopy Openness: The index quantifies canopy openness as percentage of sky pixels of a simulated hemisperical image for an opening angle of 60°. 
# 32085_6_data.csv

Laser_data <- read_csv("data/raw_data/BiodExpl/32085_6_data.csv") %>% 
  filter(exploratory=="SCH" & season=="summer23") %>% 
  # correct plot_id numbers: when plots have 4 characters (SEW1), insert 0 before the last character
  mutate(plot.id = if_else(str_length(plot.id) == 4, 
                           # insert "0" before the 4th character
                           str_replace(plot.id, "^(.{3})(.)$", "\\10\\2"),
                           plot.id))


# check missing data if merged
merged_tree_data %>% 
  left_join(Laser_data, by = c("Plot" = "plot.id")) %>% 
  filter(!Outside==TRUE) %>% 
  filter(is.na(enl) | is.na(ssci) | is.na(canopy.openness)) %>% 
  print(n=Inf)


# Stand structural attributes -------------------------------------------------
# 2014 - 2018 # not grouped by year, as different plots are sampled in different years
# 22766_4_data.csv
Stand_str_data <- read_csv("data/raw_data/BiodExpl/22766_4_data.csv") %>% 
  filter(Exploratory=="SCH") 

# check missing data if merged
merged_tree_data %>% 
  left_join(Stand_str_data, by = c("Plot" = "EP")) %>% 
  filter(!Outside==TRUE) %>% 
  filter(is.na(ssm_N )) %>% 
  print(n=Inf)


# Climate Data -------------------------------------------------

climate2024 <- read_csv("data/raw_data/BiodExpl/climate_data_May_June_July_2024.csv") %>% 
  select(plotID, datetime,
    PAR_200, precipitation_radolan, precipitation_radolan_acc,
         rH_200, Ta_200, Ta_200_heat_index, Ta_200_humidex,
         # binary variables
         "Ta_200_extremely hot days", Ta_200_extremely_cold_days,
         Ta_200_heating_degree_days) %>% 
  rename(
    Ta_200_extremely_hot_days = "Ta_200_extremely hot days") %>%
  mutate(Year = 2024, .after = plotID) %>%
  mutate(
    Month = factor(month.name[as.integer(format(datetime, "%m"))],
                   levels = month.name, ordered = TRUE), 
    .after = Year) %>% 
  summarise(
    across(
      c(PAR_200, precipitation_radolan, precipitation_radolan_acc,
        rH_200, Ta_200, Ta_200_heat_index, Ta_200_humidex),
      ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"),
    across(
      c(Ta_200_extremely_hot_days, Ta_200_extremely_cold_days,
        Ta_200_heating_degree_days),
      ~ sum(.x, na.rm = TRUE), .names = "{.col}_sum"),
    across(
      c(precipitation_radolan, rH_200, Ta_200),
      ~ {
        m <- mean(.x, na.rm = TRUE)
        s <- sd(.x,   na.rm = TRUE)
        if (is.na(m) || m == 0) NA_real_ else s / m
      },
      .names = "{.col}_CV"
    ),
      .by = c("plotID", "Year", "Month")
  )
    
    
climate2024
names(climate2024)

climate2024 %>% 
  filter(is.na(Ta_200_extremely_hot_days_sum))

climate2023 <- read_csv("data/raw_data/BiodExpl/climate_data_November_2023.csv") %>% 
select(plotID, datetime,
       PAR_200, precipitation_radolan, precipitation_radolan_acc,
       rH_200, Ta_200, Ta_200_heat_index, Ta_200_humidex,
       # binary variables
       "Ta_200_extremely hot days", Ta_200_extremely_cold_days,
       Ta_200_heating_degree_days) %>% 
  rename(
    Ta_200_extremely_hot_days = "Ta_200_extremely hot days") %>%
  mutate(Year = 2023, .after = plotID) %>%
  mutate(
    Month = factor(month.name[as.integer(format(datetime, "%m"))],
                   levels = month.name, ordered = TRUE), 
    .after = Year) %>% 
  summarise(
    across(
      c(PAR_200, precipitation_radolan, precipitation_radolan_acc,
        rH_200, Ta_200, Ta_200_heat_index, Ta_200_humidex),
      ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"),
    across(
      c(Ta_200_extremely_hot_days, Ta_200_extremely_cold_days,
        Ta_200_heating_degree_days),
      ~ sum(.x, na.rm = TRUE), .names = "{.col}_sum"),
    across(
      c(precipitation_radolan, rH_200, Ta_200),
      ~ {
        m <- mean(.x, na.rm = TRUE)
        s <- sd(.x,   na.rm = TRUE)
        if (is.na(m) || m == 0) NA_real_ else s / m
      },
      .names = "{.col}_CV"
    ),
    .by = c("plotID", "Year", "Month")
  )

climate2023

climate2023 %>% 
  filter(is.na(Ta_200_extremely_hot_days_sum))


# Merge climate data for 2023 and 2024
climate_data <- bind_rows(climate2023, climate2024)
climate_data


# check missing data if merged
merged_tree_data %>% 
  left_join(climate_data, by = c("Plot" = "plotID", "Year", "Month")) %>% 
  filter(!Outside==TRUE) %>% 
  filter(is.na(Ta_200_extremely_hot_days_sum )) %>% 
  print(n=Inf)



# MERGE ALL ENVIRONMENTAL DATA ---------------------------------------------------------------

merged_all_envir_data <- merged_tree_data %>% 
  left_join(Tree_diversity, by = c("Plot" = "EP")) %>% 
  left_join(SIM_data, by = c("Plot" = "EP")) %>% 
  left_join(ForMI_data, by = c("Plot" = "EP")) %>% 
  left_join(biodiv_data, by = c("Plot" = "EP")) %>% 
  left_join(Laser_data, by = c("Plot" = "plot.id")) %>% 
  left_join(Stand_str_data, by = c("Plot" = "EP")) %>% 
  left_join(climate_data, by = c("Plot" = "plotID", "Year", "Month")) 



merged_all_envir_data %>% 
  pull(Month) %>%
  unique()

merged_all_envir_data %>% 
  filter(is.na(Tree_abundance))

write_csv(merged_all_envir_data, "data/processed_data/Environment_ALL.csv")


# LANDSCAPE DATA -------------------------------------------------
# class level of land type is the resolution of land cover classification
# class_0: coarse level (e.g., forest, agriculture, urban)
# class_1: intermediate level (e.g., deciduous forest, cropland, residential area)
# class_2: fine level (e.g., beech forest, wheat field, high-density residential area)
# area_km2: area covered by each land cover class within the buffer
# buffer sizes: 250m and 500m around each plot

files_250m <- list.files("data/Raster_measurements/raster_measurement_250m", 
                    pattern = "^SEW.*\\.csv$", full.names = TRUE) %>%
  set_names(basename(.)) %>%  # name the list elements by filename
  map_dfr(~ readr::read_csv(.x, show_col_types = FALSE), .id = "file") %>% 
  # add buffer size
  mutate(buffer_size_m = 250) %>%
  # add a plotID extracted from filename 
  mutate(plot = tools::file_path_sans_ext(file)) %>% 
  select(-file, -plot, -fid, -id) %>% 
  mutate(class_2=ifelse(is.na(class_2), class_1, class_2)) %>% 
  # sum area for each land cover classes within each plotID 
  summarise(
    class2_area = sum(area_km2),
    .by = c("plotID", "buffer_size_m", "class_0", "class_1", "class_2")) %>% 
  mutate(class1_area = sum(class2_area), 
         .by = c("plotID", "class_1"), .before=class2_area) %>%
  mutate(class0_area = sum(class2_area), 
         .by = c("plotID", "class_0"), .before=class1_area) %>% 
  # calculate total plot area
  mutate(plot_area_km2 = sum(class2_area), .by = c("plotID"), .after=buffer_size_m) %>% 
  rename(class0_LandType=class_0,
         class1_LandType=class_1,
         class2_LandType=class_2) %>%
  pivot_longer(-c(plotID, buffer_size_m, plot_area_km2),
               names_to = c("LandType_level", ".value"),
               names_sep="_") %>% 
  rename(LandType_area_km2=area,
         LandType_code=LandType)%>% 
  # at resolution class_0 there are repetitions of land types within same plotID
  summarise(LandType_area_km2 = mean(LandType_area_km2),
            .by = c("plotID", "buffer_size_m", "plot_area_km2", "LandType_level", "LandType_code")
  )

files_250m



files_500m <- list.files("data/Raster_measurements/raster_measurement_500m", 
                         pattern = "^SEW.*\\.csv$", full.names = TRUE)%>%
  set_names(basename(.)) %>%  # name the list elements by filename
  map_dfr(~ readr::read_csv(.x, show_col_types = FALSE), .id = "file") %>% 
  # add buffer size
  mutate(buffer_size_m = 500)%>%
  # add a plotID extracted from filename 
  mutate(plot = tools::file_path_sans_ext(file)) %>% 
  select(-file, -plot, -fid, -id) %>% 
  mutate(class_2=ifelse(is.na(class_2), class_1, class_2)) %>% 
  # sum area for each land cover classes within each plotID 
  summarise(
    class2_area = sum(area_km2),
    .by = c("plotID", "buffer_size_m", "class_0", "class_1", "class_2")) %>% 
  mutate(class1_area = sum(class2_area), 
         .by = c("plotID", "class_1"), .before=class2_area) %>%
  mutate(class0_area = sum(class2_area), 
         .by = c("plotID", "class_0"), .before=class1_area) %>% 
  # calculate total plot area
  mutate(plot_area_km2 = sum(class2_area), .by = c("plotID"), .after=buffer_size_m) %>% 
  rename(class0_LandType=class_0,
         class1_LandType=class_1,
         class2_LandType=class_2) %>%
  pivot_longer(-c(plotID, buffer_size_m, plot_area_km2),
               names_to = c("LandType_level", ".value"),
               names_sep="_") %>% 
  rename(LandType_area_km2=area,
         LandType_code=LandType) %>% 
  # at resolution class_0 there are repetitions of land types within same plotID
  summarise(LandType_area_km2 = mean(LandType_area_km2),
            .by = c("plotID", "buffer_size_m", "plot_area_km2", "LandType_level", "LandType_code")
            )

files_500m

# Import Land type ID mapping
Land_type_ID <- read_csv("data/Raster_measurements/Land_type_ID.csv")

# Combine 250m and 500m landscape data
landscape_data <- files_250m %>%
  bind_rows(files_500m) %>%
  mutate(LandType_percent=LandType_area_km2/plot_area_km2*100) %>% 
   mutate(LandType_level = case_when(
    LandType_level == "class0" ~ "class_0",
    LandType_level == "class1" ~ "class_1",
    LandType_level == "class2" ~ "class_2")) %>% 
  left_join(Land_type_ID, by = c("LandType_code")) %>% 
  relocate(LandType_name, .after=LandType_code)


# check if percent adds up to 100 for each resolution of land-use type
landscape_data %>% 
  group_by(plotID, buffer_size_m, LandType_level) %>% 
  summarise(total_percent = sum(LandType_percent)) %>% 
  print(n=Inf)


write_csv(landscape_data, "data/Raster_measurements/Landscape_Type_composition_ALL_OB.csv")

# check unique land types
landscape_data %>% 
  filter(LandType_level=="class_0") %>% 
  pull(LandType_name) %>% 
  unique()


landscape_data %>% 
  filter(LandType_level=="class_1") %>%
  pull(LandType_name) %>% 
  unique()


# Main Land types - proportions per plot
land_types_proportions <-  landscape_data %>% 
  filter(LandType_level=="class_0") %>%
  # in LandType_name keep only first word
  mutate(LandType_name = word(LandType_name, 1)) %>% 
  select(plotID, buffer_size_m, LandType_name, LandType_percent) %>%
  pivot_wider(names_from = LandType_name, 
              values_from = LandType_percent,
              values_fill = 0
              ) %>%
  mutate(Forest_percent = Forest,
         Agricultural_percent=Agricultural+Bare,
         Water_bodies_percent=Mire+Water, 
         Urban_percent=Transportation + Settlements, 
         LandType_level="class_0",  
         .keep ="unused") 


# Calculate landscape heterogeneity metrics
landscape_heterogeneity <- landscape_data %>%
summarise(
  LandType_richness = n_distinct(LandType_name),
  LandType_Shannon = vegan::diversity(LandType_percent, index = "shannon"),
  LandType_even = vegan::diversity(LandType_percent, index = "invsimpson"),
  .by = c(plotID, buffer_size_m, LandType_level)) %>% 
  left_join(land_types_proportions, 
            by = c("plotID", "buffer_size_m", "LandType_level"))
  


# check missing data if merged
merged_tree_data %>% 
  left_join(landscape_heterogeneity %>% 
              filter(LandType_level=="class_0" & buffer_size_m==500),
            by = c("Plot" = "plotID")) %>% 
  filter(!Outside==TRUE) %>% 
  filter(is.na(LandType_richness)) %>% 
  print(n=Inf)



write_csv(landscape_heterogeneity, "data/processed_data/Landscape_heterogeneity.csv")



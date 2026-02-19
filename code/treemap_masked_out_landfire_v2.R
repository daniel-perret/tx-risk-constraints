# script to estimate "treatable" forest for CONUS
# Author: Mark Kreider
# February 2025

library(raster)
library(terra)
library(tidyverse)
library(sf)
library(ggplot2)
library(foreign)
library(rasterVis)


# load treemap 2016 data
treemap_2016 <- raster("treemap_2016/Data/TreeMap2016.tif")
treemap_attribute_raw <- treemap_2016 %>% attributes()
treemap_2016_attributes <- treemap_attribute_raw$data@attributes[[1]] %>%
  mutate(treemap_ID = ID)


# landfire
landfire <- raster("LF2016_FRG_200_CONUS/Tif/LC16_FRG_200.tif")
landfire_attributes_raw <- landfire %>% attributes()
landfire_attributes <- landfire_attributes_raw$data@attributes[[1]] %>%
  dplyr::rename("landfire_ID" = ID,
         "fire_regime_group" = "FRG_NEW",
         "landfire_name" = "BPS_NAME") %>%
  mutate(frequent_fire = ifelse(fire_regime_group %in% c("I-A", "I-B", "I-C", "III-A"), T, F))


# treemap-landfire combine
treemap_landfire_att_table <- read.dbf("treemap_2016/treemap_landfire_combine_attribute_table.dbf") %>% 
  dplyr::rename("treemap_ID" = TreeMap201, 
         "landfire_ID" = LC16_FRG_2)


saveRDS(treemap_landfire_att_table, "R_objects/treemap_landfire_att_table")
treemap_landfire_att_table <- readRDS("R_objects/treemap_landfire_att_table")

# SDI data for treemap (from file: "treemap_SDI.R")
treemap_SDI <- readRDS("R_objects/treemap_2016_SDI") %>%
  dplyr::rename("treemap_ID" = tm_id)

# merge altogether into combined attribtues
combined_attributes <- treemap_landfire_att_table %>% 
  merge(., landfire_attributes, by = "landfire_ID", all.x = T) %>%
  merge(., treemap_2016_attributes, by = "treemap_ID", all.x = T) %>%
  merge(., treemap_SDI, by = c("treemap_ID", "CN"), all.x = T) 


# find what proportion of each forest type is in frequent fire
frequent_fire_summary <- combined_attributes %>%
  group_by(ForTypName, frequent_fire) %>%
  reframe(n = sum(Count)) %>% # how many pixels fall into each category forest X fire regime group
  group_by(ForTypName) %>%
  reframe(proportion = n / sum(n),
          frequent_fire = frequent_fire) %>%
  filter(frequent_fire == T) %>%
  dplyr::rename("proportion_frequent_fire" = proportion) %>%
  dplyr::select(-frequent_fire) %>%
  arrange(proportion_frequent_fire) %>%
  as.data.frame()

# save summary object
saveRDS(frequent_fire_summary, "R_objects/frequent_fire_summary")
frequent_fire_summary <- readRDS("R_objects/frequent_fire_summary")

# visualize distribution across forest types
frequent_fire_summary %>%
  mutate(ForTypName = fct_reorder(ForTypName, proportion_frequent_fire)) %>%
  ggplot(aes(y = ForTypName, x = proportion_frequent_fire)) + 
  geom_col() + 
  theme(text = element_text(size = 10),
        axis.text = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  coord_flip()


# look at proportion of frequent fire for different forest types
frequent_fire_summary %>% filter(ForTypName == "Ponderosa pine")
frequent_fire_summary %>% filter(ForTypName %in% c("Alaska-yellow-cedar",
                                     "Blue spruce",
                                     "Engelmann spruce", 
                                     "Engelmann spruce / subalpine fir", 
                                     "Foxtail pine / bristlecone pine",
                                     "Miscellaneous western softwoods",
                                     "Mountain hemlock",
                                     "Red fir",
                                     "Noble fir",
                                     "Port-Orford-cedar",
                                     "Sitka spruce", 
                                     "Subalpine fir", 
                                     "Western hemlock", 
                                     "Western redcedar", 
                                     "Whitebark pine"))

# merge proportion of frequent fire with combined attributes
combined_attributes_fire_prop <- combined_attributes %>% 
  merge(., frequent_fire_summary, by = "ForTypName")


saveRDS(combined_attributes_fire_prop, "R_objects/combined_attributes_fire_prop")

# apply rules for treatment feasibility -----
combined_attributes_fire_prop <- readRDS("R_objects/combined_attributes_fire_prop")

# distribution of SDI
combined_attributes_fire_prop %>%
  ggplot() + 
  geom_density(aes(x = SDI), fill = "grey", alpha = 0.5) + 
  theme_classic() + 
  labs(x = "Stand Density Index (SDI)")


# look at distribution of relative density (RD)
combined_attributes_fire_prop %>%
  ggplot() + 
  geom_density(aes(x = RD), fill = "grey", alpha = 0.5) + 
  theme_classic() + 
  geom_vline(xintercept = 0.25, linetype = "dashed") + 
  geom_vline(xintercept = 0.35, linetype = "dashed") + 
  labs(x = "Relative density (RD)")


imperial_metric_compare <- combined_attributes_fire_prop %>%
  group_by(ForTypName) %>%
  reframe(mean_SDImax = mean(SDImax, na.rm = T))  %>%
  mutate(mean_SDImax_imperial = mean_SDImax * 0.3937) %>%
  as.data.frame()

imperial_metric_compare %>% filter(ForTypName %in% c("Ponderosa pine", "Western larch"))
imperial_metric_compare$mean_SDImax_imperial %>% max()

#### frequent fire forests
# if forest type has > 0.75 proportion frequent fire, the entire forest type is included
# if forest type has < 0.25 proportion frequent fire, the entire forest type is excluded
# if forest type has 0.25-0.75 proportion frequent fire, only frequent fire pixels are included

# if relative density (RD) > density threshold, then thin & burn (too dense to burn straight away)
# if relative density (RD) < density threshold, then Rx burn only
# density threshold can be set at 0.25 (the threshold that Peeler et al. uses https://iopscience.iop.org/article/10.1088/1748-9326/acf05a)
# ...or higher, since very little land is less than 25% relative density based on the dataset that we have. 



# get forest types in between 0.25-0.75
fully_included_forest_types <- frequent_fire_summary$ForTypName[which(frequent_fire_summary$proportion_frequent_fire >= 0.75)]
fully_excluded_forest_types <- frequent_fire_summary$ForTypName[which(frequent_fire_summary$proportion_frequent_fire <= 0.25)]
intermediate_forest_types <- 
  frequent_fire_summary$ForTypName[which(frequent_fire_summary$proportion_frequent_fire > 0.25 & 
                                           frequent_fire_summary$proportion_frequent_fire < 0.75)]


# split out between thin/burn and burn only
RD_threshold <- 0.35 # threshold between burn only & thin and burn
combined_attributes_fire_prop_treatment <- combined_attributes_fire_prop %>%
  mutate(feasible = ifelse(ForTypName %in% fully_included_forest_types, T, 
                           ifelse(ForTypName %in% fully_excluded_forest_types, F,
                                  ifelse(ForTypName %in% intermediate_forest_types, 
                                         ifelse(frequent_fire == T, T, F), 
                                         NA)))) %>%
  mutate(treatment_type = ifelse(feasible == T, 
                                 ifelse(RD <= RD_threshold, "burn only", 
                                        ifelse(RD > RD_threshold, "thin and burn", NA)), NA))


combined_attributes_fire_prop_treatment %>%
  filter(feasible == T) %>%
  group_by(treatment_type) %>%
  reframe(n = n()) %>%
  mutate(prop = n / sum(n))


# get pixel values for burn only and thin & burn
burn_only_values <- combined_attributes_fire_prop_treatment$Value[which(combined_attributes_fire_prop_treatment$treatment_type == "burn only")]
thin_burn_values <- combined_attributes_fire_prop_treatment$Value[which(combined_attributes_fire_prop_treatment$treatment_type == "thin and burn")]

saveRDS(burn_only_values_35, "R_objects/burn_only_values_35")
saveRDS(thin_burn_values_35, "R_objects/thin_burn_values_35")


burn_only_values %>% length()
thin_burn_values %>% length()




# load combined treemap - landfire

burn_only_values <- readRDS("R_objects/burn_only_values_35")
thin_burn_values <- readRDS("R_objects/thin_burn_values_35")

combined_treemap_landfire <- rast("combine_treemap_landfire.tif")


burn_only_function <- function(x) {
  x[!x %in% burn_only_values] <- NA
  x[!is.na(x)] <- 1
  return(x)
}

thin_burn_function <- function(x) {
  x[!x %in% thin_burn_values] <- NA
  x[!is.na(x)] <- 1
  return(x)
}

# Apply the mask function to the raster
burn_only_raster_35 <- app(combined_treemap_landfire, burn_only_function)
thin_burn_raster_35 <- app(combined_treemap_landfire, thin_burn_function)




### NEED to convert values to 1 / 0 (and then can export at much smaller size)

writeRaster(burn_only_raster, "Output_objects/burn_only_raster.tif")
writeRaster(thin_burn_raster, "Output_objects/thin_burn_raster.tif")


writeRaster(burn_only_raster_35, "Output_objects/burn_only_raster_35.tif")
writeRaster(thin_burn_raster_35, "Output_objects/thin_burn_raster_35.tif")








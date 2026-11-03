#########################################################################################
# A script for gpas analysis
# 
# 2025/03 Raphaël Biawa Diegamounoua
#
# source("src/gps_analysis.R")
#########################################################################################






########################################
#Retrieve functions

source("src/function_retrieving.R")

########################################
# GLOBAL PROCESSING PARAMETERS

source("src/global_parameters.R")

########################################
# Retrieve data

source("src/raw_data_loading.R")




##########################################################################################







listing_feat_SL_mod_idf_hdf <- listing_feat_SL_mod %>% filter(INSEE_REG %in% REGIONS_CODE_VAL)





st_write(listing_feat_SL_mod_idf_hdf,
         paste0("out/shp/",
                ANALYSIS_SCALE,
                "_",
                LOCALITY_NAME,
                "_",
                "listing_feat_SL_mod.shp"),
         delete_layer = TRUE)


## Export into shapefiles

st_write(geom_sf_data,
         paste0("out/shp/",
                ANALYSIS_SCALE,
                "_",
                LOCALITY_NAME,
                "_",
                "geom_sf_data.shp"),
         delete_layer = TRUE)

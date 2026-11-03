#########################################################################################
# A script to retrieve functions
# 
# 2024/12 Raphaël Biawa Diegamounoua
#
# source("src/function_retrieving.R")
#########################################################################################


######################################
# Load packages

source("src/packages_loading.R")


######################################
# Load general processing functions
source("src/processing/general_processing_functions.R")


######################################
# Load graph extraction functions

source("src/processing/graph/graph_extraction.R")


######################################
#Load functions for processing cartographic and spatial data."

source("src/processing/cartography/carto.R")


######################################
# Retrieve functions for data cleansing

source("src/processing/data_cleaning/data_cleansing.R")


######################################
# Retrieve graph statistics functions

source("src/statistics/graph_statistics/graph_statistics.R")


######################################
# Retrieve graph clustering functions

source("src/clustering/graph/graph_clustering.R")


######################################
# Retrieve general statistics functions

source("src/statistics/general_statistics.R")


######################################
# Retrieve visualization parameters
source("src/visualization/visualization_parameters.R")


######################################
# Retrieve sequence extraction functions

source("src/processing/sequence_analysis/sequence_extraction.R")


######################################
# Retrieve sequence extraction functions

source("src/clustering/cluster_description.R")

######################################
# Retrieve functions for spatial analyses

source("src/processing/spatial_processing/spatial_processing.R")

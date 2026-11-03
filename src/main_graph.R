#########################################################################################
# A script to run graph extraction, analysis and clustering
# 
# 2025/02 Raphaël Biawa Diegamounoua
#
# source("src/main_graph.R")
#########################################################################################



######################################
# Loading packages

if (! exists("log_file")) source("src/packages_loading.R")




########################################
#Retrieve functions

source("src/function_retrieving.R")

########################################
# GLOBAL PROCESSING PARAMETERS

source("src/global_parameters.R")






global_graph_start_time    <- Sys.time()

####################################################
# Graph extraction

print("Graph extraction")

#source("src/execution/graph_extraction_execution.R")


####################################################
# Graph topological analysis

print("Graph topological analysis")

#source("src/execution/graph_statistics_execution.R")


####################################################
# Graph clustering

print("Graph clustering")

#source("src/execution/graph_clustering_execution.R")



####################################################
# ERGM modeling
#source("src/modeling/substitutability/ergm.R")

source("src/modeling/substitutability/ergm_with_sampling.R")

print("Process ended !")

global_graph_end_time    <- Sys.time()

global_graph_start_time - global_graph_end_time




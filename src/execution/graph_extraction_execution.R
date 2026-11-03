#########################################################################################
# A script to perform a graph extraction
# 
# 2024/11 Raphaël Biawa Diegamounoua
#
# source("src/execution/graph_extraction_execution.R")
#########################################################################################




# Set working directory

#setwd(paste(getwd(), "These_Raphael", sep = "/"))

######################################
# Loading packages

source("src/packages_loading.R")

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
# PROCESSING PARAMETERS







if (DATA_SOURCE == "MA") {
  source("src/processing/graph/graph_MA.R")

} else if (DATA_SOURCE == "Se_loger") {
  source("src/processing/graph/graph_SL.R")
}







#Sauvegarde des graphes

write_graph(graph, 
            paste(
                  paste("in/graphs/graph",
                        ANALYSIS_SCALE, LOCALITY_NAME,
                        sep = "_"),
                  ".txt", sep = ""),
            "graphml")



######################################
##            SAVING DATA           ##
######################################
#rm(consultation_raw_data, estimation_raw_data)


#save.image(file = paste("out/Rdata", paste(ANALYSIS_SCALE, LOCALITY_NAME, "graph_extraction.RData", sep = "_"), sep = "/"))


# Remove_unuseful_objects


 lapply(c("geom_sf_cities",
          "geom_sf_districts",
          "geom_sf_departements",
          "consultation_raw_data",
          "consultation_data"),
        rm_if_exists)

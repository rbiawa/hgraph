#########################################################################################
# A script to perform a graph description
# 
# 2024/11 Raphaël Biawa Diegamounoua
#
# source("src/execution/graph_statistics_execution.R")
#########################################################################################






######################################
# Loading packages

source("src/packages_loading.R")


#Retrieve functions

source("src/function_retrieving.R")


## GLOBAL PROCESSING PARAMETERS
source("src/global_parameters.R")

#load(paste("out/Rdata", paste(ANALYSIS_SCALE, LOCALITY_NAME, "graph_extraction.RData", sep = "_"), sep = "/"))



##########################################################################################
# ANALYSING PARAMETERS



ZONES_NAME_VAR             <- switch(ANALYSIS_SCALE,
  "subregion"  =  "dep_Name",
  "city"       =  "city_Name"
)
ZONES_ID_VAR                <- switch(ANALYSIS_SCALE,
  "subregion"  =  "dep_ID",
  "city"       =  "city_ID"
)



HOMOPHILY_ATTRIBUTES        <- c("price", "area", "room_count", "item_type")
HOMOPHILY_ATTRIBUTES_LABELS <- c("price", "living area", "number of rooms", "type of item")

HOMOPHILY_THRESHOLD_CAL     <-  10

GRAPH_NAME                  <- "graph"

if (exists("graph_robust")) GRAPH_NAME   <- "graph_robust"


# PLOTTING PARAMETERS
ASSORTATIVITY_CLASS_NUMBER  <- 5
MAP_LINE_WIDTH              <- .1










# TO BE THOUGHT WETHER TO BE DONE DURING THE PRINCIPAL EXTRACTION

GRAPH_SAVING_DIRECTORY    <- "in/graphs"

GRAPH   <- get(GRAPH_NAME)

subgraph_list             <- extract_multiple_subgraphs(GRAPH,
                                                        ZONES_NAME_VAR,
                                                        ZONES_NAME_VAR,
                                                        save = FALSE,
                                                        path = GRAPH_SAVING_DIRECTORY)





#########################################################################################################
# BEGIN ANALYSING
#########################################################################################################

# A PLOT OF SUBGRAPHS

pdf(file = paste(GRAPH_PDF_SAVING_DIRECTORY, paste(LOCALITY_NAME_OUT, ANALYSE_SCALE_OUT,"subgraphs_plot.pdf", sep = "_"), sep = "/"), onefile = TRUE)

#Principal graph
plot(GRAPH, vertex.label=NA,vertex.size=5,vertex.color="darkblue", edge.arrow.size=.3, main = GRAPH$locality)


# Subgraphs
for(i in 1:length(subgraph_list)){

  plot(subgraph_list[[i]], vertex.label=NA,vertex.size=5,vertex.color="darkblue", edge.arrow.size=.3, main = paste("Graph of", names(subgraph_list)[i]))

}
dev.off()



######################################
# DESCRIPTIVE STATISTICS OF GRAPHS
######################################



######################################
## GRAPH TOPOLOGY

## Degree distribution

### Methode 1

pdf(file = paste(GRAPH_PDF_SAVING_DIRECTORY, paste(LOCALITY_NAME_OUT, ANALYSE_SCALE_OUT, "degree_distribution_M1.pdf", sep = "_"), sep = "/"), onefile = TRUE)

plot_degree_ccd(subgraph_list)

dev.off()

### Methode 2

pdf(file = paste(GRAPH_PDF_SAVING_DIRECTORY, paste(LOCALITY_NAME_OUT, ANALYSE_SCALE_OUT, "degree_distribution_M2.pdf", sep = "_"), sep = "/"), onefile = TRUE)

plot_degree_ccd_igraph(subgraph_list)

dev.off()


## Degree correlation

pdf(file = paste(GRAPH_PDF_SAVING_DIRECTORY, paste(LOCALITY_NAME_OUT, ANALYSE_SCALE_OUT, "degree_correlation.pdf", sep = "_"), sep = "/"), onefile = TRUE)

plot_degree_correlation(subgraph_list)

dev.off()




####################################################
# ANALYSIS OF THE HOMOPHILY OF CO-CONSULTED LISTINGS

graph_stats <- compute_multiple_graph_stats(subgraph_list, 
                                            HOMOPHILY_ATTRIBUTES,
                                            directed = FALSE,
                                            ZONES_ID_VAR,
                                            threshold = HOMOPHILY_THRESHOLD_CAL,
                                            save = TRUE,
                                            directory = XLSX_SAVING_DIRECTORY)

graph_stats <- graph_stats %>%
  rename(!!sym(ZONES_ID_VAR) := ID)

## Sats maps

graph_stats_geom_sf <- graph_stats %>% 
  right_join(geom_sf_data, by = ZONES_ID_VAR) %>% 
  st_as_sf()


# Map's plot

labels    <- HOMOPHILY_ATTRIBUTES_LABELS
vars      <- HOMOPHILY_ATTRIBUTES

maps      <- list()

for ( i in seq_along(vars)) {

var_name  <- paste(vars[i], "assortativity", sep = "_")

maps[[i]] <- plot_assortativity_map(graph_stats_geom_sf, var_name, k = ASSORTATIVITY_CLASS_NUMBER, name_var = labels[i], digits = 7, line_width = MAP_LINE_WIDTH)

}

pdf(file = paste(GRAPH_PDF_SAVING_DIRECTORY, paste(LOCALITY_NAME_OUT, ANALYSE_SCALE_OUT, "assortativity_coefficient_maps.pdf", sep = "_"), sep = "/"),
onefile = TRUE)

plot_assortativity_map(graph_stats_geom_sf, "deg_assortativity", k = ASSORTATIVITY_CLASS_NUMBER, name_var = "degree", digits = 7, line_width = MAP_LINE_WIDTH)

print(maps)

dev.off()




save(maps, 
  file = paste0("out/Rdata/",LOCALITY_NAME_OUT, "_", DATA_SOURCE, "_assortativity_maps_", ".RData")
  )


#####################################################
## CORRELATION BETWEEN SPATIAL AND GEODESIC DISTANCES


if (PLOT_DISTANCE){
## Distances plot
pdf(file = paste(GRAPH_PDF_SAVING_DIRECTORY, paste(LOCALITY_NAME_OUT, ANALYSE_SCALE_OUT, "distances_plot.pdf", sep = "_"), sep = "/"), onefile = TRUE)

plot_muliple_graphs_distances(subgraph_list)
dev.off()

}

## Distances Mantel's test



if (PERFORM_MANTEL){

multiple_mantel_test <- apply_multiple_mantel_test(subgraph_list)

capture.output(multiple_mantel_test, file = paste(GRAPH_TXT_SAVING_DIRECTORY, paste(LOCALITY_NAME_OUT, ANALYSE_SCALE_OUT, DATA_SOURCE, "distances_mantel_test.txt", sep = "_"), sep = "/"))


}
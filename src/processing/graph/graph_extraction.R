#########################################################################################
# Functions used during graph extraction.
# 
# 2024/11 Raphaël Biawa Diegamounoua
#
# source("src/processing/graph/graph_extraction.R")
#########################################################################################









###############################################################################################
#' Extract Network Graphs from Bipartite User-Listing Interactions
#'
#' Args :
#' 
#' data : Input data.frame or data.table containing user interactions
#' searcher_id : Column name for user identifiers
#' listing_id : Column name for listing identifiers
#' event_order : Column name for event ordering (optional for directed graphs)
#' directed : Whether to create directed graph (default: FALSE)
#' consec : For directed graphs, whether to keep only consecutive links (default: FALSE)
#' graph type ("listing", "searcher", "bipartite") : wether to return listing graph, searcher,
#'            or searcher-listing bipartite graph
#' 
#' 
#' return : igraph graph object
###############################################################################################



extract_graph <- function(data, searcher_id, listing_id, event_order = NULL,
                          directed = FALSE, consec = FALSE,
                          graph_type = c("listing", "searcher", "bipartite")) {

  graph_extract_start_time    <- Sys.time()

  # Argument validation
  graph_type <- match.arg(graph_type)
  if(directed && graph_type == "listing" && is.null(event_order)) stop("event_order requis pour directed =  TRUE")

  # Data cleaning
  k <- length(c(searcher_id, listing_id, event_order))

  DT <- data.table::as.data.table(data)
  data.table::setnames(DT, c(searcher_id, listing_id, event_order), 
                       c("searcher_id", "listing_id", "event_order")[1:k], 
                       skip_absent = TRUE)

  if(graph_type %in% c("searcher", "bipartite")| (graph_type == "listing" & ! directed) ){

    # Bipartite graph creation
    vertices <- unique(rbind(
      DT[, .(name = searcher_id, type = FALSE)],
      DT[, .(name = listing_id, type = TRUE)]
    ))
    g_bip <- igraph::graph_from_data_frame(
      d = DT[, .(searcher_id, listing_id)],
      vertices = vertices,
      directed = directed
    )

    # Graph selection

    proj <- igraph::bipartite_projection(g_bip, which = "both")
    return(switch(graph_type,
                  ,"searcher" = proj[[1]]  # Searcher projection
                  ,"listing" = proj[[2]]   # Listing projection 
                  ,"bipartite" = g_bip      # Bipartite graph
      )
    )

  } else{

    # Directed projection (for listings only)

    DT <- DT[order(searcher_id, event_order)]

    edge_gen <- if(consec) {
      function(sd) if(nrow(sd) > 1) data.table::data.table(
        from = sd$listing_id[-nrow(sd)], 
        to = sd$listing_id[-1]
      )
    } else {
      function(sd) if(nrow(sd) > 1) data.table::CJ(
        i = 1:(nrow(sd)-1), 
        j = 2:nrow(sd)
      )[i < j, .(from = sd$listing_id[i], to = sd$listing_id[j])]
    }

    edges <- DT[, edge_gen(.SD), by = searcher_id][, .N, by = .(from, to)]

    igraph::graph_from_data_frame(edges, 
                                  directed = TRUE, 
                                  vertices = unique(DT$listing_id)) %>% 
      igraph::set_edge_attr("weight", value = edges$N)

  }


}




#################################################################################################
# Extract attributes from a data frame and adds them to the nodes of a graph.
#
# graph: the input igraph object.
# data: the input data frame containing attributes to be added.
# var_names: a list of column names from the data frame to be added as node attributes.
# 
# returns: a data frame with node attributes, merging the graph's vertex attributes 
# with the corresponding attributes from the data frame based on the first column in var_names.
#
# Requirements:
#  - data.table
#################################################################################################

extract_attributes_dataframe <- function(graph, data, var_names) {
  if (length(var_names) < 1) {
    stop("The 'var_names' list must contain at least one variable.")
  }
  
  # Extract vertex attributes from the graph
  node_attributes <- setDT(vertex.attributes(graph))[, 
                          listing_id := as.character(name)]
    
  if ("geometry" %in% names(data)) {
    var_names <- c(var_names, "geometry")
  }


  # Select only the required columns from the data frame
  unique_listing <- setDT(data)[, ..var_names]
  

  
  
  # Use the first variable in var_names as the key for the join
  key_column <- var_names[1] 
  
  # Ensure the key column is unique in the data frame

  unique_listing <- unique_listing[, .SD[1], by = get(key_column)][, (key_column) := as.character(get(key_column))]


  
  # Perform the join using the first variable in var_names as the key

  ## Rename the dynamic column so it matches 'listing_id'
  setnames(unique_listing, key_column, "listing_id")

  ## Perform the join
  node_attributes <- merge(node_attributes, unique_listing, by = "listing_id", all.x = TRUE)

  
  return(node_attributes)
}




########################################################################################################
# Add attributes to the vertices of an igraph object based on user-specified mappings.
#
# graph: an igraph object to which attributes will be added.
# attrib_data: a dataframe containing the source attribute values.
# source_vars: a list of column names from the data source (dataframe) containing the attribute values.
# target_vars: a list of vertex attribute names to be added to the graph.
# 
# returns: an igraph object with the specified vertex attributes added.
########################################################################################################

add_vertex_attributes <- function(graph, attrib_data, source_vars, target_vars) {
  # Check if source_vars and target_vars have the same length
  if (length(source_vars) != length(target_vars)) {
    stop("The 'source_vars' and 'target_vars' lists must have the same length.")
  }
  
  # Add attributes to the graph
  for (i in seq_along(source_vars)) {

    vertex.attributes(graph)[[target_vars[i]]] <- attrib_data[[source_vars[i]]]
  }
  
  return(graph)
}






#################################################################################################
# Extract a subgraph from a graph given a condition and saves it if needed.
#
# graph: the input graph.
# filtering_var: the filtering variable's name (character).
# value: the filtering value.
# save: logical, whether to save the subgraphs to files (default is FALSE).
# path: directory where subgraphs will be saved if `save` is TRUE (default is getwd()).
# 
# returns: a subgraph of the input graph which nodes verify the condition "filtering_var=value".
#################################################################################################


extract_subgraph <- function(graph, filtering_var, value, save = FALSE, path = setwd()){
  graph_sub <- (induced_subgraph(graph, vids = which(vertex.attributes(graph)[[filtering_var]] == value)))  
  
  if (save == TRUE) {
  write_graph(graph_sub, 
              paste(path, paste(paste("graph_",value, sep = ""), ".txt", sep = ""), sep = "/"),
              "graphml")
  }
  return(graph_sub)
}





################################################################################################################
# Extract multiple subgraphs from a given graph and optionally saves them.
#
# graph: the input igraph object.
# filtering_var: the name of the vertex attribute used for filtering (character).
# subgraph_name_var: the vertex attribute used for naming subgraphs (character; default is filtering_var).
# save: logical, whether to save the subgraphs to files (default is FALSE).
# path: directory where subgraphs will be saved if `save` is TRUE (default is getwd()).
# 
# returns: a list of subgraphs.
################################################################################################################

extract_multiple_subgraphs <- function(graph, filtering_var, subgraph_name_var = filtering_var, save = FALSE, path = setwd()) {

  start_time              <- Sys.time()

  # Get unique and sorted values of the filtering variable
  filtering_val_vect <- unique(vertex.attributes(graph)[[filtering_var]]) %>% sort()
  
  # Function to create a subgraph and optionally save it
  create_subgraph <- function(value) {
    subgraph <- induced_subgraph(graph, vids = which(vertex.attributes(graph)[[filtering_var]] == value))
    
    if (save) {
      # Determine the subgraph name
      subgraph_name <- if (subgraph_name_var == filtering_var) value else unique(vertex.attributes(graph)[[subgraph_name_var]])[value]
      
      # Construct the file path and save the subgraph
      file_path <- file.path(path, paste0("graph_", subgraph_name, ".graphml"))
      write_graph(subgraph, file_path, format = "graphml")
    }
    
    return(subgraph)
  }
  
  # Use map to apply the create_subgraph function to each unique value
  graph_list <- filtering_val_vect %>%
    set_names() %>%
    future_map(create_subgraph)

  end_time                    <- Sys.time()

  (duration <- end_time - start_time)

  print(duration)
  
  return(graph_list)


}


##################################################################################
# Select the a graph wether to use its largest component or the whole graph
#
# Args :
#   - graph : the graph to be use (must be an igraph objetc )
#   - largest_component : a logical value indicating wether to extract the largest 
#       component or not. If TRUE, largest component is returned (Default is FALSE)
#
# Returns : 
#         - if largest component = TRUE, largest component is returned
#         - else, graph is returned
##################################################################################



select_graph <- function(graph, largest_component = FALSE){


  name <- deparse(substitute(graph))
 
!is.logical(largest_component)
  if (!is.logical(largest_component)) {
        stop("'largest_component' must be a logical value")
  }
  if(largest_component) {
    cat("largest_component is TRUE", paste0(name, "'s"), "largest component is returned", "\n")
    return(largest_component(graph))
  } else {
    cat("largest_component is FALSE", name, "is returned", "\n")
        return(graph)
  }

}






#########################################################################################
# Compute vertex spatial distance matrix
#
# Args :
#   - graph : the graph to be use (must be an igraph objetc with longitude x = lognitude
#                  and y = latitude )
#
# Returns : 
#         - distance matrix
#########################################################################################

compute_vertex_spatial_distance <- function(graph, simple_matrix = TRUE){

  start_time   <- Sys.time()
      # Calculation of the spatial distance matrix
  data_spat <- st_as_sf(data.frame(x=V(graph)$x, y=V(graph)$y),
                        coords = c("x", "y"), crs = 4326)
  spatial_distances <- st_distance(data_spat)

  spatial_distances <- matrix(as.numeric(spatial_distances)
                              , nrow = nrow(spatial_distances)
                              , ncol = ncol(spatial_distances))

  end_time   <- Sys.time()

  (duration <- end_time - start_time)

  print(duration)
  
  return(spatial_distances)
}



#########################################################################################
# Assign attribute to edges from a square matrix (not for bipartite networks)
#
# Args :
#   - attr_name : a character giving the name of the attribute to be created
#   - network : the network (graph) to be use (either a igraph or network object)
#   - attrib_matrix : the input matrix
# Returns : 
#         - the same network with "attr_name" edge attribue
#########################################################################################


add_edge_attribute <- function(attr_name, network, attrib_matrix) {


    # Get number of nodes depending on class
  n_nodes <- if (is.network(network)) {
    network.size(network)
  } else if (is_igraph(network)) {
    igraph::gorder(network)
  } else {
    stop("Input must be a network or igraph object.")
  }
  
  # Check matrix dimensions
  if (!is.matrix(attrib_matrix) || any(dim(attrib_matrix) != c(n_nodes, n_nodes))) {
    stop("The attribute matrix must be a square matrix of the same order as the number of nodes in the network.")
  }


  
  # Add attribut to network
  
  if(is.network(network)){
    # Extract values from the matrix using edge list
    edge_values <- apply(as_edgelist(asIgraph(network), names = FALSE), 1, function(x) attrib_matrix[x[1], x[2]])
    
    # Assign attribute to edges
    network::set.edge.attribute(network, attr_name, edge_values)
  }else if(is_igraph(network)){
    
    edge_values <- apply(as_edgelist(network, names = FALSE), 1, function(x) attrib_matrix[x[1], x[2]])
    
    igraph::set_edge_attr(graph=network, name=attr_name, value=edge_values)
  }
  

  return(network)
}


###########################################################################################
# Function to check if the locations where a user viewed listings form a connected subgraph
#
# Args:
#   views : a data.table of consultation with a user id and location id variables.
#   contig_graph   : a location contiguity graph (igraph object)
#   user_id        : a string indicating the name of user id column
#   loc_col        : a location id variable
#   contiguous_col : a string indicating the name of the column to be added
#
# Returns :
#   A data.table with a logical contiguous_col
###########################################################################################


check_connectivity <- function(views, contig_graph, user_id, loc_col, contiguous_col = "contiguous") {
  
  start_time    <- Sys.time()
  
  views[, (contiguous_col) := {
    locations <- unique(get(loc_col))  # Extract unique locations dynamically
    
    # Induced subgraph with only the locations viewed by the user
    contig_subgraph <- induced_subgraph(contig_graph, vids = locations)
    
    # Check if the subgraph is connected
    is_connected(contig_subgraph)
  }, by = get(user_id)]
  
  end_time    <- Sys.time()
  (duration <- end_time - start_time)
  print(duration)
  
  return(views)
}



###########################################################################################
# Function to check if the locations where a user viewed listings form a connected subgraph
#
# Args:
#   views : a data.table of consultation with a user id and location id variables.
#   contig_graph   : a location contiguity graph (igraph object)
#   group_vars        : a vector of strings indicating the names of grouping columns
#   loc_col        : a location id variable
#   contiguous_col : a string indicating the name of the column to be added
#
# Returns :
#   A data.table with a logical contiguous_col
###########################################################################################



check_connectivity_multiple_vars <- function(views, contig_graph, group_vars, loc_col, contiguous_col = "contiguous") {
  
  start_time    <- Sys.time()
  
  views[, (contiguous_col) := {
    locations <- unique(get(loc_col))  # Extract unique locations dynamically
    
    # Induced subgraph with only the locations viewed by the user
    contig_subgraph <- induced_subgraph(contig_graph, vids = locations)
    
    # Check if the subgraph is connected
    is_connected(contig_subgraph)
  }, by = group_vars]
  
  end_time    <- Sys.time()
  (duration <- end_time - start_time)
  print(duration)
  
  return(views)
}





###########################################################################################
# Function to compute spatial distance between linked nodes
#
# Args:
#   graph : a igraph object with geometry on vertex attributes (should be in WGS 84).
#   name  : a string indicating the name of the variable to be computed
#
# Returns :
#   graph with a new edge attribute named "name"
###########################################################################################



node_spatial_distance <- function(graph, name = "distance"){

  start_time    <- Sys.time()

  ### 1. Preparation of the vector of projected geometries on the fly (WGS84 → Lambert 93)
  geoms_proj <- st_transform(st_sfc(V(graph)$geometry, crs = 4326), crs = 2154)

  # Matching with names
  names(geoms_proj) <- V(graph)$name

  ### 2. Retrieving the geometries of the ends of the links
  edge_list <- as_edgelist(graph)  # matrice (from, to)

  # Vectorization: extract all points at once
  all_points_from <- geoms_proj[edge_list[,1]]
  all_points_to   <- geoms_proj[edge_list[,2]]

  ### 3. Vectorized distance calculation
  distances <- st_distance(all_points_from, all_points_to, by_element = TRUE)

  ### 4. Adding distances to the graph
  graph <- set_edge_attr(graph, name = name, value = as.numeric(distances))

  end_time    <- Sys.time()
  (duration <- end_time - start_time)
  print(duration)

    return(graph)

}


###########################################################################################
# Function to keep only the top k strongest links per node in an igraph network
#
# Args:
#   graph : an undirected igraph object with a 'weight' edge attribute.
#   k : an integer that indicates the number of strongest links to keep.
#
# Returns :
#   filtered igraph object keeping only the 3 strongest links for each node
###########################################################################################

keep_top_k_links <- function(graph, k = 3) {
  library(igraph)
  library(dplyr)

  # Extract edges from the graph as a data frame
  el <- as_data_frame(graph, what = "edges")

  # Duplicate edges to treat the network as undirected (both directions per edge)
  edges_long <- bind_rows(
    el %>% mutate(node = from, neighbor = to),
    el %>% mutate(node = to, neighbor = from)
  )

  # Select the top 3 strongest weights for each node
  topk_edges <- edges_long %>%
    group_by(node) %>%
    slice_max(weight, n = k, with_ties = FALSE) %>%
    ungroup()

  # Create standardized node pairs to remove symmetry duplicates
  topk_pairs <- topk_edges %>%
    rowwise() %>%
    mutate(p1 = min(from, to), p2 = max(from, to)) %>%
    ungroup() %>%
    distinct(p1, p2)

  # Identify which edges to keep in the original graph
  keep_edge <- mapply(function(u, v) {
    p1 <- min(u, v)
    p2 <- max(u, v)
    nrow(topk_pairs %>% filter(p1 == !!p1 & p2 == !!p2)) > 0
  }, el$from, el$to)

  # Delete edges that are not in the top 3 selections
  filtered_graph <- delete_edges(graph, E(graph)[!keep_edge])

  return(filtered_graph)
}

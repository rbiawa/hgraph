##############################################################################
# Functions used during graph anlysis.
# 
# 2024/11 Raphaël Biawa Diegamounoua
#
# source("src/statistics/graph_statistics/graph_statistics.R")
##############################################################################









#########################################################################################
# Compute weighted assortativity with standard error-based significance testing
#
# Args:
#   - graph: igraph object. The input network.
#   - node_attr: character. Name of vertex attribute (continuous or categorical).
#   - weighted: logical. Whether to consider edge weights (default: TRUE).
#   - weight_attr: character. Name of edge weight attribute (default: "weight").
#
# Returns:
#   - List with five components:
#     * observed: numeric. Assortativity coefficient (r)
#     * std_error: numeric. Standard error of r
#     * z_score: numeric. Z-score for hypothesis testing
#     * p_value: numeric. Two-tailed p-value from normal approximation
#     * method: character. Type of assortativity computed ("continuous" or "discrete")
#########################################################################################

assortnet_assortativity <- function(graph, node_attr, weighted = TRUE, weight_attr = "weight", SE = TRUE) {


 start_time                   <- Sys.time()
  # Validate inputs
  if(!is.igraph(graph)) stop("Input must be an igraph object")
  if(!node_attr %in% vertex_attr_names(graph)) {
    stop(paste("Vertex attribute", node_attr, "not found in graph"))
  }
  
  # Convert to adjacency matrix with weights
  adj_matrix <- as_adjacency_matrix(
    graph = graph,
    attr = if(weighted) weight_attr else NULL,
    sparse = FALSE
  )
  
  # Extract node attribute values
  attr_values <- vertex_attr(graph, node_attr)
  
  # Determine analysis type and compute assortativity
  if(is.numeric(attr_values)) {
    result <- assortment.continuous(
      graph = adj_matrix,
      vertex_values = attr_values,
      weighted = weighted,
      SE = SE
    )
    method_type <- "continuous"
  } else {
    if(!is.factor(attr_values)) attr_values <- as.factor(attr_values)
    result <- assortment.discrete(
      graph = adj_matrix,
      types = attr_values,
      weighted = weighted,
      SE = SE
    )
    method_type <- "discrete"
  }
  
  # Calculate significance metrics
  z_score <- result$r / result$SE
  p_value <- 2 * pnorm(-abs(z_score))  # Two-tailed test


    end_time                    <- Sys.time()

  (duration <- end_time - start_time)

  print(duration)
  
  return(list(
    observed = result$r,
    std_error = result$SE,
    z_score = z_score,
    p_value = p_value,
    method = method_type
  ))
}



#########################################################################################
# Compute assortativity measure and perform a permutation test
#
# Args:
#   - graph: An igraph object representing the network
#   - node_attr: Name of vertex attribute (character) to compute assortativity on
#   - weighted: Logical, whether to use edge weights (default: TRUE)
#   - weight_attr: Name of edge weight attribute (default: "weight")
#   - n_perm: Number of permutations for p-value calculation (default: 1000)
#
# Returns:
#   - List with three elements:
#     $observed: Observed assortativity coefficient
#     $p_value: Empirical p-value from permutation test
#     $permuted: Vector of permuted assortativity values
#########################################################################################

if (FALSE) {
  assortnet_assortativity <- function(graph, node_attr, weighted = TRUE, 
                                   weight_attr = "weight", n_perm = 1000) {
  start_time    <- Sys.time()

  # Input validation
  if(!is_igraph(graph)) stop("Input must be an igraph object")
  if(!node_attr %in% vertex_attr_names(graph)) stop("Attribute not found in graph")
  
  # Convert to adjacency matrix
  adj_matrix <- as_adjacency_matrix(
    graph = graph,
    attr = if(weighted) weight_attr else NULL,
    sparse = FALSE
  )
  
  # Extract node attribute and determine type
  attr_values <- vertex_attr(graph, node_attr)
  is_numeric  <- is.numeric(attr_values)
  
  # Compute observed assortativity
  compute_assort <- function(values) {
    if(is_numeric) {
      assortment.continuous(adj_matrix, values, weighted)$r
    } else {
      assortment.discrete(adj_matrix, as.factor(values), weighted)$r
    }
  }
  obs_assort <- compute_assort(attr_values)
  
  # Permutation test
  perm_values <- replicate(n_perm, {
    shuffled <- sample(attr_values, replace = FALSE)  # Preserve distribution
    compute_assort(shuffled)
  })
  
  # Calculate two-tailed p-value
  p_val <- mean(abs(perm_values) >= abs(obs_assort), na.rm = TRUE)

  end_time     <- Sys.time()

  print(end_time - start_time)
  return(list(
    observed = obs_assort
    ,p_value = p_val
    #,permuted = perm_values
  ))
  }
}


assortnet_assortativity <- function(graph, node_attr, weighted = TRUE, 
                                   weight_attr = "weight", n_perm = 1000) {

  start_time <- Sys.time()

  if (!is_igraph(graph)) stop("Input must be an igraph object")
  if (!node_attr %in% vertex_attr_names(graph)) stop("Attribute not found in graph")

  adj_matrix <- as_adjacency_matrix(
    graph = graph,
    attr = if (weighted) weight_attr else NULL,
    sparse = FALSE
  )

  attr_values <- vertex_attr(graph, node_attr)
  is_numeric  <- is.numeric(attr_values)

  compute_assort <- function(values) {
    if (is_numeric) {
      assortment.continuous(adj_matrix, values, weighted)$r
    } else {
      assortment.discrete(adj_matrix, as.factor(values), weighted)$r
    }
  }

  obs_assort <- compute_assort(attr_values)

  # -----------------------------------------
  # Parallelize ONLY the generation of permutations
  # -----------------------------------------
  permuted_values <- furrr::future_map(
    1:n_perm,
    ~ sample(attr_values, replace = FALSE),
    .options = furrr::furrr_options(seed = TRUE)
  )

  # Compute assortativity sequentially (fast, avoids 8GB copies)
  perm_values <- vapply(permuted_values, compute_assort, numeric(1))

  p_val <- mean(abs(perm_values) >= abs(obs_assort), na.rm = TRUE)

  end_time <- Sys.time()
  print(end_time - start_time)

  list(
    observed = obs_assort,
    p_value  = p_val
  )
}




#########################################################################################
# Compute assortativity measure and perform a permutation test
#
# Args:
#   - graph: The input igraph object
#   - attribute: The vertex attribute on which assortativity is computed
#   - n_perm: Number of permutations
#
# Returns:
#   - A list containing the observed assortativity and its associated p-value
#########################################################################################



assortativity_test <- function(graph, attribute, n_perm = 1000, directed = TRUE){

  start_time    <- Sys.time()

  if (class(vertex_attr(graph, attribute)) %in% c("factor", "character")){

    attribute_val <- as.integer(
      factor(vertex_attr(graph, attribute), 
        levels = unique(vertex_attr(graph, attribute)
        )
      )
    )

   # 1. Calculate the observed assortativity
    r_obs <- assortativity_nominal(graph,
      types    = attribute_val,
      directed = directed
    )

   # 2. Generate the null distribution by permutation
    r_perm <- replicate(n_perm,
      {
        shuffled_attr <- sample(attribute_val)  # random permutation
        assortativity_nominal(graph, types = shuffled_attr, directed = directed)
      }
    )

  } else {
    # 1. Calculate the observed assortativity
    r_obs <- assortativity(graph, values = vertex_attr(graph, attribute), directed = directed)

    # 2. Generate the null distribution by permutation
    r_perm <- replicate(n_perm,
      {
        shuffled_attr <- sample(vertex_attr(graph, attribute))
        assortativity(graph, values = shuffled_attr)
      }
    )
  }



  # 4. P_value 

  #bilatérale
  p_value <- mean(abs(r_perm) >= abs(r_obs), na.rm = TRUE)


  # Visualisation
  if (interactive()) {
      hist(r_perm, breaks = 30, main = "Null distribution", xlab = "Assortativity")
      abline(v = r_obs, col = "red", lwd = 2)
  }

  end_time     <- Sys.time()

  print(end_time - start_time)

  return(list(assortativity = r_obs
      , p_value = p_value
      #, random_simulations <- r_perm
      , hist = {
        hist(r_perm, breaks = 30, main = "Null distribution", xlab = "Assortativity")
        abline(v = r_obs, col = "red", lwd = 2)
      }
    )
  )
}







######################################################################################################################################################
# Calculates graph statistics.
#
# graph: an igraph object representing the graph for which statistics are to be computed.
# required_attributes: a vector of vertex attributes (character vector) that should be present in the graph's vertices
# and through which assortativity measures will be coputed.
#           Default is c("price", "area", "room_count").
# directed: a logical value (TRUE or FALSE) indicating whether the graph is directed or not. 
#           Default is FALSE (undirected graph).
# threshold: an integer which the graph size must be greater or equal to make possible the calculatation of the assortativity 
#           coefficients for the attributes of the nodes 
#           Default value is ecount(graph).
# 
# returns: a one-row dataframe with the following calculated statistics:
# order: the number of vertices (nodes) in the graph.
# size: the number of edges in the graph.
# mean_degree: the average degree (number of edges per vertex).
# clustering_coef: the global clustering coefficient of the graph.
# mean_dist: the average shortest path length between all pairs of vertices.
# diameter: the diameter of the graph (the longest shortest path between any two vertices).
# deg_assortativity: the degree assortativity of the graph.
# <attribute>_assortativity: assortativity for each required attribute specified by the user (e.g., "price_assortativity").
######################################################################################################################################################

compute_graph_stats <- function(graph, required_attributes = c("price", "area", "room_count"), directed = FALSE, threshold = ecount(graph)) {

  start_time              <- Sys.time()
  # Check: The input must be an igraph object
  if (!inherits(graph, "igraph")) {
    stop("The input must be an igraph object.")
  }

  # Check: The graph must have at least two vertices
  #if (vcount(graph) < 2) {
  #  warning("The graph is too small to compute statistics.")
  #  return(NULL)
  #}

  # Check: Required vertex attributes must be present
  if (!all(required_attributes %in% vertex_attr_names(graph))) {
    missing_attributes <- setdiff(required_attributes, vertex_attr_names(graph))
    stop(paste("The following required vertex attributes are missing:", 
               paste(missing_attributes, collapse = ", ")))
  }

  # Initialize a data frame to store the statistics
  stats <- data.frame(
    order = NA,
    size = NA,
    mean_degree = NA,
    clustering_coef = NA,
    mean_dist = NA,
    diameter = NA,
    deg_assortativity = NA
  )

  # Compute graph statistics and store them in the data frame
  stats$order             <- vcount(graph)
  stats$size              <- ecount(graph)
  stats$mean_degree       <- mean(degree(graph, mode = if (directed) "all" else "total"))
  stats$clustering_coef   <- transitivity(graph, type = "global")
  stats$mean_dist         <- mean_distance(graph, directed = directed)
  stats$diameter          <- diameter(graph, directed = directed)
  stats$deg_assortativity <- assortativity_degree(graph, directed = directed)

  if (ecount(graph) >= threshold) {

        # Compute assortativity for each required attribute
    for (attribute in required_attributes) {
      col_name          <- paste0(attribute, "_assortativity")

      if (class(vertex.attributes(graph)[[attribute]]) %in% c("factor", "character")){

        stats[[col_name]] <- assortativity_nominal(graph, 
                                                  types    = as.integer(factor(vertex.attributes(graph)[[attribute]], 
                                                  levels   = unique(vertex.attributes(graph)[[attribute]]))),
                                                  directed = directed)

      } else {
        stats[[col_name]] <- assortativity(graph, vertex.attributes(graph)[[attribute]], types2 = NULL, directed = directed)
      }
    
    }

  } 



  end_time                    <- Sys.time()

  (duration <- end_time - start_time)

  print(duration)
  return(stats)
}


##################################################################################################################################
# Computes and optionally exports statistics for multiple graphs.
# Requires furrr package.
#
# graph_list: a named list of graphs (each element must be an igraph object).
# required_attributes: a vector of vertex attributes (character vector) used for assortativity measures.
#                      Default is c("price", "area", "room_count").
# directed: logical value (TRUE or FALSE) indicating whether the graphs are directed or not. 
#           Default is FALSE (undirected graphs).
# ID_var_name: the name of the vertex attribute used as the locality ID variable.
# threshold: an integer which the graph size must be greater or equal to make possible the calculatation of the assortativity 
#           coefficients for the attributes of the nodes. 
#           Default value is NULL.
# save: logical value (TRUE or FALSE) indicating whether the graph statistics dataframe should be saved to a .xlsx file. 
#       Default is FALSE.
# directory: directory where the output file will be saved (if save = TRUE). Default is the working directory (getwd()).
#
# returns: a dataframe containing calculated statistics for each graph in the list.
##################################################################################################################################

compute_multiple_graph_stats <- function(graph_list, 
                                         required_attributes = c("price", "area", "room_count"), 
                                         directed = FALSE, 
                                         ID_var_name, 
                                         threshold = NULL,
                                         save = FALSE, 
                                         directory = getwd()) {
  start_time              <- Sys.time()
 
  # Validate inputs
  if (!is.list(graph_list) || !all(sapply(graph_list, inherits, "igraph"))) {
    stop("graph_list must be a list of igraph objects.")
  }
  
  if (is.null(names(graph_list))) {
    stop("graph_list must be a named list.")
  }
  
  if (!is.character(ID_var_name) || length(ID_var_name) != 1) {
    stop("ID_var_name must be a single string representing a vertex attribute name.")
  }
  
  # Define a helper function to compute statistics for a single graph
  process_graph <- function(graph, graph_name, ID_var_name, required_attributes, directed, threshold = threshold) {
    # Check if the required ID attribute is present
    if (!ID_var_name %in% vertex_attr_names(graph)) {
      stop(paste("The vertex attribute", ID_var_name, "is missing in graph", graph_name))
    }
    
    # Compute statistics for the graph

    if (is.null(threshold)) {
      graph_stat <- compute_graph_stats(graph, required_attributes, directed)
    } else {
       graph_stat <- compute_graph_stats(graph, required_attributes, directed, threshold)
    }
    
    
    # Retrieve the ID for the graph (assuming a single ID per graph)
    graph_ID <- vertex.attributes(graph)[[ID_var_name]][1]
    
    # Add the graph's name and ID as columns
    graph_stat <- graph_stat %>%
      mutate(Locality = graph_name, ID = graph_ID)
  
    return(graph_stat)
  }
  
  # Use parallel processing to compute statistics for all graphs
  require(furrr)
  plan(multisession) # Enable parallel computation
  
  graph_stats_list <- future_map(seq_along(graph_list),
    .f = ~ process_graph(graph_list[[.x]], names(graph_list)[[.x]], 
                         ID_var_name, required_attributes, directed, threshold)
  )
  
  # Combine the results into a single dataframe
  graph_stats <- do.call(bind_rows, graph_stats_list)
  
  # Reorganize columns
  graph_stats <- graph_stats %>%
    dplyr::select(ID, Locality, everything())
  
  # Save to a file if required
  if (save) {
    output_file <- file.path(directory, "graphs_statistics.xlsx")
    write_xlsx(graph_stats, output_file)
    message(paste("Statistics saved to:", output_file))
  }

  end_time                    <- Sys.time()

 (duration <- end_time - start_time)

 print(duration)

  return(graph_stats)
}




##############################################################################
# Plots of degree complementary cumulative distribution..
#
# graph_list: the input graph list.
# 
# returns: plots from all input degree complementary cumulative distribution.
##############################################################################


# Fist methode of degree distribution graph function----

plot_degree_ccd <- function(graph_list) {
  for (i in seq_along(graph_list)) {
    graph <- graph_list[[i]]
    name  <- names(graph_list)[i]
    x     <- degree(graph)
    y     <- complementary_ecdf(x)
    
    if (max(y) > 0) {
      plot(x, y, type = "p",
           main = paste("CCD of degrees of the graph for", name),
           xlab = "degrees (log scale)", ylab = "probability (log scale)",
           log = "xy")
      # xlab = "degrees (linear scale)", ylab = "probability (linear scale)")
    } else {
      plot(x, y, type = "p",
           main = paste("CCD of degrees of the graph for", name),
           xlab = "degrees", ylab = "probability (linear scale)")
    }
  }
}



##############################################################################
# Plots of degree complementary cumulative distribution.
#
# graph_list: the input graph list.
# 
# returns: plots from all input degree complementary cumulative distribution.
##############################################################################

# Second methode of degree distribution graph function----

plot_degree_ccd_igraph <- function(graph_list) {
  for (i in seq_along(graph_list)) {
    graph <- graph_list[[i]]
    name  <- names(graph_list)[i]
    x     <- degree_distribution(graph, cumulative = TRUE)
    
    plot(x, 
         main = paste("CCD of degrees of the graph for", name),
         xlab = "degrees (log scale)", ylab = "probability (log scale)",
         log = "xy")
    # xlab = "degrees (linear scale)", ylab = "probability (linear scale)")
  }
}



##############################################################################
# Plots degree correlation (or degree Assortativity).
#
# graph_list: the input graph list.
# 
# returns: plots from all input degree complementary cumulative distribution.
##############################################################################


plot_degree_correlation <- function(graph_list) {
  for (i in seq_along(graph_list)) {
    graph <- graph_list[[i]]
    name  <- names(graph_list)[i]
    x     <- degree(graph)
    y     <- knn(graph)$knn
    
    if (ecount(graph) > 0) {
      plot(x, y, type = "p",
           main = paste("Degree correlation plot of the graph for", name),
           xlab = "degrees", ylab = "average degree of neighbors"
           # log = "xy"
      )
    }
  }
}




##############################################################################
# Plots a stistic variable map.
#
# data : the input dataframe.
# var : the variable to be ploted
# name_var : the ploted variable label
# nclasse : a integer for the number of discretisation class
# line_width : numeric number for line width (Default is .5)
# 
# returns: a plot of a stistic variable map.
##############################################################################


plot_map <- function(data, var, name_var, nclasse = 6, line_width = .5) {

  # Initialize breaks based on the values of `var`
  if (all(var < 0)) {
    # If all values are negative, create classes across the range
    breaks <- classIntervals(var, n = nclasse, style = "quantile")$brks
    palette <- "Blues"  # Palette for negative values
    direction <- -1  # Decreasing gradient

  } else if (all(var >= 0)) {
    # If all values are positive, create classes across the range
    breaks <- classIntervals(var, n = nclasse, style = "quantile")$brks
    palette <- "Reds"  # Palette for positive values
    direction <- 1  # Increasing gradient

  } else {
    # If `var` contains both positive and negative values
    breaks_neg <- classIntervals(var[var < 0], n = nclasse / 2, style = "quantile")$brks
    breaks_pos <- classIntervals(var[var >= 0], n = nclasse / 2, style = "quantile")$brks
    breaks <- unique(c(breaks_neg, 0, breaks_pos))
    palette <- "RdYlBu"  # Divergent palette for mixed values
    direction <- -1  # Decreasing gradient
  }

  # Create the discretized variable with the new classes
  data$var_discretized <- cut(var, breaks = breaks, include.lowest = TRUE)

  # Create the map with appropriate discretization and color palette
  plot1 <- ggplot(data = data) +
    geom_sf(aes(fill = var_discretized), color = "black", linewidth = line_width) +
    scale_fill_brewer(palette = palette, direction = direction) +  # Use dynamic direction
    theme_minimal() +
    coord_sf(datum = NA) +
    annotation_scale(location = "br", line_width = .5) +
    labs(title = paste("Assortativity coefficient of", name_var),
         fill = "Assortativity coefficient")

  # Create the density plot
  plot2 <- ggplot(data = data) + 
    geom_density(aes(x = var, y = ..density..)) +
    theme_minimal() + 
    labs(title = "Variable density")

  # Display the plots with adjusted sizes
  grid.arrange(plot1, plot2, ncol = 2, widths = c(3, 1), heights = c(4, 1))
}


#########################################################################################################
# Plots an assortativity coefficient map with consistent colors across graphs.
#
# data: an sf object containing spatial data.
# var: the name of the variable to map (character, must be in 'data').
# k: an integer defining the number of classes for both negative and positive values (default is 3).
# name_var: name of the variable to be ploted (a character)
# digits: an integer specifying the number of decimal places to use in labels (default is 7).
# line_width : numeric number for line width (Default is .5)
#
# The function:
# - Harmonizes colors across graphs for comparability.
# - Dynamically calculates breakpoints for negatives, positives, and zero.
# - Assigns classes and their colors simultaneously to avoid mismatch.
# - Ensures that the legend is displayed with the correct class-color mapping.
#########################################################################################################

plot_assortativity_map <- function(data, var, k = 3, name_var = '', digits = 7, line_width = .5) {

  # Validate inputs
  if (!inherits(data, "sf")) stop("The input 'data' must be an sf object.")
  if (!var %in% names(data)) stop("The specified variable must exist in 'data'.")
  if (!is.numeric(data[[var]])) stop("The selected variable must be numeric.")
  if (!is.numeric(k) || k <= 0 || k != as.integer(k)) stop("The input 'k' must be a positive integer.")
  if (!is.numeric(digits) || digits <= 0 || digits != as.integer(digits)) stop("The input 'digits' must be a positive integer.")
  
  # Extract the variable values
  var_values <- data[[var]]
  
  # Define the breakpoints for negative and positive values
  neg_breaks <- seq(-1, 0, length.out = k + 1)
  pos_breaks <- seq(0, 1, length.out = k + 1)
  
  # Define color palettes for negative and positive values without the white
  neg_colors <- colorRampPalette(c("darkblue", "#e5f0f5"))(k)  # Dark blue to sky blue for negatives
  pos_colors <- colorRampPalette(c("#fce8e8", "#a81e1e"))(k)    # Light coral to red for positives
  
  # Prepare classes and colors
  classes <- character(length(var_values))
  colors <- character(length(var_values))
  
  # Assign classes and colors for negative values
  for (i in seq_len(k)) {
    lower <- neg_breaks[i]
    upper <- neg_breaks[i + 1]
    condition <- var_values >= lower & var_values < upper
    classes[condition] <- paste0("[", round(lower, digits), ", ", round(upper, digits), ")")
    colors[condition] <- neg_colors[i]
  }
  
  # Assign class and color for zero
  condition <- var_values == 0
  classes[condition] <- "(0)"
  colors[condition] <- "#FFFFFF"  # Pure white for zero class
  
  # Assign classes and colors for positive values
  for (i in seq_len(k)) {
    lower <- pos_breaks[i]
    upper <- pos_breaks[i + 1]
    condition <- var_values > lower & var_values <= upper
    classes[condition] <- paste0("(", round(lower, digits), ", ", round(upper, digits), "]")
    colors[condition] <- pos_colors[i]
  }
  
  # Convert classes to a factor with defined levels
  all_levels <- c(
    paste0("[", round(head(neg_breaks, -1), digits), ", ", round(tail(neg_breaks, -1), digits), ")"),
    "(0)",
    paste0("(", round(head(pos_breaks, -1), digits), ", ", round(tail(pos_breaks, -1), digits), "]")
  )
  classes <- factor(classes, levels = all_levels)
  
  # Add the discretized variable and colors back to the data
  data$var_discretized <- classes
  data$var_colors <- colors
  
  # Create a named color vector for the legend
  color_mapping <- setNames(c(neg_colors, "#FFFFFF", pos_colors), all_levels)
  
  # Debugging: Display the count of observations in each class and their colors
  cat("Debugging: Count of observations in each class:\n")
  print(table(data$var_discretized, useNA = "ifany"))
  
  # Create the map
  plot <- ggplot(data) +
    geom_sf(aes(fill = var_discretized), color = "black", linewidth = line_width) +
    scale_fill_manual(
      values = color_mapping,
      na.value = "#e0dada",  # Light gray for missing values
      name = "Assortativity coefficient"
    ) +
    theme_minimal() +
    coord_sf(datum = NA) +
    annotation_scale(location = "br", line_width = .5) +
    labs(
      title = paste("Assortativity Coefficient of", name_var), 
      fill = "Classes"

    ) +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "right"
    )
  
  # Display the map
  print(plot)

}



##############################################################################
# Computes and plots geodesic and spatial distance distributions.
#
# graph : the input graph.
# locality : the graph locality name
# 
# returns: the smoothed data for further analysis if necessary.
##############################################################################

# Graph between Geodesic and Spatial Distances----

analyze_geodesic_spatial <- function(graph, locality = "") {

  start_time              <- Sys.time()
  
  # Calculate the spatial distance matrix
  data_spat <- st_as_sf(data.frame(x=V(graph)$x, y=V(graph)$y),
                        coords = c("x", "y"), crs = 4326)
  spatial_distances <- st_distance(data_spat)
  
  # Calculate the geodesic distance matrix
  geodesic_distances <- distances(graph, weights = 1 / E(graph)$weight)
  
  # Handle infinite distances (unconnected nodes)
  max_geodesic_distance <- max(geodesic_distances[is.finite(geodesic_distances)],na.rm = TRUE)
  gap <- 1  # Define a gap to replace infinite values
  geodesic_distances[is.infinite(geodesic_distances)] <- max_geodesic_distance + gap
  
  # Convert the matrices into vectors
  geodesic_values <- as.vector(geodesic_distances)
  spatial_values <- as.vector(spatial_distances)
  
  # Check if the matrices are the same size
  if (length(geodesic_values) != length(spatial_values)) {
    stop("The geodesic distance matrix and the spatial distance matrix must have the same dimensions.")
  }
  
  # Create a data frame to facilitate operations
  data <- data.frame(geodesic_values, spatial_values)
  
  # Calculate the average spatial distance for each unique geodesic distance
  smoothed_data <- aggregate(spatial_values ~ geodesic_values, data = data, FUN = mean)
  
  # Calculate the standard deviation of spatial distances for each unique geodesic distance
  stdev_data <- aggregate(spatial_values ~ geodesic_values, data = data, FUN = sd)
  
  # Prepare data for polygon()
  xs <- smoothed_data$geodesic_values
  avg_dist <- smoothed_data$spatial_values
  stdev_dist <- stdev_data$spatial_values
  
  # Plot the relationship between geodesic distances and average spatial distances with uncertainty area
  plot(
    NULL,
    xlim = range(xs, na.rm = TRUE),
    ylim = range(avg_dist - stdev_dist, avg_dist + stdev_dist, na.rm = TRUE),
    xlab = "Geodesic Distance", 
    ylab = "Average Spatial Distance", 
    main = paste("Relationship Between Geodesic and Spatial Distances (Smoothed) in", "\n", locality)
  )
  
  # Plot the uncertainty area with polygon()
  polygon(
    x = c(xs, rev(xs)),
    y = c(avg_dist - stdev_dist, rev(avg_dist + stdev_dist)),
    col = rgb(0.5, 0, 0.5, alpha = 0.09),  # Purple zone with transparency
    border = NA
  )
  
  # Plot the average line with lines()
  lines(xs, avg_dist, col = "purple", lwd = 2)

  end_time                    <- Sys.time()

  (duration <- end_time - start_time)

  print(duration)
  
  # Return the smoothed data for further analysis if necessary
  return(list(model_data = smoothed_data, stdev_data = stdev_data))
}





##############################################################################
# Computes and plots spatial and geodesic distances distributions.
#
# graph : the input graph.
# locality : the graph locality name
# 
##############################################################################

# Graph between Geodesic and Spatial Distances----

analyze_spatial_geodesic <- function(graph, locality = "") {

  start_time              <- Sys.time()
  
  # Calculate the spatial distance matrix
  data_spat <- st_as_sf(data.frame(x=V(graph)$x, y=V(graph)$y),
                        coords = c("x", "y"), crs = 4326)
  spatial_distances <- st_distance(data_spat)
  
  # Calculate the geodesic distance matrix
  geodesic_distances <- distances(graph, weights = 1 / E(graph)$weight)
  
  # Handle infinite distances (unconnected nodes)
  max_geodesic_distance <- max(geodesic_distances[is.finite(geodesic_distances)],na.rm = TRUE)
  gap <- 1  # Define a gap to replace infinite values
  geodesic_distances[is.infinite(geodesic_distances)] <- max_geodesic_distance + gap
  
  # Convert the matrices into vectors
  geodesic_values <- as.vector(geodesic_distances)
  spatial_values <- as.vector(spatial_distances)
  
  # Check if the matrices are the same size
  if (length(geodesic_values) != length(spatial_values)) {
    stop("The geodesic distance matrix and the spatial distance matrix must have the same dimensions.")
  }
  
  # Create a data frame to facilitate operations
  data <- data.frame(spatial_values, geodesic_values)
   

  # Sort the data by spatial_values
  data <- data[order(data$spatial_values), ]

  # Plot the relationship between spatial distances and geodesic distances
  ggplot(data) +
    geom_line(aes(x = spatial_values, y = geodesic_values)) +  # Draw a line connecting the points
    geom_point(aes(x = spatial_values, y = geodesic_values)) + # Add points for better clarity
    scale_x_continuous(limits = range(data$spatial_values, na.rm = TRUE)) +
    scale_y_continuous(limits = range(data$geodesic_values, na.rm = TRUE)) +
    labs(
      x = "Spatial distance",
      y = "Geodesic distance",
      title = paste("Relationship between spatial and geodesic distances in", "\n", locality)
    ) +
    theme_minimal()

  end_time                    <- Sys.time()

  (duration <- end_time - start_time)

  print(duration)

}







##############################################################################
# Plots geodesic and spatial distance distributions from a list of graphs.
#
# graph_list : the input graph list (a list).
#  
##############################################################################

# Application to the list of graphs function

plot_muliple_graphs_distances <- function(graph_list) {
  start_time              <- Sys.time()
  for (i in seq_along(graph_list)) {
    graph <- graph_list[[i]]
    name  <- names(graph_list)[i]
    if(length(E(graph)) > 1) {
      analyze_geodesic_spatial(graph, name)
    } else {
      plot(graph, vertex.label=NA,vertex.size=5,vertex.color="darkblue", edge.arrow.size=.3, main = paste("graph de", name))
    }
  }
  end_time                    <- Sys.time()

  (duration <- end_time - start_time)

  print(duration)
}







##############################################################################
# Performs distances correlation Mantel's test.
#
# graph : the input graph.
# 
# returns: Mantel's test result.
##############################################################################

# A function that takes a graph, calculates its two distance matrices, and applies the Mantel test to them----

apply_mantel_test <- function(graph){
  start_time              <- Sys.time()

  # Calculation of the spatial distance matrix
  data_spat <- st_as_sf(data.frame(x=V(graph)$x, y=V(graph)$y),
                        coords = c("x", "y"), crs = 4326)
  spatial_distances <- st_distance(data_spat)
  
  # Calculation of the geodesic distance matrix
  geodesic_distances <- distances(graph, weights = 1 / E(graph)$weight)
  
  # Handling infinite distances (unconnected nodes)
  max_geodesic_distance <- max(geodesic_distances[is.finite(geodesic_distances)], na.rm = TRUE)
  gap         <- 1  # Define a gap to replace infinite values
  geodesic_distances[is.infinite(geodesic_distances)] <- max_geodesic_distance + gap
  
  # Mantel test
  mantel_test <- mantel(as.dist(geodesic_distances), as.dist(spatial_distances))


  end_time                    <- Sys.time()
  (duration <- end_time - start_time)

  mantel_test$execution_time  <- duration

  return(mantel_test) 
  
}



##############################################################################
# Performs distances correlation Mantel's test from a list of graphs 
#   (requires apply_mantel_test).
#
# graph_list : the input graph list.
# 
# returns: a list of Mantel's test results.
##############################################################################


apply_multiple_mantel_test <- function(graph_list) {

  # Start message
  message("Starting Multiple Mantel's test")

  # Record the start time
  start_time <- Sys.time()

  # Define a helper function to run Mantel's test on a graph
  run_test <- function(graph) {
    if (ecount(graph) > 0) {
      return(apply_mantel_test(graph)) # Apply Mantel's test if the graph has edges
    } else {
      return(NULL) # Return NULL if the graph has no edges
    }
  }

  # Apply the Mantel test in parallel to all graphs
  mantel_test_res <- future_map(graph_list, ~ run_test(.x))

  # Assign names to the results based on the names of the input graph list
  names(mantel_test_res) <- names(graph_list)

  # Record the end time
  end_time <- Sys.time()

  # Calculate and print the duration
  duration <- end_time - start_time
  print(duration)

  # End message
  message("Completed Multiple Mantel's test")

  # Return the results
  return(mantel_test_res)
}



##############################################################################
# Performs distances correlation Mantel's test from a list of graphs 
#   (requires apply_mantel_test).
#
# graph_list : the input graph list.
# 
# returns: a list of Mantel's test results.
##############################################################################


apply_multiple_mantel_test  <- function(graph_list) {

  print("Multiple Mantel's test start")

  start_time              <- Sys.time()
  mantel_test_res         <- list()

  for (i in seq_along(graph_list)) {
    name <- names(graph_list)[i] 
    if (ecount(graph_list[[i]])) {
      mantel_test_res[[name]] <- apply_mantel_test(graph_list[[i]])
    }
  }
  # Record the end time
  end_time <- Sys.time()

  # Calculate and print the duration
  duration <- end_time - start_time
  print(duration)

  # End message
  message("Completed Multiple Mantel's test")

  return(mantel_test_res)
}

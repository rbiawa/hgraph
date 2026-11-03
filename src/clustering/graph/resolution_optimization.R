#########################################################################################
# Functions used for graph clustering.
# 
# 2025/02 Raphaël Biawa Diegamounoua
#
# source("src/clustering/graph/resolution_optimization.R")
#########################################################################################






#########################################################################################
# Function: test_leiden_resolutions
#
# Purpose:
#   Test multiple Leiden resolutions on a given graph and store the resulting partitions.
#
# Args:
#   - graph      : igraph object on which the clustering is performed.
#   - res_min    : minimum resolution value to test.
#   - res_max    : maximum resolution value to test.
#   - res_step   : step size between tested resolutions.
#   - n_iter     : number of iterations for the Leiden algorithm.
#   - beta       : beta parameter of the Leiden algorithm (controls randomness).
#   - objective  : objective function used by Leiden ("modularity" or "CPM").
#
# Returns:
#   A list where each element corresponds to a tested resolution and contains:
#     * resolution : the tested resolution value
#     * membership : the cluster membership vector for all vertices
#     * n_clusters : the number of detected clusters
#   The function also prints the total computation time.
#########################################################################################


test_leiden_resolutions <- function(
    graph,
    res_min = 0.5,
    res_max = 3,
    res_step = 0.1,
    n_iter = 10,
    beta = 0.01,
    objective = "modularity"
) {
  # Build resolution grid
  resolutions <- seq(res_min, res_max, by = res_step)
  
  # Storage list
  results <- vector("list", length(resolutions))

  t_start <- Sys.time()
  
  for (i in seq_along(resolutions)) {
    res <- resolutions[i]
    
    # Run Leiden at this resolution
    cl <- cluster_leiden(
      graph               = graph,
      objective_function  = objective,
      resolution          = res,
      beta                = beta,
      n_iterations        = n_iter
    )
    
    # Store membership
    results[[i]] <- list(
      resolution = res,
      membership = membership(cl),
      n_clusters = length(unique(membership(cl))),
      modularity = cl$quality,
      comm_size  = sizes(cl)
    )
  }
  
        # End timer
      t_end <- Sys.time()
      
      # Compute elapsed time
      elapsed <- t_end - t_start

      cat("time elapsed \n")
      print(elapsed)

  return(results)
}


#########################################################################################
# Function: eta2_from_partition
#
# Purpose:
#   Compute Eta² (effect size) for a quantitative variable given a clustering partition.
#   Eta² corresponds to the proportion of variance explained by the grouping structure.
#
# Args:
#   - values   : numeric vector (e.g., log_price) of length equal to the number of vertices.
#   - clusters : vector of cluster memberships (integer or factor), same length as `values`.
#
# Returns:
#   A single numeric value corresponding to Eta², computed as:
#       SS_between / SS_total
#   where:
#       SS_total   = total sum of squares
#       SS_between = between-cluster sum of squares (weighted by cluster sizes)
#########################################################################################

eta2_from_partition <- function(values, clusters) {
  # Build dataframe
  df <- data.frame(
    value   = values,
    cluster = as.factor(clusters)
  )
  
  # Remove missing values
  df <- df[!is.na(df$value), ]
  
  # Global mean
  mu <- mean(df$value)
  
  # Total sum of squares
  ss_total <- sum((df$value - mu)^2)
  
  # Between-cluster sum of squares
  stats <- df  %>%
    dplyr::group_by(cluster)  %>%
    dplyr::summarise(
      n_k  = dplyr::n(),
      mu_k = mean(value)
    )
  
  ss_between <- sum(stats$n_k * (stats$mu_k - mu)^2)
  
  # Eta² = SS_between / SS_total
  eta2 <- ss_between / ss_total
  return(eta2)
}

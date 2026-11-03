#########################################################################################
# Functions used for spatial processings.
# 
# 2026/10 Raphaël Biawa Diegamounoua
#
# source("src/processing/spatial_processing/spatial_processing.R")
#########################################################################################




###########################################################################################
# Function to filter spatial outliers and isolated sub clusters
#
# Args:
#   sf_pts   : an sf object containing spatial point listings
#   eps      : numeric value for DBSCAN epsilon radius in meters (default: 250)
#   min_pts  : integer value for DBSCAN minimum points per cluster (default: 4)
#   buffer_dist : numeric distance in meters to bridge spatial clusters (default: 1000)
#   crs_proj : EPSG code for spatial metric transformation (default: 2154 for Lambert-93)
#
# Returns:
#   filtered sf object retaining only points from the largest connected spatial component
###########################################################################################

filter_isolated_spatial_clusters <- function(sf_pts,
                                             eps = 250,
                                             min_pts = 4,
                                             buffer_dist = 1000,
                                             crs_proj = 2154) {

  # Step 1: Spatial projection and DBSCAN clustering
  pts_proj <- st_transform(sf_pts, crs = crs_proj)
  coords_matrix <- st_coordinates(pts_proj)
  dbscan_res <- dbscan::dbscan(coords_matrix, eps = eps, minPts = min_pts)
  
  # Step 2: Remove spatial noise (cluster 0)
  non_noise_index <- dbscan_res$cluster != 0
  
  if (!any(non_noise_index)) {
    warning("All points were classified as noise by DBSCAN.")
    return(sf_pts[0, ])
  }
  
  pts_clean_proj <- pts_proj[non_noise_index, ]
  pts_clean_proj$cluster_id <- dbscan_res$cluster[non_noise_index]
  
  # Step 3: Compute spatial buffers and adjacency graph
  clusters_sf <- pts_clean_proj %>%
    group_by(cluster_id) %>%
    summarise(geometry = st_union(geometry), .groups = "drop")
  
  clusters_buffered <- st_buffer(clusters_sf, dist = buffer_dist)
  adj_matrix <- st_intersects(clusters_buffered, sparse = FALSE)
  
  cluster_graph <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)
  
  # Step 4: Extract the largest connected component
  main_component <- largest_component(graph = cluster_graph)
  valid_cluster_ids <- V(main_component)
  
  # Step 5: Filter original points
  filtered_pts <- sf_pts[dbscan_res$cluster %in% valid_cluster_ids, ]
  
  return(filtered_pts)
}

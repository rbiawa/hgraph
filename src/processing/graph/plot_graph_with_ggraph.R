#########################################################################################
# A script to perform submerket transitions
# 
# 2026/06 Raphaël Biawa Diegamounoua
#
# source("src/processing/graph/plot_graph_with_ggraph.R")
#########################################################################################






#=========================================================================
# Generates a spatialized directed submarket (community) network visualization
#
# Args :
#       - meta_graph : a directed igraph object representing the submarket network
#       - node_size_range : numeric vector of length 2 defining node size scaling
#       - edge_width_range : numeric vector of length 2 defining edge width scaling
#       - palette_name : RColorBrewer palette name for the price gradient
#       - legend_title : title of the price scale legend
# Returns :
#       A ggraph/ggplot object visualizing the directed network with self-loops
#=========================================================================

plot_community_price_graph <- function(meta_graph,
                                       node_size_range = c(6, 20),
                                       edge_width_range = c(0.2, 2.5),
                                       palette_name = "Reds",
                                       legend_title = "Median price €/sqm") {
  
  # Convert igraph to tbl_graph
  tg <- as_tbl_graph(meta_graph)
  
  # Build continuous color palette
  pal <- brewer.pal(9, palette_name)
  pal_fun <- colorRampPalette(pal)
  
  # Prepare graph variables
  tg <- tg %>%
    mutate(
      price_cont = median_sqm_price,
      size_plot = scales::rescale(as.numeric(size_comm), to = node_size_range)
    )
  
  # Extract manual layout matrix from node x and y attributes
  nodes_df <- as_tibble(tg, active = "nodes")
  layout_mat <- cbind(nodes_df$x, nodes_df$y)
  
  # Build directed ggraph plot
  p <- ggraph(
    tg, 
    layout = "manual",
    x = layout_mat[, 1], 
    y = layout_mat[, 2]
  ) +
    # Render self-loops (submarket retention)
    geom_edge_loop(
      aes(width = weight),
      alpha = 0.4,
      colour = "grey40"
    ) +
    # Render directed edges with fan curvature and arrows
    geom_edge_fan(
      aes(width = weight),
      alpha = 0.5,
      colour = "grey40",
      lineend = "round",
      arrow = arrow(length = unit(2.5, 'mm'), type = 'closed'),
      end_cap = circle(6, 'mm')  # Stops the arrow before hitting node center
    ) +
    # Nodes
    geom_node_point(
      aes(size = size_plot, fill = price_cont),
      shape = 21,
      colour = "black",
      stroke = 0.3
    ) +
    # Labels
    geom_node_text(
      aes(label = name),
      repel = TRUE,
      size = 3,
      colour = "black"
    ) +
    # Scales and themes
    scale_fill_gradientn(
      colours = pal_fun(100),
      name = legend_title
    ) +
    scale_size_identity() +
    scale_edge_width(range = edge_width_range) +
    theme_void() +
    theme(
      legend.position = "left",
      plot.margin = margin(10, 10, 10, 10)
    )
  
  return(p)
}



#=========================================================================
# Generates a spatialized submarket (community) network visualization
#
# Args :
#       - meta_graph : an igraph object representing the meta-graph of submarkets
#       - node_size_range : numeric vector of length 2 defining node size scaling
#       - edge_width_range : numeric vector of length 2 defining edge width scaling
#       - palette_name : RColorBrewer palette name for the price gradient
#       - legend_title : title of the price scale legend
# Returns :
#       A ggraph/ggplot object visualizing the network layout
#=========================================================================


plot_community_price_undir_graph <- function(meta_graph,
                                       node_size_range = c(6, 20),
                                       edge_width_range = c(0.2, 3),
                                       palette_name = "Reds",
                                       legend_title = "Median price €/sqm") {
  
  # Convert igraph to tbl_graph
  tg <- as_tbl_graph(meta_graph)
  
  # Build continuous color palette
  pal <- brewer.pal(9, palette_name)
  pal_fun <- colorRampPalette(pal)
  
  # Prepare graph variables
  tg <- tg %>%
    mutate(
      price_cont = median_sqm_price,
      size_plot = scales::rescale(as.numeric(size_comm), to = node_size_range)
    )
  
  # Extract manual layout matrix from node x and y attributes
  nodes_df <- as_tibble(tg, active = "nodes")
  layout_mat <- cbind(nodes_df$x, nodes_df$y)
  
  # Build ggraph plot
  p <- ggraph(
    tg, 
    layout = "manual",
    x = layout_mat[, 1], 
    y = layout_mat[, 2]
  ) +
    # Edges
    geom_edge_link(
      aes(width = weight),
      alpha = 0.5,
      colour = "grey40",
      lineend = "round"
    ) +
    # Nodes
    geom_node_point(
      aes(size = size_plot, fill = price_cont),
      shape = 21,
      colour = "black",
      stroke = 0.3
    ) +
    # Labels
    geom_node_text(
      aes(label = name),
      repel = TRUE,
      size = 3,
      colour = "black"
    ) +
    # Scales and themes
    scale_fill_gradientn(
      colours = pal_fun(100),
      name = legend_title
    ) +
    scale_size_identity() +
    scale_edge_width(range = edge_width_range) +
    theme_void() +
    theme(
      legend.position = "left",
      plot.margin = margin(10, 10, 10, 10)
    )
  
  return(p)
}
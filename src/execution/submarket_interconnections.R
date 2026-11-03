#########################################################################################
# A script for submarket interconnetion's analysis.
# 
# 2026/10 Raphaël Biawa Diegamounoua
#
# source("src/execution/submarket_interconnections.R")
#########################################################################################

#source("src/execution/graph_clustering_execution.R")

retrieve_package("devtools")
if(!require("ggtrout")) devtools::install_github("eachambers/ggtrout") ; library(ggtrout)

retrieve_package("PNWColors")
retrieve_package("wesanderson")
retrieve_package("Polychrome")
retrieve_package("ggraph")
retrieve_package("tidygraph")

{

if (! exists("edges")) {
  edges <- as_data_frame(GRAPH, what = "edges")

  #if (interactive()) head(edges) %>% View()
}

submarket_connections <- edges %>%
  mutate(
    c_min = pmin(ci, cj),
    c_max = pmax(ci, cj)
  ) %>%
  group_by(c_min, c_max) %>%
  summarise(weight = sum(weight), .groups = "drop")


if (interactive()) submarket_connections %>% View()

# Porosity

outer_link <- submarket_connections %>%
  dplyr::filter(c_min != c_max) %>%
  group_by(c_min) %>%
  dplyr::summarise(outer_links = sum(weight))

submarket_connections_dist <- submarket_connections %>%
  dplyr::filter(c_min != c_max)

difference_c_max_c_min <- setdiff(unique(submarket_connections_dist$c_max),
  unique(submarket_connections_dist$c_min)
)

outer_link_comp <- submarket_connections_dist %>%
  filter(c_max %in% difference_c_max_c_min) %>%
  group_by(c_max) %>%
  dplyr::summarise(outer_links = sum(weight)) %>%
  rename(c_min = c_max)


outer_link <- outer_link %>%
  rbind(outer_link_comp)

if (interactive()) outer_link %>% View()

inner_link <- submarket_connections %>%
  dplyr::filter(c_min == c_max) %>%
  group_by(c_min) %>%
  dplyr::summarise(inner_links = sum(weight))

if (interactive()) inner_link %>% View()

porosity_df <- outer_link %>%
  dplyr::left_join(inner_link,
                   by = "c_min") %>%
  mutate(porosity = outer_links / inner_links,
    porosity_pct = outer_links * 100 / (outer_links + inner_links)
  )


if (interactive()) porosity_df %>% View()

# Submarket graph

submarket_network <- graph_from_data_frame(
  submarket_connections,
  directed = FALSE,
  vertices = NULL
)

}


V(submarket_network)$size_comm <- comm_size[V(submarket_network)$name]

V(submarket_network)$inner_links <- porosity_df$inner_links[
  match(V(submarket_network)$name,
    porosity_df$c_min
  )
]

V(submarket_network)$outer_links <- porosity_df$outer_links[
  match(V(submarket_network)$name,
    porosity_df$c_min
  )
]

V(submarket_network)$porosity <- porosity_df$porosity[
  match(V(submarket_network)$name,
    porosity_df$c_min
  )
]

V(submarket_network)$porosity_pct <- porosity_df$porosity_pct[
  match(V(submarket_network)$name,
    porosity_df$c_min
  )
]


K <- out[[k_res]]$n_clusters
submarket_colors_leiden <- Polychrome::createPalette(K, seedcolors = c("#ff0000", "#00ff00", "#0000ff"))
submarket_colors_leiden <- submarket_colors_leiden[as.integer(V(submarket_network)$name)]




V(GRAPH)$optim_submarket <- out[[k_res]]$membership


#==========================
# Submarket location
#==========================

coords <- data.frame(
  id = V(GRAPH)$name,
  x  = V(GRAPH)$x,
  y  = V(GRAPH)$y,
  comm = membership
)

if (interactive()) head(coords) %>% View()

# Retrieve de centroid of the submarket

comm_centroids <- coords %>%
  group_by(comm) %>%
  summarise(
    cx = mean(x, na.rm = TRUE),
    cy = mean(y, na.rm = TRUE),
    n  = n()
  )

V(submarket_network)$x <- comm_centroids$cx[ match(V(submarket_network)$name, comm_centroids$comm) ]
V(submarket_network)$y <- comm_centroids$cy[ match(V(submarket_network)$name, comm_centroids$comm) ]


comm_size

if (interactive()) {
  plot(
    submarket_network,
    vertex.color = submarket_colors_leiden,
    vertex.size = 2 * log(V(submarket_network)$size_comm),
    edge.width  = log1p(E(submarket_network)$weight)
  )
}

submarket_network <- igraph::simplify(submarket_network, remove.loops = TRUE)



# Create POINT geometries
#geom_points <- sf::st_as_sf(comm_centroids, coords = c("cx", "cy"), crs = 4326)$geometry

# Assign them to the graph
V(submarket_network)$geometry <- geom_points

submarket_network <- node_spatial_distance(graph = submarket_network,
                                    name = "distance")


porosity_median_sq_price <- assortnet_assortativity(
  graph = submarket_network
  , node_attr = "porosity"
  , weighted = TRUE
  , weight_attr = "weight"
  , n_perm = 100
)

porosity_distance <- assortnet_assortativity(
  graph = submarket_network
  , node_attr = "porosity"
  , weighted = TRUE
  , weight_attr = "distance"
  , n_perm = 100
)

variables <- c("median_price", "median_sqm_price", "median_area", "room_count",
  "main_item_type", "pct_item_type", "main_city", "main_maille_habitat",
  "main_dep", "pct_city", "pct_maille_habitat", "pct_dep"
)

for (var_name in variables) {
  submarket_network <- set_vertex_attr(submarket_network, name = var_name, value = comm_measures[[var_name]][
    match(V(submarket_network)$name, comm_measures$comm)
  ])
}

summary(V(submarket_network)$porosity)

summary(V(submarket_network)$porosity_pct)

cor.test(V(submarket_network)$porosity, V(submarket_network)$porosity_pct)

V(submarket_network)$median_sqm_price_log  <- log(V(submarket_network)$median_sqm_price)

cor.test(V(submarket_network)$porosity, V(submarket_network)$median_sqm_price)

cor.test(V(submarket_network)$porosity, V(submarket_network)$median_sqm_price_log)

cor.test(strength(submarket_network), V(submarket_network)$porosity)

cor.test(strength(submarket_network), V(submarket_network)$size_comm)

cor.test(V(submarket_network)$porosity, V(submarket_network)$size_comm)

cor.test(strength(submarket_network), V(submarket_network)$median_sqm_price)



# Filter top links

submarket_network_filtered <- keep_top_k_links(submarket_network, k = 3)

#==================
# A ggraph plot
#==================



if (require("ggraph")) {

  # Coovert into tbl_graph
  tg <- as_tbl_graph(submarket_network_filtered)

  # Palette Brewer rouge (déjà choisie)
  #pal <- brewer.pal(9, "Reds")

  pal = pnw_palette("Moth", 9)

  pal = rev(pal)
  pal_fun <- colorRampPalette(pal)

  # Add colors for porosity
  tg <- tg %>%
    mutate(
      porosity_cont = porosity_pct,   # For legend
      size_plot = scales::rescale(as.numeric(size_comm), to = c(6, 20))
    )


  # Layout = centroids
  layout_mat <- cbind(tg$x, tg$y)

  # Plot ggraph
  porosity_gradient_graph <- ggraph(
    tg, layout = "manual",
    x = layout_mat[,1], y = layout_mat[,2]
  ) +

    # Edges
    geom_edge_link(
      aes(width = log(weight)),
      alpha = 0.5,
      colour = "grey40",
      lineend = "round"
    ) +

    # Nodes
    geom_node_point(
      aes(size = size_plot, fill = porosity_cont),
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

    # Color palette
    scale_fill_gradientn(
      colours = pal_fun(100),
      name = "Porosity (%)",
      guide = guide_colourbar(
        title.position = "top",
        barwidth = unit(11, "cm"),
        barheight = unit(0.8, "cm")
      )
    ) +

    scale_size_identity() +

    # edge setting
    scale_edge_width(
      range = c(0.5, 3),
      name = "Log weight",
      guide = guide_legend(
        title.position = "top",
        nrow = 1
      )
    ) +

    theme_void() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.box.align = "center",
      plot.margin = margin(0, 00, 0, 00),
      legend.text = element_text(size = 25, angle = 45, hjust = 1), 
      legend.title = element_text(size = 35, face = "bold"),
      legend.spacing.x = unit(4, "cm")     # <-- Add horizontal space between the two captions
    )
}


if (interactive() & exists("porosity_gradient_graph")) porosity_gradient_graph


if (exists("porosity_gradient_graph")) {

  file_path_pdf <- file.path(
    GRAPH_PDF_SAVING_DIRECTORY, 
    paste0(LOCALITY_NAME_OUT, "_", ANALYSE_SCALE_OUT, "_porosity_gradient_graph.pdf")
  )

  file_path_svg <- file.path(
    GRAPH_SVG_SAVING_DIRECTORY, 
    paste0(LOCALITY_NAME_OUT, "_", ANALYSE_SCALE_OUT, "_porosity_gradient_graph.svg")
  )  


  ggsave(
    filename = file_path_pdf,
    plot = porosity_gradient_graph,
    width = 25,
    height = 25,
    units = "in",
    device = cairo_pdf,
    limitsize = FALSE
  )


  ggsave(
  filename = file_path_svg,
  plot = porosity_gradient_graph,
  width = 25,
  height = 25,
  units = "in",
  device = "svg",
  limitsize = FALSE
  )
}


#==========================
# Local spatial correlation
#==========================


# 1. Extract the weighted adjacency matrix from the network using the 'weight' attribute
adj_matrix <- as_adjacency_matrix(submarket_network, attr = "weight", sparse = FALSE)

#adj_matrix <- as_adjacency_matrix(submarket_network, attr = "distance", sparse = FALSE)

# 2. Convert the adjacency matrix into a weights list object (row-standardized style "W")
weights_list <- mat2listw(adj_matrix, style = "W")

# 3. Extract the target variable (e.g., porosity) from the vertices
porosity_values <- V(submarket_network)$porosity

# 4. Compute Global Moran's I for porosity using the network weights
global_moran_res <- moran.test(porosity_values, weights_list)
  
print(global_moran_res)

# 5. Compute Local Moran's I (LISA) using the network weights
lisa_results <- localmoran(porosity_values, weights_list)

summary(lisa_results[,1])

# 6. Add LISA metrics (Local Statistic 'Ii' and p-value) back to the graph vertices
V(submarket_network)$lisa_i <- lisa_results[, "Ii"]
V(submarket_network)$lisa_p <- lisa_results[, "Pr(z != E(Ii))"]

# 7. Classify nodes into Moran quadrants (High-High, Low-Low, High-Low, Low-High)
scaled_porosity <- scale(porosity_values)[, 1]
spatial_lag_values <- lag.listw(weights_list, scaled_porosity)

V(submarket_network)$quadrant <- case_when(
  scaled_porosity >= 0 & spatial_lag_values >= 0 ~ "High-High",
  scaled_porosity < 0 & spatial_lag_values < 0   ~ "Low-Low",
  scaled_porosity >= 0 & spatial_lag_values < 0   ~ "High-Low",
  scaled_porosity < 0 & spatial_lag_values >= 0   ~ "Low-High"
)

# Optional: Flag non-significant nodes (e.g., p-value > 0.05) if you want to isolate true clusters
V(submarket_network)$significant_cluster <- ifelse(V(submarket_network)$lisa_p < 0.05, 
                                                   V(submarket_network)$quadrant, 
                                                   "Not Significant")


# Filter top links

submarket_network_filtered <- keep_top_k_links(submarket_network, k = 3)

c("#FF0000", "#80FF00", "#00FFFF", "#8000FF")

if (interactive()) {
  
  # 1. Convert the categorical variable into a factor to ensure stable color mapping
  quadrant_factor <- as.factor(V(submarket_network_filtered)$quadrant)
  
  # 2. Define a color palette matching the number of categories
  # (Using R's built-in palette or RColorBrewer, e.g., Set1 or Set2)
  num_categories <- length(levels(quadrant_factor))
  color_palette <- rainbow(num_categories) # Ou une palette qualitative de RColorBrewer
  
  # 3. Assign colors to vertices based on their factor level
  vertex_colors <- color_palette[quadrant_factor]
  
  plot(
    submarket_network_filtered,
    vertex.size = log(V(submarket_network_filtered)$size_comm),
    vertex.color = vertex_colors, # Utilisation du vecteur de couleurs mappé
    edge.width = log1p(E(submarket_network_filtered)$weight),
    edge.curved = 0
  )
}



if (require("ggraph")) {

  # Coovert into tbl_graph
  tg <- as_tbl_graph(submarket_network_filtered)


  # Palette Brewer rouge (déjà choisie)
  pal <- colorRampPalette(c("#1f78b4", "#ffff33", "#e31a1c"))(9)

  #pal = pnw_palette("Bay", 9)

  #pal = rev(pal)
  pal_fun <- colorRampPalette(pal)

  # Add colors for porosity LISA
  tg <- tg %>%
    mutate(
      lisa_i = lisa_i,   # For legend
      size_plot = scales::rescale(as.numeric(size_comm), to = c(6, 20))
    )


  # Layout = centroids
  layout_mat <- cbind(tg$x, tg$y)

  # Plot ggraph
  porosity_lisa_graph <- ggraph(
    tg, layout = "manual",
    x = layout_mat[,1], y = layout_mat[,2]
  ) +

    # Edges
    geom_edge_link(
      aes(width = log(weight)),
      alpha = 0.5,
      colour = "grey40",
      lineend = "round"
    ) +

    # Nodes
    geom_node_point(
      aes(size = size_plot, fill = lisa_i),
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

    # Color palette
    scale_fill_gradientn(
      colours = pal_fun(100),
      name = "Porosity LISA",
      guide = guide_colourbar(
        title.position = "top",
        barwidth = unit(11, "cm"),
        barheight = unit(0.8, "cm")
      )
    ) +

    scale_size_identity() +

    # edge setting
    scale_edge_width(
      range = c(0.5, 3),
      name = "Log weight",
      guide = guide_legend(
        title.position = "top",
        nrow = 1
      )
    ) +

    theme_void() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.box.align = "center",
      plot.margin = margin(0, 00, 0, 00),
      legend.text = element_text(size = 25, angle = 45, hjust = 1), 
      legend.title = element_text(size = 35, face = "bold"),
      legend.spacing.x = unit(4, "cm")     # <-- Add horizontal space between the two captions
    )
}


if (interactive() & exists("porosity_lisa_graph")) porosity_lisa_graph


if (exists("porosity_lisa_graph")) {

  file_path_pdf <- file.path(
    GRAPH_PDF_SAVING_DIRECTORY, 
    paste0(LOCALITY_NAME_OUT, "_", ANALYSE_SCALE_OUT, "_porosity_lisa_graph.pdf")
  )

  file_path_svg <- file.path(
    GRAPH_SVG_SAVING_DIRECTORY, 
    paste0(LOCALITY_NAME_OUT, "_", ANALYSE_SCALE_OUT, "_porosity_lisa_graph.svg")
  )  


  ggsave(
    filename = file_path_pdf,
    plot = porosity_lisa_graph,
    width = 25,
    height = 25,
    units = "in",
    device = cairo_pdf,
    limitsize = FALSE
  )


  ggsave(
  filename = file_path_svg,
  plot = porosity_lisa_graph,
  width = 25,
  height = 25,
  units = "in",
  device = "svg",
  limitsize = FALSE
  )
}


#===================================
# Price/m² gradient graph
#===================================



if (require("ggraph")) {

  # Color Brewer red
  pal <- brewer.pal(9, "Reds")

  #pal = rev(pal)
  pal_fun <- colorRampPalette(pal)

  # Add colors for porosity LISA
  tg <- tg %>%
    mutate(
      price_cont = median_sqm_price,   # For legend
      size_plot = scales::rescale(as.numeric(size_comm), to = c(6, 20))
    )


  # Layout = centroids
  layout_mat <- cbind(tg$x, tg$y)

  # Plot ggraph
  price_gradient_graph <- ggraph(
    tg, layout = "manual",
    x = layout_mat[,1], y = layout_mat[,2]
  ) +

    # Edges
    geom_edge_link(
      aes(width = log(weight)),
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

    # Color palette
    scale_fill_gradientn(
      colours = pal_fun(100),
      name = "Median price euros/m²",
      guide = guide_colourbar(
        title.position = "top",
        barwidth = unit(11, "cm"),
        barheight = unit(0.8, "cm")
      )
    ) +

    scale_size_identity() +

    # edge setting
    scale_edge_width(
      range = c(0.5, 3),
      name = "Log weight",
      guide = guide_legend(
        title.position = "top",
        nrow = 1
      )
    ) +

    theme_void() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.box.align = "center",
      plot.margin = margin(0, 00, 0, 00),
      legend.text = element_text(size = 25, angle = 45, hjust = 1), 
      legend.title = element_text(size = 35, face = "bold"),
      legend.spacing.x = unit(4, "cm")     # <-- Add horizontal space between the two captions
    )
}


if (interactive() && exists("price_gradient_graph")) price_gradient_graph


if (exists("price_gradient_graph")) {

  price_grad_file_path_pdf <- file.path(
    GRAPH_PDF_SAVING_DIRECTORY,
    paste0(LOCALITY_NAME_OUT, "_", ANALYSE_SCALE_OUT, "_sqm_price_gradient_graph.pdf")
  )

  price_grad_file_path_svg <- file.path(
    GRAPH_SVG_SAVING_DIRECTORY,
    paste0(LOCALITY_NAME_OUT, "_", ANALYSE_SCALE_OUT, "_sqm_price_gradient_graph.svg")
  )


  ggsave(
    filename = price_grad_file_path_pdf,
    plot = price_gradient_graph,
    width = 25,
    height = 25,
    units = "in",
    device = cairo_pdf,
    limitsize = FALSE
  )


  ggsave(
  filename = price_grad_file_path_svg,
  plot = price_gradient_graph,
  width = 25,
  height = 25,
  units = "in",
  device = "svg",
  limitsize = FALSE
  )
}





#===================================
# Latent demand report area
#===================================


# 1. Compute spatial lag of median log price per square meter (weighted neighbor average)
sqm_price_lag <- lag.listw(weights_list, V(submarket_network)$median_sqm_price)

# 2. Calculate relative price difference against the immediate spatial neighborhood
# (Negative = cheaper than neighbors; Positive = more expensive than neighbors)
V(submarket_network)$relative_sqm_price_diff <- V(submarket_network)$median_sqm_price - sqm_price_lag

# 3. Test Pearson correlation between porosity and local relative price gradient
cor_res_sqm <- cor.test(
  V(submarket_network)$porosity,
  V(submarket_network)$relative_sqm_price_diff
)

# Print correlation results
print(cor_res_sqm)

#########################################################################################
# A script to perform a graph clustering
# 
# 2024/11 Raphaël Biawa Diegamounoua
#
# source("src/execution/graph_clustering_execution.R")
#########################################################################################



######################################
# Loading packages

source("src/packages_loading.R")


#Retrieve functions

source("src/function_retrieving.R")


## GLOBAL PROCESSING PARAMETERS
source("src/global_parameters.R")


retrieve_package("PNWColors")
retrieve_package("wesanderson")
retrieve_package("Polychrome")
retrieve_package("ggraph")
retrieve_package("tidygraph")

#=====================================
# Add inner category edge variable
#=====================================

add_edge_indicator <- function(graph, node_attr, new_attr_name = "same_group") {

  edges <- igraph::ends(graph, igraph::E(graph), names = FALSE)
  node_values <- igraph::vertex_attr(graph, node_attr)

  indicator <- as.integer(node_values[edges[,1]] == node_values[edges[,2]])

  graph <- igraph::set_edge_attr(graph, new_attr_name, value = indicator)

 # return(graph)
}


if ((exists("graph", envir = .GlobalEnv) & ! is_igraph(graph))) {

  #source("src/execution/graph_extraction_execution.R")

  load(paste0("out/Rdata/graph/",
              LOCALITY_NAME_OUT, "_graph_attr.RData")
  )
}

summary(E(graph)$weight) %>% print()

is_connected(graph)

min_weight <- 2

graph_robust <- delete_edges(graph, E(graph)[weight < min_weight])

graph_robust <- select_graph(graph = graph_robust, largest_component = TRUE)

#==============================
# Graph retrieving
#==============================

GRAPH_NAME                               <- "graph"

if (exists("graph_robust")) GRAPH_NAME   <- "graph_robust"

#if (exists("GRAPH")) GRAPH_NAME   <- "GRAPH"


GRAPH   <- get(GRAPH_NAME)

freq(V(GRAPH)$dep_Name) %>% print()

GRAPH_NAME

is_connected(GRAPH)

#================================
# Add maille habitat on vertices
#================================

sum(is.na(match(V(GRAPH)$INSEE_COM, city_maille_habitat$INSEE_COM)))
any(duplicated(city_maille_habitat$INSEE_COM))




V(GRAPH)$maille_habitat <- city_maille_habitat$zone_inter_fact40_40000[
  match(V(GRAPH)$INSEE_COM, city_maille_habitat$INSEE_COM)
]



vars_to_add <- c("indic.menag.14", "indic.rs.14", "indic.vac.14",
                 "indic.prix_rev.12", "indic.jeun.15", "indic.transac.12",
                 "indic.soc.15", "indic.suroc.15", "indic.duroc.15")



for (var_name in vars_to_add) {
  GRAPH <- set_vertex_attr(GRAPH, name = var_name, value = city_maille_habitat[[var_name]][
    match(V(GRAPH)$INSEE_COM, city_maille_habitat$INSEE_COM)
  ])
}

V(GRAPH)$indic.menag.14[1:10]

cor.test(V(GRAPH)$price, V(GRAPH)$indic.prix_rev.12)

#=========================================
# Save graph into .graphml
#=========================================

dir.create("out/graphml", recursive = TRUE, showWarnings = FALSE)

if (REGIONS_CODE_VAL == "11" && FALSE) {
  
  nodes_sample <- sample(V(GRAPH)$name, floor(.3 * vcount(GRAPH)))

  graph_sample <- igraph::induced_subgraph(GRAPH, vids = nodes_sample)

  write_graph(graph_sample, file = paste0("out/graphml/",
                                   LOCALITY_NAME_OUT, "_graph.graphml"),
              format = "graphml")
  rm(graph_sample, nodes_sample)

} else {
  write_graph(GRAPH, file = paste0("out/graphml/",
                                   LOCALITY_NAME_OUT, "_graph.graphml"),
              format = "graphml")
}



#=========================================
#
# Homophily analysis
#
#=========================================

vcount(GRAPH)

ecount(GRAPH)

edge_density(GRAPH)

mean(degree(GRAPH, mode = "total"))

assortativity_degree(GRAPH)

clustering_coef <- mean(transitivity(GRAPH, type = "weighted"), na.rm = TRUE)

#clustering_coef <- compute_weighted_clustering_significance(GRAPH, n_permutations = 500)

GRAPH <- add_edge_indicator(
  GRAPH,
  node_attr = "maille_habitat",
  new_attr_name = "same_maille_habitat"
)


GRAPH <- add_edge_indicator(
  GRAPH,
  node_attr = "sl_insee_city_id",
  new_attr_name = "same_city"
)


GRAPH <- add_edge_indicator(
  GRAPH,
  node_attr = "INSEE_DEP",
  new_attr_name = "same_department"
)


GRAPH <- add_edge_indicator(
  GRAPH,
  node_attr = "INSEE_REG",
  new_attr_name = "same_region"
)


V(GRAPH)$sqm_price <- V(GRAPH)$price / V(GRAPH)$area

V(GRAPH)$log_sqm_price <- log(V(GRAPH)$sqm_price)


sum(E(GRAPH)$same_city * E(GRAPH)$weight) / sum(E(GRAPH)$weight)

sum(E(GRAPH)$same_maille_habitat * E(GRAPH)$weight) / sum(E(GRAPH)$weight)

sum(E(GRAPH)$same_department * E(GRAPH)$weight) / sum(E(GRAPH)$weight)

sum(E(GRAPH)$same_region * E(GRAPH)$weight) / sum(E(GRAPH)$weight)



if (TRUE) {
  print("Quik assortativity")

  room_count_assortativity <- assortativity_test(GRAPH, "room_count",
                     n_perm = 100, directed = FALSE) %>% print()

  price_assortativity <- assortativity_test(GRAPH, "log_price",
                     n_perm = 100, directed = FALSE) %>% print()
  sqm_price_assortativity <- assortativity_test(GRAPH, "log_sqm_price",
                     n_perm = 100, directed = FALSE) %>% print()
  area_assortativity <- assortativity_test(GRAPH,"log_area",
                     n_perm = 100, directed = FALSE)  %>% print()
  item_type_assortativity <- assortativity_test(GRAPH, "item_type",
                     n_perm = 100, directed = FALSE) %>% print()
  city_assortativity <- assortativity_test(GRAPH, "sl_insee_city_id",
                     n_perm = 100, directed = FALSE) %>% print()
  maille_habitat_assortativity <- assortativity_test(GRAPH, "maille_habitat",
                     n_perm = 100, directed = FALSE) %>% print()
  dep_assortativity <- assortativity_test(GRAPH, "INSEE_DEP",
                     n_perm = 100, directed = FALSE) %>% print()
  
  price_rev_assortativity <- assortativity_test(GRAPH, "indic.prix_rev.12",
                     n_perm = 100, directed = FALSE) %>% print()
}


assortativity_list <- list(
  price_assortativity          = price_assortativity,
  area_assortativity           = area_assortativity,
  item_type_assortativity      = item_type_assortativity,
  room_count_assortativity     = room_count_assortativity,
  city_assortativity           = city_assortativity,
  maille_habitat_assortativity = maille_habitat_assortativity,
  dep_assortativity            = dep_assortativity
)

assortitvity_list[[1]]

if (FALSE) {
  print("Price assortativity")

  price_assortativity <- assortnet_assortativity(
    graph = GRAPH
    , node_attr = "price"
    , weighted = TRUE
    , weight_attr = "weight"
    , n_perm = 100
  )

  price_assortativity %>% print()

  log_price_assortativity <- assortnet_assortativity(
    graph = GRAPH
    , node_attr = "log_price"
    , weighted = TRUE
    , weight_attr = "weight"
    , n_perm = 100
  )


  log_price_assortativity %>% print()


  print("Area assortativity")

  area_assortativity <- assortnet_assortativity(
    graph = GRAPH
    , node_attr = "area"
    , weighted = TRUE
    , weight_attr = "weight"
    , n_perm = 100
  )

  area_assortativity %>% print()



  log_area_assortativity <- assortnet_assortativity(
    graph = GRAPH
    , node_attr = "log_area"
    , weighted = TRUE
    , weight_attr = "weight"
    , n_perm = 100
  )

  log_area_assortativity %>% print()



  print("item_type assortativity")

  item_type_assortativity <- assortnet_assortativity(
    graph = GRAPH
    , node_attr = "item_type"
    , weighted = TRUE
    , weight_attr = "weight"
    , n_perm = 100
  )

  item_type_assortativity %>% print()
}




#=========================================
#
# Louvain and Leiden clustering
#
#=========================================



#==============================
# Louvain clustering
#==============================

louvain_clustering <- cluster_louvain(graph = GRAPH)

membership(louvain_clustering) %>% freq()

louvain_clustering$modularity

modularity(louvain_clustering)

membership_louvain <- membership(louvain_clustering)

sizes(louvain_clustering)

length(unique(louvain_clustering$membership))


#==============================
# Leiden clustering
#==============================

leiden_clustering <- cluster_leiden(graph = GRAPH,
  objective_function = "modularity",
  beta               = 0.01,
  resolution         = 1,
  n_iterations       = 30
)

#names(leiden_clustering)

leiden_clustering$nb_clusters

leiden_clustering$quality

sizes(leiden_clustering)

leiden_clustering$algorithm

leiden_clustering$vcount

membership_leiden <- membership(leiden_clustering)



membership_leiden %>% freq()




NMI(membership(louvain_clustering),
membership(leiden_clustering)
)


#infomap_clustering <- cluster_infomap(graph = GRAPH)

#infomap_clustering

#membership_infomap <- membership(infomap_clustering)

length(COLORS)

retrieve_package("viridis")

colors_leiden <- viridis(leiden_clustering$nb_clusters, option = "turbo")

colors_louvain <- viridis(max(louvain_clustering$membership), option = "turbo")


node_colors_louvain <- colors_louvain[as.integer(louvain_clustering$membership)]

node_colors_leiden  <- colors_leiden[as.integer(leiden_clustering$membership)]


length(unique(node_colors_leiden))

GRAPH_PNG_SAVING_DIRECTORY <- "out/png/graphs"

dir.create(GRAPH_PNG_SAVING_DIRECTORY, recursive = TRUE, showWarnings = FALSE)

if (FALSE) {
  png(file = paste(GRAPH_PNG_SAVING_DIRECTORY,
                 paste(LOCALITY_NAME_OUT,
                       ANALYSE_SCALE_OUT,
                       "leiden_community_plot.png",
                       sep = "_"),
                 sep = "/"),
  width = 3000, height = 2000
  )

  #plot(leiden_clustering, graph)
  {
  #par(mar=c(0,0,0,0))
    plot(GRAPH, 
      vertex.label=NA,
      vertex.size=.5,
      edge.arrow.size=.001,
      edge.size = .0001,     # E(GRAPH)$weigth,
      vertex.color = node_colors_leiden,
      main = "Leiden")
  }

  dev.off()




  png(file = paste(GRAPH_PNG_SAVING_DIRECTORY,
                  paste(LOCALITY_NAME_OUT,
                        ANALYSE_SCALE_OUT,
                        "louvain_community_plot.png",
                        sep = "_"),
                  sep = "/"),
    width = 3000, height = 2000
  )

  #plot(louvain_clustering, graph)
  {
  #par(mar=c(0,0,0,0))
    plot(GRAPH, 
        vertex.label=NA,
        vertex.size=.5,
        edge.arrow.size=.001,
        edge.size = .0001,     # E(GRAPH)$weigth,
        vertex.color = node_colors_louvain,
        main = "Louvain")
  }

  dev.off()
}



#=========================================
#
# Cluster description
#
#=========================================

# Import submarket variable
submarket          <- paste0("submarket_", membership(leiden_clustering))

V(GRAPH)$submarket <- submarket




NMI(V(GRAPH)$submarket,
  V(GRAPH)$dep_Name
)

NMI(V(GRAPH)$submarket,
  V(GRAPH)$maille_habitat
)

NMI(V(GRAPH)$submarket,
  V(GRAPH)$sl_insee_city_id
)


submarket_data <- igraph::vertex.attributes(GRAPH) %>% as.data.frame()

names(submarket_data)

if (interactive()) head(submarket_data) %>% View()

desc_var <- c("room_count", "item_type", "log_price", "sqm_price",
 "log_sqm_price", "log_area", "dep_Name", "submarket")

submarket_descritpion <- FactoMineR::catdes(submarket_data %>% 
                                              dplyr::select(desc_var),
                                            num.var = length(desc_var))

pdf(file = paste(GRAPH_PDF_SAVING_DIRECTORY,
                 paste(LOCALITY_NAME_OUT,
                       ANALYSE_SCALE_OUT,
                       "leiden_community_descritpion.pdf",
                       sep = "_"),
                 sep = "/"),
  width = 15, height = 15
)
FactoMineR::plot.catdes(submarket_descritpion)

dev.off()


submarket_descritpion$test.chi2
submarket_descritpion$quanti.var


#FactoMineR::print.catdes(submarket_descritpion)

#FactoMineR::meansComp()


#=========================================
#
# Resolution analysis
#
#=========================================

source("src/clustering/graph/resolution_optimization.R")

#  Compute Eta² for each resolution 

eta2_from_partition(V(GRAPH)$log_price, membership(leiden_clustering))

eta2_from_partition(V(GRAPH)$sqm_price, membership(leiden_clustering))



if (FALSE) {
  out <- test_leiden_resolutions(
  graph   = GRAPH,
  res_min = 0.5,
  res_max = 20,
  res_step = 0.1,
  n_iter = 20,
  beta = 0.01
  )


#out[[3]]$resolution
#out[[3]]$n_clusters
#length(out[[3]]$membership)


dir.create("out/Rdata/graph/clustering/leiden", recursive = TRUE, showWarnings = FALSE)
save(out,
  file = paste0("out/Rdata/graph/clustering/leiden/",
                LOCALITY_NAME_OUT,"_resolution_variation.RData")
)

}

if (!(exists("out"))) {
  load(paste0("out/Rdata/graph/clustering/leiden/",
              LOCALITY_NAME_OUT, "_resolution_variation.RData")
  )
}



#  Compute Eta² for each resolution 
eta2_values <- sapply(out, function(x) {
  eta2_from_partition(V(GRAPH)$sqm_price, x$membership)
})


eta2_values

length(eta2_values)

k_res <- which.max(eta2_values)

eta2_from_partition(V(GRAPH)$sqm_price, out[[k_res]]$membership)

out[[k_res]]$resolution
out[[k_res]]$modularity
max(eta2_values)

#  Build summary table
summary_eta2 <- data.frame(
  resolution = sapply(out, function(x) x$resolution),
  n_clusters = sapply(out, function(x) x$n_clusters),
  modularity = sapply(out, function(x) x$modularity),
  eta2       = eta2_values,
  department_NMI = sapply(out, function(x) NMI(x$membership, V(GRAPH)$dep_Name)),
  maille_habitat_NMI = sapply(out, function(x) NMI(x$membership, V(GRAPH)$maille_habitat)), 
  city_NMI = sapply(out, function(x) NMI(x$membership, V(GRAPH)$sl_insee_city_id))
)

if (interactive()) summary_eta2 %>% View()


if (FALSE) {
  spatial_NMI <- data.frame(
  department_NMI = sapply(out, function(x) NMI(x$membership, V(GRAPH)$dep_Name)),
  city_NMI = sapply(out, function(x) NMI(x$membership, V(GRAPH)$sl_insee_city_id))
  )

spatial_NMI

}



if (interactive()) {
  # Plot Eta² as a function of the Leiden resolution 
  ggplot(summary_eta2, aes(x = resolution, y = eta2)) +
    geom_line(color = "steelblue", linewidth = 1.2) +
    geom_point(color = "black", size = 2) +
    geom_vline(
      xintercept = summary_eta2$resolution[which.max(summary_eta2$eta2)],
      linetype = "dashed",
      color = "red",
      linewidth = 1
    ) +
    labs(
      title = "Eta²(sqm_price) across Leiden resolutions",
      x = "Resolution parameter",
      y = "Eta² (sqm_price)"
    ) +
    theme_minimal(base_size = 14)
}




if (interactive()) {
  # Plot Eta² as a function of the Leiden resolution 
  ggplot(summary_eta2, aes(x = resolution, y = city_NMI)) +
    geom_line(color = "steelblue", linewidth = 1.2) +
    geom_point(color = "black", size = 2) +
    geom_vline(
      xintercept = summary_eta2$resolution[which.max(summary_eta2$eta2)],
      linetype = "dashed",
      color = "red",
      linewidth = 1
    ) +
    labs(
      title = "Eta²(sqm_price) across Leiden resolutions",
      x = "Resolution parameter",
      y = "city NMI"
    ) +
    theme_minimal(base_size = 14)
}



# A all in one plot

{

  # Load required packages using your custom loader
  retrieve_package("dplyr")
  retrieve_package("tidyr")
  retrieve_package("ggplot2")
  retrieve_package("scales")


  # Prepare data in long format
  df_long <- summary_eta2 %>%
    dplyr::select(resolution, eta2, modularity, department_NMI, maille_habitat_NMI, city_NMI) %>%
    tidyr::pivot_longer(
      cols = c(eta2, modularity, department_NMI, maille_habitat_NMI, city_NMI),
      names_to = "metric",
      values_to = "value"
    )

  # Define colors for each metric
  metric_colors <- c(
    eta2 = "steelblue",
    modularity = "firebrick",
    department_NMI = "darkorange",
    maille_habitat_NMI = "black",
    city_NMI = "darkgreen"
  )

  # Plot all metrics on the same graph
  plot_indicators_vs_res <- ggplot(df_long, aes(x = resolution, y = value, color = metric)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2) +
    scale_color_manual(
      values = metric_colors,
      labels = c(
        eta2 = "Eta² (sqm price)",
        modularity = "Modularity",
        department_NMI = "NMI departments",
        maille_habitat_NMI = "NMI maille habitat",
        city_NMI = "NMI municipalities"
      )
    ) +
    geom_vline(
      xintercept = summary_eta2$resolution[which.max(summary_eta2$eta2)],
      linetype = "dashed",
      color = "black",
      linewidth = 0.8
    ) +
    labs(
      title = "Joint evolution of Eta² (sqm_price), modularity and spatial NMI across Leiden resolutions",
      x = "Resolution parameter",
      y = "Normalized value",
      color = "Metric"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12)
    )
}



if (interactive()) plot_indicators_vs_res



pdf(file = paste(GRAPH_PDF_SAVING_DIRECTORY,
                 paste(LOCALITY_NAME_OUT,
                       ANALYSE_SCALE_OUT,
                       "plot_indicators_vs_res.pdf",
                       sep = "_"),
                 sep = "/"),
  width = 15, height = 15
)
plot_indicators_vs_res

dev.off()





#=========================================
# Change point
#=========================================

retrieve_package("trend")


# Pettitt test on Eta²
pettitt_eta2 <- trend::pettitt.test(summary_eta2$eta2)

# Pettitt test on modularity
pettitt_mod <- trend::pettitt.test(summary_eta2$modularity)

# Pettitt test on NMI (departments)
pettitt_dep <- trend::pettitt.test(summary_eta2$department_NMI)

# Pettitt test on NMI (maille habitat)
pettitt_maille_hab <- trend::pettitt.test(summary_eta2$maille_habitat_NMI)

# Pettitt test on NMI (municipalities)
pettitt_city <- trend::pettitt.test(summary_eta2$city_NMI)

# Extract change-points
change_points <- dplyr::tibble(
  metric = c("eta2", "modularity", "department_NMI", "maille_habitat_NMI", "city_NMI"),
  change_index = c(
    pettitt_eta2$estimate,
    pettitt_mod$estimate,
    pettitt_dep$estimate,
    pettitt_maille_hab$estimate,
    pettitt_city$estimate
  ),
  change_resolution = summary_eta2$resolution[c(
    pettitt_eta2$estimate,
    pettitt_mod$estimate,
    pettitt_dep$estimate,
    pettitt_maille_hab$estimate,
    pettitt_city$estimate
  )]
)

change_points



#=========================================
# Convergence area
#=========================================

source("src/execution/stability_identification.R")



# --- 1. Normalize metrics to [0,1] ---
df_norm <- summary_eta2 %>%
  dplyr::select(resolution, eta2, modularity, department_NMI, city_NMI)

# --- 2. Compute distance between the three curves ---
df_norm <- df_norm %>%
  dplyr::mutate(
    distance = sqrt(
      (eta2 - modularity)^2 +
      (eta2 - city_NMI)^2 +
      (modularity - city_NMI)^2
    )
  )

# --- 3. Identify the resolution where the curves are closest ---
#optimal_row <- df_norm %>% dplyr::slice(which.min(distance))

#optimal_row$resolution

#k_res_optim <- which.min(df_norm$distance)

k_res_optim <- bp_test$breakpoints[1]

k_res_optim_2 <- bp_test$breakpoints[2]

df_norm$resolution[k_res_optim]

optimal_row <- df_norm %>% dplyr::slice(k_res_optim)

optimal_row_2 <- df_norm %>% dplyr::slice(k_res_optim_2)

# Compute raw distance between city and department NMI along with eta².
df_dist <- summary_eta2 %>%
  dplyr::select(resolution, eta2, department_NMI, city_NMI) %>%
  dplyr::mutate(
    distance = sqrt(
      (eta2 - city_NMI)^2 +
        (eta2 - department_NMI)^2 +
        (city_NMI - department_NMI)^2
    )
  )

# Identify the resolution where the curves converge the most
optimal_row_eta_city_dep <- df_dist %>% dplyr::slice(which.min(distance))

optimal_row_eta_city_dep$resolution



{

  # --- 1. Pettitt change-point detection on Eta² ---
  pettitt_eta2 <- trend::pettitt.test(summary_eta2$eta2)
  change_index <- pettitt_eta2$estimate
  change_resolution <- summary_eta2$resolution[change_index]

  # --- 2. Prepare long-format data for plotting ---
  df_long <- summary_eta2 %>%
    dplyr::select(resolution, eta2, modularity, department_NMI, maille_habitat_NMI, city_NMI) %>%
    tidyr::pivot_longer(
      cols = c(eta2, modularity, department_NMI, maille_habitat_NMI, city_NMI),
      names_to = "metric",
      values_to = "value"
    )

  # --- 3. Define colors ---
  metric_colors <- c(
    eta2 = "steelblue",
    modularity = "firebrick",
    department_NMI = "darkorange",
    maille_habitat_NMI = "black",
    city_NMI = "darkgreen"
  )

  # --- 4. Plot all metrics with Pettitt change-point ---
  plot_indicators_vs_res_chng_pt <- ggplot(df_long, aes(x = resolution, y = value, color = metric)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2) +
    scale_color_manual(
      values = metric_colors,
      labels = c(
        eta2 = "Eta² (price)",
        modularity = "Modularity",
        department_NMI = "NMI departments",
        maille_habitat_NMI = "NMI maille habitat",
        city_NMI = "NMI municipalities"
      )
    ) +
    # Vertical line at maximum Eta²
    geom_vline(
      xintercept = summary_eta2$resolution[which.max(summary_eta2$eta2)],
      linetype = "dashed",
      color = "black",
      linewidth = 0.8
    ) +
      annotate(
      "text",
      x = summary_eta2$resolution[which.max(summary_eta2$eta2)],
      y = max(df_long$value),
      label = paste0("Max Eta² value\n(resolution = ", round(summary_eta2$resolution[which.max(summary_eta2$eta2)], 2), ")"),
      color = "black",
      vjust = -0.5,
      size = 4
    ) +    
    # convergence value
    geom_vline(
      xintercept = optimal_row$resolution,
      linetype = "dotted",
      color = "red",
      linewidth = 1
    ) +
      annotate(
      "text",
      x = optimal_row$resolution,
      y = max(df_long$value),
      label = paste0("Convergence value\n(resolution = ", round(optimal_row$resolution, 2), ")"),
      color = "red",
      vjust = -0.5,
      size = 4
    ) +
    # 2nd convergence value
    geom_vline(
      xintercept = optimal_row_2$resolution,
      linetype = "dotted",
      color = "red",
      linewidth = 1
    ) +
      annotate(
      "text",
      x = optimal_row_2$resolution,
      y = max(df_long$value),
      label = paste0("Convergence value\n(resolution = ", round(optimal_row_2$resolution, 2), ")"),
      color = "red",
      vjust = -0.5,
      size = 4
    ) +
     # convergence value with department
# geom_vline(
#   xintercept = optimal_row_eta_city_dep$resolution,
#   linetype = "dotted",
#   color = "orange",
#   linewidth = 1
# ) +
#   annotate(
#     "text",
#     x = optimal_row_eta_city_dep$resolution,
#     y = max(df_long$value),
#     label = paste0("Convergence value dept\n(resolution = ", round(optimal_row_eta_city_dep$resolution, 2), ")"),
#     color = "orange",
#     vjust = -0.5,
#     size = 4
#   ) +  
    # Vertical line at Pettitt change-point
    geom_vline(
      xintercept = change_resolution,
      linetype = "dotted",
      color = "purple",
      linewidth = 1
    ) +
    annotate(
      "text",
      x = change_resolution,
      y = max(df_long$value),
      label = paste0("Pettitt change-point\n(resolution = ", round(change_resolution, 2), ")"),
      color = "purple",
      vjust = -0.5,
      size = 4
    ) +
    labs(
      title = "Joint evolution of Eta², modularity and spatial NMI across Leiden resolutions",
      subtitle = "Including Pettitt non-parametric change-point detection",
      x = "Resolution parameter",
      y = "Normalized value",
      color = "Metric"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12)
    )

}


if (interactive()) plot_indicators_vs_res_chng_pt



pdf(file = paste(GRAPH_PDF_SAVING_DIRECTORY,
                 paste(LOCALITY_NAME_OUT,
                       ANALYSE_SCALE_OUT,
                       "plot_indicators_vs_res_chng_pt.pdf",
                       sep = "_"),
                 sep = "/"),
  width = 15, height = 15
)
plot_indicators_vs_res_chng_pt

dev.off()


eta2_from_partition(V(GRAPH)$sqm_price, out[[k_res_optim]]$membership)

out[[k_res_optim]]$resolution
out[[k_res_optim]]$modularity






#=========================================
#
# Submarket graph
#
#=========================================

#membership <- membership(leiden_clustering)

#comm_size <- sizes(leiden_clustering)


#k_res <- which.max(eta2_values)

k_res <- k_res_optim

#k_res <- change_index

membership <- out[[k_res]]$membership

comm_size  <- out[[k_res]]$comm_size

out[[k_res]]$n_clusters

out[[k_res]]$modularity

summary_eta2[k_res, ]

NMI(membership,
  V(GRAPH)$dep_Name
)

NMI(membership,
  V(GRAPH)$maille_habitat
)

NMI(membership,
  V(GRAPH)$sl_insee_city_id
)

edges <- as_data_frame(GRAPH, what = "edges")

#if (interactive()) head(edges) %>% View()

edges$ci <- membership[edges$from]
edges$cj <- membership[edges$to]

#if (interactive()) head(edges, 100) %>% View()

# Submarket edge list

meta_edges <- edges %>%
  filter(ci != cj) %>%
  mutate(
    c_min = pmin(ci, cj),
    c_max = pmax(ci, cj)
  ) %>%
  group_by(c_min, c_max) %>%
  summarise(weight = sum(weight), .groups = "drop")


if (interactive()) meta_edges %>% View()

# Submarket graph

meta_graph <- graph_from_data_frame(
  meta_edges,
  directed = FALSE,
  vertices = NULL
)



V(meta_graph)$size_comm <- comm_size[V(meta_graph)$name]


retrieve_package("Polychrome")

K <- out[[k_res]]$n_clusters
submarket_colors_leiden <- Polychrome::createPalette(K, seedcolors = c("#ff0000", "#00ff00", "#0000ff"))
submarket_colors_leiden <- submarket_colors_leiden[as.integer(V(meta_graph)$name)]




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

V(meta_graph)$x <- comm_centroids$cx[ match(V(meta_graph)$name, comm_centroids$comm) ]
V(meta_graph)$y <- comm_centroids$cy[ match(V(meta_graph)$name, comm_centroids$comm) ]



comm_size

if (interactive()) {
  plot(
    meta_graph,
    vertex.color = submarket_colors_leiden,
    vertex.size = 2*log(V(meta_graph)$size_comm),
    edge.width  = log1p(E(meta_graph)$weight)
  )
}



if (FALSE){
  png(file = paste(GRAPH_PNG_SAVING_DIRECTORY,
                 paste(LOCALITY_NAME_OUT,
                       ANALYSE_SCALE_OUT,
                       "leiden_community_plot_optim_res.png",
                       sep = "_"),
                 sep = "/"),
  width = 3000, height = 2000
  )

  #plot(leiden_clustering, graph)
  {
    par(mar=c(0,0,0,0))
    plot(
      GRAPH,
      vertex.label = NA,
      vertex.size = .5,
      edge.arrow.size = .001,
      edge.size = .0001,
      vertex.color = submarket_colors_leiden[as.integer(out[[k_res]]$membership)],
      main = "Leiden"
    )
  }

dev.off()
}



# incurve links

#is_simple(meta_graph)

E(meta_graph)$curved <- 0.2   # a value ranging from 0 to 1

# if complex graph and curve only needed for multiple links :

#which_multiple(meta_graph)

if(!is_simple(meta_graph)) E(meta_graph)$curved <- ifelse(which_multiple(meta_graph), 0.2, 0)



#==========================
# Submarket attributes
#==========================

df_comm <- data.frame(
  comm           = membership,
  price          = V(GRAPH)$price,
  sqm_price      = V(GRAPH)$sqm_price,
  area           = V(GRAPH)$area,
  item_type      = V(GRAPH)$item_type,
  city           = V(GRAPH)$sl_insee_city_id,
  maille_habitat = V(GRAPH)$maille_habitat,
  dep            = V(GRAPH)$INSEE_DEP,
  room_count     = V(GRAPH)$room_count
)

for (var_name in vars_to_add) {
  df_comm[[var_name]] <- vertex_attr(GRAPH, var_name)
}


# Helper function to extract dominant modality and its percentage
get_dominant_info <- function(x) {
  valid_x <- x[!is.na(x)]
  if (length(valid_x) == 0) {
    return(list(mode = NA_character_, pct = NA_real_))
  }

  # Count frequencies
  counts <- table(valid_x)
  top_mod <- names(counts)[which.max(counts)]
  top_pct <- (max(counts) / length(valid_x)) * 100

  return(list(mode = top_mod, pct = round(top_pct, 2)))
}


cor.test(df_comm$sqm_price, df_comm$indic.prix_rev.12, method = "pearson")


comm_measures <- df_comm %>%
  group_by(comm) %>%
  summarise(
    # Continuous metrics
    median_price     = median(price, na.rm = TRUE),
    median_sqm_price = median(sqm_price, na.rm = TRUE),
    median_area      = median(area, na.rm = TRUE),
    room_count       = median(room_count, na.rm = TRUE),
    n                = n(),

    # Dynamic mean & median for all variables in vars_to_add
    across(
      all_of(vars_to_add),
      list(
        mean   = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE)
      ),
      .names = "{.fn}_{.col}"
    ),

    # Categorical dominant modalities and percentages
    main_item_type = get_dominant_info(item_type)$mode,
    pct_item_type  = get_dominant_info(item_type)$pct,

    main_city = get_dominant_info(city)$mode,
    pct_city  = get_dominant_info(city)$pct,

    main_maille_habitat = get_dominant_info(maille_habitat)$mode,
    pct_maille_habitat  = get_dominant_info(maille_habitat)$pct,

    main_dep = get_dominant_info(dep)$mode,
    pct_dep  = get_dominant_info(dep)$pct,

    .groups = "drop"
  )


if (interactive()) View(comm_measures)

cor.test(comm_measures$median_sqm_price, comm_measures$median_indic.prix_rev.12, method = "pearson")


var_list_mean <- paste0("mean_", vars_to_add)

var_list_median <- paste0("median_", vars_to_add)

var_list_all <- c(var_list_mean, var_list_median)


var_list <- c("median_price", "median_sqm_price", "median_area", "room_count",
  "main_item_type", "pct_item_type", "main_city", "main_maille_habitat",
  "main_dep", "pct_city", "pct_maille_habitat", "pct_dep",
  var_list_all
)

for (var_name in var_list) {
  meta_graph <- set_vertex_attr(meta_graph, name = var_name, value = comm_measures[[var_name]][
    match(V(meta_graph)$name, comm_measures$comm)
  ])
}


cor.test(V(meta_graph)$median_sqm_price, V(meta_graph)$median_indic.prix_rev.12, method = "pearson")

cor.test(V(meta_graph)$median_sqm_price, V(meta_graph)$mean_indic.prix_rev.12, method = "pearson")

retrieve_package("RColorBrewer")

pal_brew <- colorRampPalette(brewer.pal(9, "YlOrRd"))  # ou "Viridis"
V(meta_graph)$pal_brew <- pal_brew(100)[as.numeric(cut(V(meta_graph)$median_price, 100))]

V(meta_graph)$pal_brew_sqm_price <- pal_brew(100)[as.numeric(cut(V(meta_graph)$median_sqm_price, 100))]


retrieve_package("viridis")

pal_vir <- viridis(100)

V(meta_graph)$pal_vir <- pal_vir[
  as.numeric(cut(V(meta_graph)$median_price, breaks = 100))
]

V(meta_graph)$pal_vir_sqm_price <- pal_vir[
  as.numeric(cut(V(meta_graph)$median_sqm_price, breaks = 100))
]


# Plot


if (interactive()) {
  plot(
  meta_graph,
  vertex.size = 2 * log(V(meta_graph)$size_comm),
  vertex.color = V(meta_graph)$pal_brew_sqm_price, # V(meta_graph)$pal_brew
  edge.width = log1p(E(meta_graph)$weight),
  edge.curved = .5
  )

}


if (interactive()) {
  plot(
  meta_graph,
  vertex.size = 2 * log(V(meta_graph)$size_comm),
  vertex.color = V(meta_graph)$pal_brew_sqm_price, # V(meta_graph)$pal_brew
  edge.width = log1p(E(meta_graph)$weight),
  edge.curved = 0
  )

}


# For more readablity

E(meta_graph)$alpha <- scales::rescale(E(meta_graph)$weight, to = c(0.2, 1))
V(meta_graph)$size_plot <- scales::rescale(as.numeric(V(meta_graph)$size_comm), to = c(6, 20))



#==================
# an enhanced plot
#==================

# Normaliser les poids entre 0.1 et 1
alpha_vals <- scales::rescale(E(meta_graph)$weight, to = c(0.1, 1))

# Couleur de base
base_col <- "grey40"

# Convertir en RGBA
E(meta_graph)$color <- apply(
  col2rgb(base_col) / 255,
  2,
  function(rgb) rgb(rgb[1], rgb[2], rgb[3], alpha_vals)
)


if (interactive()) {
  plot(
    meta_graph,
    edge.color = E(meta_graph)$color,
    vertex.size = 2*log(V(meta_graph)$size_comm),
    vertex.color = V(meta_graph)$pal_brew,
    edge.width = log1p(E(meta_graph)$weight),
    edge.curved = 0.5
  )

}

if (interactive()) {
  plot(
    meta_graph,
    edge.color = E(meta_graph)$color,
    vertex.size = 2*log(V(meta_graph)$size_comm),
    vertex.color = V(meta_graph)$pal_brew_sqm_price,
    edge.width = log1p(E(meta_graph)$weight),
    edge.curved = 0
  )

}


pdf(file = paste(GRAPH_PDF_SAVING_DIRECTORY,
                 paste(LOCALITY_NAME_OUT,
                       ANALYSE_SCALE_OUT,
                       "sqm_price_gradient_graph_of_comunity.pdf",
                       sep = "_"),
                 sep = "/"),
  width = 15, height = 15
)
plot(
meta_graph,
edge.color = E(meta_graph)$color,
vertex.size = log(V(meta_graph)$size_comm),
vertex.color = V(meta_graph)$pal_brew_sqm_price,
edge.width = log1p(E(meta_graph)$weight),
edge.curved = 0
)

dev.off()


pdf(file = paste(GRAPH_PDF_SAVING_DIRECTORY,
                 paste(LOCALITY_NAME_OUT,
                       ANALYSE_SCALE_OUT,
                       "graph_of_comunity.pdf",
                       sep = "_"),
                 sep = "/"),
  width = 15, height = 15
)
plot(
meta_graph,
edge.color = E(meta_graph)$color,
vertex.size = log(V(meta_graph)$size_comm),
vertex.color = submarket_colors_leiden,
edge.width = log1p(E(meta_graph)$weight),
edge.curved = 0
)

dev.off()


#==================
# A ggraph plot
#==================
retrieve_package("ggraph")
retrieve_package("tidygraph")


if (require("ggraph")) {

  # Convert into tbl_graph
  tg <- as_tbl_graph(meta_graph)

  # Color Brewer red
  pal <- brewer.pal(9, "Reds")

  #pal = pnw_palette("Moth", 9)

  #pal = rev(pal)

  pal_fun <- colorRampPalette(pal)

  # Add colors through median price/m²
  tg <- tg %>%
    mutate(
      price_cont = median_sqm_price,   # variable continue pour la légende
      size_plot = scales::rescale(as.numeric(size_comm), to = c(6, 20))
    )


  # Layout = centroid
  layout_mat <- cbind(tg$x, tg$y)

  # Plot ggraph
  price_gradient_graph_of_comunity <- ggraph(
    tg, layout = "manual",
    x = layout_mat[,1], y = layout_mat[,2]
  ) +

    # edges
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

    # color gradient
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

    # edges and color legend
    scale_edge_width(
      range = c(0.5, 3),
      name = "weight",
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
      legend.spacing.x = unit(4, "cm")     # <-- Ajoute l'espace horizontal entre les deux légendes
    )
}


if (interactive() & exists("price_gradient_graph_of_comunity")) price_gradient_graph_of_comunity


if (exists("price_gradient_graph_of_comunity")) {

  file_path <- file.path(
    GRAPH_PDF_SAVING_DIRECTORY,
    paste0(LOCALITY_NAME_OUT, "_", ANALYSE_SCALE_OUT, "_sqm_price_gradient_graph_of_comunity.pdf")
  )


  ggsave(
    filename = file_path,
    plot = price_gradient_graph_of_comunity,
    width = 25,
    height = 25,
    units = "in",
    device = cairo_pdf,
    limitsize = FALSE
  )
}


V(meta_graph)$median_sqm_price_log <- log(V(meta_graph)$median_sqm_price)

V(meta_graph)$median_price_log     <- log(V(meta_graph)$median_price)

V(meta_graph)$median_area_log     <- log(V(meta_graph)$median_area)
# Create POINT geometries
geom_points <- sf::st_as_sf(comm_centroids, coords = c("cx", "cy"), crs = 4326)$geometry

# Assign them to the graph
V(meta_graph)$geometry <- geom_points

meta_graph <- node_spatial_distance(graph = meta_graph,
                                    name = "distance")


E(meta_graph)$log_distance <- log1p(E(meta_graph)$distance)

#================================
# Submarkets Assortativity
#================================

summary(V(meta_graph)$pct_item_type)
summary(V(meta_graph)$pct_city)
summary(V(meta_graph)$pct_maille_habitat)
summary(V(meta_graph)$pct_dep)

{

  price_rev_assortativity_submarkets <- assortnet_assortativity(
    graph = meta_graph
    , node_attr = "median_indic.prix_rev.12"
    , weighted = TRUE
    , weight_attr = "weight"
    , n_perm = 100
  )

  median_sqm_price_assortativity_submarkets <- assortnet_assortativity(
    graph = meta_graph
    , node_attr = "median_sqm_price"
    , weighted = TRUE
    , weight_attr = "weight"
    , n_perm = 100
  )


  log_median_price_assortativity_submarkets <- assortnet_assortativity(
    graph = meta_graph
    , node_attr = "median_price_log"
    , weighted = TRUE
    , weight_attr = "weight"
    , n_perm = 100
  )

  log_median_area_assortativity_submarkets <- assortnet_assortativity(
    graph = meta_graph
    , node_attr = "median_area_log"
    , weighted = TRUE
    , weight_attr = "weight"
    , n_perm = 100
  )

  room_count_assortativity_submarkets <- assortnet_assortativity(
  graph = meta_graph
  , node_attr = "room_count"
  , weighted = TRUE
  , weight_attr = "weight"
  , n_perm = 100
  )

  main_item_type_assortativity_submarkets <- assortnet_assortativity(
    graph = meta_graph
    , node_attr = "main_item_type"
    , weighted = TRUE
    , weight_attr = "weight"
    , n_perm = 100
  )

  pct_item_type_assortativity_submarkets <- assortnet_assortativity(
    graph = meta_graph
    , node_attr = "pct_item_type"
    , weighted = TRUE
    , weight_attr = "weight"
    , n_perm = 100
  )


  main_maille_habitat_assortativity_submarkets <- assortnet_assortativity(
    graph = meta_graph
    , node_attr = "main_maille_habitat"
    , weighted = TRUE
    , weight_attr = "weight"
    , n_perm = 100
  )


  main_dep_assortativity_submarkets <- assortnet_assortativity(
    graph = meta_graph
    , node_attr = "main_dep"
    , weighted = TRUE
    , weight_attr = "weight"
    , n_perm = 100
  )


  pct_maille_habitat_assortativity_submarkets <- assortnet_assortativity(
    graph = meta_graph
    , node_attr = "pct_maille_habitat"
    , weighted = TRUE
    , weight_attr = "weight"
    , n_perm = 100
  )


  pct_dep_assortativity_submarkets <- assortnet_assortativity(
    graph = meta_graph
    , node_attr = "pct_dep"
    , weighted = TRUE
    , weight_attr = "weight"
    , n_perm = 100
  )
}


median_sqm_price_assortativity_submarkets %>% print()

log_median_price_assortativity_submarkets %>% print()

log_median_area_assortativity_submarkets %>% print()

room_count_assortativity_submarkets %>% print()

main_item_type_assortativity_submarkets %>% print()

main_maille_habitat_assortativity_submarkets %>% print()

main_dep_assortativity_submarkets %>% print()

pct_item_type_assortativity_submarkets %>% print()

pct_maille_habitat_assortativity_submarkets %>% print()

pct_dep_assortativity_submarkets %>% print()


submarket_assortativity_list <- list(
  median_sqm_price = median_sqm_price_assortativity_submarkets,
  log_median_area  = log_median_area_assortativity_submarkets,
  main_item_type   = main_item_type_assortativity_submarkets,
  main_maille_habitat = main_maille_habitat_assortativity_submarkets,
  main_dep            = main_dep_assortativity_submarkets,
  room_count          = room_count_assortativity_submarkets
#  ,pct_maille_habitat  = pct_maille_habitat_assortativity_submarkets,
#  pct_dep             = pct_dep_assortativity_submarkets
)


#=====================
# Spatial correlation
#=====================


# Pearson
cor.test(edge_attr(meta_graph, "weight"), edge_attr(meta_graph, "distance"), method = "pearson")
cor.test(edge_attr(meta_graph, "weight"), edge_attr(meta_graph, "log_distance"), method = "pearson")


# Kendall
cor.test(edge_attr(meta_graph, "weight"), edge_attr(meta_graph, "distance"), method = "kendall")
cor.test(edge_attr(meta_graph, "weight"), edge_attr(meta_graph, "log_distance"), method = "kendall")



#==================================
# Submarket community
#==================================

#  Run Leiden clustering on the metagraph
meta_cl <- cluster_leiden(
  graph               = meta_graph,
  objective_function  = "modularity",
  resolution          = 1.0,
  beta                = 0.01,
  n_iterations        = 20
)

meta_membership <- membership(meta_cl)

V(meta_graph)$meta_membership <- meta_membership


#  Cluster sizes in the metagraph
meta_cluster_sizes <- table(meta_membership)
meta_cluster_sizes



#  Average degree per meta-cluster
deg <- degree(meta_graph)

avg_degree_by_cluster <- tapply(deg, meta_membership, mean)
avg_degree_by_cluster


#  Total edge weight per meta-cluster
edge_weights <- E(meta_graph)$weight

total_weight_by_cluster <- tapply(
  edge_weights,
  meta_membership[ends(meta_graph, E(meta_graph))[,1]],
  sum
)

total_weight_by_cluster

#  Internal density of each meta-cluster 
internal_density <- sapply(unique(meta_membership), function(k) {
  sub <- induced_subgraph(meta_graph, which(meta_membership == k))
  edge_density(sub)
})

internal_density


#  Compute betweenness centrality on the metagraph
betw <- betweenness(
  meta_graph,
  directed = FALSE,
  weights = 1 / E(meta_graph)$weight,  # optional: weight inversion
  normalized = TRUE
)

betw


#  Modularity of the metagraph partition
meta_cl$quality


#  Plot metagraph with cluster colors
if (interactive()) plot(
  meta_graph,
  edge.color = E(meta_graph)$color,
  vertex.size = log(V(meta_graph)$size_comm),
  vertex.color = meta_membership,
  edge.width = log1p(E(meta_graph)$weight),
  edge.curved = 0
)



pdf(file = paste(GRAPH_PDF_SAVING_DIRECTORY,
                 paste(LOCALITY_NAME_OUT,
                       ANALYSE_SCALE_OUT,
                       "submarkets_comunities.pdf",
                       sep = "_"),
                 sep = "/"),
  width = 15, height = 15
)
plot(
  meta_graph,
  edge.color = E(meta_graph)$color,
  vertex.size = log(V(meta_graph)$size_comm),
  vertex.color = meta_membership,
  edge.width = log1p(E(meta_graph)$weight),
  edge.curved = 0
)

dev.off()





#########################################################################################
# Build bipartite graph User × Submarket and derive:
#   (1) Submarket co-visit graph
#   (2) User similarity graph
#
# Requirements:
#   - events_unique: raw event log with fullvisitorid and id_listing
#   - submarket_data: data.frame with columns (name = id_listing, submarket)
#########################################################################################


#=========================================
#
# Submarket covisit graph
#
#=========================================

if (exists("events_unique") & FALSE) {

  #  1. Attach submarket information to events
  # Only keep events whose listing belongs to the filtered GRAPH
  events_sm <- events_unique  %>%
    inner_join(submarket_data, by = c("id_listing" = "name"))

  #  2. Keep only users who visited at least 2 distinct submarkets
  users_valid <- events_sm  %>%
    group_by(fullvisitorid)  %>%
    summarise(n_sm = n_distinct(submarket))  %>%
    filter(n_sm >= 2)  %>%
    pull(fullvisitorid)

  events_sm <- events_sm  %>%
    filter(fullvisitorid %in% users_valid)

  #  3. Build bipartite graph: Users × Submarkets
  # Each edge = user visited submarket
  g_bip <- graph_from_data_frame(
    events_sm[, c("fullvisitorid", "submarket")],
    directed = FALSE
  )

  # Mark node types: TRUE = user, FALSE = submarket
  V(g_bip)$type <- V(g_bip)$name %in% events_sm$fullvisitorid

  #  4. Bipartite projection 
  proj <- bipartite_projection(g_bip)

  # (A) Submarket co-visit graph
  g_submarkets <- proj$proj2

  # (B) User similarity graph
  g_users <- proj$proj1

  #  5. Optional: compute basic structural indicators 
  # Submarket graph indicators
  deg_sub <- degree(g_submarkets)
  betw_sub <- betweenness(g_submarkets, normalized = TRUE)

  # User graph indicators
  deg_users <- degree(g_users)
  betw_users <- betweenness(g_users, normalized = TRUE)

  #  6. Print summaries 
  cat("Submarket graph:", vcount(g_submarkets), "nodes,", ecount(g_submarkets), "edges\n")
  cat("User graph:", vcount(g_users), "nodes,", ecount(g_users), "edges\n")

}
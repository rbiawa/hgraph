#########################################################################################
# A script to perform submerket transitions
# 
# 2026/06 Raphaël Biawa Diegamounoua
#
# source("src/execution/submarket_transitions.R")
#########################################################################################

# ============================================================
# Packages
# ============================================================

retrieve_package("igraph")
retrieve_package("dplyr")
retrieve_package("tidyr")
retrieve_package("assortnet")
retrieve_package("Matrix")
retrieve_package("purrr")

# ============================================================
# 0. Submarket transition's graph
# ============================================================

submarket_transition_graph <- graph_from_data_frame(
  transitions,
  directed = TRUE,
  vertices = NULL
)


submarket_transition_graph

reciprocity(submarket_transition_graph) == 1


V(submarket_transition_graph)$name %>% as.integer() %>% sort()


if (exists("comm_centroids")) {
  V(submarket_transition_graph)$x <- comm_centroids$cx[ match(V(submarket_transition_graph)$name, comm_centroids$comm) ]
  V(submarket_transition_graph)$y <- comm_centroids$cy[ match(V(submarket_transition_graph)$name, comm_centroids$comm) ]
}


if (exists("df_comm")) {
  V(submarket_transition_graph)$median_sqm_price <- df_comm$sqm_price[
  match(V(submarket_transition_graph)$name, df_comm$comm)
 ]
}

if (exists("comm_size")) {
  V(submarket_transition_graph)$size_comm <- comm_size[V(submarket_transition_graph)$name]
}


# Normaliser les poids entre 0.1 et 1
alpha_vals <- scales::rescale(E(submarket_transition_graph)$weight, to = c(0.1, 1))


V(meta_graph)$pal_brew_sqm_price


V(submarket_transition_graph)$pal_brew_sqm_price <- V(meta_graph)$pal_brew_sqm_price[ 
  match(V(submarket_transition_graph)$name,
        V(meta_graph)$name)
]

V(submarket_transition_graph)$median_area_log <- V(meta_graph)$median_area_log[ 
  match(V(submarket_transition_graph)$name,
        V(meta_graph)$name)
]

# Couleur de base
base_col <- "grey40"

# Convertir en RGBA
E(submarket_transition_graph)$color <- apply(
  col2rgb(base_col) / 255,
  2,
  function(rgb) rgb(rgb[1], rgb[2], rgb[3], alpha_vals)
)



if (exists("df_comm")) {

  pdf(file = paste(GRAPH_PDF_SAVING_DIRECTORY,
                 paste(LOCALITY_NAME_OUT,
                       ANALYSE_SCALE_OUT,
                       "sqm_price_gradient_graph_of_comunity_directed.pdf",
                       sep = "_"),
                 sep = "/"),
  width = 15, height = 15
)

  plot(
    submarket_transition_graph,
    edge.color = E(submarket_transition_graph)$color,
    vertex.size = log(V(submarket_transition_graph)$size_comm),
    vertex.color = V(submarket_transition_graph)$pal_brew_sqm_price,
    edge.width = log1p(E(submarket_transition_graph)$weight),
    edge.curved = 0
  )

dev.off()

}

if (interactive()) {
  plot(
    meta_graph,
    edge.color = E(meta_graph)$color,
    vertex.size = log(V(meta_graph)$size_comm),
    vertex.color = V(meta_graph)$pal_brew_sqm_price,
    edge.width = log1p(E(meta_graph)$weight),
    edge.curved = 0
  )
}

reciprocity(meta_graph) == 1

#==================
# A ggraph plot
#==================
source("src/processing/graph/plot_graph_with_ggraph.R")


p_transition <- plot_community_price_graph(submarket_transition_graph)


if (interactive()) p_transition

if (interactive()) plot_community_price_undir_graph(meta_graph)


if (exists("p_transition")) {
  pdf(file = paste(GRAPH_PDF_SAVING_DIRECTORY,
                   paste(LOCALITY_NAME_OUT,
                         ANALYSE_SCALE_OUT,
                         "sqm_price_gradient_submarket_transitions.pdf",
                         sep = "_"),
                   sep = "/"),
    width = 15, height = 15
  )

  p_transition %>% print()

  dev.off()

}


# ============================================================
# 0.1 Prepare graph objects
# ============================================================

g_noloop <- igraph::simplify(
  submarket_transition_graph,
  remove.multiple = FALSE,
  remove.loops = TRUE
)

reciprocity(g_noloop) == 1

plot_g_noloop <- plot_community_price_graph(g_noloop)

if (interactive()) plot_g_noloop




pdf(file = paste(GRAPH_PDF_SAVING_DIRECTORY,
                paste(LOCALITY_NAME_OUT,
                    ANALYSE_SCALE_OUT,
                    "sqm_price_gradient_submarket_transitions_noloops.pdf",
                    sep = "_"),
                sep = "/"),
width = 15, height = 15
)

 plot_g_noloop %>% print()

dev.off()


# ============================================================
# 1. Basic strength measures (in-strength, out-strength)
# ============================================================

strength_df <- data.frame(
  submarket = V(g_noloop)$name,
  median_area_log = V(g_noloop)$median_area_log,
  median_sqm_price = V(g_noloop)$median_sqm_price,
  in_strength  = igraph::strength(g_noloop, mode = "in",  weights = E(g_noloop)$weight),
  out_strength = igraph::strength(g_noloop, mode = "out", weights = E(g_noloop)$weight)
) %>%
  mutate(net_inflow   = (in_strength - out_strength)/(in_strength + out_strength))


# Interpretation:
# in_strength: total incoming flows from other submarkets.
# out_strength: total outgoing flows to other submarkets.
# Loops excluded because they do not represent transitions.

cor.test(strength_df$median_sqm_price, strength_df$net_inflow, method = "kendall")

cor.test(strength_df$median_area_log, strength_df$net_inflow, method = "kendall")


# ============================================================
# 3. Asymmetry of flows between pairs
# ============================================================

edges_asym <- igraph::as_data_frame(g_noloop, what = "edges") %>%
  dplyr::rename(i = from, j = to, w_ij = weight) %>%
  dplyr::left_join(
    igraph::as_data_frame(g_noloop, what = "edges") %>%
      dplyr::rename(j = from, i = to, w_ji = weight),
    by = c("i", "j")
  ) %>%
  dplyr::mutate(
    w_ji = ifelse(is.na(w_ji), 0, w_ji),
    asymmetry = (w_ij - w_ji) / (w_ij + w_ji + 1e-9)
  )


# Interpretation:
# asymmetry > 0: flows predominantly from i to j.
# asymmetry < 0: flows predominantly from j to i.
# Loops excluded because they do not encode directionality.

# ============================================================
# 4. Transition probabilities and entropy of outgoing flows
# ============================================================

A <- igraph::as_adjacency_matrix(g_noloop, attr = "weight", sparse = FALSE)
P <- A / (rowSums(A) + 1e-9)

my_entropy <- function(p) {
  p <- p[p > 0]
  -sum(p * log(p))
}

strength_df$entropy_out <- apply(P, 1, my_entropy)

# Interpretation:
# High entropy: users explore many alternatives.
# Low entropy: users converge to a small set of destinations.
# Loops excluded because they artificially reduce entropy.

# ============================================================
# 5. Price gradient along edges
# ============================================================



price <- V(submarket_transition_graph)$median_sqm_price
names(price) <- V(submarket_transition_graph)$name

edges_price <- igraph::as_data_frame(g_noloop, what = "edges") %>%
  dplyr::mutate(
    price_i = price[from],
    price_j = price[to],
    price_diff = price_j - price_i
  )

# Interpretation:
# price_diff > 0: transitions toward more expensive markets.
# price_diff < 0: transitions toward cheaper markets.
# Loops excluded because price_diff = 0 by construction.

# ============================================================
# 6. Correlation between flow intensity and price gradient
# ============================================================

cor.test(edges_price$weight, edges_price$price_diff, method = "kendall")

# Interpretation:
# Positive correlation: flows tend to move toward higher-priced areas.
# Negative correlation: flows tend to move toward cheaper areas.

# ============================================================
# 7. PageRank (weighted)
# ============================================================

strength_df$pagerank <- igraph::page_rank(
  g_noloop,
  directed = TRUE,
  weights = E(g_noloop)$weight
)$vector

# Interpretation:
# PageRank identifies structurally important submarkets.
# Loops excluded because they inflate self-importance.
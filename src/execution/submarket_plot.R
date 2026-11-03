#########################################################################################
# A script to plot submarket on maps
# 
# 2026/06 Raphaël Biawa Diegamounoua
#
# source("src/execution/submarket_plot.R")
#########################################################################################

k_res <- k_res_optim

V(GRAPH)$optim_submarket <- out[[k_res]]$membership



dep_geom_data <- filter_spatial_data(data = geom_sf_departements,
  filter_column = REGIONS_CODE_VAR,
  region_codes =  REGIONS_CODE_VAL,
  selected_columns = c("dep_ID", "dep_Name", "INSEE_DEP", "INSEE_REG")
)



#============================================================
# 1. Harmoniser les noms de géométrie
#============================================================
maille_habitat_studied <- maille_habita %>% 
  rename(geometry = geom) %>%
  st_as_sf()

#============================================================
# 2. Filtrer la maille habitat sur la zone d’étude (départements)
#============================================================
maille_habitat_studied <- st_intersection(maille_habitat_studied, dep_geom_data)

#============================================================
# 3. Extraire les points du graphe (déjà dans GRAPH)
#============================================================
pts <- GRAPH %>% 
  igraph::as_data_frame(what = "vertices") %>% 
  st_as_sf(coords = c("x", "y"), crs = st_crs(dep_geom_data))




maille_habitat_studied <-  maille_habitat_studied %>%
  filter(dep_ID %in% pts$dep_ID)


dep_geom_data <- dep_geom_data %>%
  filter(dep_ID %in% pts$dep_ID)

city_geom_data_studied <- city_geom_data %>%
  filter(INSEE_DEP %in% unique(pts$INSEE_DEP))

#============================================================
# 4. Liste des sous‑marchés
#============================================================

submarkets <- sort(unique(pts$optim_submarket))


#=========================================================
# Add English names for administrative layers
#=========================================================
dep_geom_data$layer          <- "Departments"
maille_habitat_studied$layer <- "Housing grid" # Maille habitat
city_geom_data_studied$layer <- "Municipalities"

#=========================================================
# Identify surrounding departments to avoid 'island maps'
#=========================================================
deps_study <- unique(dep_geom_data$INSEE_DEP)

WHOLE_DPT <- FALSE

{

  # 1. Buffer around study area (10 km)
  city_neighbors <- city_geom_data %>%
    filter(INSEE_DEP %in% deps_study) %>%
    st_union() %>%
    st_buffer(5000) %>%   # 10 km buffer around study area
    st_intersects(geom_sf_cities) %>%
    unlist()


  # 2. Select departments that are NOT part of the study area
  geom_neighbors <- geom_sf_cities[city_neighbors, ]

  dep_geom_neighbors <- st_intersection(
    # x = L'objet sf en premier (les attributs seront conservés)
    st_boundary(geom_sf_departements) %>%
      st_union(),

    # y = L'objet sfc en second (sert uniquement de masque de découpe)
    geom_neighbors %>%
      filter(! INSEE_COM %in% city_geom_data$INSEE_COM) %>%
      st_union()
  )
}


#========================================
# Plot each submarket separatly
#========================================

dir.create("out/png/submarkets/leiden_clustering",
           recursive = TRUE, showWarnings = FALSE)

retrieve_package("cowplot")

for (sm in submarkets) {

  #=========================================================
  # Filter points for the current submarket
  #=========================================================
  pts_sm <- pts %>% filter(optim_submarket == sm)
  col_sm <- submarket_colors_leiden[as.integer(sm)]

  #=========================================================
  # Compute submarket attributes
  #=========================================================
  listing_count     <- nrow(pts_sm)
  median_sqm_price  <- median(pts_sm$sqm_price, na.rm = TRUE)
  mean_sqm_price    <- mean(pts_sm$sqm_price, na.rm = TRUE)
  mean_area         <- mean(pts_sm$area, na.rm = TRUE)
  mean_room_count   <- mean(pts_sm$room_count, na.rm = TRUE)

  # Main property type
  type_dom <- pts_sm %>%
    count(item_type) %>%
    arrange(desc(n)) %>%
    slice(1) %>%
    sf::st_drop_geometry()

  principal_type <- type_dom$item_type
  pct_principal_type <- round(100 * type_dom$n / nrow(pts_sm), 1)

  #=========================================================
  # Text box content
  #=========================================================

  # 1. Wrap individual text components if they might be too long
#principal_type <- str_wrap(principal_type, width = 20)

  # 2. Build the string with explicit newlines
    info_txt <- paste0(
      "Number of listings: ", listing_count, "\n",
      "Median square meter price: \n", round(median_sqm_price), " €/m²\n",
      "Mean square meter price: \n", round(mean_sqm_price), " €/m²\n",
      "Mean area: ", round(mean_area), " m²\n",
      "Mean room count: ", round(mean_room_count, 2), "\n",
      "Main property type: \n", principal_type, "(", pct_principal_type, "%)"
  )


  #=========================================================
  # Build the map
  #=========================================================
  p <- ggplot() +

    # Surrounding departments (contextual background)
    geom_sf(
      data = geom_neighbors,
      fill = NA,
      color = "gray",
      linewidth = .05,
      alpha = 0.2
    ) +
    geom_sf(
      data = dep_geom_neighbors,
      fill = NA,
      color = "gray1",
      linewidth = .4,
      alpha = 0.4
    ) +

    # Administrative boundaries with legend
    geom_sf(
      data = dep_geom_data,
      aes(color = layer),
      fill = NA,
      linewidth = .4,
      alpha = 0.4
    ) +
    geom_sf(
      data = maille_habitat_studied,
      aes(color = layer),
      fill = NA,
      linewidth = .2,
      alpha = 0.5
    ) +
    geom_sf(
      data = city_geom_data_studied,
      aes(color = layer),
      fill = NA,
      linewidth = .05,
      alpha = 0.5
    ) +

    # Submarket points
    geom_sf(
      data = pts_sm,
      color = col_sm,
      size = 0.5,
      alpha = 0.4
    ) +

    # Manual legend for administrative layers
    scale_color_manual(
      name = "Administrative boundaries",
      values = c(
        "Departments"   = "black",
        "Housing grid"  = "gray40",
        "Municipalities" = "gray60"
      )
    ) +
    ggtitle(paste("Submarket", sm)) +
    theme_bw(base_size = 16) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      legend.position = "right",
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 14)
    ) +
    annotation_scale() +
    coord_sf(datum = NA) +
    annotation_north_arrow(
      location = "tr",
      which_north = "true",
      style = north_arrow_fancy_orienteering,
      height = unit(1, "cm"),
      width = unit(1, "cm")
    )

  #=============================
  # Add submarket attributes
  #=============================

  # 1. Extract the legend from your plot
  legend <- get_legend(p)

  # 2. Format the text (wrap lines to fit the sidebar width)
  #info_txt <- str_wrap(info_txt, width = 25)

  # 3. Create the text block
  text_block <- ggdraw() +
    draw_label(
      info_txt,
      x = 0.05,       # Slightly offset from left boundary
      y = 0.8,        # Anchored towards the top
      hjust = 0,      # Left-aligned text
      vjust = 1,      # Anchored from top
      size = 14        # Text size
    )

  # 4. Stack legend and text vertically
  sidebar <- plot_grid(
    legend,
    text_block,
    ncol = 1,
    align = "v",
    axis = "l",
    rel_heights = c(1, 2) # Give more vertical space to text if needed
  )

  # 5. Combine the main plot and the sidebar
  p <- plot_grid(
    p + theme(legend.position = "none"),
    sidebar,
    rel_widths = c(3, 1.2) # Increase right column width to prevent text overflow
  )

  #=========================================================
  # Save output
  #=========================================================
  ggsave(
    filename = paste0(
      "out/png/submarkets/leiden_clustering/",
      LOCALITY_NAME_OUT, "_submarket_", sm, ".png"
    ),
    plot = p,
    width = 8,
    height = 6,
    dpi = 300,
    bg = "white"
  )
}




#====================================
# Plot all submarket one a unique map
#====================================

TITLE_REGION <- c("IDF", "NPDC")[LOC_ORDER]

{
  #=========================================================
  # Filter points for the current submarket
  #=========================================================
  pts_sm <- pts
  col_sm <- submarket_colors_leiden[as.integer(pts$optim_submarket)]

  #=========================================================
  # Build the map
  #=========================================================
  {  p <- ggplot() +

    # Surrounding departments (contextual background)
    geom_sf(
      data = geom_neighbors,
      fill = NA,
      color = "gray",
      linewidth = .05,
      alpha = 0.2
    ) +
    geom_sf(
      data = dep_geom_neighbors,
      fill = NA,
      color = "gray1",
      linewidth = .4,
      alpha = 0.4
    ) +

    # Administrative boundaries with legend
    geom_sf(
      data = dep_geom_data,
      aes(color = layer),
      fill = NA,
      linewidth = .4,
      alpha = 0.4
    ) +
    geom_sf(
      data = maille_habitat_studied,
      aes(color = layer),
      fill = NA,
      linewidth = .2,
      alpha = 0.5
    ) +
    geom_sf(
      data = city_geom_data_studied,
      aes(color = layer),
      fill = NA,
      linewidth = .05,
      alpha = 0.5
    ) +

    # Submarket points
    geom_sf(
      data = pts_sm,
      color = col_sm,
      size = 0.5,
      alpha = 0.4
    ) +

    # Manual legend for administrative layers
    scale_color_manual(
      name = "Administrative boundaries",
      values = c(
        "Departments"    = "black",
        "Housing grid"   = "gray40",
        "Municipalities" = "gray60"
      )
    ) +

    theme_bw(base_size = 20) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),

      # map margins
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
      panel.spacing = unit(0, "pt"),

      # legend setting
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = element_text(size = 17, face = "bold"),
      legend.text = element_text(size = 20),
      legend.margin = margin(t = -15, r = 0, b = 0, l = 0, unit = "pt"),
      legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
    ) +

    annotation_scale(
      location = "bl",
      pad_x = unit(0.0, "cm"),
      pad_y = unit(0.2, "cm")
    ) +

    coord_sf(datum = NA) +

    annotation_north_arrow(
      location = "br",
      which_north = "true",
      pad_x = unit(0.0, "cm"),
      pad_y = unit(-0.1, "cm"),
      height = unit(1.5, "cm"),
      width = unit(1.5, "cm"),
      style = north_arrow_fancy_orienteering
    )
  }
  #=========================================================
  # Save output
  #=========================================================
  ggsave(
    filename = paste0(
      "out/png/submarkets/leiden_clustering/",
      LOCALITY_NAME_OUT, "_submarkets_all", ".png"
    ),
    plot = p,
    width = 10,
    height = 10,
    dpi = 300,
    bg = "white",
    limitsize = FALSE
  )
}







#==================================
# Submarket retention rate
#==================================

#out[[k_res]]$membership["180074663"]

if (! exists("events_clean")) source("src/execution/time_sequentiality_analysis.R")

# 1. Convert to data.table (if not already done)
events_submarket <- events_clean %>%
  filter(id_listing %in% names(out[[k_res]]$membership)) %>%
  mutate(submarket = out[[k_res]]$membership[match(id_listing, names(out[[k_res]]$membership))])


if ((! is.null(pts$cluster_attr_gps) && FALSE)) {
  events_submarket <- events_clean %>%
    filter(id_listing %in% pts$name) %>%
    mutate(submarket = pts$cluster_attr_gps[match(id_listing, pts$name)])
}


freq(events_submarket$submarket)

# 2. Compute the submarket retention rate
submarket_retention <- events_submarket[
  # Group by user and count distinct submarkets visited
  , .(n_submarkets = uniqueN(submarket)), by = fullvisitorid
][
  # Calculate the proportion of users with exactly 1 submarket
  , mean(n_submarkets == 1)
]

# Print the result as a percentage
cat("Submarket Retention:", round(submarket_retention * 100, 2), "%\n")











#================================
#
# Search VS price structure
#
#================================


compute_submarket_retention <- function(events_clean, out, k_res) {
  
  # 1. Extract membership vector for the specified k_res
  membership_vec <- out[[k_res]]$membership
  
  # 2. Convert events_clean to data.table if it's not one already
  if (!is.data.table(events_clean)) {
    dt_events <- as.data.table(events_clean)
  } else {
    dt_events <- events_clean
  }
  
  # 3. Filter listings present in membership and assign submarket
  events_submarket <- dt_events[
    id_listing %in% names(membership_vec)
  ][
    , submarket := membership_vec[match(id_listing, names(membership_vec))]
  ]
  
  # 4. Compute submarket retention rate
  submarket_retention <- events_submarket[
    , .(n_submarkets = uniqueN(submarket)), by = fullvisitorid
  ][
    , mean(n_submarkets == 1)
  ]

  return(submarket_retention)
}



# Call the function
retention_rate <- compute_submarket_retention(
  events_clean = events_clean,
  out = out,
  k_res = k_res
)

# Display result
cat("Submarket Retention Rate:", round(retention_rate * 100, 2), "%\n")

# NPDC 2021 : 82.39 %


#===================================
# Application to all elements of out
#===================================

same_submarket_seq_rate_values <- sapply(seq_along(out), function(k) {
  cat("\nProcessing k_res =", k, "\n")
  compute_submarket_retention(events_clean = events_clean, out = out, k_res = k)
})


#===========================================================
# Precompute static graph attributes (vertex names only)
#===========================================================

if (TRUE) save(same_submarket_seq_rate_values,
  file = paste0("out/Rdata/graph/clustering/leiden/",
                LOCALITY_NAME_OUT,"_same_submarket_seq_rate_values.RData")
)


summary_eta2 <- data.frame(
  resolution = sapply(out, function(x) x$resolution),
  n_clusters = sapply(out, function(x) x$n_clusters),
  modularity = sapply(out, function(x) x$modularity),
  eta2       = eta2_values,
  department_NMI = sapply(out, function(x) NMI(x$membership, V(GRAPH)$dep_Name)),
  maille_habitat_NMI = sapply(out, function(x) NMI(x$membership, V(GRAPH)$maille_habitat)), 
  city_NMI = sapply(out, function(x) NMI(x$membership, V(GRAPH)$sl_insee_city_id)),
  same_submarket_seq_rate = same_submarket_seq_rate_values
)

{
  # --- 1. Pettitt change-point detection on Eta² ---
  pettitt_eta2 <- trend::pettitt.test(summary_eta2$eta2)
  change_index <- pettitt_eta2$estimate
  change_resolution <- summary_eta2$resolution[change_index]

  # --- 2. Prepare long-format data for plotting ---
  df_long <- summary_eta2 %>%
    dplyr::select(resolution, eta2, same_submarket_seq_rate, modularity, department_NMI, maille_habitat_NMI, city_NMI) %>%
    tidyr::pivot_longer(
      cols = c(eta2, modularity, same_submarket_seq_rate, department_NMI, maille_habitat_NMI, city_NMI),
      names_to = "metric",
      values_to = "value"
    )

  # --- 3. Define colors ---
  metric_colors <- c(
    eta2 = "steelblue",
    same_submarket_seq_rate = "magenta",
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
        same_submarket_seq_rate = "unique submarket search rate",
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
      label = paste0("Max Eta² value\n(resolution = ",
                     round(summary_eta2$resolution[which.max(summary_eta2$eta2)], 2),
                     ")"),
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
      label = paste0("Convergence value\n(resolution = ",
                     round(optimal_row$resolution, 2), ")"),
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

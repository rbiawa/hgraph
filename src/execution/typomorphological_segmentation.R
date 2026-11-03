#########################################################################################
# A script to perform a typomorphological segmentation
# 
# 2026/08 Raphaël Biawa Diegamounoua
#
# source("src/execution/typomorphological_segmentation.R")
#########################################################################################



retrieve_package("lme4")
retrieve_package("performance")
retrieve_package("MuMIn")

# 1. Extract GPS coordinates (X, Y) from sf geometry
coords <- st_coordinates(pts$geometry)

# 2. Prepare the feature matrix (Physical attributes + Spatial coordinates)
# Handle categorical variable 'item_type' via dummy encoding
item_type_dummies <- model.matrix(~ item_type - 1, data = pts)

# Combine quantitative features, dummies, and spatial coordinates
features_df <- data.frame(
  longitude  = coords[, 1],
  latitude   = coords[, 2],
  area       = pts$area,
  room_count = pts$room_count
) %>%
  cbind(item_type_dummies)

# Check for missing values before scaling
complete_cases_idx <- complete.cases(features_df)

# 3. Standardize all variables (mean = 0, sd = 1)
scaled_features <- scale(features_df[complete_cases_idx, ])

nclust <- length(unique(pts$optim_submarket))

# 4. Perform K-Means clustering with K = nclust
set.seed(123)
features_kmeans <- kmeans(
  scaled_features,
  centers = nclust,
  nstart = 25,
  iter.max = 100
)

# 5. Assign cluster IDs back to the dataset
pts$cluster_attr_gps <- NA
pts$cluster_attr_gps[complete_cases_idx] <- as.factor(features_kmeans$cluster)


aricode::NMI(as.vector(pts$optim_submarket),
  as.vector(pts$cluster_attr_gps)
)


#=========================
# Null model
#=========================

# 6. Fit the Null Linear Mixed Model (REML)
model_0_attr_gps <- lme4::lmer(
  log(sqm_price) ~ 1 + (1 | cluster_attr_gps),
  data = pts
)

# 7. Summary and ICC computation
summary(model_0_attr_gps)
performance::icc(model_0_attr_gps)


#================================
# Model with variables at lavel 1
#================================

model_1_attr_gps <- lme4::lmer(log(sqm_price) ~ 1 + log_area + room_count + item_type +
    (1 | cluster_attr_gps),
  data = pts
)

summary(model_1_attr_gps)

performance::icc(model_1_attr_gps)

MuMIn::r.squaredGLMM(model_1_attr_gps)

performance::r2_nakagawa(model_1_attr_gps)


#===========================
# Plot submarkets
#===========================

retrieve_package("Polychrome")

dir.create("out/png/submarkets/typomorphological_clustering",
           recursive = TRUE, showWarnings = FALSE)


# Submarket colors
submarket_colors_typo <- Polychrome::createPalette(nclust, seedcolors = c("#ff0000", "#00ff00", "#0000ff"))


submarkets_typo <- sort(unique(pts$cluster_attr_gps))


#======================
# Plotting loop
#======================

retrieve_package("cowplot")

for (sm in submarkets_typo) {

  #=========================================================
  # Filter points for the current submarket
  #=========================================================
  pts_sm <- pts %>% filter(cluster_attr_gps == sm)
  col_sm <- submarket_colors_typo[as.integer(sm)]

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
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      legend.position = "right"
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
      size = 9        # Text size
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
      "out/png/submarkets/typomorphological_clustering/",
      LOCALITY_NAME_OUT, "_typo_submarket_", sm, ".png"
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
  col_sm <- submarket_colors_typo[as.integer(pts$cluster_attr_gps)]

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
    ggtitle(paste("Submarkets in", TITLE_REGION)) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      legend.position = "right"
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
      size = 9        # Text size
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
      "out/png/submarkets/typomorphological_clustering/",
      LOCALITY_NAME_OUT, "_typo_submarkets_all", ".png"
    ),
    plot = p,
    width = 8,
    height = 6,
    dpi = 300,
    bg = "white"
  )
}

#########################################################################################
# Housing price GWR modeling
# 
# 2025/09 Raphaël Biawa Diegamounoua
#
# source("src/modeling/gwr.R")
#########################################################################################

# contextual data : https://www.insee.fr/fr/outil-interactif/7737357/documentation.html#rp_carreaux
# https://www.insee.fr/fr/information/3544265


#install.packages('languageserver', repos='https://cran.r-project.org/')


#==============================================
#
#               SCALABLE GWR
#
#==============================================


#======================
# Packages
#======================

retrieve_package("GWmodel")
retrieve_package("scgwr")
retrieve_package("reticulate")
retrieve_package("sp")


#?GWmodel::gwr.scalable()
#?scgwr::scgwr()


#======================
# Linear model
#======================


# 1. Modèle OLS classique (sans pondération spatiale)
mod_ols <- lm(log(sqm_price) ~ log_area + room_count + item_type, data = pts)

# 2. Comparaison OLS vs GWR
summary(mod_ols)$adj.r.squared    # R² OLS
AIC(mod_ols)

#======================
# GWR modeling
#======================

# 1. Extraction des coordonnées et conversion en SpatialPointsDataFrame
coords <- st_coordinates(pts)
pts_sp <- as(pts, "Spatial")

# 2. Formule hédonique type (ex. log du prix en fonction de la surface et des pièces)
# Remplacez/ajoutez des variables explicatives selon vos besoins
formule <- log(sqm_price) ~ log_area + room_count + item_type

# 3. Exécution de gwr.scalable
# bw.adapt = nombre de voisins (ex: 100 ou 500)
res_scagwr <- gwr.scalable(
  formula = formule,
  data = pts_sp,
  bw.adapt = 500
)

# 4. Consultation des résultats
summary(res_scagwr)


res_scagwr$GW.diagnostic


#pts$coef_log_area <- res_scagwr$SDF$log_area


#================================
# GWR market segmentation
#================================


names(res_scagwr$SDF)



# 3. Standardize all variables (mean = 0, sd = 1)
scaled_gwr_coeff <- scale(res_scagwr$SDF[, 1:4] %>%
    as.data.frame()
)


nclust <- length(unique(pts$optim_submarket))

# 4. Perform K-Means clustering with
set.seed(123)
gwr_submarket <- kmeans(
  scaled_gwr_coeff,
  centers = nclust,
  nstart = 25,
  iter.max = 100
)

# 5. Assign cluster IDs back to the dataset
pts$gwr_submarket <- NA
pts$gwr_submarket <- as.factor(gwr_submarket$cluster)


aricode::NMI(as.vector(pts$optim_submarket),
  as.vector(pts$gwr_submarket)
)


aricode::NMI(as.vector(pts$gwr_submarket),
  as.vector(pts$cluster_attr_gps)
)

#=========================
# Null model
#=========================

# 6. Fit the Null Linear Mixed Model (REML)
model_0_gwr_submarket <- lme4::lmer(
  log(sqm_price) ~ 1 + (1 | gwr_submarket),
  data = pts
)

# 7. Summary and ICC computation
summary(model_0_gwr_submarket)
performance::icc(model_0_gwr_submarket)


#================================
# Model with variables at lavel 1
#================================

model_1_gwr_submarket <- lme4::lmer(log(sqm_price) ~ 1 + log_area + room_count + item_type +
    (1 | gwr_submarket),
  data = pts
)

summary(model_1_gwr_submarket)

performance::icc(model_1_gwr_submarket)

MuMIn::r.squaredGLMM(model_1_gwr_submarket)

performance::r2_nakagawa(model_1_gwr_submarket)




#===========================
# Plot submarkets
#===========================

retrieve_package("Polychrome")

dir.create("out/png/submarkets/gwr_clustering",
           recursive = TRUE, showWarnings = FALSE)


# Submarket colors
submarket_colors_gwr <- Polychrome::createPalette(nclust, seedcolors = c("#ff0000", "#00ff00", "#0000ff"))


submarkets_gwr       <- sort(unique(pts$gwr_submarket))


#======================
# Plotting loop
#======================

retrieve_package("cowplot")

for (sm in submarkets_gwr) {

  #=========================================================
  # Filter points for the current submarket
  #=========================================================
  pts_sm <- pts %>% filter(gwr_submarket == sm)
  col_sm <- submarket_colors_gwr[as.integer(sm)]

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
      "out/png/submarkets/gwr_clustering/",
      LOCALITY_NAME_OUT, "_gwr_submarket_", sm, ".png"
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
  col_sm <- submarket_colors_gwr[as.integer(pts$gwr_submarket)]



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
    ggtitle(paste("Submarkets in ", TITLE_REGION)) +
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
      "out/png/submarkets/gwr_clustering/",
      LOCALITY_NAME_OUT, "_gwr_submarkets_all", ".png"
    ),
    plot = p,
    width = 8,
    height = 6,
    dpi = 300,
    bg = "white"
  )
}

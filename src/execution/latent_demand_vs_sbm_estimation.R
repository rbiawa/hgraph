#########################################################################################
# Compute latent demand per submarket
# 
# 2026/09 Raphaël Biawa Diegamounoua
#
# source("src/execution/latent_demand_vs_sbm_estimation.R")
#########################################################################################


#source("src/execution/submarket_plot.R")
#source("src/execution/typomorphological_segmentation.R")
#source("src/modeling/gwr.R")
#source("src/modeling/hedonic_price_modeling/multilevel_price_modeling.R")
#source("src/execution/export_assortativity.R")
#source("src/execution/multilevel_model_exportation.R")

#=====================================
# Import bimonthly data clicks data
#=====================================

{
  events_2m_1 <- read_parquet("in/raw/Se_loger/data_guilhem_01_02_2021.parquet") %>%
    as.data.table()
  
  events_2m_1[, order := 1:nrow(events_2m_1)]
  
  
  events_2m_2 <- read_parquet("in/raw/Se_loger/data_guilhem_03_04_2021.parquet") %>%
    as.data.table()
  
  events_2m_2[, order := 1:nrow(events_2m_2)]
  
  
  events_2m_3 <- read_parquet("in/raw/Se_loger/data_guilhem_05_06_2021.parquet") %>%
    as.data.table()
  
  events_2m_3[, order := 1:nrow(events_2m_3)]
  
  events_2m_4 <- read_parquet("in/raw/Se_loger/data_guilhem_07_08_2021.parquet") %>%
    as.data.table()
  
  events_2m_4[, order := 1:nrow(events_2m_4)]
  
  events_2m_5 <- read_parquet("in/raw/Se_loger/data_guilhem_09_10_2021.parquet") %>%
    as.data.table()
  
  events_2m_5[, order := 1:nrow(events_2m_5)]
  
  events_2m_6 <- read_parquet("in/raw/Se_loger/data_guilhem_11_12_2021.parquet") %>%
    as.data.table()
  
  events_2m_6[, order := 1:nrow(events_2m_6)]

}




events_12m_conc <- rbind(events_2m_1, events_2m_2,
                         events_2m_3, events_2m_4,
                         events_2m_5, events_2m_6)

rm(events_2m_1, events_2m_2,
   events_2m_3, events_2m_4,
   events_2m_5, events_2m_6)

events_12m_conc[, date := as.Date(date, format = "%Y%m%d")]

if (interactive()) View(head(events_12m_conc))


#=====================================
# Contact data
#=====================================


mail_phone <- read_parquet("in/raw/Se_loger/mail_phone/mail_phone_2021.parquet") %>%
  as.data.table()


if (interactive()) View(head(mail_phone))

#==================================
#
# Revisited listings latent demande
#
#==================================

# --- 1. Identify revisited listings ---
# A listing is revisited if the same user sees it at least twice
setDT(events_12m_conc)


revisited_listings <- events_12m_conc[, .N, by = .(fullvisitorid, id_listing)][
  N > 1, 
  .(fullvisitorid, id_listing)
]

if (interactive()) View(head(revisited_listings))

latent_demand_revisited <- revisited_listings %>%
  .[, .(unique_clicks = uniqueN(fullvisitorid)), by = id_listing] %>%
  .[order(-unique_clicks)]

head(latent_demand_revisited)


#==================================
#
# Contact listings latent demande
#
#==================================


if (interactive()) View(head(mail_phone))

latent_demand_contact <- mail_phone %>%
  .[, .(unique_clicks = uniqueN(fullvisitorid)), by = id_listing] %>%
  .[order(-unique_clicks)]


head(latent_demand_contact)


#================================
# Latent demand per sbmkt
#================================

#======================================================================
# On revisited listings

# Convert to data.table if not already done
pts_dt <- as.data.table(pts)
setDT(latent_demand_revisited)

# Left join on id_listing (keeps only rows from pts)
pts_rev <- merge(
  pts_dt,
  latent_demand_revisited,
  by = "id_listing",
  all = FALSE
)

# Fill NA values with 0 for listings without revisits
pts_rev[is.na(unique_clicks), unique_clicks := 0]

# Calculate mean and median clicks per submarket
stats_submarket_rev <- pts_rev[, .(
  nb_listings   = .N,
  mean_clicks   = mean(unique_clicks),
  median_clicks = median(unique_clicks)
), by = optim_submarket][order(-mean_clicks)]

stats_submarket_rev$optim_submarket <- as.character(stats_submarket_rev$optim_submarket)

#======================================================================
# On contacted listings


# Convert to data.table if not already done
setDT(latent_demand_contact)

# Inner join: keep only listings present in both tables
pts_contact <- merge(
  pts_dt,
  latent_demand_contact,
  by = "id_listing",
  all = FALSE
)

# Compute mean and median clicks per optim_submarket
stats_submarket_contact <- pts_contact[, .(
  nb_listings   = .N,
  mean_clicks   = mean(unique_clicks),
  median_clicks = median(unique_clicks)
), by = optim_submarket][order(-mean_clicks)]

stats_submarket_contact$optim_submarket <- as.character(stats_submarket_contact$optim_submarket)


#================================
# Retrieve data to submarkets
#================================

V(meta_graph)$mean_clicks_rev <- stats_submarket_rev$mean_clicks[
  match(V(meta_graph)$name, stats_submarket_rev$optim_submarket)
]

V(meta_graph)$median_clicks_rev <- stats_submarket_rev$median_clicks[
  match(V(meta_graph)$name, stats_submarket_rev$optim_submarket)
]

V(meta_graph)$mean_clicks_contact <- stats_submarket_contact$mean_clicks[
  match(V(meta_graph)$name, stats_submarket_contact$optim_submarket)
]

V(meta_graph)$median_clicks_contact <- stats_submarket_contact$median_clicks[
  match(V(meta_graph)$name, stats_submarket_contact$optim_submarket)
]


#==================================
# Correlation test
#==================================

# Median

cor.test(vertex_attr(meta_graph, "median_clicks_rev"), vertex_attr(meta_graph, "median_sqm_price"), method = "pearson")

cor.test(vertex_attr(meta_graph, "median_clicks_contact"), vertex_attr(meta_graph, "median_sqm_price"), method = "pearson")

# Mean

cor.test(vertex_attr(meta_graph, "mean_clicks_rev"), vertex_attr(meta_graph, "median_sqm_price"), method = "pearson")

cor.test(vertex_attr(meta_graph, "mean_clicks_contact"), vertex_attr(meta_graph, "median_sqm_price"), method = "pearson")


#===========================================
# Search containment
#===========================================

# 1. Convert to data.table and drop geometry

submarket_vars <- c("maille_habitat", "sl_insee_city_id",
                    "cluster_attr_gps", "gwr_submarket", "optim_submarket")

# 2. Join datasets and FILTER users with > 1 listing immediately
joined_dt <- pts_dt[revisited_listings, on = "id_listing", nomatch = NULL]

# Keep only users who have visited at least 2 listings in total
valid_users <- joined_dt[, .N, by = fullvisitorid][N > 1, fullvisitorid]
joined_dt <- joined_dt[fullvisitorid %in% valid_users]

# 3. Streamlined function for each submarket
calc_containment_dt <- function(var_name, dt) {
  
  # Group by user and count distinct submarkets (ignoring NAs for this variable)
  stats_dt <- dt[!is.na(get(var_name)),
                 .(nb_submarkets = uniqueN(get(var_name))),
                 by = fullvisitorid]
  
  total_users <- nrow(stats_dt)
  single_submarket_users <- stats_dt[nb_submarkets == 1, .N]
  containment_rate <- (single_submarket_users / total_users) * 100
  
  data.table(
    submarket_type   = var_name,
    total_users_min2 = total_users,
    single_submarket_users = single_submarket_users,
    containment_rate_pct   = sprintf("%.2f", containment_rate)
  )
}

# 4. Compute results
containment_results <- rbindlist(lapply(submarket_vars, calc_containment_dt, dt = joined_dt))
print(containment_results)




if (exists("metrics")) {

  # Map names directly
  mapping <- c(
    "maille_habitat"   = "model_1_mh",
    "sl_insee_city_id" = "model_1_city",
    "cluster_attr_gps" = "model_1_attr_gps",
    "gwr_submarket"    = "model_1_gwr_submarket",
    "optim_submarket"  = "model_conconsult_1"
  )

  # Reshape containment_results
  new_row <- as.data.table(
    c(list(Metric = "RR (\\%)"),
      setNames(as.list(containment_results$containment_rate_pct),
              mapping[containment_results$submarket_type]))
  )

  names(new_row) == names(metrics)
  # Combine with metrics
  metrics_updated <- rbind(as.data.table(metrics), new_row, fill = TRUE)

  print(metrics_updated)



}


if (exists('metrics_updated')) {

  #==============================
  # Export into .tex
  #==============================

  filename_multilevel_updated <- paste0(
    "out/tex/submarkets/",
    LOCALITY_NAME_OUT, "_price_multimodels_comparison_all", ".tex"
  )

  export_region_tex(metrics_updated, filename_multilevel_updated,
    clustering_names = c(
      "Maille habita",
      "Munici\\allowbreak{}pality",
      "Typo and loc",
      "Price GWR",
      "Co-cons"
    )
  )

}

#==========================
# Execute merging
#==========================

if (TRUE) {
    merge_tex_tables(
    file_region1  = "out/tex/submarkets/IDF_price_multimodels_comparison_all.tex",
    file_region2  = "out/tex/submarkets/HDF_price_multimodels_comparison_all.tex",
    label_region1 = "Île-de-France (IdF)",
    label_region2 = "Nord-Pas-de-Calais (NPdC)",
    output_file   = "out/tex/submarkets/merged_table_regions_all.tex"
  )

}

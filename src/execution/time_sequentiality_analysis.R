#########################################################################################
# A script to perform submarket run
# 
# 2026/05 Raphaël Biawa Diegamounoua
#
# source("src/execution/time_sequentiality_analysis.R")
#########################################################################################


#source("src/packages_loading.R")


#=====================================
# Import fourmonthly clicks data
#=====================================


events_4m_1 <- read_parquet("in/raw/Se_loger/date_clicks/date_clicks_2021_1.parquet") %>%
  as.data.table()

events_4m_2 <- read_parquet("in/raw/Se_loger/date_clicks/date_clicks_2021_2.parquet") %>%
  as.data.table()

events_4m_3 <- read_parquet("in/raw/Se_loger/date_clicks/date_clicks_2021_3.parquet") %>%
  as.data.table()



events_12m <- rbind(events_4m_1, events_4m_2, events_4m_3)

rm(events_4m_1, events_4m_2, events_4m_3)

gc()

events_12m[, date := as.Date(date, format = "%Y%m%d")]


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

#=========================================================
# 0. Prepare click logs and listing info, align them, merge them
#=========================================================

# Click logs (event-level, true log order)
events_dt <- as.data.table(events_12m)
events_dt[, date := as.Date(date)]
events_dt[, order_global := .I]   # keep the natural log order

# Event index within each visit (to align events with listings)
events_dt[, event_id := seq_len(.N), by = .(fullvisitorid, date, visitid)]

# Listing info (visit-level, no hour)
events_conc <- as.data.table(events_12m_conc)
events_conc[, date := as.Date(date)]
events_conc[, event_id := seq_len(.N), by = .(fullvisitorid, date, visitid)]

# Merge on visit keys + event index (prevents cartesian explosion)
events_merged <- merge(
  events_dt,
  events_conc,
  by = c("fullvisitorid", "date", "visitid", "event_id"),
  all.x = TRUE,
  suffixes = c("_click", "_listing")
)


if (interactive()) head(events_merged, 1000) %>% View()

#=========================================================
# 2. Deduplicate identical events within visit-hour
#=========================================================

events_unique <- unique(
  events_merged,
  by = c("fullvisitorid", "date", "hour", "visitid")
)


#=========================================================
# 3. Build a real chronological index (t_real)
#=========================================================

events_unique[, date_num := as.numeric(date)]

events_unique[, t_real :=
  data.table::frankv(
    list(date_num, hour, visitid),
    ties.method = "dense"
  )
]


#=========================================================
# 4. Compute Kendall correlation (descriptive only)
#=========================================================

if (FALSE) {
  user_corr <- events_unique[
  , .(
      corr = suppressWarnings(cor(order_global, t_real, method = "kendall")),
      n_events = .N
    ),
  by = fullvisitorid
]

summary(user_corr$corr)
}

#=========================================================
# 5. Classify users by strict monotonicity of order_global
#=========================================================

user_direction <- events_unique[
  order(t_real),  # ensure real chronological order
  .(
    direction = {
      og <- order_global
      if (all(diff(og) > 0)) {
        1L     # strictly increasing
      } else if (all(diff(og) < 0)) {
        -1L    # strictly decreasing
      } else {
        0L     # inconsistent order
      }
    },
    n_events = .N
  ),
  by = fullvisitorid
]


freq(user_direction$direction)

#=========================================
# 6. Keep only users with consistent order (+1 or -1)
#=========================================



valid_users <- user_direction[direction != 0, fullvisitorid]


# IMPORTANT: filter from the full merged table, not the deduplicated one
events_valid <- events_merged[
  fullvisitorid %in% valid_users
]


#=========================================================
# 7. Split users into +1 and -1 groups
#=========================================================

users_pos <- user_direction[direction == 1, fullvisitorid]
users_neg <- user_direction[direction == -1, fullvisitorid]

events_pos <- events_valid[fullvisitorid %in% users_pos]
events_neg <- events_valid[fullvisitorid %in% users_neg]


#=========================================================
# 8. Reverse the order for users with direction = -1
#=========================================================

events_neg <- events_neg[
  order(fullvisitorid, -order_global)   # reverse order within each user
]


#=========================================================
# 9. Recombine +1 and -1 users
#=========================================================

events_clean <- rbind(events_pos, events_neg)

if (interactive()) head(events_clean, 1000) %>% View()

#=========================================================
# 10. Rebuild a clean sequential order per user
#=========================================================

events_clean[, order_clean := seq_len(.N), by = fullvisitorid]

if (interactive()) head(events_clean, 1000) %>% View()


#=========================================================
# 11. Memory cleanup: remove intermediate tables
#=========================================================

rm(events_dt)        # raw click table
rm(events_conc)      # raw listing table
rm(events_merged)    # merged raw table
rm(events_unique)    # deduplicated table
rm(events_valid)     # filtered users before splitting
rm(events_pos)       # positive direction users
rm(events_neg)       # negative direction users

# Optional: remove user_direction if not needed anymore
# rm(user_direction)

gc()  # trigger garbage collection


dir.create("out/Rdata/events/",
           recursive = TRUE, showWarnings = FALSE)

filename_events_clean <- paste0(
  "out/Rdata/submarkets_sequences/",
  LOCALITY_NAME_OUT, "_", YEAR, "_events_clean", ".RData"
)

save(events_clean,
  file = filename_events_clean
)

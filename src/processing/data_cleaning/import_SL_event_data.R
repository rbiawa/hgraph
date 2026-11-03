##############################################################################
# import events yearly data.
# 
# 2026/02 Raphaël Biawa Diegamounoua
#
# source("src/processing/data_cleaning/import_SL_event_data.R")
##############################################################################



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

gc()

# Same row number ?
nrow(events_12m_conc) == nrow(events_12m)



#======================
# Retrieving id_listing
#======================

setorder(events_12m_conc, fullvisitorid, date, visitid)

setorder(events_12m, fullvisitorid, date, visitid, hour)


events_12m[, order := seq_len(.N), by = .(fullvisitorid, date, visitid)]

events_12m_conc[, order := seq_len(.N), by = .(fullvisitorid, date, visitid)]




events_year <- events_12m[
  events_12m_conc,
  on = .(fullvisitorid, date, visitid, order)
]
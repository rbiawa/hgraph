
#########################################################################################
# Process SeLoger dataset
# 
# 2025/06 Raphaël Biawa Diegamounoua
#
# source("src/processing/data_cleaning/event_data_SL.R")
#########################################################################################





##################################################################
##                                                              ##
##                   EVENT DATA CLEANING                        ##
##                                                              ##
##################################################################

if (FALSE) {
  
# 1: Load the data for the specified year ---

cat("Step 1: Load the data for the specified year \n")

events <- read_parquet("in/raw/Se_loger/data_guilhem_01_02_2021.parquet") %>%
  as.data.table()

events[, order := 1:nrow(events)]

#events <- load_parquet_year("in/raw/Se_loger", YEAR)

#print(summary(as.Date(events$date, format = "%Y%m%d"), na.rm = T))
}

gc()



# 2. User Behavior Metrics Calculation ---

cat("2. User Behavior Metrics Calculation \n") 


test <- features %>% 
  mutate(invalide_com = !(insee_city_id %in% geom_sf_cities$INSEE_COM))

table(test$invalide_com)

freq(test$invalide_com)


test_3 <- features %>% 
  filter(!(insee_city_id %in% vertex.attributes(city_contig_graph)$name))


user_metrics <- events[
  features[insee_city_id %in% vertex.attributes(city_contig_graph)$name][, !"geometry", with = FALSE],
  on = "id_listing",
  nomatch = 0
]



test_1 <- user_metrics %>% 
  mutate(invalide_com = !(id_listing %in% features$id_listing))

table(test_1$invalide_com)

freq(test_1$invalide_com)


test_2 <- user_metrics %>% 
  mutate(invalide_com = !(insee_city_id %in% vertex.attributes(city_contig_graph)$name))


freq(test_2$invalide_com)





user_metrics <- user_metrics[order(fullvisitorid, id_listing)
][
  , .SD[1], by = .(fullvisitorid, id_listing)
]




# Locality contiguity variables

user_metrics <- check_connectivity(user_metrics
                                   , contig_graph   = dep_contig_graph
                                   , user_id        = "fullvisitorid"
                                   , loc_col        = "dep_ID"
                                   , contiguous_col = "dep_contig")


## City contiguity


test_4 <- user_metrics %>% 
  filter(!(insee_city_id %in% vertex.attributes(city_contig_graph)$name))



user_metrics <- check_connectivity(user_metrics
                                   , contig_graph   = city_contig_graph
                                   , user_id        = "fullvisitorid"
                                   , loc_col        = "insee_city_id"
                                   , contiguous_col = "city_contig")


if(interactive()) View(head(user_metrics, 100))

# user metrics variables

user_metrics[, date := as.Date(date, format = "%Y%m%d")]


{
  index_start_time   <- Sys.time()
  
  user_metrics <- user_metrics[, .(
    # Consultation behavior
    total_listings = .N,
    sessions = uniqueN(visitid),
    avg_listings_per_session = .N / uniqueN(visitid),
    consultation_duration = as.numeric(max(date) - min(date)) + 1,
    
    # Listing characteristics
    mean_price = mean(price),
    price_gini = ineq::Gini(price),
    price_iqr  = IQR(price, na.rm = TRUE) / median(price, na.rm = TRUE),
    mean_area  = mean(area),
    area_gini  = ineq::Gini(area),
    area_iqr   = IQR(area, na.rm = TRUE) / median(area, na.rm = TRUE),
    #room_mean = mean(room_count),
    #room_cv = sd(room_count) / mean(room_count),
    
    # Property type diversity
    
    type_simpson = simpson.unb(table(item_type)),
    
    room_count_simpson = simpson.unb(table(fct_room_count)),
    
    main_property_type = names(sort(table(item_type), decreasing = TRUE))[1],
    
    # City diversity
    #city_simpson = simpson(table(city)),
    city_simpson = simpson.unb(table(insee_city_id)),
    
    # Subregion diversity
    #dep_simpson  = simpson(table(subregion)),
    dep_simpson = simpson.unb(table(subregion)),
    
    # Locality contiguity
    dep_contig = as.factor(dep_contig)  # Preserve locality contiguity
    ,city_contig = as.factor(city_contig)
  ),
  
  by = fullvisitorid
  
  ][, main_property_type := as.factor(main_property_type)]
  
  
  Sys.time() - start_time 

}


save.image("F:/These_Raphael/Rdata/20260130_user_metrics.RData")


user_metrics_1 <- user_metrics


user_metrics <- user_metrics[, .SD[1], by = fullvisitorid][total_listings > 1, ]


# 3. Remove users that have consulted only one listing ---

cat("3. Remove users that have consulted only one listing \n") 


#user_metrics <- user_metrics[total_listings > 2, ]

if (interactive()) View(head(user_metrics, 100))

users_with_na <- user_metrics[!complete.cases(user_metrics)]

cat(nrow(users_with_na), "with a missing value \n")

#############################################
##             VARIABILITY INDEX           ##
#############################################



lapply(c(
  "users_with_na"
),
rm_if_exists)

gc()


pdf("out/pdf/outliers.pdf")


boxplot(user_metrics[, sapply(user_metrics, is.numeric), with = FALSE], las = 2)


# With scaled variables

boxplot( user_metrics[, lapply(.SD, scale), .SDcols = is.numeric], 
         las = 2,
         main = "Boxplots with scaled variables")


dev.off()


# 4. Serious User Identification ---------------------------------------

cat("4. Serious User Identification \n") 


summary(user_metrics[, .(total_listings,
                         sessions,
                         avg_listings_per_session,
                         consultation_duration,
                         mean_price,
                         mean_area,
                         price_cv,
                         area_cv,
                         #room_cv,
                         room_count_simpson,
                         type_simpson,
                         #city_simpson,
                         dep_simpson,
                         main_property_type,
                         dep_contig
)])




#Calculate variability_index score
user_metrics[, variability_index := (
  price_cv
  + area_cv
  #+ room_cv
  + room_count_simpson
  + type_simpson
  #+ city_simpson
  + dep_simpson
)
]

summary(user_metrics$variability_index)

# Identify 20% serious users
#serious_users <- user_metrics[variability_index <= quantile(variability_index, 0.2)]


num_plot <- plot_continuous_variables(user_metrics)

pdf("out/pdf/other/user_metrics_num_plot.pdf")


for (var in names(num_plot)) {
  grid.newpage()
  grid.draw(num_plot[[var]])
}

dev.off()


# log plot


num_plot_log <- plot_continuous_variables(user_metrics, "log")

pdf("out/pdf/other/user_metrics_num_plot_log.pdf")


for (var in names(num_plot_log)) {
  grid.newpage()
  grid.draw(num_plot_log[[var]])
}

dev.off()






cat_plot <- plot_categorical_variables(user_metrics %>%
                                         dplyr::select(- fullvisitorid)
)

dir.create("out/pdf/other/")

pdf("out/pdf/other/user_metrics_cat_plot.pdf")


for (var in names(cat_plot)) {
  grid.draw(cat_plot[[var]])
}

dev.off()

lapply(c(
  "num_plot"
  ,"cat_plot"
  # , "p"
),
rm_if_exists)

gc()




#########################################################################################
# Process SeLoger dataset
# 
# 2025/06 Raphaël Biawa Diegamounoua
#
# source("src/processing/data_cleaning/data_SL.R")
#########################################################################################






##################################################################
##                                                              ##
##                  FEATURES DATA CLEANING                      ##
##                                                              ##
##################################################################


# Retrieve listing features from SL
listing_feat_SL <- read_parquet("in/raw/Se_loger/Listing_features.parquet") %>%
  setDT()

# Compute subregion variable
listing_feat_SL[, subregion := substr(zipcode, start = 1, stop = 2)]


#===============================#
#       IDF and HDF data        #
#===============================#

idf_hdf_subregion_code <- c("02", "59", "60", "62"
                            , "75", "77", "78", "80"
                            , as.character(91:95))


idf_hdf_subregion_label <- c("Aisne", "Nord", "Oise", "Pas-de-Calais"
                             , "Paris", "Seine-et-Marne", "Yvelines", "Somme"
                             , "Essonne", "Hauts-de-Seine", "Seine-Saint-Denis"
                             , "Val-de-Marne", "Val-d'oise" )


# Subregion label

subregion_idf_hdf <- data.table(subregion = idf_hdf_subregion_code,
                                subregion_lab = idf_hdf_subregion_label)



# Filter data on HDF and IDF regions

listing_feat_SL_idf_hdf <- listing_feat_SL[subregion %in% idf_hdf_subregion_code, ]


listing_feat_SL_idf_hdf <- merge(listing_feat_SL_idf_hdf,
                                 subregion_idf_hdf,
                                 by = "subregion",
                                 all.x = TRUE
)


rm(listing_feat_SL, subregion_idf_hdf)


# Filter on houses and apartments


listing_feat_SL_idf_hdf <- listing_feat_SL_idf_hdf[
  item_type %in% c("ITEM_TYPE.APARTMENT", "ITEM_TYPE.HOUSE"),
]


freq(listing_feat_SL_idf_hdf$subregion_lab)

write_xlsx(freq(listing_feat_SL_idf_hdf$subregion_lab), "out/xlsx/raw_frequences_subregion_lab.xlsx")


# Price by square meter

listing_feat_SL_idf_hdf[, sqm_price := price/area]


## Square meter price analysis : Comparison with other sources (MeilleursAgents and SeLoger)



extern_sqm_price_MA <- read_excel("in/extern_data/extern_price_data.xlsx",
                                  sheet = "MA") %>%
  as.data.table()



 setnames(extern_sqm_price_MA,
          c("min_sqm_price", "mean_sqm_price", "max_sqm_price"),
          c("min_sqm_price_MA", "mean_sqm_price_MA", "max_sqm_price_MA"))


extern_sqm_price_MA[, source := NULL]

extern_sqm_price_SL <- read_excel("in/extern_data/extern_price_data.xlsx",
                                    sheet = "SL") %>%
  as.data.table()



setnames(extern_sqm_price_SL,
         c("min_sqm_price", "mean_sqm_price", "max_sqm_price"),
         c("min_sqm_price_SL", "mean_sqm_price_SL", "max_sqm_price_SL"))


extern_sqm_price_SL[, source := NULL]



extern_sqm_price <- merge(extern_sqm_price_MA[, subregion_lab := NULL],
  extern_sqm_price_SL[, subregion_lab := NULL],
  by = c("subregion", "item_type")
)


# Filtering sqm_price on extern sources


listing_feat_SL_idf_hdf <- merge(listing_feat_SL_idf_hdf
  , extern_sqm_price
  , by = c("subregion", "item_type")
  , all.x = TRUE
)


  
  # Filter data which sqm_price is out of range min and max between MA and SL extern data
  
listing_feat_SL_idf_hdf <-listing_feat_SL_idf_hdf[
                                                  sqm_price >= min(min_sqm_price_MA, min_sqm_price_SL) 
                                                  & 
                                                 sqm_price <= max(max_sqm_price_MA, max_sqm_price_SL)
                                                  , ]



#=============================#
#        GPS ANALYSIS         #
#=============================#


# Filter valid Gps 

listing_feat_SL_idf_hdf <- setDT(listing_feat_SL_idf_hdf)[
  (abs(latitude) <= 90 &
     abs(longitude) <= 180) &
    !(latitude == 0 & longitude == 0),
]



# Convertion into sf

listing_feat_SL_idf_hdf <- st_as_sf(listing_feat_SL_idf_hdf
                                    ,coords = c("longitude", "latitude")
                                    ,crs = 4326)

# Keep matching gps 

listing_feat_SL_idf_hdf <- listing_feat_SL_idf_hdf %>%
  st_join(geom_sf_cities,
          left = TRUE)


listing_feat_SL_idf_hdf <- setDT(listing_feat_SL_idf_hdf)[
  subregion == INSEE_DEP
]

freq(listing_feat_SL_idf_hdf$subregion_lab)

write_xlsx(freq(listing_feat_SL_idf_hdf$subregion_lab), "out/xlsx/match_gps_frequences_subregion_lab.xlsx")

#######################################################
##           SL-INSEE city ID retrieving             ##
#######################################################


sl_city_insee_id <- read_excel("in/extern_data/sl_city_insee_id.xlsx") %>%
  as.data.table()


listing_feat_SL_idf_hdf <- merge(
  listing_feat_SL_idf_hdf[city != "-", ]
  , sl_city_insee_id[, .(city, sl_insee_city_id)]
  , by = c("city")
  , all.x = TRUE
)


if (interactive()) View(head(listing_feat_SL_idf_hdf, 100))





#######################################################
##                  Retrieve IGN data                ##
#######################################################

if (FALSE) {
  
  insee_200m_tile <- st_read("grille200m_metropole_gpkg/grille200m_metropole.gpkg")
 
  insee_200m_tile <- insee_200m_tile %>%
    st_join(geom_sf_data %>%
        st_transform(2154)
      , left = FALSE
    )
  
  
  # insee_200m_tile <- insee_200m_tile %>% 
  #   filter(INSEE_REG %in% c("11", "32"))
  
}


#===============================#
##     Contiguity matrix       ##
#===============================#



# subregion contiguity matrix


dep_contig <- flowcontig(bkg = geom_sf_departements
                         , code = "dep_ID"
                         , k=1
                         , algo = "automatic")

# Transformation of the contiguity list into a matrix if needed (set FALSE to TRUE)

if (FALSE){
  
  dep_mat_contig <- flowtabmat(dep_contig, matlist = "M")
} 


# Subregion contiguity graph

dep_contig_graph <- graph_from_data_frame(dep_contig[1:2],
  directed = FALSE
) %>% simplify(, remove.multiple = TRUE, remove.loops = TRUE)

if (interactive()) {
  plot(dep_contig_graph
       , vertex.label = NA
       , vertex.size = 5
       , arrow.size = .5
  )
}





# city contiguity matrix

if (TRUE) {
city_contig <- flowcontig(bkg = geom_sf_cities %>%
                          filter(INSEE_REG %in% c("11", "32"))
                          , code = "city_ID"
                          , k=1
                          , algo = "automatic")


# Transformation of the contiguity list into a matrix if needed (set FALSE to TRUE)


  
if(FALSE) city_mat_contig <- flowtabmat(city_contig, matlist = "M")



# city contiguity graph

  city_contig_graph <- graph_from_data_frame(city_contig[1:2],
                                             directed = FALSE) %>%
    simplify(, remove.multiple = TRUE, remove.loops = TRUE)

if (interactive()) {
  plot(city_contig_graph
       , vertex.label = NA
       , vertex.size = 1
       , arrow.size = .5
  )
}



is_connected(city_contig_graph)

igraph::is_simple(city_contig_graph)

}

#===========================================================#
##         Retrieve city and  insee_200m_tile's id         ##
#===========================================================#

listing_feat_SL_idf_hdf <- listing_feat_SL_idf_hdf %>%
  left_join(st_drop_geometry(geom_sf_departements) %>%
      dplyr::select(dep_ID, dep_Name, INSEE_DEP),
    by = "INSEE_DEP"
  )




listing_feat_SL_idf_hdf <- listing_feat_SL_idf_hdf %>% 
  mutate(city_match = sl_insee_city_id == city_ID
 )


freq(listing_feat_SL_idf_hdf$city_match)%>% 
  print()

#9.5% of non city match

# not_city_match <- listing_feat_SL_idf_hdf %>% 
#                    filter(city_match == FALSE)
# 
# writexl::write_xlsx(not_city_match, "not_city_match.xlsx")
# 
# rm(not_city_match)




features <- setDT(listing_feat_SL_idf_hdf) [
    ,.(subregion_lab, item_type, subregion, id_listing,
     city, sl_insee_city_id, price, area, room_count, transaction_type, zipcode,
     sqm_price, INSEE_COM, dep_ID, INSEE_DEP, INSEE_REG, city_ID, city_Name, dep_Name,
     city_match, geometry)
] %>% na.omit()


features <- features[room_count > 0 & transaction_type == "TRANSACTION_TYPE.SELL", ]




#==============================================================#
# Room count : must be greater or equal tha 9sqm accordint to
# Article 4 of "décret no 2002-120 du 30 janvier 2002" :
# https://www.legifrance.gouv.fr/loda/id/JORFTEXT000000217471
#==============================================================#

features <- features[, avg_room_sqm := (area/room_count)]

#[avg_room_sqm >= 9, ]

summary(features$avg_room_sqm)

bp <- boxplot(features$avg_room_sqm)

bp$out

summary(bp$out)

100*length(bp$out)/nrow(features)




## We use distribution outliers for time being, but it will be improved

features <- features[!features$avg_room_sqm %in% bp$out, ]

features[, fct_room_count := paste0(room_count, " rooms")]

freq(features$fct_room_count)



num_plot <- plot_continuous_variables(features)

pdf("out/pdf/other/features_num_plot.pdf")


for (var in names(num_plot)) {
  grid.newpage()
  grid.draw(num_plot[[var]])
}

dev.off()


cat_plot <- plot_categorical_variables(features %>%
    dplyr::select(subregion_lab, item_type, INSEE_REG, fct_room_count)
)


pdf("out/pdf/other/features_cat_plot.pdf")


for (var in names(cat_plot)) {
    grid.draw(cat_plot[[var]])
}

dev.off()



if (interactive()) slice_max(features, order_by = area, n = 100) %>% View()

lapply(c("extern_sqm_price_MA"
         , "extern_sqm_price_SL"
         , "listing_feat_SL_idf_hdf"
         , "event_01_02_2021"
         , "sl_city_insee_id"
         , "num_plot"
         , "cat_plot"
),
rm_if_exists)

gc()


if (TRUE) {





##################################################################
##                                                              ##
##                   EVENT DATA CLEANING                        ##
##                                                              ##
##################################################################


# 1: Load the data for the specified year ---

cat("Step 1: Load the data for the specified year \n")

events <- read_parquet("in/raw/Se_loger/data_guilhem_01_02_2021.parquet") %>%
  as.data.table()

events[, order := 1:nrow(events)]

#events <- load_parquet_year("in/raw/Se_loger", YEAR)

#print(summary(as.Date(events$date, format = "%Y%m%d"), na.rm = T))


gc()



# 2. User Behavior Metrics Calculation ---

cat("2. User Behavior Metrics Calculation \n") 

user_metrics <- events[
  features[, !"geometry", with = FALSE],
  on = "id_listing",
  nomatch = 0
]



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

user_metrics <- check_connectivity(user_metrics
                                   , contig_graph   = city_contig_graph
                                   , user_id        = "fullvisitorid"
                                   , loc_col        = "sl_insee_city_id"
                                   , contiguous_col = "city_contig")


if(interactive()) View(head(user_metrics, 100))

# user metrics variables

user_metrics[, date := as.Date(date, format = "%Y%m%d")]


user_metrics <- user_metrics[, .(
  # Consultation behavior
  total_listings = .N,
  sessions = uniqueN(visitid),
  avg_listings_per_session = .N / uniqueN(visitid),
  consultation_duration = as.numeric(max(date) - min(date)) + 1,

  # Listing characteristics
  mean_price = mean(price),
  price_cv = sd(price) / mean(price),
  mean_area = mean(area),
  area_cv = sd(area) / mean(area),
  #room_mean = mean(room_count),
  #room_cv = sd(room_count) / mean(room_count),

  # Property type diversity

  type_simpson = simpson.unb(table(item_type)),

  room_count_simpson = simpson.unb(table(fct_room_count)),

  main_property_type = names(sort(table(item_type), decreasing = TRUE))[1],

  # City diversity
  #city_simpson = simpson(table(city)),
  city_simpson = simpson.unb(table(sl_insee_city_id)),

  # Subregion diversity
  #dep_simpson  = simpson(table(subregion)),
  dep_simpson = simpson.unb(table(subregion)),

# Locality contiguity
  dep_contig = as.factor(dep_contig),  # Preserve locality contiguity
  city_contig = as.factor(city_contig)
),

by = fullvisitorid

][, main_property_type := as.factor(main_property_type)]





user_metrics <- user_metrics[, .SD[1], by = fullvisitorid]


# 3. Remove users that have consulted only one listing ---

cat("3. Remove users that have consulted only one listing \n") 


user_metrics <- user_metrics[total_listings > 1, ]

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
          city_simpson,
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
  + city_simpson
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


}

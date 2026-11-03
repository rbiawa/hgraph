#########################################################################################
# Process SeLoger dataset
# 
# 2025/06 Raphaël Biawa Diegamounoua
#
# source("src/processing/data_cleaning/listing_features_data_SL.R")
#########################################################################################






##################################################################
##                                                              ##
##                  FEATURES DATA CLEANING                      ##
##                                                              ##
##################################################################


# Retrieve listing features from SL
listing_feat_SL <- read_parquet("in/raw/Se_loger/Listing_features.parquet") %>%
  setDT() %>%
  dplyr::select(- "__index_level_0__")

listing_feat_SL_comp <- read_parquet("in/raw/Se_loger/listings_manquants.parquet") %>%
  setDT() %>%
  rename(id_listing = id)


names(listing_feat_SL) == names(listing_feat_SL_comp)


listing_feat_SL <- rbind(listing_feat_SL, listing_feat_SL_comp)


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



# Filter data on Nord-Pas-de Calais and IDF regions : rm c("02", "60", "80") also

idf_npdc_subregion_code <- c("59", "62"
                            , "75", "77", "78"
                            , as.character(91:95))


listing_feat_SL_idf_hdf <- listing_feat_SL[subregion %in% idf_npdc_subregion_code, ]


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


#===========================================
# Remove non unique id_listing
#===========================================

any(duplicated(features$id_listing))

sum(duplicated(features$id_listing))

dups <- features$id_listing[duplicated(features$id_listing)]
unique(dups)

length(dups)


features <- features[!(features$id_listing %in% dups), ]

any(duplicated(features$id_listing))

dir.create("out/pdf/other")

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


# Av-plots : https://www.geeksforgeeks.org/r-language/how-to-create-added-variable-plots-in-r/
# https://statistics.arabpsychology.com/create-added-variable-plots-in-r/



#=======================================
#
# Maille habitat
#
#=======================================

# http://dataviz.statistiques.developpement-durable.gouv.fr/maille_habitat/
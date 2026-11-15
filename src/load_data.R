################################################################################
# A script to load data
# 
# 
#
# source("src/load_data.R")
################################################################################



#==================================
# Load events dataset
#==================================

events <- read_parquet("in/events.parquet") %>%
  as.data.table()

events <- events %>%
  mutate(
    datetime = as.POSIXct(datetime)
  )


#==========================================
#   Laod mail_phone data : serious measure     
#==========================================


mail_phone <- read_parquet("in/mail_phone.parquet") %>%
  as.data.table()



freq(mail_phone$is_logged)

freq(mail_phone$event_action)

look_for(mail_phone)


action_summary <- mail_phone[, .(
  
  # Number of listings with at least one email sent
  nb_listing_mailed = uniqueN(id_listing[event_action == "mail_form-submitted"]),
  
  # Number of listings with at least one phone number display
  nb_listing_phone_disp = uniqueN(id_listing[event_action == "phone_display-number"])
  
), by = fullvisitorid]

summary(action_summary$nb_listing_phone_disp)
summary(action_summary$nb_listing_mailed)

rm(action_summary)


#==========================================
#   Load listings' features      
#==========================================

features <- read_parquet("in/features.parquet") %>%
  as.data.table()



#===============================#
#    Compute Contiguity matrices       
#===============================#

geom_sf_departments <- st_read("in/geom_sf_departments.gpkg")

geom_sf_cities <- st_read("in/geom_sf_cities.gpkg")


# subregion contiguity matrix


dep_contig <- flowcontig(bkg = geom_sf_departments
                         , code = "dep_ID"
                         , k=1
                         , algo = "automatic")

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
  city_contig <- flowcontig(bkg = geom_sf_cities
                            , code = "city_ID"
                            , k=1
                            , algo = "automatic")
  
  
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


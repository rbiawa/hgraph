#########################################################################################
# Extract SeLoger joint consultation graph
# 
# 2025/06 Raphaël Biawa Diegamounoua
#
# source("src/processing/graph/graph_SL.R")
#########################################################################################



##########################################################################################
# PROCESSING PARAMETERS 


USED_VARIABLES            <- c(LISTING_VAR_NAME,
                               "price",
                               "area",
                               "room_count",
                               "city",
                               "item_type")

INPUT_ATTRIBUTES_NAMES    <- c("city",
                               "price",
                               "area",
                               "room_count",
                               "item_type")

ANALYSIS_ATTRIBUTE        <- INPUT_ATTRIBUTES_NAMES

NODES_ATTRIBUTES          <- c("city",
                               "price",
                               "area",
                               "room_count",
                               "item_type")


INPUT_ATTRIBUTES_NAMES          <- c(INSEE_COLUMNS, INPUT_ATTRIBUTES_NAMES)
NODES_ATTRIBUTES                <- c(INSEE_COLUMNS, NODES_ATTRIBUTES)





#########################################################################################################
# BEGIN PROCESSING
#########################################################################################################





#load("out/Rdata/serious_search/work_dataspace_25022026.RData")

#load("out/Rdata/serious_search/events_features.RData")


######################################
# Filtering the data to retain only the desired locality (to be improved).



any(duplicated(features$id_listing))


if(!exists("events") & !exists("events_year")){

  events <- read_parquet("in/raw/Se_loger/data_guilhem_01_02_2021.parquet") %>%
  as.data.table()

events[, order := 1:nrow(events)]
}



if(!exists("events") & exists("events_year")) events <- events_year

events <- events[id_listing %in%
    filter_locality_optimal(features,
                            LOCALITY_VARIABLE_NAME,
                            LOCALITY_VAL)$id_listing,
]


# events <- set_min_listing_number(data = events,
#                                  USER_VARS_NAME,
#                                  LISTING_VAR_NAME,
#                                  min_listing_numb = MIN_LISTING_NUMBER
# )


#events[, revisits := .N, 
#       by = .(fullvisitorid, id_listing)]

head(events)


events_unique <- events[, .(revisits = .N), 
                        by = .(fullvisitorid, id_listing)]

if(interactive()) View(head(events_unique, 100))

summary(events_unique$revisits)

events_unique <- events_unique[revisits > 1,]

summary(events_unique$revisits)


events_unique[, n_listings_revisited := .N,
              by = fullvisitorid]

summary(events_unique$n_listings_revisited)
events_unique <- events_unique[n_listings_revisited > 1,]
summary(events_unique$n_listings_revisited)





######################################
#Graph extraction
{
  graph_extraction_start_time    <- Sys.time()

  # raw_graph <- extract_graph(data = events_unique
  #                            , searcher_id = USER_VARS_NAME
  #                            , listing_id  = LISTING_VAR_NAME
  #                            , event_order = EVENT_ID_VAR
  #                            , directed    = FALSE
  #                            , consec      = FALSE
  #                            , graph_type  = "listing"
  #)

  # Non directed graph

  vertices <- unique(rbind(
    events_unique[, .(name = fullvisitorid, type = FALSE)],
    events_unique[, .(name = id_listing, type = TRUE)]
  ))

  g_bip <- igraph::graph_from_data_frame(
    d = events_unique[, .(from = fullvisitorid, to = id_listing)],
    vertices = vertices,
    directed = FALSE
  )

  # Graph selection
  #proj_both <- igraph::bipartite_projection(g_bip, which = "both") # not recommanded here
  raw_graph <- igraph::bipartite_projection(g_bip, which = "true")



  graph_extraction_end_time     <- Sys.time()

  print(graph_extraction_end_time - graph_extraction_start_time)

}

raw_graph

dir.create("out/Rdata/graph", recursive = TRUE, showWarnings = FALSE)


graph_name <- paste0(paste(LOCALITY_NAME, collapse = "_"), "_graph")

if (TRUE) save(raw_graph,
               file = paste0("out/Rdata/graph/", graph_name, "_raw.RData"))

summary(E(raw_graph)$weight)

raw_graph <- delete_edges(raw_graph, E(raw_graph)[weight < 2])

summary(E(raw_graph)$weight)

# adding locality name, analysing scale and extraction duration

raw_graph$locality            <- LOCALITY_NAME

raw_graph$analysis_scale      <- ANALYSIS_SCALE

raw_graph$extraction_duration <- graph_extraction_end_time - graph_extraction_start_time




##            First plot of the raw graph       ##
##################################################


if(FALSE) {
  png(filename = "out/png/graph/raw_graph.png", width = 2400, height = 1800, res = 300)

  par(mar=c(0,0,0,0))

  plot(raw_graph, vertex.label=NA,vertex.size=.5,vertex.color="darkblue", edge.arrow.size=.3, alpha = 0.3)

  dev.off()
}

igraph::components(raw_graph)$csize |> summary()

######################################
# Exctracting the principal component

graph  <- select_graph(graph = raw_graph, largest_component = TRUE)

any_multiple(graph)

is_simple(graph)

is_connected(graph)

if(FALSE) {
  png(filename = "out/png/graph/graph.png", width = 2400, height = 1800, res = 300)

  par(mar=c(0,0,0,0))

  plot(graph, vertex.label=NA,vertex.size=.5,vertex.color="darkblue", edge.arrow.size=.3, alpha = 0.3)

  dev.off()
}


##############################
##  Adding node attributes ##
##############################

vertex.attributes(graph) <- vertex.attributes(graph) %>%
  as.data.frame() %>%
  left_join(features %>%
      dplyr::select(id_listing, price, area, room_count, item_type, city,
             city_ID, city_Name, dep_ID, dep_Name, INSEE_DEP,
             INSEE_COM, sl_insee_city_id, INSEE_REG, INSEE_DEP, geometry) %>%
      mutate(name = as.character(id_listing)),
    by = "name"
  )




#View(add_attributs_dataframe[1:1000,])

## Adding attributes to the graph             ##
################################################


## Gps coordinates----

V(graph)$x <- st_coordinates(
  st_as_sf(vertex.attributes(graph)%>%
  as.data.frame()
  )
  )[, 1]

V(graph)$y <- st_coordinates(
  st_as_sf(vertex.attributes(graph)%>%
  as.data.frame()
  )
  )[, 2]



is_connected(graph)


#  Price and area log

V(graph)$log_price <- log(V(graph)$price)

V(graph)$log_area <- log(V(graph)$area)


##############################
##  Adding edge attributes ##
##############################


### Distance between linked nodes


graph <- node_spatial_distance(graph = graph,
                               name = "distance")


###  Visualize
edge_attr(graph, "distance")

edge_attr(graph) %>% names()


if(TRUE) save(graph, 
              file = paste0("out/Rdata/graph/", graph_name, "_attr.RData"))


cor.test(edge_attr(graph, "weight"), edge_attr(graph, "distance"))

cor.test(edge_attr(graph, "weight"), edge_attr(graph, "distance"), method = "spearman")

#cor.test(edge_attr(graph, "weight"), edge_attr(graph, "distance"), method = "kendall")



#View(as.data.frame(vertex.attributes(graph))[1:1000,])


## One more plot

if(FALSE) {
  png("out/png/graph/graph_attr.png", width = 2400, height = 1800, res = 300)

  par(mar = c(0,0,0,0))

  plot(graph, vertex.label=NA,vertex.size=.2,vertex.color="darkblue", edge.arrow.size=.09, alpha = 0.09)

  dev.off()

}
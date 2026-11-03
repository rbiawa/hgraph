#########################################################################################
# A script to load raw data
# 
# 2026/03 Raphaël Biawa Diegamounoua
#
# source("src/quick_extract_graph.R")
#########################################################################################






#=======================
# Loading packages
#=======================

source("src/packages_loading.R")


#=======================
#Retrieve functions
#=======================

source("src/function_retrieving.R")


#================================
# Data
#================================

print("Data loading")

load("out/Rdata/serious_search/work_dataspace_25022026.RData")


events <- events[id_listing %in%
    filter_locality_optimal(features,
                            LOCALITY_VARIABLE_NAME,
                            LOCALITY_VAL)$id_listing,
]



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


if (interactive()) View(head(events_unique, 1000))

#================================
# Graph
#================================


 # Bipartite graph creation


dim(vertices)

length(unique(events_unique$fullvisitorid))

length(unique(events_unique$id_listing))

if (interactive()) View(head(vertices, 1000))



vertices <- unique(rbind(
  events_unique[, .(name = fullvisitorid, type = FALSE)],
  events_unique[, .(name = id_listing, type = TRUE)]
))




print("Graph extraction")

{
graph_extraction_start_time    <- Sys.time()

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


summary(E(raw_graph)$weight)


#================================
# Save Graph
#================================
print("Graph saving")

save(raw_graph,
     file = "out/Rdata/substitutability/raw_graph.RData")

print("Execution ended !")
#########################################################################################
# A script to cluster submarkets
# 
# 2026/09 Raphaël Biawa Diegamounoua
#
# source("src/execution/submarket_clustering.R")
#########################################################################################


source("src/execution/latent_demand_vs_sbm_estimation.R")




#=================================
# Retrieve data
#=================================


if (interactive()) igraph::vertex.attributes(meta_graph) %>%
  as.data.frame() %>% View()

submarket_data_frame <- igraph::vertex.attributes(meta_graph) %>%
  as.data.frame() %>%
  dplyr::select(name, x, y, median_price, median_sqm_price, median_area,
    room_count, main_item_type, main_dep, median_clicks_rev,
    median_clicks_contact, median_indic.menag.14, median_indic.rs.14, median_indic.vac.14,
    median_indic.prix_rev.12, median_indic.jeun.15, median_indic.transac.12,
    median_indic.soc.15, median_indic.suroc.15, median_indic.duroc.15
  )


cor.test(submarket_data_frame$median_sqm_price, submarket_data_frame$median_indic.prix_rev.12)

#=================================
# PCA
#=================================

if (interactive() & FALSE) {
  Factoshiny(submarket_data_frame)
}



if (FALSE){

  res.PCA <- PCA(submarket_data_frame[,-c(1)],
    quali.sup  = c(7, 8),
    quanti.sup = c(1, 2, 3),
    graph = FALSE
  )

  summary(res.PCA)

  if (interactive()) {
    plot.PCA(res.PCA,choix='var',title="Graphe des variables de l'ACP")
    plot.PCA(res.PCA,invisible=c('ind.sup'),title="Graphe des individus de l'ACP",label =c('ind','quali'))
  }








#=================================
# HCPC PCA
#=================================


  res.HCPC <- HCPC(res.PCA,
      nb.clust = -1,
      consol = FALSE,
      graph = FALSE
  )

  if (interactive()) {
    plot.HCPC(res.HCPC, choice = 'tree', title = 'Hierachical tree')

    plot.HCPC(res.HCPC, choice = 'map',draw.tree = FALSE, title='Factorial plane')

    plot.HCPC(res.HCPC, choice = '3D.map', ind.names=FALSE, 
      centers.plot=FALSE, angle=60, title = 'Hierachical tree on factorial plane')

    plot.HCPC(res.HCPC, choice='bar', title='Hierachical tree')

    plot.HCPC(res.HCPC, choice='bar', title='Hierachical tree')
  }


  names(res.HCPC$data.clust)

  submarket_cluster_des <- FactoMineR::catdes(res.HCPC$data.clust,
                                              num.var = ncol(res.HCPC$data.clust))

  if (interactive()) FactoMineR::plot.catdes(submarket_cluster_des)


  pdf(file = paste(GRAPH_PDF_SAVING_DIRECTORY,
                  paste(LOCALITY_NAME_OUT,
                        ANALYSE_SCALE_OUT,
                        "submarket_clustering.pdf",
                        sep = "_"),
                  sep = "/"),
    width = 15, height = 15
  )
  FactoMineR::plot.catdes(submarket_cluster_des)

  dev.off()

}

#==============================
# ADFM
#==============================

if (FALSE){
  res.FAMD <- FAMD(submarket_data_frame,
  sup.var=c(1,2,3,4,9,10,11),
  graph=FALSE
  )

  res.FAMD<-FAMD(submarket_data_frame,sup.var=c(1,2,3,9,10,11,12,13,14,15,16,17,18,19,20),graph=FALSE)
}

res.FAMD <- FAMD(submarket_data_frame,
  sup.var = c(1,4,9,10,11,12,13,14,15,16,17,18,19,20),
  graph = FALSE
)

summary(res.FAMD)

if (interactive()) {
  plot.FAMD(res.FAMD,invisible=c('quali.sup','ind.sup'),title="Graphe des individus et des modalités")
  plot.FAMD(res.FAMD,axes=c(1,2),choix='var',title="Graphe des variables")
  plot.FAMD(res.FAMD, choix='quanti',select='cos2  0.5',title="Cercle des corrélations")
}

res.HCPC <- HCPC(res.FAMD,nb.clust = -1, consol=FALSE,graph=FALSE)

if (interactive()) {

  plot.HCPC(res.HCPC,choice='tree',title='Arbre hiérarchique')
  plot.HCPC(res.HCPC,choice='map',draw.tree=TRUE,title='Plan factoriel')
  plot.HCPC(res.HCPC,choice='3D.map',ind.names=FALSE,centers.plot=FALSE,angle=60,title='Arbre hiérarchique sur le plan factoriel')
}


#===============================
# DBSCAN
#===============================

ind_coords <- res.FAMD$ind$coord

dist_to_nearest <- kNNdist(ind_coords, k = 4)

dist_to_nearest <- as.data.frame(sort(dist_to_nearest, decreasing = FALSE))

names(dist_to_nearest) <- "distance"

if (interactive()) {
  ggplot(data = dist_to_nearest)+
  geom_path(aes(x = 1:nrow(dist_to_nearest), y = distance), size = 1) +
  labs(x = "Points sorted in ascending order by distance",
       y = "Distance to the fourth-closest point") +
    geom_hline(yintercept = 0.82, color = "#08306b", linetype="dashed", size=1) +
    geom_hline(yintercept = 1.02, color = "#00441b", linetype="dashed", size=1) +
    geom_hline(yintercept = 1.55, color = "#67000d", linetype="dashed", size=1) +
    geom_hline(yintercept = 1.92, color = "#3f007d", linetype="dashed", size=1)
}


dbscan_082  <- dbscan(ind_coords, eps = 0.82, minPts  = 4)
dbscan_102  <- dbscan(ind_coords, eps = 1.02, minPts  = 4)
dbscan_155 <- dbscan(ind_coords, eps = 1.55, minPts = 4)
dbscan_192 <- dbscan(ind_coords, eps = 1.92, minPts = 4)

dbscan_082
dbscan_102



#===============================
# Retrieve clusters
#===============================

submarket_data_frame$cluster_hcpc <- res.HCPC$data.clust$clust


V(meta_graph)$cluster_hcpc <- submarket_data_frame$cluster_hcpc[
  match(V(meta_graph)$name, submarket_data_frame$name)
]

length(V(meta_graph)$cluster_hcpc) == length(V(meta_graph)$meta_membership)

NMI(V(meta_graph)$cluster_hcpc, V(meta_graph)$meta_membership)

if (interactive()) plot(
  meta_graph,
  edge.color = E(meta_graph)$color,
  vertex.size = log(V(meta_graph)$size_comm),
  vertex.color = V(meta_graph)$meta_membership,
  edge.width = log1p(E(meta_graph)$weight),
  edge.curved = 0
)


if (interactive()) plot(
  meta_graph,
  edge.color = E(meta_graph)$color,
  vertex.size = log(V(meta_graph)$size_comm),
  vertex.color = V(meta_graph)$cluster_hcpc,
  edge.width = log1p(E(meta_graph)$weight),
  edge.curved = 0
)


questionr::freq(V(meta_graph)$cluster_hcpc)

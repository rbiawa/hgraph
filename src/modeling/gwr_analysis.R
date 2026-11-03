## Comparaison des deux calibrations

diagGwr <- cbind(
  rbind(nNeigh.exp,nNeigh.bisq),
  rbind(GWR.exp$GW.diagnostic$gw.R2,GWR.bisq$GW.diagnostic$gw.R2),
  rbind(GWR.exp$GW.diagnostic$AIC,GWR.bisq$GW.diagnostic$AIC)) %>% 
  `colnames<-`(c("Nb Voisins","R2","AIC")) %>% 
  `rownames<-`(c("EXPONENTIAL","BISQUARE"))


diagGwr

# * La GWR avec pondération exponentielle est la plus performante


GWR.exp


# * Interprétation des résultats bruts de la GWR :


sink("out/txt/supply_submarkets/gwr/gwr_result_4blocs.txt")

cat("\n")
cat("\n")
cat("Résultat du modèle exponentiel", "\n")


GWR.exp


cat("\n")
cat("\n")
cat("Résultat du modèle bisquare", "\n")
cat("\n")
cat("\n")

if (exists("GWR.bisq")) GWR.bisq



cat("\n")
cat("\n")


sink()





map_gwr_coef <- function(gwr_result ,var, shape = gwr_shape){
  tm_shape(shape)+
  tm_borders() +  # useful to see the shape's boundaries
  tm_shape(gwr_result$SDF %>%
           as.data.frame() %>%
           st_as_sf(coords = c("coords.x1", "coords.x2")
                    ,crs = 4326)
  ) + tm_dots(col = var, style = "quantile")+
      tm_scale_bar(position = c("left", "bottom")) +
    tm_compass(type = "arrow", position = c("right", "top"))
}

# Fonction de cartographie automatique des coefficients GWR

pdf(file = "out/pdf/supply_submarkets/gwr/gwr_coef_map.pdf",
    onefile = TRUE)

map_gwr_coef(GWR.exp, "log_area")

map_gwr_coef(GWR.exp, "log_area_TV")

map_gwr_coef(GWR.exp, "item_typeITEM_TYPE.HOUSE")

map_gwr_coef(GWR.exp, "item_typeITEM_TYPE.HOUSE_TV")

dev.off()





pdf(file = "out/pdf/supply_submarkets/gwr/mgwr_coef_map.pdf",
    onefile = TRUE)

map_gwr_coef(MGWR.exp, "log_area")

map_gwr_coef(MGWR.exp, "log_area_TV")

map_gwr_coef(MGWR.exp, "item_typeITEM_TYPE.HOUSE")

map_gwr_coef(MGWR.exp, "item_typeITEM_TYPE.HOUSE_TV")

map_gwr_coef(MGWR.exp, "room_count")

map_gwr_coef(MGWR.exp, "room_count_TV")

dev.off()






# all_coeff_SDF <- rbind(gwr.exp_bloc1$SDF %>%
#                          as.data.frame(),
#                        gwr.exp_bloc2$SDF %>%
#                          as.data.frame(),
#                        gwr.exp_bloc3$SDF %>%
#                          as.data.frame(),
#                        gwr.exp_bloc4$SDF %>%
#                          as.data.frame()
#                        )


#Housing market regionalization

# Stape 1 : sacaling data

gwrB.scaled <- GWR.exp$SDF %>% 
  as.data.frame() %>% 
  dplyr::select(1:4) %>% 
  mutate_all(~scale(.)) %>% 
  rename_with(~paste(.x, "b", sep = "_"))



# kmeans algorithm


kmeans_res <- kmeans(gwrB.scaled,
                     5,
                     iter.max = 100,
                     nstart = 100)


table(kmeans_res$cluster)

tm_shape(
  GWR.exp$SDF %>%
    as.data.frame() %>%
    mutate(submarket = kmeans_res$cluster) %>%
    st_as_sf(coords = c("coords.x1", "coords.x2"), crs = 4326)
) +
  tm_dots(col = "submarket", palette = "Set1", size = 0.5, alpha = 0.6) +
  tm_shape(gwr_shape) +
  tm_borders() +
  tm_layout(
    title = "Housing submarket typology",
    legend.outside = TRUE,
    legend.outside.position = "right",
    outer.margins = c(0,0,0,0),
    inner.margins = c(0.02,0.02,0.02,0.02)
  ) +
  tm_scale_bar(position = c("left", "bottom"), text.size = 0.6) +
  tm_compass(type = "arrow", position = c("right", "top"), size = 2)





tm_shape(gwr_shape) +
  tm_borders() +   # affiche les limites du fond
  # éventuellement un remplissage neutre
  # tm_fill(col = "lightgrey", alpha = 0.3) +
  
tm_shape(
  MGWR.exp$SDF %>%
    as.data.frame() %>%
    mutate(submarket = kmeans_res$cluster) %>%
    st_as_sf(coords = c("coords.x1", "coords.x2"), crs = 4326)
) +
  tm_dots(col = "submarket", palette = "Set1", size = 0.5, alpha = 0.6) +
  
tm_layout(
  title = "Housing submarket typology",
  legend.outside = TRUE,
  legend.outside.position = "right"
) +
tm_scale_bar(position = c("left", "bottom")) +
tm_compass(type = "arrow", position = c("right", "top"))





pdf(file = "out/pdf/supply_submarkets/gwr/kmeans_submarket_map.pdf",
    onefile = TRUE)

tm_shape(
  GWR.exp$SDF %>%
    as.data.frame() %>%
    mutate(submarket = kmeans_res$cluster) %>%
    st_as_sf(coords = c("coords.x1", "coords.x2"), crs = 4326)
) +
  tm_dots(col = "submarket", palette = "Set1", size = 0.3, alpha = 0.3) +
  tm_shape(gwr_shape) +
  tm_borders() +
  tm_layout(
    title = "Housing submarket typology",
    legend.outside = TRUE,
    legend.outside.position = "right",
    outer.margins = c(0,0,0,0),
    inner.margins = c(0.02,0.02,0.02,0.02)
  ) +
  tm_scale_bar(position = c("left", "bottom"), text.size = 0.6) +
  tm_compass(type = "arrow", position = c("right", "top"), size = 2)

dev.off()







# Satape 2 : SKATER algorithm

knn <- knearneigh(GWR.exp$SDF, k=60)
nb <- knn2nb(knn)

n.comp.nb(nb)

# Force connectivity with edit.nb() not to be used with RStudio
#nb <- edit.nb(nb, which(n.comp.nb(nb) > 1))

plot(nb, coords = coordinates(GWR.exp$SDF), col="blue")



# Calibration of edge costs and spatial weighting

costs <- nbcosts(nb, data = gwrB.scaled)


summary(unlist(costs))
any(is.na(unlist(costs)))         # NA ?
any(unlist(costs) == 0)  


costs_clean <- lapply(costs, function(w) {
  w[w == 0] <- 1e-6  # ou med_global * 0.01
  return(w)
})


summary(unlist(costs_clean))
any(is.na(unlist(costs_clean)))         # NA ?
any(unlist(costs_clean) == 0)  


costsW <- nb2listw(nb, costs, style = "B")




mat <- listw2mat(costsW)
mat[mat == 0] <- NA  # ignore les non-arêtes

g <- graph_from_adjacency_matrix(mat, mode = "undirected", weighted = TRUE, diag = FALSE)
components(g)$no  # doit être égal à 1




# Tree minimization and classification

costsTree <- mstree(costsW)

plot(costsTree, coords = coordinates(GWR.exp$SDF), col="blue", main = "Arbre portant minimal")

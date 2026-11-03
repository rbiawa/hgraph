
retrieve_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name)
  }
  
  library(package_name, character.only = TRUE)
}




# Load Required Packages ------------------------------------------------------

library(data.table)
library(lubridate)
library(tidyverse)

retrieve_package("tidyverse")


# New packages

retrieve_package("entropy")
retrieve_package("cluster")
retrieve_package("ggdendro")
retrieve_package("distances")  # for big matrices
retrieve_package("fpc")        # for Big data AHC
retrieve_package("ggfortify")  # For Biplot (PCA variable + individuals) plot
retrieve_package("plotly")     # for interactive plots
retrieve_package("GGally")     # for Scatterplot matrix (2D)
retrieve_package("factoextra") # for PCA variable contribution
##############################################

#rm(listing_feat_SL_idf_hdf_gps_insee, listing_feat_SL, listing_feat_SL_idf_hdf, listing_feat_SL_idf_hdf_hs_apt)


#rm(seq_lenght_01_02_2021, seq_lenght_03_04_2021, seq_lenght_05_06_2021, seq_lenght_24, seq_lenght_246, seq_lenght_46, seq_lenght_26)


#rm(seq_lenght_01_02_2021, seq_lenght_03_04_2021, seq_lenght_05_06_2021, seq_lenght_24, seq_lenght_246, seq_lenght_46, seq_lenght_26)
###################################################"
#Data

events <- setDT(event_01_02_2021)


features <- listing_feat_SL_idf_hdf_MA [
  #1:100000 
  ,.(subregion_lab, item_type, subregion, id_listing, 
    city, price, area, room_count, transaction_type, zipcode,
    sqm_price, min_sqm_price, mean_sqm_price, max_sqm_price)
] %>% na.omit()


features <- features[(sqm_price>=min_sqm_price & sqm_price<=max_sqm_price & room_count>0), ]





#rm(extern_sqm_price_MA, extern_sqm_price_SL)

##################################################################
##                                                              ##
##                      DATA CLEANING                           ##
##                                                              ##
##################################################################


k <- 0


# 1. Data Preparation & Listing Relevance Filtering ---------------------------
# Convert date column to Date type
events[, date := ymd(date)]

# Calculate listing exposure duration and normalized consultation frequency
listing_stats <- events[
  , .(consult_count = .N, 
      first_date = min(date), 
      last_date = max(date)), 
  by = id_listing
][, exposure_days := as.numeric(last_date - first_date) + 1
][, norm_freq := consult_count / exposure_days]

# Calculate the IQR-based threshold

#q <- quantile(listing_stats$norm_freq, probs = c(0.25, 0.75), na.rm = TRUE)

#q <- quantile(listing_stats$consult_count, probs = c(0.25, 0.75), na.rm = TRUE)

min_threshold <- quantile(listing_stats$consult_count, probs = k/100, na.rm = TRUE)

#iqr <- q[2] - q[1]
#adaptive_threshold <- q[2] + 1.5 * iqr  # Standard upper threshold for outlier detection

# Filter relevant listings
#relevant_listings <- listing_stats[norm_freq >= adaptive_threshold, id_listing]


relevant_listings <- listing_stats[norm_freq > min_threshold, id_listing]


# Filter events to relevant listings
filtered_events <- events[id_listing %in% relevant_listings]


perc <- (nrow(events) - nrow(filtered_events))*100/nrow(events)

message(paste0(perc, "% of events were deleted"))



# 2. User Behavior Metrics Calculation ----------------------------------------

user_metrics <- filtered_events[
  features, 
  on = "id_listing",
  nomatch = 0
][, .(
  # Consultation behavior
  total_listings = .N,
  sessions = uniqueN(visitid),
  avg_listings_per_session = .N / uniqueN(visitid),
  consultation_duration = as.numeric(max(date) - min(date)) + 1,
  
  # Listing characteristics
  mean_price = mean(price),
  price_cv = sd(price)/mean(price),
  mean_area = mean(area),
  area_cv = sd(area)/mean(area),
  room_mean = mean(room_count),
  room_cv = sd(room_count)/mean(room_count),
  
  # Property type diversity
  type_entropy = entropy(table(item_type)),
  main_property_type = names(sort(table(item_type), decreasing = TRUE))[1],
  
  # City diversity
  city_entropy = entropy(table(city))
  
), by = fullvisitorid]


# Filter users that have consulted only one listing
user_metrics <- user_metrics[total_listings > 1,]

users_with_na <- user_metrics[!complete.cases(user_metrics)]

# 3. User Segmentation with Hierarchical Clustering --------------------------
# Select clustering variables
cluster_vars <- c(
  "total_listings", "sessions", "avg_listings_per_session", "consultation_duration",
  "mean_price", "price_cv", "mean_area", "area_cv", "room_cv", "type_entropy"
  , "city_entropy"
)


#############################################
##      Variables standardization          ##
#############################################


scaled_data <- scale(user_metrics[, ..cluster_vars])

# Compute distance matrix
#dist_matrix <- dist(scaled_data, method = "euclidean")

dist_matrix <- distances::distances(scaled_data)
  
  
  
  
  
  
  
  
  
################################################################
##                                                            ##
##                  Searcher clustering                       ##
##                                                            ##
################################################################
  
  
  
  ## Optimal cluster number
  
  
  ### Elbow graphic
  
 # fviz_nbclust(scaled_data, kmeans, method = "wss")
  
  
  
  
### Estimation
if (nrow(scaled_data <= 65536)) pamk_result <- pamk(scaled_data)

#il y a 1424553 observations, mais pas plus de 65536 sont acceptées
  

 if (exists("pamk_result")){
   nb_c     <- pamk_result$nc
   
   cat("Nombre optimal de clusters estimé :", nb_c, "\n")
   
 } else {nb_c = 5 }


  
  
  
  
  if(FALSE){
  # Hierarchical clustering with Ward's method
  hc <- hclust(as.dist(dist_matrix), method = "ward.D2")
  
  # Cut tree to create 3 clusters
  user_metrics[, cluster := cutree(hc, k = nb_c)]
  
  #birch_res <- birch(scaled_data, radius = 0.5) # for big data

  } else{
  
  set.seed(123)
  kmeans_model <- kmeans(scaled_data, centers = nb_c, nstart = 50)
  user_metrics[, cluster := as.factor(kmeans_model$cluster)]
}


#Avis : Les étapes de transfer (quick-TRANSfer stage) ont dépassé le maximum (= 71227650)



# 4. Serious User Identification ---------------------------------------------
# Calculate seriousness score
user_metrics[, seriousness := scales::rescale(
      (1 - price_cv) 
    * (1 - area_cv)
    * (1 - room_cv)
    #* consultation_duration
    * (1- scales::rescale(type_entropy, to = c(0, 1)))
    * (1- scales::rescale(city_entropy, to = c(0, 1)))
  , to = c(0, 100)
)]

# Identify top 20% serious users
#serious_users <- user_metrics[seriousness >= quantile(seriousness, 0.8)]

# 5. Visualization & Interpretation ------------------------------------------


if(exists("hc")){
  # Dendrogram of user clusters
  ggdendrogram(hc, labels = FALSE) +
    geom_hline(yintercept = hc$height[length(hc$height)-2], color = "red") +
    labs(title = "User Behavior Dendrogram",
         subtitle = "Hierarchical clustering with Ward's method")
  
}










# PCA projection colored by clusters
pca <- prcomp(scaled_data)




# Expained inertia ( importance of components)
summary(pca)

# Contribution in principal components
pca$rotation[, 1:4]



# Biplot

autoplot(pca, loadings = TRUE, loadings.label = TRUE)


# Visualization





pca_dt <- data.table(
  fullvisitorid = user_metrics$fullvisitorid,
  PC1 = pca$x[,1],
  PC2 = pca$x[,2],
  PC3 = pca$x[,3], 
  PC4 = pca$x[,4],
  Cluster = factor(user_metrics$cluster),
  Seriousness = user_metrics$seriousness
)






ggplot(pca_dt, aes(PC1, PC2, color = Cluster, size = Seriousness)) +
  geom_point(alpha = 0.6) +
  scale_size_continuous(range = c(1, 4)) +
  labs(title = "User Segmentation by Search Behavior",
       x = "PC1: General Engagement",
       y = "PC2: Search Specificity") +
  theme_minimal()





##############################################
# 3D visualisation

explained_inertia <- round(pca$sdev^2 / sum(pca$sdev^2) * 100, 1)


# 3D visualization



# Creation of labels with explained variance
labels <- c(
  paste0("PC1: ", explained_inertia[1], "%"),
  paste0("PC2: ", explained_inertia[2], "%"),
  paste0("PC3: ", explained_inertia[3], "%"),
  paste0("PC4: ", explained_inertia[4], "%")
)

# Interactive 3D graphics

if(FALSE){
  plot_ly(
    data = pca_dt,
    x = ~PC1,
    y = ~PC2,
    z = ~PC3,
    color = ~Cluster,
    size = ~Seriousness,
    type = "scatter3d",
    mode = "markers",
    marker = list(sizemode = 'diameter', opacity = 0.7)
  ) %>%
    layout(
      scene = list(
        xaxis = list(title = labels[1]),
        yaxis = list(title = labels[2]),
        zaxis = list(title = labels[3])
      ),
      title = "3D user segmentation (61% variance explained)"
    )

}

# Scatterplot matrix (2D)


#library(GGally)

# Combine the first 3 components
pca_4d <- data.table(
  PC1 = pca$x[,1],
  PC2 = pca$x[,2],
  PC3 = pca$x[,3],
  PC4 = pca$x[,4],  
  Cluster = user_metrics$cluster,
  Seriousness = user_metrics$seriousness
)


# Matrix creation
ggpairs(
  pca_4d,
  columns = 1:4,
  mapping = aes(color = Cluster, alpha = 0.5),
  upper = list(continuous = "density"),
  lower = list(continuous = "points"),
  diag = list(continuous = "densityDiag")
) +
  theme_minimal()




# Variables contribution on axes


fviz_contrib(pca, choice = "var", axes = 1)  # PC1
fviz_contrib(pca, choice = "var", axes = 2)  # PC2


fviz_contrib(pca, choice = "var", axes = 3)  # PC3
fviz_contrib(pca, choice = "var", axes = 4)  # PC4


# Visualisation combinée (PC1 + PC2)
fviz_pca_var(pca, col.var = "contrib", 
             gradient.cols = c("blue", "red"))



# Interprétation :
#   Les barres rouges dépassant la ligne pointillée (valeur seuil) indiquent les variables significatives.
#   La couleur reflète l'intensité de la contribution.





##################################################################

###    Silhouette de la clusterisation


# silhouette_score <- silhouette(kmeans_model$cluster, dist_matrix)
# fviz_silhouette(silhouette_score)

# La matrice est trop grande



################################################################
##                                                            ##
##                        Class summary                       ##
##                                                            ##
################################################################



# 6. Cluster Profiling Report ------------------------------------------------
cluster_profile <- user_metrics[
  , lapply(.SD, mean), 
  by = cluster,
  .SDcols = c(cluster_vars, "seriousness")
][order(-seriousness)]


# Format profile table
formatted_profile <- cluster_profile[, .(
  Cluster = cluster,
  `Total Listings` = round(total_listings),
  Sessions = round(sessions),
  `Listings/Session` = round(avg_listings_per_session, 1),
  `Duration (Days)` = round(consultation_duration),
  `Price CV (%)` = round(price_cv * 100, 1),
  `Area CV (%)` = round(area_cv * 100, 1),
  `Room Variety` = round(room_cv, 2),
  `Type Diversity` = round(type_entropy, 2),
  `Seriousness Score` = round(seriousness)
)]



















# Print formatted table
print(knitr::kable(
  formatted_profile,
  caption = "Cluster Profile Summary",
  align = c("l", rep("r", 8))
))

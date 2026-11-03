library(ggplot2)
library(gridExtra)  # Pour ggarrange
library(dplyr)
library(grid)

# Main function
plot_continuous_variables <- function(data) {
  # Select numeric continuous variables
  continuous_vars <- names(data)[sapply(data, is.numeric)]
  results <- list()
  
  for (var in continuous_vars) {
    x <- data[[var]]
    x <- na.omit(x)  # Remove NA values
    
    # Calculate quartiles and mean
    quartiles <- quantile(x, probs = c(0.25, 0.5, 0.75))
    mean_val <- mean(x)
    
    # 🟦 Density plot with vertical lines for quartiles and mean
    p1 <- ggplot(data, aes(x = .data[[var]])) +
      geom_density(fill = "lightgray") +
      geom_vline(xintercept = quartiles, color = "blue", linetype = "dashed") +
      geom_vline(xintercept = mean_val, color = "red", linetype = "solid") +
      labs(title = paste("Distribution of", var), x = var, y = "Density") +
      theme_minimal()
    
    # 🎻 Violin boxplot
    p2 <- ggplot(data, aes(x = "", y = .data[[var]])) +
      geom_violin(fill = "lightblue", color = "black") +
      geom_boxplot(width = 0.2, fill = "white", outlier.color = "red") +
      labs(title = paste("Violin boxplot of", var), x = "", y = var) +
      theme_minimal()
    
    # ⬅️➡️ Side-by-side arrangement
    figure <- gridExtra::grid.arrange(p1, p2, ncol = 2)
    
    # Save plot to list
    results[[var]] <- figure
  }
  
  return(results)
}




###################################################################

# Création d'un jeu de données avec 3 variables continues
set.seed(123)  # Pour la reproductibilité
data_fictive <- data.frame(
  hauteur = rnorm(100, mean = 170, sd = 10),       # en cm
  poids = rnorm(100, mean = 65, sd = 15),          # en kg
  temperature = rnorm(100, mean = 36.6, sd = 0.5)  # en °C
)

####################################################################



# Appel de la fonction
resultats_graphs <- plot_continuous_variables(data_fictive)

pdf("developpement_R/try.pdf")

for (var in names(resultats_graphs)) {
  grid.newpage()
  grid.draw(resultats_graphs[[var]])
}

dev.off()




plot_categorical_variables <- function(data) {
  # Sélectionner les variables non numériques
  categorical_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
  results <- list()
  
  for (var in categorical_vars) {
    #  Bar plot des fréquences
    p <- ggplot(data, aes(x = .data[[var]])) +
      geom_bar(fill = "lightblue", color = "black") +
      labs(title = paste("Histogram of", var), x = var, y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    results[[var]] <- p
  }
  
  return(results)
}


###################################################################################

# Exemple avec des données fictives
df <- data.frame(
  gender = sample(c("Male", "Female"), 100, replace = TRUE),
  statut = sample(c("Student", "Worker", "Retired"), 100, replace = TRUE)
)

plots_cat <- plot_categorical_variables(df)

###################################################################################


# Affichage de tous les histogrammes

for (p in plots_cat) {
  print(p)  # Les objets ggplot peuvent être affichés directement
}



pdf("developpement_R/categorical.pdf")

for (p in plots_cat) {
  print(p)  # Les objets ggplot peuvent être affichés directement
}

dev.off()


















## Node attributes

summary(E(graph)$weight)


vertex.attributes(graph) <- vertex.attributes(graph) %>%
  as.data.frame() %>%
  left_join(features %>%
      dplyr::select(id_listing, price,
             area, room_count, city,
             city_ID, dep_ID, INSEE_REG) %>%
      mutate(name = as.character(id_listing)),
    by = "name"
  )



if (interactive()) vertex.attributes(graph) %>% View()

table(vertex.attributes(graph)$INSEE_DEP)

table(listing_feat_SL_idf_hdf$subregion)

assortativity(graph, V(graph)$price)

#[1] 0.8483008

assortativity(graph, V(graph)$area)

#[1] 0.8129646

assortativity(graph, V(graph)$room_count)

#[1] 0.7811491


assortativity_test(graph, "price")


assortativity_test(graph, "city", 1000)

assortativity_test(graph, "INSEE_DEP", 1000)

assortnet_assortativity(
    graph = graph
  , node_attr = "price"
  , weighted = TRUE
  , weight_attr = "weight"
  , n_perm = 1
)



print("item_type assortativity")

assortnet_assortativity(
    graph = graph
  , node_attr = "item_type"
  , weighted = TRUE
  , weight_attr = "weight"
  , n_perm = 1
)


print("city assortativity")

assortnet_assortativity(
    graph = graph
  , node_attr = "city"
  , weighted = TRUE
  , weight_attr = "weight"
  , n_perm = 1
)



############################################

# For plot

pdf("developpement_R/try.pdf")

for (var in names(resultats_graphs)) {
  cat("📊 Variable :", var, "\n")  # Affiche le nom de la variable
  grid.newpage()
  grid.draw(resultats_graphs[[var]])
}

dev.off()
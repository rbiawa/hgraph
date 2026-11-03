#########################################################################################
# Functions used for graph clustering.
# 
# 2025/02 Raphaël Biawa Diegamounoua
#
# source("src/clustering/graph/graph_clustering.R")
#########################################################################################



#########################################################################################
# Retrieves submarkets identified through a graph clustering algorithm
# 
# Args :
# - graph : a graph through wich clusterisation was made (should be a igraph object)
# - submarket : a vector of submarket (or membership) to wich ench graph's vertice belongs
# - event_data : a dataframe containing consultation informations and vertices attributs
# - listing_id_var : a event_data's column indicating ad's ID
#
# Returns:a dataframe containing all event_data's columns plus submarket's column
# 
#########################################################################################

retrieve_graph_submarket <- function(graph, submarket, event_data, listing_id_var) {

  event_data <- event_data %>% 
      as_tibble() %>%
      mutate(name = as.character(!!sym(listing_id_var)))

  V(graph)$submarket <- submarket


  submarket_data     <- as.data.frame(vertex.attributes(graph)) %>%
                        dplyr::select(name, submarket) %>%
                        mutate(name = as.character(name))




  data_submarket     <-  submarket_data %>%
      left_join(event_data, by = c("name" = "name")) 

  return(data_submarket)

}


##############################################################################################
# Creates a dataset with a transition type variable between submarkets (wether intra or inter)
# Args:
# - data: A data frame containing information about users, event ID and submarket variable.
# - user_id_var: A character string representing the name of the user identifier column.
# - event_id_var : A variable (probably  a character string, but not necessary) identifying 
#       the event order.
# - submarket_var : A variable from data giving the submarket number a ad belongs to.
# - min_listing_numb: An integer specifying the minimum number of listings a user must 
#       have consulted to be retained in the dataset (Default is 2).
#
# Returns:
# - data with two more variables (next_submarket, transition_type).
##############################################################################################


extract_submarket_transition <- function(data,
                                         user_id_var,
                                         event_id_var,
                                         submarket_var,
                                         min_listing_numb = 2) {

  data_submarket <- setDT(data)[ 
    order(get(user_id_var), get(event_id_var)), 
      next_submarket := shift(get(submarket_var), type = "lead"), by = get(user_id_var)
    ][
      !is.na(next_submarket),
    ][
      , transition_type := ifelse(get(submarket_var) == next_submarket, "intra", "inter")  
    ][
        , if (.N >= min_listing_numb) .SD, by = get(user_id_var)
    ]

return(data_submarket)

}



##############################################################################################
# Computes single market operating sequences  rate
# Args:
# - data: A data frame containing information about users, event ID and submarket variable.
# - user_id_var: A character string representing the name of the user identifier column.
# - event_id_var : A variable (probably  a character string, but not necessary) identifying 
#       the event order.
# - submarket_var : A variable from data giving the submarket number a ad belongs to.
# - min_listing_numb: An integer specifying the minimum number of listings a user must 
#       have consulted to be retained in the dataset (Default is 2).
#
# Returns:
# - numeric value ranges from 0 to 1
##############################################################################################


compute_same_submarket_seq_rate <- function(data,
                                            user_id_var = "user_uid",
                                            event_id_var = "event_id",
                                            submarket_var = "submarket",
                                            min_listing_numb = 2){

type_of_transition <- extract_submarket_transition(data = data,
                                                   user_id_var,
                                                   event_id_var,
                                                   submarket_var,
                                                   min_listing_numb)


# Number of sequences operating in a single market
same_submarket_sequences <- type_of_transition[
    , .(all_same = all(get(submarket_var) == get(submarket_var)[1])), by = get(user_id_var)
][
    all_same == TRUE, .N]


# Rate of single market sequences
 same_submarket_seq_rate <- same_submarket_sequences / length(unique(type_of_transition[[user_id_var]]))

return(same_submarket_seq_rate)

}





####################################################################################################
# Simulate single market operating sequences' rates with changes in Louvain clustering modularity
#
# Args:
# - event_data : a dataframe containing consultation informations and vertices attributs.
# - iter_nb: number of iteration (a non zero integer)
# - user_id_var: A character string representing the name of the user identifier column.
# - event_id_var : A variable (probably  a character string, but not necessary) identifying 
#       the event order.
# - listing_id_var : a event_data's column indicating ad's ID
# - min_listing_numb: An integer specifying the minimum number of listings a user must 
#       have consulted to be retained in the dataset (Default is 2).
#
# Returns:
# - data.table containting single market operating sequences  rate variable and clustering 
#     modularity variable
####################################################################################################




simulate_same_submarket_seq_rate <- function(graph,
                                             event_data,
                                             iter_nb,
                                             user_id_var = "user_uid",
                                             event_id_var = "event_id",
                                             listing_id_var = "listing_id",
                                             min_listing_numb = 2){

  start_time <- Sys.time()

  rate_val     <- numeric(iter_nb)

  modularity   <- numeric(iter_nb)

  nb_clusters  <- numeric(iter_nb)

  order        <- numeric(iter_nb)

  for (i in 1:iter_nb){
    louvain_clustering <- cluster_louvain(graph = graph)

    # Import submarket variable
    submarket          <- louvain_clustering$membership

    data_submarket <- retrieve_graph_submarket(graph = graph,
                                               submarket,
                                               event_data,
                                               listing_id_var)

    rate <- compute_same_submarket_seq_rate(data = data_submarket,
                                            user_id_var, event_id_var,
                                            "submarket",
                                            min_listing_numb)

    mod  <- modularity(louvain_clustering)

    nb_clust <- length(unique(submarket))

    order[i]        <- i
    rate_val[i]     <- rate
    modularity[i]   <- mod
    nb_clusters[i]  <- nb_clust

  }
  out_data <- data.table(order, rate_val, modularity, nb_clusters)


  end_time <- Sys.time()
  print(end_time - start_time)

  return(out_data)

}


library(igraph)

#########################################################################################
# Computes the weighted global clustering coefficient and evaluates its statistical significance 
# against a weight-permutation null model.
# 
# Args :
# - graph : an igraph object representing the co-consultation network (must have a 'weight' edge attribute)
# - n_permutations : an integer specifying the number of random permutations for the null model (default = 500)
#
# Returns: a list containing the observed weighted clustering coefficient, the null model mean, 
#          the standard deviation, the z-score, and the empirical p-value.
# 
#########################################################################################
compute_weighted_clustering_significance <- function(graph, n_permutations = 500) {
  # 1. Compute the observed weighted clustering coefficient
  C_real <- mean(transitivity(graph, type = "weighted"), na.rm = TRUE)
  
  # 2. Extract edge weights for permutation
  edge_weights <- E(graph)$weight
  
  # 3. Run the null model via weight permutations
  null_distribution <- replicate(n_permutations, {
    E(graph)$weight <- sample(edge_weights)
    mean(transitivity(graph, type = "weighted"), na.rm = TRUE)
  })
  
  # 4. Calculate descriptive statistics of the null distribution
  C_null_mean <- mean(null_distribution, na.rm = TRUE)
  C_null_sd <- sd(null_distribution, na.rm = TRUE)
  
  # 5. Calculate Z-score and empirical p-value (ratio of null values >= C_real)
  z_score <- if (C_null_sd > 0) (C_real - C_null_mean) / C_null_sd else 0
  p_value <- sum(null_distribution >= C_real) / n_permutations
  
  # Handle zero p-value for numerical reporting safety
  if (p_value == 0) {
    p_value <- 1 / n_permutations
  }
  
  # 6. Return results as a structured list
  return(list(
    observed_clustering = C_real,
    null_mean = C_null_mean,
    null_sd = C_null_sd,
    z_score = z_score,
    p_value = p_value
  ))
}


#########################################################################################
# Computes the weighted global clustering coefficient and evaluates its significance
# against a degree-preserving topology null model (rewired graph with permuted weights).
#
# Args :
# - graph          : an igraph object representing the co-consultation network
# - n_permutations : number of randomizations for the null distribution (default = 500)
# - n_swaps_factor : multiplier for edge swapping iterations (default = 10)
#
# Returns: list with observed clustering, null mean, sd, z-score, and p-value.
#########################################################################################
compute_weighted_clustering_vs_random_graph <- function(graph, 
                                                        n_permutations = 500, 
                                                        n_swaps_factor = 10) {
  
  # 1. Compute observed weighted transitivity
  C_real <- mean(transitivity(graph, type = "weighted"), na.rm = TRUE)
  
  # Extract topology and edge attributes
  n_edges    <- ecount(graph)
  n_iter     <- n_edges * n_swaps_factor
  obs_weights <- E(graph)$weight
  
  # 2. Run degree-preserving randomized null model
  null_distribution <- replicate(n_permutations, {
    # Rewire edges preserving degree sequence
    null_g <- rewire(graph, with = keeping_degseq(loops = FALSE, niter = n_iter))
    
    # Assign edge weights (either permuted observed weights or sampled)
    E(null_g)$weight <- sample(obs_weights, replace = FALSE)
    
    # Calculate weighted transitivity
    mean(transitivity(null_g, type = "weighted"), na.rm = TRUE)
  })
  
  # 3. Descriptive statistics
  C_null_mean <- mean(null_distribution, na.rm = TRUE)
  C_null_sd   <- sd(null_distribution, na.rm = TRUE)
  
  # 4. Z-score and p-value calculation
  z_score <- if (C_null_sd > 0) (C_real - C_null_mean) / C_null_sd else 0
  p_value <- sum(null_distribution >= C_real) / n_permutations
  
  if (p_value == 0) {
    p_value <- 1 / n_permutations
  }
  
  return(list(
    observed_clustering = C_real,
    null_mean           = C_null_mean,
    null_sd             = C_null_sd,
    z_score             = z_score,
    p_value             = p_value
  ))
}
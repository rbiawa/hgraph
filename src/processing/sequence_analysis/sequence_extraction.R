#########################################################################################
# A script to perform a sequences extraction
# 
# 2024/11 Raphaël Biawa Diegamounoua
#
# source("src/processing/sequence_analysis/sequence_extraction.R")
#########################################################################################






######################################################################################
# Calculates the proportion of listings consulted more than once by the same user.
#
# Args:
# - data: A data frame containing the dataset.
# - vars: A character vector of length 2 with the following elements:
#   - vars[1]: Name of the variable representing the user identifier.
#   - vars[2]: Name of the variable representing the listing identifier.
#
# Returns:
# - A numeric value representing the proportion of listings consulted more than once.
######################################################################################

calculate_repeat_proportion <- function(data, vars) {
  # Validate inputs
  if (!is.data.frame(data)) stop("The input 'data' must be a data frame.")
  if (!is.character(vars) || length(vars) != 2) {
    stop("The input 'vars' must be a character vector of length 2.")
  }
  
  # Extract variable names
  user_var <- vars[1]
  listing_var <- vars[2]
  
  # Ensure the specified variables exist in the dataset
  if (!all(c(user_var, listing_var) %in% names(data))) {
    stop("The specified variables in 'vars' must exist in the dataset.")
  }
  
  # Count distinct user-listing pairs and total rows
  distinct_pairs <- data %>%
    distinct(across(all_of(vars))) %>%
    nrow()
  
  total_rows <- nrow(data)
  
  # Calculate proportion of listings consulted more than once
  repeat_proportion <- (total_rows - distinct_pairs) / total_rows
  
  # Return the result
  return(repeat_proportion)
}


##################################################################################################
# Filters a dataset to retain rows with complete cases for specified variables.
#
# Args:
# - data: A data frame to be filtered.
# - vars: A character vector specifying the names of the columns to check for completeness.
#
# Returns:
# - A filtered data frame containing only rows with complete cases for the specified variables.
##################################################################################################

filter_complete_cases <- function(data, vars) {
  # Validate inputs
  if (!is.data.frame(data)) stop("The input 'data' must be a data frame.")
  if (!is.character(vars) || length(vars) == 0) {
    stop("The input 'vars' must be a non-empty character vector.")
  }
  
  # Ensure the specified variables exist in the dataset
  missing_vars <- vars[!vars %in% names(data)]
  if (length(missing_vars) > 0) {
    stop(paste("The following variables are not present in the dataset:", paste(missing_vars, collapse = ", ")))
  }
  
  # Filter the dataset
  filtered_data <- data %>%
    filter(complete.cases(across(all_of(vars))))
  
  # Return the filtered dataset
  return(filtered_data)
}


#######################################################################################
# Filters out users who have consulted fewer than a specified number of listings.
#
# Args:
# - data: A data frame containing the dataset.
# - user_var: A character string representing the name of the user identifier column.
# - min_listing_numb: An integer specifying the minimum number of listings a user must 
#   have consulted to be retained in the dataset (Default is 2).
#
# Returns:
# - A filtered data frame containing only users who meet the minimum threshold.
#######################################################################################

filter_users_by_listings_numb <- function(data, user_var, min_listing_numb = 2) {
  # Validate inputs
  if (!is.data.frame(data)) stop("The input 'data' must be a data frame.")
  if (!is.character(user_var) || length(user_var) != 1) {
    stop("The input 'user_var' must be a single character string.")
  }
  if (!user_var %in% names(data)) {
    stop(paste("The variable", user_var, "is not present in the dataset."))
  }
  if (!is.numeric(min_listing_numb) || min_listing_numb <= 0 || min_listing_numb != as.integer(min_listing_numb)) {
    stop("The input 'min_listing_numb' must be a positive integer.")
  }
  
  # Filter the dataset
  filtered_data <- data %>%
    group_by(across(all_of(user_var))) %>%
    mutate(listing_numb = n()) %>%
    filter(listing_numb >= min_listing_numb) %>%
    ungroup()
  
  # Return the filtered dataset
  return(filtered_data)
}


###################################################################################
# Takes a quantitative variable as input and classifies its values 
# into three classes ("Low", "Medium", "High") using the Fisher-Jenks method 
# for optimal breaks. It handles special cases where the variable has only 
# one or two unique values by returning fixed or simplified classifications.
#
# Args:
# - v: A numeric vector to be classified.
#
# Returns:
# A factor variable with four levels: "fixed", "Low", "Medium", "High".
###################################################################################

discretize_fisher_jenks <- function(v) {
  # Check if the input vector is numeric
  if (!is.numeric(v)) {
    stop("The input variable 'v' must be numeric.")
  }
  
  # Handle special cases based on the number of unique values in 'v'
  if (length(unique(v)) == 1) {
    # All values are the same
    classes <- factor(rep("fixed", length(v)), levels = c("fixed", "Low", "Medium", "High"))
  } else if (length(unique(v)) == 2) {
    # Only two unique values, split into "Low" and "High"
    gbreaks <- c(min(v), mean(range(v)), max(v))
    classes <- cut(v, breaks = gbreaks, labels = c("Low", "High"), include.lowest = TRUE)
    levels(classes) <- c("Low", "Medium", "High")  # Add missing "Medium" level for consistency
  } else {
    # General case: use Fisher-Jenks to determine the breaks
    gbreaks <- getBreaks(v, nclass = 3, method = "fisher-jenks")
    classes <- cut(v, breaks = gbreaks, labels = c("Low", "Medium", "High"), include.lowest = TRUE)
  }
  
  return(classes)
}

###########################################################################################
# Discretizes a numeric vector (v_cut) into four classes 
# ("lo", "me", "hi", "vhi") using the Fisher-Jenks method to determine the optimal 
# breaks. The breaks are calculated based on a second vector (v_breaks), which defaults 
# to the same as v_cut if not provided.
#
# Args:
# - v_breaks: A numeric vector used to calculate the breaks using the Fisher-Jenks method.
# - v_cut: A numeric vector to be discretized, defaults to the same as v_breaks.
#
# Returns:
# A factor variable with four levels: "lo", "me", "hi", "vhi".
###########################################################################################

global_cut <- function(v_breaks, v_cut = v_breaks) {
  # Calculate the breaks using Fisher-Jenks method
  gbreaks <- getBreaks(v_breaks, nclass = 4, method = "fisher-jenks")
  
  # Discretize the second vector (v_cut) into classes based on the calculated breaks
  classes <- cut(v_cut, breaks = gbreaks, labels = c("lo", "me", "hi", "vhi"), include.lowest = TRUE)
  
  return(classes)
}


######################################################################################################
# Discretizes variables in a dataset using breaks from another dataset.
# The new columns are named with the prefix "discretized_" to indicate discretization.
# Requires the function "global_cut".
#
# Args:
# - data_cut: A data frame containing the variables to be discretized.
# - vars_to_cut: A vector of variable names (strings) to be discretized.
# - data_breaks: A data frame containing the variables to get breaks from. Default is data_cut.
# - vars_for_breaks: A vector of variable names (strings) to get breaks from. Default is vars_to_cut.
#
# Returns:
# A data frame with the original variables and new columns for the discretized 
# variables (prefixed with "discretized_").
######################################################################################################

discretize_variables <- function(data_cut, vars_to_cut, data_breaks = data_cut, vars_for_breaks = vars_to_cut) {
  # Ensure that vars_to_cut and vars_for_breaks are the same length
  if (length(vars_to_cut) != length(vars_for_breaks)) {
    stop("The vectors 'vars_to_cut' and 'vars_for_breaks' must have the same length.")
  }
  
  for (i in seq_along(vars_to_cut)) {
    # Ensure the variable exists in the data_cut dataset
    if (!vars_to_cut[i] %in% names(data_cut)) {
      stop(paste0("Variable '", vars_to_cut[i], "' is not found in the '",  data_cut, "' dataset."))
    }

    # Ensure the variable exists in the data_breaks dataset
    if (!vars_for_breaks[i] %in% names(data_breaks)) {
      stop(paste0("Variable '", vars_for_breaks[i], "' is not found in the'", data_breaks, "'  dataset."))
    }
    
    # Create the new variable name
    new_var_name <- paste0("discretized_", vars_to_cut[i])
    
    # Apply the global_cut function
    data_cut[[new_var_name]] <- global_cut(
      v_cut = data_cut[[vars_to_cut[i]]],
      v_breaks = data_breaks[[vars_for_breaks[i]]]
    )
  }
  
  return(data_cut)
}







#########################################################################################
# Creates sequences from a dataset by discretizing specified variables using a 
# given discretization function. Organizes the data into sequence format and 
# generates sequences for each variable in the input list.
# Requires the `discretize_variables` function.
#
# Args:
# - data: The input dataset to process.
# - user_listing_id_var_names: A vector of variable names (as strings) specifying the 
#      user identifier, listing identifier, and event identifier, in that order.
# - chanels_var_names: A vector of variable names (as strings) specifying the 
#   variables to discretize and include in the sequence creation.
# - chanels_labels : clanels labels
#
# Returns:
# - A list of sequences, where each entry corresponds to a variable from 
#   `chanels_var_names`.
#########################################################################################

create_sequences <- function(data, user_listing_id_var_names, chanels_var_names, chanels_labels) {
  start_time <- Sys.time()
  seq_chanels_list <- list()

  # Extract variable names
  user_var <- user_listing_id_var_names[1]
  listing_var <- user_listing_id_var_names[2]
  event_id_var <- user_listing_id_var_names[3]

  # Extract unique listings for reference
  unique_listing <- data %>%
    distinct(!!sym(listing_var), .keep_all = TRUE)

  # Sort data by user and event ID, and create a rank variable
  data <- data %>%
    arrange(!!sym(user_var), !!sym(event_id_var)) %>%
    group_by(!!sym(user_var)) %>%
    mutate(ad = row_number()) %>%
    ungroup()

  # Process each channel variable
  for (i in seq_along(chanels_var_names)) {

    var_name <- chanels_var_names[i]
    var_lab  <- chanels_labels[i] 
    # Discretize the variable using the provided discretization function
    data <- discretize_variables(
      data_cut = data,
      vars_to_cut = var_name,
      data_breaks = unique_listing,
      vars_for_breaks = var_name
    )

    # Rename the discretized variable for clarity
    discretized_var <- paste0("discretized_", var_name)
    evolution_var   <- paste0(var_name, "_evol")
    data <- data %>%
      rename(!!sym(evolution_var) := !!sym(discretized_var))

    # Transform the data to wide format using pivot_wider
    wider_data <- data %>%
      dplyr::select(!!sym(user_var), ad, !!sym(evolution_var)) %>%
      pivot_wider(
        names_from = ad,
        values_from = !!sym(evolution_var),
        names_prefix = "ad_"
      )

    # Extract sequence matrix and create sequences
    seq_matrix <- wider_data %>% dplyr::select(-!!sym(user_var))

    sequences <- seqdef(
      seq_matrix,
      id = wider_data[[user_var]],
      states = sort(paste0(levels(data[[evolution_var]]), "_", substr(var_lab, 1, 2)))
    )

    # Store the sequences in the list
    seq_chanels_list[[var_name]] <- sequences
  }

  end_time <- Sys.time()
  print(end_time - start_time)

  return(seq_chanels_list)
}



###########################################################################################
# Extracts submarkets sequences from a dataframe containing a submarket variable
# Args:
# - data: A data frame containing information about users, event ID and submarket variable.
# - user_id_var: A character string representing the name of the user identifier column.
# - event_id_var : A variable (probably  a character string, but not necessary) identifying 
#       the event order.
# - submarket_var : A variable from data giving the submarket number a ad belongs to
# - min_listing_numb: An integer specifying the minimum number of listings a user must 
#   have consulted to be retained in the dataset (Default is 2).
#
# Returns:
# - A filtered data frame containing only users who meet the minimum threshold.
###########################################################################################

extract_submarket_seq <- function(data, user_id_var, event_id_var, submarket_var, min_listing_numb = 2) {

  # 1. Sort events and create a within-user event index
  data <- data %>%
    arrange(!!sym(user_id_var), !!sym(event_id_var)) %>%
    group_by(!!sym(user_id_var)) %>%
    mutate(ad = row_number()) %>%
    ungroup()

  # 2. Filter users with enough events
  data_submarket <- filter_users_by_listings_numb(
    data = data,
    user_var = c(user_id_var),
    min_listing_numb = min_listing_numb
  )

  # 3. Wide format
  data_submarket_large <- data_submarket %>%
    dplyr::select(!!sym(user_id_var), ad, !!sym(submarket_var)) %>%
    tidyr::pivot_wider(
      names_from  = ad,
      values_from = !!sym(submarket_var),
      names_prefix = "ad"
    )

  # 4. Extract matrix
  mat_submarket <- data_submarket_large %>% 
    dplyr::select(-!!sym(user_id_var))

  # 5. Recode numeric states → "sbm_x"
  mat_submarket <- as.data.frame(lapply(mat_submarket, function(col) {
    ifelse(is.na(col), NA, paste0("sbm_", col))
  }))

  # 6. Build alphabet from observed values
  alphabet_real <- sort(unique(unlist(mat_submarket)))
  alphabet_real <- alphabet_real[!is.na(alphabet_real)]

  # 7. Build sequence object
  seq_submarket <- seqdef(
    mat_submarket,
    id       = data_submarket_large[[user_id_var]],
    alphabet = alphabet_real,
    states   = alphabet_real,
    labels   = alphabet_real,
    xtstep   = 1
  )

  return(seq_submarket)
}

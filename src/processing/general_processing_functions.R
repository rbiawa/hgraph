#########################################################################################
# Functions used for general processing.
# 
# 2024/11 Raphaël Biawa Diegamounoua
#
# source("src/processing/general_processing_functions.R")
#########################################################################################






##############################################################################
# Filter the locality (requires rlang package).
#
# Args:
# data: the input dataset.
# loc_var: locality variable.
# loc_val: locality value.
# 
# Returns: 
#   A filtered dataset observations of which verify loc_var = loc_val.
##############################################################################

filter_locality <- function(data, loc_var, loc_val) {

  data %>% filter(!!sym(loc_var) == loc_val)
}




##############################################################################
# Filter the dataset based on specified localities.
#
# Args:
#   data: The input dataset.
#   loc_var_names: A vector of locality variable names (e.g., region, department, city).
#   loc_val: A vector of locality values to filter on.
#
# Returns:
#   A filtered dataset where loc_var_names contains loc_val, or an error if not found.
##############################################################################

filter_locality_optimal <- function(data, loc_var_names, loc_val) {
  if (any(loc_val %in% unique(data[[loc_var_names[1]]]))) {
    return(data %>% filter(!!rlang::sym(loc_var_names[1]) %in% loc_val))
  } else if (any(loc_val %in% unique(data[[loc_var_names[2]]]))) {
    return(data %>% filter(!!rlang::sym(loc_var_names[2]) %in% loc_val))
  } else if (any(loc_val %in% unique(data[[loc_var_names[3]]]))) {
    return(data %>% filter(!!rlang::sym(loc_var_names[3]) %in% loc_val))
  } else {
    stop(paste0(
      loc_val, " is neither a valid value in ", 
      paste(loc_var_names, collapse = ", "), 
      ". Please check the locality variables or value."
    ))
  }
}



###################################################################################
# Function to get a valid numeric input from the user.
# The function repeatedly prompts the user until a valid numeric value is entered.
#
# Args:
#   prompt: A string message to prompt the user for input.
#
# Returns:
#   A numeric value entered by the user.
###################################################################################

get_numeric_input <- function(prompt) {
  repeat {
    # Ask for input
    user_input <- readline(prompt)
    
    # Attempt to convert input to numeric
    numeric_value <- suppressWarnings(as.numeric(user_input))
    
    # Check if the input is valid (not NA)
    if (!is.na(numeric_value)) {
      return(numeric_value)  # Return the numeric value if valid
    } else {
      cat("Invalid input. Please enter a valid numeric value.\n")  # Error message for invalid input
    }
  }
}



###################################################################################
# Remove an object if present in global environment
#
# Args:
#   object name.
###################################################################################


rm_if_exists <- function(obj_name) {
  if (exists(obj_name, envir = .GlobalEnv)) {
    rm(list = obj_name, envir = .GlobalEnv)
  }
}


###################################################################################
# Plot conitnuous variables
#
# Args:
#   data : a dataframe.
# 
# Returns : a list of plots
###################################################################################


plot_continuous_variables <- function(data, transformation = "identity") {
  
  if (! transformation %in% c("identity", "log", "log1p"))
    stop("transformation must be identity, log or log1p")
  
  # transformation function
  tfun <- switch(
    transformation,
    identity = identity,
    log      = function(x) log(x),
    log1p    = function(x) log1p(x)
  )
  
  continuous_vars <- names(data)[sapply(data, is.numeric)]
  results <- list()
  
  for (var in continuous_vars) {
    
    x <- tfun(data[[var]])
    x <- na.omit(x)
    
    quartiles <- quantile(x, probs = c(0.25, 0.5, 0.75))
    mean_val  <- mean(x)
    
    p1 <- ggplot(data, aes(x = tfun(.data[[var]]))) +
      geom_density(fill = "lightgray") +
      geom_vline(xintercept = quartiles, color = "blue", linetype = "dashed") +
      geom_vline(xintercept = mean_val, color = "red") +
      labs(x = paste0(transformation, "(", var, ")"), y = "Density") +
      theme_minimal()
    
    p2 <- ggplot(data, aes(x = "", y = tfun(.data[[var]]))) +
      geom_violin(fill = "lightblue") +
      geom_boxplot(width = 0.2, fill = "white") +
      labs(y = paste0(transformation, "(", var, ")")) +
      theme_minimal()
    
    results[[var]] <- gridExtra::grid.arrange(p1, p2, ncol = 2)
  }
  
  results
}


###################################################################################
# Plot categorical variables
#
# Args:
#   data : a dataframe.
# 
# Returns : a list of plots
###################################################################################


plot_categorical_variables <- function(data, use_percent = FALSE) {
  
  categorical_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.logical(x))]
  results <- list()
  
  for (var in categorical_vars) {
    
    p <- ggplot(data, aes(x = .data[[var]]))
    
    if (use_percent) {
      p <- p +
        geom_bar(aes(y = after_stat(count / sum(count))),
                 fill = "lightblue", color = "black") +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(
          title = paste("Histogram of", var),   # <-- titres en anglais
          x = var,
          y = "Percentage"
        )
    } else {
      p <- p +
        geom_bar(fill = "lightblue", color = "black") +
        labs(
          title = paste("Histogram of", var),   # <-- titres en anglais
          x = var,
          y = "Count"
        )
    }
    
    p <- p +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    results[[var]] <- p
  }
  
  return(results)
}


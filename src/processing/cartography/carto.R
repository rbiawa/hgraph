#########################################################################################
# Functions used for cartographic data processing.
# 
# 2024/11 Raphaël Biawa Diegamounoua
#
# source("src/processing/cartography/carto.R")
#########################################################################################






###################################################################################
# Filters spatial data based on a vector of region codes and selects specified columns.
#
# data: the input spatial dataframe.
# filter_column: the name of the column used for filtering (as a string).
# region_codes: a vector of region codes used to filter the data.
# selected_columns: a vector of column names to retain in the output.
#
# returns: a spatial dataframe filtered by the region codes and retaining selected columns.
###################################################################################

filter_spatial_data <- function(data, filter_column, region_codes, selected_columns) {
  # Filter data by the vector of region codes
  filtered_data <- data %>% filter(!!sym(filter_column) %in% region_codes)
  
  # Select only the specified columns
  filtered_data <- filtered_data %>% dplyr::select(all_of(selected_columns))
  
  return(filtered_data)
}



#############################################################################################
# Filters a dataset by excluding rows that match a vector of codes in a specified column.
#
# data: the input dataframe to be filtered.
# filter_column: the name of the column used for filtering (as a string).
# exclude_codes: a vector of codes to exclude from the dataset.
#
# returns: a dataframe filtered to exclude rows where the filter_column matches any code
# in the exclude_codes vector.
#############################################################################################

exclude_rows_by_codes <- function(data, filter_column, exclude_codes) {
  # Filter the data by excluding rows where the column matches the codes in exclude_codes
  filtered_data <- data %>% filter(! (!!sym(filter_column) %in% exclude_codes))
  return(filtered_data)
}





#############################################################################################
# Filters city municipal district data based on a vector of city codes.
#
# data: the input spatial dataframe containing city districts.
# city_code_var: the column name containing city codes (default is "INSEE_COM").
# city_code_vals: a vector of city codes to filter (e.g., c(75056, 13055) for Paris and Marseille).
# selected_columns: a vector of column names to retain in the output (must be specified by the user).
#
# returns: a spatial dataframe filtered to include only the specified cities' districts
#          and retaining only the selected columns.
#############################################################################################

filter_districts <- function(data, city_code_var = "INSEE_COM", city_code_vals, selected_columns) {
  # Check if selected_columns is provided
  if (missing(selected_columns)) {
    stop("You must specify the columns to retain using the 'selected_columns' argument.")
  }
  
  # Filter data by the vector of city codes
  filtered_data <- data %>% filter(!!sym(city_code_var) %in% city_code_vals)
  
  # Select only the specified columns
  filtered_data <- filtered_data %>% dplyr::select(all_of(selected_columns))
  
  return(filtered_data)
}






#######################################################################
# Joins attribute data with spatial geometry data.
#
# attribute_data: the input attribute dataset (dataframe or sf object).
# spatial_data: the spatial data with geometries (sf object).
# join_columns: the columns used for joining (default: names(spatial_data)).
#
# returns: a spatial dataframe with attributes and geometry combined.
#######################################################################

join_spatial_data <- function(attribute_data, spatial_data, join_columns = names(spatial_data)) {

  joined_data <- st_join(attribute_data %>% st_as_sf(),
    spatial_data %>% dplyr::select(all_of(join_columns)),
    join = st_within
  )
  return(joined_data)

}



print("Script 'carto.R' completed.")

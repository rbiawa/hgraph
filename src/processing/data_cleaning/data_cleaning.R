#########################################################################################
# Functions used for cleaning the data.
# 
# 2024/11 Raphaël Biawa Diegamounoua
#
# source("src/processing/data_cleaning/data_cleaning.R")
#########################################################################################




#Selection of complete cases

filtered_event_data <- filtered_event_data %>%
  filter(
    complete.cases(listing_id, event_id, region, subregion, city, current_price, area, room_count, item_type)
  )


# Delete duplicates of ads viewed more than once by the same user

filtered_event_data <- filtered_event_data %>%
  distinct(user_uid, listing_id, .keep_all = TRUE)



# Create the variable that counts the number of ads viewed by each user

filtered_event_data <- filtered_event_data %>%
  group_by(user_uid) %>%
  mutate(nombre_annonces_consultees = n())%>% 
  ungroup()


filtered_event_data <- filtered_event_data %>%
  group_by(user_uid) %>%
  filter(nombre_annonces_consultees > 1) %>% 
  ungroup()


# Remove users who are “outliers” in terms of the number of ads conculted

boxplot(filtered_event_data$nombre_annonces_consultees)



while(length(boxplot.stats(filtered_event_data$nombre_annonces_consultees)$out)>0){
  filtered_event_data <- filtered_event_data %>% filter(!nombre_annonces_consultees %in% boxplot.stats(filtered_event_data$nombre_annonces_consultees)$out)
}




boxplot(filtered_event_data$nombre_annonces_consultees)


# Unvovering relevant research

# Create the variable that calculates the average price of ads viewed by each user

filtered_event_data <- filtered_event_data %>%
  group_by(user_uid) %>%
  mutate(prix_moyen_annonces = mean(current_price))


# Create the variable that calculates the relative deviation of each price from the average for each user

filtered_event_data <- filtered_event_data %>%
  mutate(ecart_relatif_prix = abs(current_price - prix_moyen_annonces) / prix_moyen_annonces)



# Create the “coherent” variable, which indicates whether the relative deviation of the price from the average is less than 0.25

filtered_event_data <- filtered_event_data %>%
  mutate(coherent = (ecart_relatif_prix < 0.25))




# Create the variable “taux_coherence” which represents the consistency rate in percentage for each user.

filtered_event_data <- filtered_event_data %>%
  group_by(user_uid) %>%
  mutate(taux_coherence = 100 * sum(coherent) / n())




# Delete observations with a “coherence_rate” of less than 80%.

filtered_event_data <- filtered_event_data %>%
  filter(taux_coherence >= 80)


##################################################
## 		   SPATIAL COORDINATES OF ADS           ##----
##################################################



# Recovery of spatial information from estimates

loc_estim <- estimation_raw_data %>% dplyr::select(street_id, address_id, zip, coordinates) %>% distinct()


# Transformation of WKB strings (PostGIS format) into sf format (R format)

loc_estim$geometry = st_as_sfc(structure(as.list(loc_estim$coordinates), class = "WKB"), EWKB = T, crs=4326)
loc_estim <- st_as_sf(loc_estim)




# Delete rows with missing values in the “address_id” column
loc_estim <- loc_estim %>%
  filter(!is.na(address_id))

# Delete duplicates in the “address_id” column

loc_estim <- loc_estim %>%
  distinct(address_id, .keep_all = TRUE)



# Selection of listings and their address and street information

annonces_zone <- filtered_event_data %>% dplyr::select(listing_id, event_id, address_id, street_id, current_price, area, room_count, region, subregion, arrmun, city, item_type) %>% distinct()


filtered_event_data_sf <- left_join(annonces_zone, loc_estim, by=c("address_id"="address_id")) %>% st_as_sf()


filtered_event_data_gps <- left_join(filtered_event_data, loc_estim, by=c("address_id"="address_id"))


# Filter observations with non-NA values in the “coordinates” column

filtered_event_data_gps_filtered <- filtered_event_data_sf %>% filter(!st_is_empty(geometry))




filtered_event_data_gps_filtered$lon <- st_coordinates(filtered_event_data_gps_filtered)[,1]
filtered_event_data_gps_filtered$lat <- st_coordinates(filtered_event_data_gps_filtered)[,2]


filtered_event_data <- filtered_event_data_gps_filtered





###################################################################################
# Function to normalize strings
#
# Args:
#   x : variable name or a string.
###################################################################################




normalize_string <- function(x) {
  require(stringi)
  x <- stri_trans_general(x, "Latin-ASCII")  # Remove the accents
  x <- gsub("[-']", " ", x)  # Replace dashes and apostrophes with spaces
  x <- tolower(x)  # Convert to lowercase
  x <- gsub("\\s+", " ", x)  # Remove multiple spaces
  x <- trimws(x)  # Remove unnecessary spaces at the beginning and end of the string.
  return(x)
}








##################################################################
# Compute binary normalized entropy
#
# Args :
#   - x a table of categorical variable
#
# Returns : normalized entropy
##################################################################


normalized_entropy <- function(x) {
  k <- length(x)
  if (k > 1) {
    entropy(x, unit = "log2") / log2(k)
  } else {
    0
  }
}


#########################################################################################
# A script to load raw data
# 
# 2024/12 Raphaël Biawa Diegamounoua
#
# source("src/raw_data_loading.R")
#########################################################################################




######################################
# Loading packages

source("src/packages_loading.R")




########################################
#Retrieve functions

source("src/function_retrieving.R")

########################################
# GLOBAL PROCESSING PARAMETERS

source("src/global_parameters.R")


CITY_COLUMNS <- c("city_ID", "city_Name", "INSEE_COM", "INSEE_DEP", 
                  "INSEE_REG", "INSEE_CAN", "SIREN_EPCI",  "POPULATION")

########################################
# A hadoc function for data loading

##################################################################################
#' Load and merge all .parquet files from a given year
#'
#' Args:
#' dir_path : Character. Path to the directory containing .parquet files
#' year : Integer or character. The target year to filter files
#' 
#' return : A data.table containing all combined rows from that year
##################################################################################


load_parquet_year <- function(dir_path, year) {

  # Ensure 'year' is character for pattern matching
  year <- as.character(year)

  # List all .parquet files that include the year in their name
  files <- list.files(
    path = dir_path,
    pattern = paste0("data_guilhem_.*", year, "\\.parquet$"),
    full.names = TRUE
  )

  # Stop if no file is found
  if (length(files) == 0) {
    stop(paste("No .parquet files found for year", year, "in", dir_path))
  } else {
    message(length(files), " file(s) found for year ", year)
  }

  # Read and combine all parquet files
  data_list <- lapply(files, read_parquet)
  combined_data <- rbindlist(data_list, use.names = TRUE, fill = TRUE)

  return(combined_data)
}







######################################
#                                    #
#       Loading INSEE shapefiles     #
#                                    #
######################################


print("Load INSEE spatial data")

geom_sf_cities      <- st_read("in/carto/COMMUNE.shp") %>%
  rename(city_ID = ID,
         city_Name = NOM)


geom_sf_departements <- st_read("in/carto/DEPARTEMENT.shp") %>%
  rename(dep_ID = ID,
         dep_Name = NOM)


geom_sf_districts    <- st_read("in/carto/ARRONDISSEMENT_MUNICIPAL.shp") %>%
  rename(city_ID = ID,
         city_Name = NOM)




geom_sf_cities <- geom_sf_cities %>%
  dplyr::select(all_of(CITY_COLUMNS))


geom_tab_metropolitan <- left_join(geom_sf_districts,
                                   st_drop_geometry(geom_sf_cities) %>%
                                    dplyr::select(INSEE_COM, INSEE_DEP, INSEE_REG, INSEE_CAN, SIREN_EPCI),
                                   by = "INSEE_COM") %>%
                                   dplyr::select(city_ID, city_Name, INSEE_COM, INSEE_DEP, INSEE_REG, INSEE_CAN,
                                   SIREN_EPCI, POPULATION)




geom_tab_metropolitan   <- filter_districts(geom_tab_metropolitan,
  DISTRICTS_CITY_CODE_VAR,
  DISTRICTS_CITY_CODE_VAL,
  CITY_COLUMNS
)

geom_sf_cities   <- exclude_rows_by_codes(geom_sf_cities,
  IDENT_COL,
  METROPOLITAN_CITIES
)


geom_sf_cities   <- rbind(geom_sf_cities, geom_tab_metropolitan)


#=========================
# Maille habitat
#=========================

maille_habita <- st_read("in/carto/maille_habitat/maille.gpkg")

if(interactive()) View(maille_habita)

maille_habita_data <- read.csv("in/carto/maille_habitat/data/maille_40_40000.csv", sep = ";")

if(interactive()) View(maille_habita_data)

#=========================
# Join maille habitat with
#    city data
#=========================

city_geom_data <- filter_spatial_data(data = geom_sf_cities,
  filter_column    = REGIONS_CODE_VAR,
  region_codes     =  REGIONS_CODE_VAL,
  selected_columns = CITY_COLUMNS
)

city_centroid <- st_point_on_surface(city_geom_data)

if(interactive()) View(city_centroid)


st_crs(city_centroid)
st_crs(maille_habita)
maille_habita <- st_transform(maille_habita, st_crs(city_centroid))

city_centroid <- st_join(city_centroid, maille_habita, join = st_within)

city_maille_habitat <- st_drop_geometry(city_centroid)

if(interactive()) View(city_maille_habitat)

city_indicators <- read.csv("in/carto/maille_habitat/data/bdd_indicateurs.csv", sep = ";")

city_maille_habitat <- dplyr::left_join(city_maille_habitat,
  city_indicators %>%
    dplyr::select(Codgeo, starts_with("indic.")),
  by = join_by(INSEE_COM == Codgeo)
)

#=========================
# geometry data
#=========================

dep_geom_data <- filter_spatial_data(data = geom_sf_departements,
    filter_column = REGIONS_CODE_VAR,
    region_codes =  REGIONS_CODE_VAL,
    selected_columns = c("dep_ID", "dep_Name", "INSEE_DEP", "INSEE_REG")
  )

geom_sf_data <- switch(ANALYSIS_SCALE,
  "city"      = filter_spatial_data(data = geom_sf_cities,
    filter_column    = REGIONS_CODE_VAR,
    region_codes     =  REGIONS_CODE_VAL,
    selected_columns = CITY_COLUMNS
),
  "subregion" = filter_spatial_data(data = geom_sf_departements,
    filter_column = REGIONS_CODE_VAR,
    region_codes =  REGIONS_CODE_VAL,
    selected_columns = c("dep_ID", "dep_Name", "INSEE_DEP", "INSEE_REG")
  )
)





######################################
#                                    #
#   Loading consultation datasets    #
#                                    #
######################################

print("Load consultation data")

if(DATA_SOURCE == "MA") {
  source("src/processing/data_cleaning/data_MA.R")
} else {
  source("src/processing/data_cleaning/listing_features_data_SL.R")

if(EVENT_ANALYSIS)  source("src/processing/data_cleaning/import_SL_event_data.R")
}


#===================================
#
# Save data
#
#===================================

if(TRUE) save(features, events_year,
              file = "out/Rdata/serious_search/events_features.RData")
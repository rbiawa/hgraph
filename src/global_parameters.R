#########################################################################################
# A script to set global parameters
# 
# 2024/11 Raphaël Biawa Diegamounoua
#
# source("src/global_parameters.R")
#########################################################################################








##############################################
# DATA SOURCE

DATA_SOURCE <- factor("MA", levels = c("MA", "Se_loger"))

DATA_SOURCE <- c("MA", "Se_loger")[2]


YEAR <- 2021




##############################################
# INSEE DATA (shapefiles)

USE_INSEE_SF_DATA         <- logical(1)

DO_SPATAIAL_ANALYSE       <- logical(1)

ANSWER <- TRUE

DO_SPATAIAL_ANALYSE       <- if(DATA_SOURCE == "Se_loger"){TRUE} else {
   ANSWER
}




###############################################
#  ANALYSIS TYPE

 if (!exists("EVENT_ANALYSIS")) EVENT_ANALYSIS <- TRUE





##############################################
# ANALYSING PARAMETERS

LOC_ORDER                       <- c(1, 2)[1]

ANALYSIS_SCALE            <- c("subregion", "city")[2]

LOCALITY_VAL              <- if(DATA_SOURCE == "MA"){
    c("Ile-de-France", "Hauts-de-France")
} else {
    c("11", "32")
}[LOC_ORDER] # [1:2] #c("Ile-de-France", "Hauts-de-France")

LOCALITY_NAME             <- if(length(LOCALITY_VAL)>1) {
                            "IDF-HDF"} else{switch(LOCALITY_VAL
                                                   , "Ile-de-France"   = "IDF"
                                                   , "Hauts-de-France" = "HDF"
                                                   , "11"              = "IDF"
                                                   , "32"              = "HDF"
                                                  )
}

SEQUENCES_CLASSES_NUMBER  <- 5


MIN_LISTING_NUMBER <- 2



######################################
# Adding INSEE variables while working on regions or subregions

#if(LOCALITY_NAME %in% unique(unlist(consultation_raw_data[c(REGION_VAR_NAME, SUB_REGION_VAR_NAME)]))) {

if (DO_SPATAIAL_ANALYSE)    USE_INSEE_SF_DATA   <- TRUE

INSEE_COLUMNS                   <- switch(ANALYSIS_SCALE,
  "subregion"  =  c("dep_ID", "dep_Name", "INSEE_DEP", "INSEE_REG"),
  "city"       =  c("city_ID", "city_Name", "INSEE_DEP", "INSEE_REG")
)

IDENT_COL                       <- c("city_ID")
METROPOLITAN_CITIES             <- c('COMMUNE_0000000009736048')

REGIONS_CODE_VAR                <- "INSEE_REG" 
REGIONS_CODE_VAL                <- c("11", "32")[LOC_ORDER] 

DISTRICTS_CITY_CODE_VAR         <- c("INSEE_COM")
DISTRICTS_CITY_CODE_VAL         <- c(75056)        # "13055" "75056" "69123" (Marseille, Paris, Lyon)










##############################################
# DATA RELATED PARAMETERS

if(DATA_SOURCE == "MA") {

REGION_VAR_NAME           <- "region"
SUB_REGION_VAR_NAME       <- "subregion"
CITY_VAR_NAME             <- "city"

USER_VARS_NAME            <- "user_uid"

LISTING_VAR_NAME          <- "listing_id"

EVENT_ID_VAR              <- "event_id"



USER_LISTING_VARS_NAMES   <- c(USER_VARS_NAME, LISTING_VAR_NAME) 

LOCALITY_VARIABLE_NAME    <- c(REGION_VAR_NAME, SUB_REGION_VAR_NAME, CITY_VAR_NAME)


} else {

REGION_VAR_NAME           <- "INSEE_REG"
SUB_REGION_VAR_NAME       <- "subregion"
CITY_VAR_NAME             <- "city"

USER_VARS_NAME            <- "fullvisitorid"

LISTING_VAR_NAME          <- "id_listing"

EVENT_ID_VAR              <- "order"

USER_LISTING_VARS_NAMES   <- c(USER_VARS_NAME, LISTING_VAR_NAME) 

LOCALITY_VARIABLE_NAME    <- c(REGION_VAR_NAME, SUB_REGION_VAR_NAME, CITY_VAR_NAME)

}



## DISTANCE CORRELATION PARAMETERS

PERFORM_MANTEL <- logical(1)

PERFORM_MANTEL <- FALSE


PLOT_DISTANCE <- logical(1)

PLOT_DISTANCE <- FALSE

## SIMULATION PARAMETERS

LOUVAIN_SIM <- 200






# SAVE PARAMETERS
GRAPH_PDF_SAVING_DIRECTORY        <- "out/pdf/graphs"
XLSX_SAVING_DIRECTORY             <- "out/xlsx"

GRAPH_TXT_SAVING_DIRECTORY        <- "out/txt/graphs"

GRAPH_PNG_SAVING_DIRECTORY        <- "out/png/graphs"

GRAPH_SVG_SAVING_DIRECTORY        <- "out/svg/graphs"

dir.create(GRAPH_PDF_SAVING_DIRECTORY, recursive = TRUE, showWarnings = FALSE)
dir.create(XLSX_SAVING_DIRECTORY, recursive = TRUE, showWarnings = FALSE)
dir.create(GRAPH_TXT_SAVING_DIRECTORY, recursive = TRUE, showWarnings = FALSE)
dir.create(GRAPH_PNG_SAVING_DIRECTORY, recursive = TRUE, showWarnings = FALSE)
dir.create(GRAPH_SVG_SAVING_DIRECTORY, recursive = TRUE, showWarnings = FALSE)

# OUTFILES PARAMETERS

ANALYSE_SCALE_OUT           <- ANALYSIS_SCALE

LOCALITY_NAME_OUT           <- LOCALITY_NAME 

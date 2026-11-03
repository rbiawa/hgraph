#########################################################################################
# A script to perform a sequence analysis
# 
# 2024/11 Raphaël Biawa Diegamounoua
#
# source("src/execution/sequence_extraction_execution.R")
#########################################################################################




# Set working directory

setwd(paste(getwd(), "These_Raphael", sep = "/"))


#Retrieve functions

source("src/function_retrieving.R")






##########################################################################################
# ANALYSING PARAMETERS

ANALYSIS_SCALE            <- "city" #(see in "src/processing/general_processing_functions.R")

LOCALITY_NAME             <- "Versailles" #(see in "src/processing/general_processing_functions.R")

SEQ_ANALYSIS_CHANELS      <- c("current_price", "area")

CHANELS_LABELS            <- c("price", "area")













######################################
#Loading consultation datasets


consultation_raw_data   <- read_csv("in/raw/MA/AnnIDF19.csv")







#########################################################################################################
# BEGIN PROCESSING
#########################################################################################################


######################################
# Filtering the data to retain only the desired locality.

filtered_data_frame <- filter_locality_optimal(consultation_raw_data, LOCALITY_VARIABLE_NAME , LOCALITY_NAME)



# Calculates the proportion of listings consulted more than once by the same user.

calculate_repeat_proportion(filtered_data_frame, USER_LISTING_VARS_NAMES)


# Filter out users who have consulted fewer than a specified number of listings.

seq_data <- filter_users_by_listings_numb(filtered_data_frame, USER_VARS_NAME)


# Filter complet cases
seq_data <- seq_data %>%
  filter(complete.cases(across(c(LOCALITY_VARIABLE_NAME, USER_LISTING_VARS_NAMES, EVENT_ID_VAR, SEQ_ANALYSIS_CHANELS))))


# Analysing consulted listing numbers outlier

boxplot(seq_data$listing_numb)


# TO BE CHECKED 
while(length(boxplot.stats(seq_data$listing_numb)$out)>0){
      seq_data <- seq_data %>% filter(!listing_numb %in% boxplot.stats(seq_data$listing_numb)$out)
  } 

boxplot(seq_data$listing_numb)


# Create sequences list

seq_list <- create_sequences(seq_data, c(USER_LISTING_VARS_NAMES, EVENT_ID_VAR), SEQ_ANALYSIS_CHANELS, CHANELS_LABELS)


######################################
# SEQUENCE CLASSIFICATION (ACH Classification Using Ward's Method)

## CAT multidomain distances based on channel specific cost methods
MDdist <- seqMD(seq_list, method="OM",
    sm =list("INDELSLOG", "INDELSLOG"), what="diss")

## Retrieving the MD sequences
MDseq <- seqMD(seq_list)
alphabet(MDseq)


# Classification of sequences using the previous matrix as a sequence similarity matrix in a hierarchical clustering (HC)

seq.distMD <- hclust(as.dist(MDdist), method = "ward.D2")



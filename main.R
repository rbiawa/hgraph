##############################################################################
# Main processing script
# 
# 
#
# source("main.R")
##############################################################################


#==============================
# Processing parameters
#==============================

# Leave R after processing ? If so, set LEAVE_R to TRUE
LEAVE_R <- FALSE

# Event action label

 ## for phone call (or number displaying)
PHONE_NUMBER_DISPLAY <- "phone_display-number" 

 ## For mail form submission
MAIL_FORM_SUBMISSION <- "mail_form-submitted"

# Spatial variable names for department and city
DEP_ID_VARIABLE      <- "dep_ID"
CITY_ID_VARIABLE     <- "city_ID"


# Set the sample size for clustering (is set to 100 as default value)
SAMPLE_PROPORTION    <- 100 # in [0,100]



#=========================
# Create directories
#=========================

source("src/create_directories.R")

#=========================
# Laod packages
#=========================

source("src/packages_loading.R")

#=========================
# Load data
#=========================

source("src/load_data.R")

#============================
# Compute contact indicators
#============================

source("src/serious_search/contact_indicators.R")

#===============================================
# Script to compute listings' revisit indicators 
#   and search variability indicators
#===============================================

source("src/serious_search/serious_search_through_revisit.R")


#=============================================================================
# Script to analyze search engagement through listings' revisit indicators and 
#      search variability indicators
#=============================================================================

source("src/serious_search/serious_search_through_revisit_analysis.R")



#=============================================================================
# Script to analyze search engagement through listings' revisit indicators and 
#      search variability indicators on a sample of the online searchers
#=============================================================================

source("src/serious_search/serious_search_through_revisit_sample_analysis.R")

#===========================================
# Behavioural Temporality: Revist vs Contact
#===========================================

source("src/serious_search/revisit_contact_temporality.R")


#===========================================
# Analyze the revisiting temporality
#===========================================

source("src/serious_search/revisit_time_analysis.R")



#==============================
# Plot serious search graphics
#==============================

source("src/plot/serious_search_pdf_plot.R")


#====================================================
# Plot serious search clustering graphics on a sample
#====================================================

# source("src/plot/sample_search_cluster_plot.R")

#================================= 
# Export tables into latex format
#================================= 

# source("src/plot/save_tables.R")



#===========================
# Close R ?
#===========================

if (LEAVE_R) q(save = "no", status = 0, runLast = FALSE)

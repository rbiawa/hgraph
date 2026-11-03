##############################################################################
# Packages used during the thesis.
# 
# 2024/11 Raphaël Biawa Diegamounoua
#
# source("src/packages_loading.R")
##############################################################################







#######################################################################################################
# Function that checks if a package is installed or not. If yes, it loads it into the session
# otherwise, it downloads it before loading it into memory.
#
# package_name: a string of a package name , which must be enclosed in quotation marks ("").
# 
#######################################################################################################


retrieve_package <- function(package_name) {

  # Check if package is already installed
  if (!requireNamespace(package_name, quietly = TRUE)) {

    # Try installing the package
    install.packages(
      package_name,
      repos = "https://cloud.r-project.org",
      type = "source",
      dependencies = TRUE,
      verbose = FALSE
    )

    # Check again after installation
    if (requireNamespace(package_name, quietly = TRUE)) {
      cat("Package", package_name, "installed successfully!\n")
    } else {
      cat("ERROR: Package", package_name, "could NOT be installed.\n")
      return(invisible(FALSE))
    }
  }

  # Load the package
  library(package_name, character.only = TRUE, verbose = FALSE)
  invisible(TRUE)
}





#######################################################################################################
# Set parallel plan based on the environment (local or cluster) and operating system
# 
# - Uses 'cluster' mode if running on a cluster (e.g., SLURM, PBS).
# - On Windows: defaults to multisession.
# - On Linux/macOS: defaults to multicore unless a cluster is detected.
#######################################################################################################

# This function configures a safe and portable parallelization plan.
# It works in all environments: local machines, Jean-Zay interactive sessions,
# and Slurm batch jobs. It never exceeds the number of CPUs actually available.
# 
# Key idea:
# - In interactive sessions on Jean-Zay, cgroups restrict R to 1 CPU even if
#   Slurm allocates more cores. Trying to use more workers causes errors.
# - In batch jobs, cgroups correctly expose all allocated CPUs.
# - Therefore, we take the minimum between:
#       (1) CPUs visible to R (cgroups)
#       (2) CPUs allocated by Slurm (SLURM_CPUS_PER_TASK)
# - This guarantees stability and avoids all parallelly/future errors.

set_parallel_plan <- function() {
  # Check required packages
  if (!requireNamespace("furrr", quietly = TRUE) ||
      !requireNamespace("future", quietly = TRUE) ||
      !requireNamespace("parallelly", quietly = TRUE)) {
    stop("Packages 'furrr', 'future', and 'parallelly' are required.")
  }

  # Number of CPUs actually visible to R (cgroups, Docker, Singularity, etc.)
  cores_cgroups <- parallelly::availableCores()

  # Number of CPUs allocated by Slurm (batch jobs)
  cores_slurm <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))

  # Final number of workers = minimum of the two
  n_workers <- max(1L, min(cores_cgroups, cores_slurm))

  # Choose backend depending on OS
  if (.Platform$OS.type == "windows") {
    future::plan(future::multisession, workers = n_workers)
    plan_type <- sprintf("multisession (%d workers)", n_workers)
  } else {
    # On Linux/macOS: use multicore if supported, otherwise multisession
    if (future::supportsMulticore()) {
      future::plan(future::multicore, workers = n_workers)
      plan_type <- sprintf("multicore (%d workers)", n_workers)
    } else {
      future::plan(future::multisession, workers = n_workers)
      plan_type <- sprintf("multisession (%d workers)", n_workers)
    }
  }

  # Diagnostic message
  message(
    "Parallelization plan set to: ", plan_type,
    " | cgroups=", cores_cgroups,
    " | slurm=", cores_slurm
  )
}







######################################################
##                   LOADIGN PACKAGES               ##
######################################################

# For general settings
retrieve_package("lubridate") # For date variables
retrieve_package("gridExtra") # For window management



retrieve_package("arrow") # for ".parquet" file loading
retrieve_package("purrr")
retrieve_package("dplyr")
retrieve_package("tidyr")
retrieve_package("knitr")
retrieve_package("stringr")
retrieve_package("questionr")
retrieve_package("readr")
retrieve_package("ggplot2")
retrieve_package("readxl")
retrieve_package("writexl")
retrieve_package("rlang")
retrieve_package("stats")
retrieve_package("skimr")
retrieve_package("entropy") # For entropy computation
retrieve_package("car") # To be checked on the server (not installing)
retrieve_package("corrplot")


# For data manipulation
retrieve_package("data.table")
retrieve_package("future")
retrieve_package("furrr")

# Configure parallelization plan based on OS
set_parallel_plan()


# For graph analysis and modeling
retrieve_package("igraph")
retrieve_package("tidygraph")
retrieve_package("assortnet") # For assortativity
retrieve_package("network")
retrieve_package("intergraph") #From conversion of igraph object into network one and vice versa.

## For ERGM modeling
retrieve_package("Rglpk")
retrieve_package("ergm")
retrieve_package("ergm.count")
retrieve_package("latentnet")
retrieve_package("snowFT") # required for multithreaded MCMC
retrieve_package("JANE")


## For GWR modeling



retrieve_package("tmap")
retrieve_package("plotly")
#retrieve_package("gtsummary")
retrieve_package("GGally")
retrieve_package("broom.helpers")
retrieve_package("GWmodel")
#retrieve_package("spdep")



# For graph partitionning
retrieve_package("sbm")
retrieve_package("greed")

# For spatial analysis
retrieve_package("sf") # For cartography
retrieve_package("cartography")
retrieve_package("geosphere")
retrieve_package("ggspatial") # To enhance ggplot2 with cartography
retrieve_package("RColorBrewer") # For color palette management
retrieve_package("classInt") # For variable discretization
retrieve_package("cartograflow") # For contiguity
retrieve_package("spdep") # For Moran's autocorrelation test
retrieve_package("vegan") # For Mantel test and diversity metrics
retrieve_package("ineq") # For Gini index


# For sequences analysis

retrieve_package("TraMineR")
retrieve_package("WeightedCluster")
retrieve_package("seqhandbook")
retrieve_package("seqHMM") # To be checked on the server (not installing)




# For statistical modeling
retrieve_package("lmtest")
retrieve_package("pROC")
retrieve_package("pscl")
retrieve_package("DescTools")
retrieve_package("ROCR")
retrieve_package("ResourceSelection") # for Hosmer-Lemeshow test
retrieve_package("MASS")
retrieve_package("AER")


# PCA and clustering

retrieve_package("memoise") # For memory optimisation
retrieve_package("rprojroot")
retrieve_package("stringi") # For string prossessing
retrieve_package("plotly")
retrieve_package("DataExplorer")
retrieve_package("DT")
retrieve_package("Hmisc")
retrieve_package("leaflet")
retrieve_package("moments")
retrieve_package("DescTools") # for normalized kurtosis

retrieve_package("FactoMineR")
retrieve_package("Factoshiny")
retrieve_package("cluster")
retrieve_package("ClusterR") # For minibatch
retrieve_package("ggdendro")
retrieve_package("distances")  # for big matrices
retrieve_package("fpc")        # for Big data AHC
retrieve_package("ggfortify")  # For Biplot (PCA variable + individuals) plot
retrieve_package("plotly")     # for interactive plots
retrieve_package("GGally")     # for Scatterplot matrix (2D)
retrieve_package("factoextra") # for PCA analysis and visualization

retrieve_package("gridExtra")   # for data.frame printing
#retrieve_package("svglite")    # for SVG image saving
#retrieve_package("patchwork")  # for images combination
retrieve_package("gridExtra")   # for images combination with ggarrange
library(grid)                   #is a R base package (no install)




 
# Cluster number


retrieve_package("NbClust")
retrieve_package("clusterCrit")



# Change points
retrieve_package("changepoint")


##############################################

old_par <- par(no.readonly = TRUE)

#########################################################################################
# A script to perform a graph description
#
# 2024/11 Raphaël Biawa Diegamounoua
#
# source("src/execution/multilevel_model_exportation.R")
#########################################################################################



retrieve_package("purrr")
retrieve_package("knitr")
retrieve_package("insight")

#=========================================================================
# Function to extract metrics from a list of multilevel models
#
# Args :
#       - model_list : a list of multilevel models results
#
# Returns :
#       A dataframe wich colums are the perfomance of the models :
#           - R2 marginal, R2 conditional, ICC adjusted, ICC unadjuested
#=========================================================================

extract_metrics <- function(model_list) {
  map_dfc(model_list, function(mod) {
    r2_val <- r2_nakagawa(mod)
    icc_adj <- icc(mod, adjusted = TRUE)$ICC_adjusted
    icc_unadj <- icc(mod, adjusted = FALSE)$ICC_unadjusted
    n_clust <- insight::n_grouplevels(mod)$N_levels

    c(
      "R2_marg"   = sprintf("%.3f", as.numeric(r2_val$R2_marginal)),
      "R2_cond"   = sprintf("%.3f", as.numeric(r2_val$R2_conditional)),
      "ICC_adj"   = sprintf("%.3f", as.numeric(icc_adj)),
      "ICC_unadj" = sprintf("%.3f", as.numeric(icc_unadj)),
      "Clusters"  = as.character(n_clust)
    )
  }) %>%
    mutate(Metric = c(
      "$R^2_{\\text{m}}$",
      "$R^2_{\\text{c}}$",
      "$\\text{ICC}_{\\text{a}}$",
      "$\\text{ICC}_{\\text{u}}$",
      "Cluster nb"
    )) %>%
    dplyr::select(Metric, everything())
}


#=========================================================================
# Export individual TeX fragment for a given region
#
# Args :
#       - metrics_df : a dataframe of models performances :
#           - R2 marginal, R2 conditional, ICC adjusted, ICC unadjuested
#       - filename : the output file path
#       - cluster_names : names of clusterings to be compared
# Returns :
#       A .tex file under filename
#=========================================================================



export_region_tex <- function(
  metrics_df,
  filename,
  clustering_names = c(
    "Maille habita",
    "Munici\\allowbreak{}pality",
    "Typo and location",
    "Price GWR",
    "Co-consul\\allowbreak{}tation"
  )
) {

  # 1. Assign column names
  colnames(metrics_df) <- c("Metric", clustering_names)

  # 2. Make the values in the last column bold
  last_col <- ncol(metrics_df)
  metrics_df[[last_col]] <- paste0("\\textbf{", metrics_df[[last_col]], "}")

  # 3. Also make the header of the last column bold (optional)
  colnames(metrics_df)[last_col] <- paste0("\\textbf{", colnames(metrics_df)[last_col], "}")

  # 4. Conversion to LaTeX (escape = FALSE preserves the \textbf)
  tex_code <- kable(
    metrics_df,
    format = "latex",
    escape = FALSE,
    booktabs = TRUE,
    linesep = ""
  )

  writeLines(as.character(tex_code), filename)
}

#===============================
#
#===============================

dir.create("out/tex/submarkets/",
           recursive = TRUE, showWarnings = FALSE)

filename_multilevel <- paste0(
  "out/tex/submarkets/",
  LOCALITY_NAME_OUT, "_price_multimodels_comparison", ".tex"
)

models <- list(
  model_1_mh = model_1_mh,
  model_1_city = model_1_city,
  model_1_attr_gps = model_1_attr_gps,
  model_1_gwr_submarket = model_1_gwr_submarket,
  model_conconsult_1 = model_conconsult_1
)



metrics  <- extract_metrics(models)

export_region_tex(metrics, filename_multilevel,
  clustering_names = c(
    "Maille habita",
    "Munici\\allowbreak{}pality",
    "Typo and loc",
    "Price GWR",
    "Co-cons"
  )
)


#=========================================================================
# A function that automaticly merge two .tex files of same structure
#
# Args :
#       - file_region1, file_region2 : input .tex files
#       - label_region1, label_region2 : labels to be affected to regions
#       - output_file : the output file path
# Returns :
#       A .tex file under output_file
#=========================================================================


merge_tex_tables <- function(file_region1, file_region2, label_region1, label_region2, output_file) {
  # 1. Read TeX files
  lines1 <- readLines(file_region1, encoding = "UTF-8")
  lines2 <- readLines(file_region2, encoding = "UTF-8")
  
  # Helper to parse table components
  parse_tex_components <- function(lines) {
    top_idx    <- grep("\\\\toprule", lines)
    mid_idx    <- grep("\\\\midrule", lines)
    bottom_idx <- grep("\\\\bottomrule", lines)
    
    # Clean header string and extract column names
    header_raw <- lines[(top_idx + 1):(mid_idx - 1)]
    # Remove trailing \\ from header line before splitting
    header_clean <- gsub("\\s*\\\\\\\\.*$", "", header_raw)
    
    col_names <- unlist(strsplit(header_clean, "\\s*&\\s*"))
    col_headers <- col_names[-1] # Remove 'Metric' column label
    
    # Extract data rows
    raw_rows <- lines[(mid_idx + 1):(bottom_idx - 1)]
    # Strip trailing \\ from data rows to prevent unwanted middle breaks
    data_rows <- gsub("\\s*\\\\\\\\.*$", "", raw_rows)
    
    list(headers = col_headers, rows = data_rows)
  }
  
  # 2. Parse both regional tables
  comp1 <- parse_tex_components(lines1)
  comp2 <- parse_tex_components(lines2)
  
  n_col1 <- length(comp1$headers)
  n_col2 <- length(comp2$headers)

  tot_col <- n_col1 + n_col2
  
  col1_start <- 2
  col1_end   <- 1 + n_col1
  col2_start <- col1_end + 1
  col2_end   <- col1_end + n_col2
  
  # Format column headers for the table
  headers1_formatted <- paste(paste0("\\textbf{", comp1$headers, "}"), collapse = " & ")
  headers2_formatted <- paste(paste0("\\textbf{", comp2$headers, "}"), collapse = " & ")
  header_row <- paste0("\\textbf{Metric} & ", headers1_formatted, " & ", headers2_formatted, " \\\\")
  
  # 3. Combine data rows (Merge region 1 metrics + region 2 metrics)
  combined_data_rows <- map2_chr(comp1$rows, comp2$rows, function(r1, r2) {
    # Extract metric name from the first region row
    metric_name <- gsub("^([^&]+)&.*$", "\\1", r1)
    
    # Extract numeric values after the first &
    vals1 <- gsub("^[^&]+\\s*&\\s*", "", r1)
    vals2 <- gsub("^[^&]+\\s*&\\s*", "", r2)
    
    # Return a clean single line with only ONE trailing \\ at the very end
    paste0(metric_name, "& ", vals1, " & ", vals2, " \\\\")
  })
  
  # 4. Construct complete tabularx output structure
  merged_tex <- c(
    "\\small",
    paste0("\\begin{tabularx}{\\textwidth}{l *{", tot_col, "}{C}}"),
    "\\toprule",
    paste0(" & \\multicolumn{", n_col1, "}{c}{\\textbf{", label_region1, "}} & \\multicolumn{", n_col2, "}{c}{\\textbf{", label_region2, "}} \\\\"),
    paste0("\\cmidrule(lr){", col1_start, "-", col1_end, "} \\cmidrule(lr){", col2_start, "-", col2_end, "}"),
    header_row,
    "\\midrule",
    combined_data_rows,
    "\\bottomrule",
    "\\end{tabularx}"
  )
  
  # Write file
  writeLines(merged_tex, output_file, useBytes = FALSE)
}



#==========================
# Execute merging
#==========================

merge_tex_tables(
  file_region1  = "out/tex/submarkets/IDF_price_multimodels_comparison.tex",
  file_region2  = "out/tex/submarkets/HDF_price_multimodels_comparison.tex",
  label_region1 = "Île-de-France (IdF)",
  label_region2 = "Nord-Pas-de-Calais (NdDC)",
  output_file   = "out/tex/submarkets/merged_table_regions.tex"
)

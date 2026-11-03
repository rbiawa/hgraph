#########################################################################################
# Export assortativity coefficients
# 
# 2026/08 Raphaël Biawa Diegamounoua
#
# source("src/execution/export_assortativity.R")
#########################################################################################

retrieve_package("purrr")
retrieve_package("dplyr")
retrieve_package("knitr")

# Image loading and combination
retrieve_package("magick")
retrieve_package("pdftools")
retrieve_package("ggplot2")
retrieve_package("patchwork")
retrieve_package("svglite")

#=========================================================================
# Function to export assortativity coefficients into .tex
#
# Args :
#       - assortativity_list : a list of assortativity coefficients
#       - labels             : named vector of variable labels
#       - filename           : output .tex filepath
#       - weighted           : boolean to extract weighted ('observed')
#                               or standard assortativity
#
# Returns :
#       A .tex file containing formatted assortativity coefficients
#=========================================================================

export_region_assortativity <- function(
  assortativity_list,
  labels = NULL,
  filename,
  weighted = FALSE
) {

  # Extract metric values dynamically
  df_metrics <- map_dfr(names(assortativity_list), function(var_name) {
    item <- assortativity_list[[var_name]]

    if (!weighted) {
      coef_val <- item$assortativity
      p_val    <- item$p_value
    } else {
      coef_val <- item$observed
      p_val    <- item$p_value
    }

    # Format p-value significance indication if needed
    p_label <- if (!is.null(p_val) && p_val < 0.001) "< 0.001" else sprintf("%.3f", p_val)

    # Resolve variable label safely from named vector
    clean_label <- if (!is.null(labels) && var_name %in% names(labels)) labels[[var_name]] else var_name

    tibble(
      Variable    = clean_label,
      Coefficient = sprintf("%.3f", coef_val)
    )
  })

  # Format with kable
  tex_code <- kable(
    df_metrics,
    format   = "latex",
    escape   = FALSE,
    booktabs = TRUE,
    linesep  = ""
  )

  writeLines(as.character(tex_code), filename)
}

#=========================================================================
# Export execution setup
#=========================================================================

dir.create("out/tex/graph_statistics/",
           recursive = TRUE, showWarnings = FALSE)

filename_assortativity <- paste0(
  "out/tex/graph_statistics/",
  LOCALITY_NAME_OUT, "_assortativity_coeff", ".tex"
)

var_labels <- c(
  "price_assortativity"          = "Price",
  "area_assortativity"           = "Area",
  "item_type_assortativity"      = "Property type",
  "room_count_assortativity"     = "Room count",
  "city_assortativity"           = "Municipality (City)",
  "maille_habitat_assortativity" = "Housing grid",
  "dep_assortativity"            = "Department"
)

# Fixed variable name typo: 'assortativity_list' instead of 'assortitvity_list'
export_region_assortativity(
  assortativity_list,
  labels = var_labels,
  filename = filename_assortativity
)

submarket_assort_labels <- c(
  "median_sqm_price"    = "Median sqm price",
  "log_median_area"     = "log median area",
  "main_item_type"      = "Main property type",
  "main_maille_habitat" = "Housing grid",
  "main_dep"            = "Main department",
  "room_count"          = "Room count "
)

export_region_assortativity(
  submarket_assortativity_list,
  labels = submarket_assort_labels,
  filename = paste0(
    "out/tex/graph_statistics/",
    LOCALITY_NAME_OUT, "_submarkets_assortativity_coeff", ".tex"
  ),
  weighted = TRUE
)

#=========================================================================
# A function that automatically merges two .tex files of same structure
#
# Args :
#       - file_region1, file_region2 : input .tex files
#       - label_region1, label_region2 : labels to be assigned to regions
#       - output_file : the output file path
# Returns :
#       A .tex file under output_file
#=========================================================================

merge_assortativity_tables <- function(
  file_region1, file_region2, 
  label_region1, label_region2, output_file
) {
  
  # Read individual TeX files
  lines1 <- readLines(file_region1, encoding = "UTF-8")
  lines2 <- readLines(file_region2, encoding = "UTF-8")
  
  # Parse table data lines safely
  parse_rows <- function(lines) {
    mid_idx    <- grep("\\\\midrule", lines)
    bottom_idx <- grep("\\\\bottomrule", lines)
    
    if (length(mid_idx) == 0 || length(bottom_idx) == 0) {
      stop("Could not find \\midrule or \\bottomrule in TeX file.")
    }
    
    raw_rows <- lines[(mid_idx + 1):(bottom_idx - 1)]
    raw_rows <- raw_rows[nchar(trimws(raw_rows)) > 0] # Exclude empty lines
    gsub("\\s*\\\\\\\\.*$", "", raw_rows)
  }
  
  rows1 <- parse_rows(lines1)
  rows2 <- parse_rows(lines2)
  
  if (length(rows1) != length(rows2)) {
    stop(sprintf(
      "Mismatch in row counts between TeX files (%d vs %d). Check assortativity lists.",
      length(rows1), length(rows2)
    ))
  }
  
  # Combine data rows side by side using string split
  combined_data_rows <- map2_chr(rows1, rows2, function(r1, r2) {
    parts1 <- trimws(unlist(strsplit(r1, "&")))
    parts2 <- trimws(unlist(strsplit(r2, "&")))
    
    var_name <- parts1[1]
    val1     <- parts1[2]
    val2     <- parts2[2]
    
    paste0(var_name, " & ", val1, " & ", val2, " \\\\")
  })
  
  # Header setup
  header_row <- "\\textbf{Attribute / Variable} & \\textbf{Coefficient} & \\textbf{Coefficient} \\\\"
  
  # Complete tabularx LaTeX structure
  merged_tex <- c(
    "\\small",
    "\\begin{tabularx}{\\textwidth}{l *{2}{C}}",
    "\\toprule",
    paste0(" & \\multicolumn{1}{c}{\\textbf{", label_region1, "}} & \\multicolumn{1}{c}{\\textbf{", label_region2, "}} \\\\"),
    "\\cmidrule(lr){2-2} \\cmidrule(lr){3-3}",
    header_row,
    "\\midrule",
    combined_data_rows,
    "\\bottomrule",
    "\\end{tabularx}"
  )
  
  writeLines(merged_tex, output_file, useBytes = FALSE)
}

#=========================================================================
# Execute merging
#=========================================================================

merge_assortativity_tables(
  file_region1  = "out/tex/graph_statistics/IDF_assortativity_coeff.tex",
  file_region2  = "out/tex/graph_statistics/HDF_assortativity_coeff.tex",
  label_region1 = "Île-de-France (IdF)",
  label_region2 = "Nord-Pas-de-Calais (NPdC)",
  output_file   = "out/tex/graph_statistics/merged_assortativity_coeff.tex"
)

merge_assortativity_tables(
  file_region1  = "out/tex/graph_statistics/IDF_submarkets_assortativity_coeff.tex",
  file_region2  = "out/tex/graph_statistics/HDF_submarkets_assortativity_coeff.tex",
  label_region1 = "Île-de-France (IdF)",
  label_region2 = "Nord-Pas-de-Calais (NPdC)",
  output_file   = "out/tex/graph_statistics/merged_submarkets_assortativity_coeff.tex"
)

#=============================================================================
# A function to load and merge images into a grid
#
# Args :
#       - image_sources : a vector of image file paths
#       - labels        : optional vector of subtitles for each image
#       - ncol          : number of columns for display (default is 2)
#       - output_file   : output file path string
#       - dpi           : plot resolution (default is 300)
# Returns :
#       Exports a combined grid figure (PDF, PNG, SVG)
#=============================================================================

combine_images_universal <- function(image_sources,
                                     labels = NULL,
                                     ncol = 2,
                                     output_file = "grid.pdf",
                                     dpi = 300) {
  
  # If no label is provided, initialize an empty character vector
  if (is.null(labels)) {
    labels <- rep("", length(image_sources))
  } else if (length(labels) != length(image_sources)) {
    stop("The number of 'labels' must match the number of images in 'image_sources'.")
  }
  
  # 1. Load images and attach subtitles
  plots_list <- map2(image_sources, labels, function(src, label_text) {
    is_pdf <- grepl("\\.pdf$", src, ignore.case = TRUE)
    
    if (is_pdf) {
      bitmap <- pdf_render_page(src, page = 1, dpi = dpi)
      img <- image_read(bitmap)
    } else {
      img <- image_read(src)
    }
    
    # Convert magick image object into a ggplot2 raster grob
    grob <- grid::rasterGrob(img, interpolate = TRUE)
    
    # Wrap in ggplot with theme_void
    p <- ggplot() + 
      annotation_custom(grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
      theme_void()
    
    # Add subtitle if provided
    if (nchar(label_text) > 0) {
      p <- p + ggtitle(label_text) +
        theme(
          plot.title = element_text(
            hjust = 0.5,           # Center subtitle
            face = "bold",          # Bold text
            size = 12,             # Font size
            margin = margin(b = 5) # Bottom margin
          )
        )
    }
    
    return(p)
  })
  
  # 2. Merge grid with patchwork and eliminate gaps between subplots
  combined_plot <- wrap_plots(plots_list, ncol = ncol) & 
    theme(
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt") # Tighten plot spacing
    )
  
  # 3. Create output directory if necessary
  out_dir <- dirname(output_file)
  if (out_dir != "." && !dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  # 4. Export combined figure (PDF, SVG, PNG, JPG)
  ggsave(
    filename = output_file,
    plot     = combined_plot,
    dpi      = dpi,
    width    = ncol * 5,
    height   = ceiling(length(image_sources) / ncol) * 5
  )

  message("File created successfully : ", output_file)
}

if (FALSE) {
  #=========================================================================
  # Combine submarket images and graphs
  #=========================================================================

  dir.create("out/img/submakets/leiden/",
            recursive = TRUE, showWarnings = FALSE)

  sources_mix <- c(
    "out/png/submarkets/leiden_clustering/IDF_submarkets_all.png",
    "out/png/submarkets/leiden_clustering/HDF_submarkets_all.png",
    "out/pdf/graphs/IDF_city_sqm_price_gradient_graph_of_comunity_ggraph.pdf",
    "out/pdf/graphs/HDF_city_sqm_price_gradient_graph_of_comunity_ggraph.pdf"
  )

  mes_labels <- c(
    "(a) IdF Submarket map",
    "(b) NPdC Submarket map",
    "(c) IdF Submarket interconnection network",
    "(d) NPdC Submarket interconnection network"
  )

  # 1. Export as PDF (2x2 grid)
  combine_images_universal(
    image_sources = sources_mix,
    labels        = mes_labels,
    ncol          = 2,
    output_file   = "out/img/submakets/leiden/combined_submarkets.pdf"
  )

  # 2. Export as PNG (2x2 grid)
  combine_images_universal(
    image_sources = sources_mix,
    labels        = mes_labels,
    ncol          = 2,
    output_file   = "out/img/submakets/leiden/combined_submarkets.png"
  )
}
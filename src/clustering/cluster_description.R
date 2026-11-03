#########################################################################################
# Cluster description
# 
# 2025/06 Raphaël Biawa Diegamounoua
#
# source("src/clustering/cluster_description.R")
#########################################################################################






##############################################################################
# describe_clustering
#
# Description:
# This function analyzes clustering results by characterizing clusters using both
# numerical and categorical variables. It calculates Eta² values for numerical 
# variable contributions, conducts V-tests to identify significantly contributing
# variables per cluster, and uses chi-squared tests and standardized residuals 
# to describe categorical variables. Optional scaling and significance thresholding
# are supported.
#
# Arguments:
# - data: A data frame containing the clustering results, including the cluster variable.
# - cluster_var: A string specifying the column name representing cluster assignments.
# - scale: Logical. If TRUE, numeric variables are standardized before analysis.
# - alpha: Numeric. The significance level used to filter variables (default is 0.05).
#
# Returns:
# A structured list with the following components:
# - data.clust: The cluster assignment variable (as a factor).
# - desc.var:
#     - quanti.var: Eta² and p-values for all numeric variables.
#     - quanti: Per-cluster V-tests for numeric variables with significant contributions.
#     - quali: Cleanly formatted output for significant categorical modalities per cluster.
# - desc.ind: Reserved for future implementation of representative individuals.
# - call: A list recording the parameters used in the function call.
##############################################################################

describe_clustering <- function(data, cluster_var, scale = FALSE, alpha = 0.05) {
  library(dplyr)
  
  if (!is.factor(data[[cluster_var]])) {
    data[[cluster_var]] <- as.factor(data[[cluster_var]])
  }
  
  num_vars <- data %>% dplyr::select(where(is.numeric), -all_of(cluster_var))
  cat_vars <- data %>% dplyr::select(where(is.factor), -all_of(cluster_var))
  
  if (scale && ncol(num_vars) > 0) {
    num_vars <- as.data.frame(scale(num_vars))
  }
  
  data.clust <- data[[cluster_var]]
  
  eta2_values <- sapply(num_vars, function(x) {
    mod <- aov(x ~ data[[cluster_var]])
    summary(mod)[[1]]$`Sum Sq`[1] / sum(summary(mod)[[1]]$`Sum Sq`)
  })
  
  p_values_eta <- sapply(num_vars, function(x) {
    mod <- aov(x ~ data[[cluster_var]])
    summary(mod)[[1]]$`Pr(>F)`[1]
  })
  
  desc_quanti_var <- data.frame(
    Variable = names(eta2_values),
    Eta2 = eta2_values,
    P_value = p_values_eta
  ) %>% arrange(desc(Eta2))
  
  quanti_clusters <- list()
  for (cl in levels(data[[cluster_var]])) {
    idx        <- data[[cluster_var]] == cl
    mean_cl    <- colMeans(num_vars[idx, , drop = FALSE])
    mean_all   <- colMeans(num_vars)
    sd_cl <- apply(num_vars[idx, , drop = FALSE], 2, sd)
    sd_all     <- apply(num_vars, 2, sd)
    n_cl       <- sum(idx)
    N          <- nrow(data)
    correction <- sqrt((N - n_cl) / (N - 1))
    vtest      <- (mean_cl - mean_all) / (sd_all / sqrt(n_cl) * correction)
    pvals                 <- 2 * (1 - pnorm(abs(vtest)))
    quanti_clusters[[cl]] <- data.frame(
      Variable            = names(vtest),
      V_test              = vtest,
      mean_within         = mean_cl,
      mean_overall        = mean_all,
      sd_within           = sd_cl,
      sd_overall          = sd_all,
      
      
      P_value = pvals
    ) %>% filter(P_value < alpha) %>% arrange(desc(abs(V_test)))
  }
  
  quali_clusters <- list()
  if (ncol(cat_vars) > 0) {
    for (var in names(cat_vars)) {
      tbl <- table(data[[cluster_var]], data[[var]])
      chi <- suppressWarnings(chisq.test(tbl))
      res_std <- round(chi$stdres, 2)
      sig_cells <- which(abs(res_std) > qnorm(1 - alpha / 2), arr.ind = TRUE)
      
      if (nrow(sig_cells) > 0) {
        lines <- apply(sig_cells, 1, function(idx) {
          cluster <- rownames(res_std)[idx[1]]
          modality <- colnames(res_std)[idx[2]]
          residual <- res_std[idx[1], idx[2]]
          status <- ifelse(residual > 0, "overrepresented", "underrepresented")
          paste0("• Cluster ", cluster, " → ", modality, " = ", status, 
                 " (residual = ", sprintf("%+.2f", residual), ")")
        })
        chi_summary <- paste0(
          "** Variable: ", var,
          "  |  Chi² = ", round(chi$statistic, 2),
          ", df = ", chi$parameter,
          ", p-value = ", signif(chi$p.value, 3)
        )
        quali_clusters[[var]] <- c(chi_summary, lines)
      }
    }
  }
  
  call <- list(cluster_var = cluster_var, scale = scale, alpha = alpha)
  
  return(list(
    data.clust = data.clust,
    desc.var = list(
      quanti.var = desc_quanti_var,
      quanti = quanti_clusters,
      quali = quali_clusters
    ),
    desc.ind = NULL,
    call = call
  ))
}








#########################################################################
# Summarize a continuous variable by group (a factor)
#
# Args:
#   - data : the dataset to be used
#   - var  : name of the numeric variable (must be a string)
#   - gp_var : name of the grouping variable (must be a string)
#
# Return : a summary dataframe
#########################################################################



summary_stats_by_group <- function(data, var, gp_var) {
  var_sym <- rlang::sym(var)
  gp_syms <- rlang::syms(gp_var)
  
  data %>%
    group_by(!!!gp_syms) %>%
    summarise(
      min    = min(!!var_sym, na.rm = TRUE),
      q1     = quantile(!!var_sym, 0.25, na.rm = TRUE),
      mean   = mean(!!var_sym, na.rm = TRUE),
      median = median(!!var_sym, na.rm = TRUE),
      q3     = quantile(!!var_sym, 0.75, na.rm = TRUE),
      max    = max(!!var_sym, na.rm = TRUE),
      .groups = "drop"
    )
}

#########################################################################################
# A script to perform a sequence analysis
# 
# 2026/06 Raphaël Biawa Diegamounoua
#
# source("src/execution/sequence_analysis_execution.R")
#########################################################################################



######################################
# Loading packages

source("src/packages_loading.R")

#Retrieve functions

source("src/function_retrieving.R")

# Loading global parameters
source("src/global_parameters.R")



# Retrieving visualization parameters
source("src/visualization/visualization_parameters.R")





#==========================================
#
# Seq analysis
#
#==========================================

retrieve_package("WeightedCluster")
#===========================================================
# 1. Sort events and create within-user order index
#===========================================================

df <- events_gc %>%
  arrange(fullvisitorid, order_clean) %>%
  group_by(fullvisitorid) %>%
  mutate(pos = row_number()) %>%
  ungroup()

#===========================================================
# 2. Keep only users with at least 2 events
#===========================================================

df2 <- df %>%
  group_by(fullvisitorid) %>%
  filter(n() >= 2) %>%
  ungroup()

#===========================================================
# 3. Wide format: one row per user, one column per event
#===========================================================

wide <- df2 %>%
  dplyr::select(fullvisitorid, pos, submarket) %>%
  pivot_wider(
    names_from  = pos,
    values_from = submarket,
    names_prefix = "t"
  )

#===========================================================
# 4. Extract raw matrix of states
#===========================================================

mat <- wide %>% dplyr::select(-fullvisitorid)

#===========================================================
# 5. Recode numeric states into "sbm_x"
#===========================================================

mat <- as.data.frame(lapply(mat, function(col) {
  ifelse(is.na(col), NA, paste0("sbm_", col))
}))

#===========================================================
# 6. Build alphabet from observed values only
#===========================================================

alphabet <- sort(unique(unlist(mat)))
alphabet <- alphabet[!is.na(alphabet)]

#===========================================================
# 7. Build TraMineR sequence object (full dataset)
#===========================================================

seq_full <- seqdef(
  mat,
  id       = wide$fullvisitorid,
  alphabet = alphabet,
  states   = alphabet,
  labels   = alphabet,
  xtstep   = 1
)

#===========================================================
# 8. Extract unique sequences + weights
#===========================================================

tab <- seqtab(seq_full, idxs = 0, weighted = TRUE)   # table of unique sequences

if (interactive()) head(tab)


#===========================================================
# 9. Build weighted sequences
#===========================================================

## 1. Extraction des séquences uniques
seq_uniq_mat <- tab %>% dplyr::select(starts_with("t"))

## 2. Récupération du symbole void utilisé par seqtab()
void_sym <- attr(tab, "void")   # normalement "%"

## 3. Remplacement des voids dans les VALEURS
seq_uniq_mat[seq_uniq_mat == void_sym] <- NA

## 4. Conversion en character pour éliminer les niveaux factor
seq_uniq_mat <- seq_uniq_mat %>% 
  mutate(across(everything(), ~ as.character(.)))

## 5. Recalcul de l’alphabet réel (sans NA, sans "%")
alphabet <- sort(unique(unlist(seq_uniq_mat)))
alphabet <- alphabet[!is.na(alphabet)]

## 6. Poids = fréquences des séquences uniques
weights <- attr(tab, "freq")$Freq

summary(weights)


weighted_seq <- seqdef(
  seq_uniq_mat,
  weights = weights,
  alphabet = alphabet,
  states   = alphabet,
  labels   = alphabet,
  xtstep   = 1
)


attributes(weighted_seq) %>% names()

attr(weighted_seq, "weights") %>% summary()

attr(weighted_seq, "alphabet")

#===========================================================
# 10. Compute distances on unique sequences only
#===========================================================

dist_uniq <- seqdist(weighted_seq, method = "OM", sm = "INDELSLOG")


#===========================================================
# 11. Weighted clustering (Ward)
#===========================================================

wardCluster <- hclust(as.dist(dist_uniq), method= "ward.D2",
 members = attr(weighted_seq, "weights")
 )

#===========================================================
# 12. Choose best k (example: highest ASW)
#===========================================================

wardRange <- WeightedCluster::as.clustrange(
  wardCluster,
  diss = dist_uniq,
  weights = attr(weighted_seq, "weights"),
  ncluster = 50
)

summary(wardRange, max.rank = 3)

if (interactive()) {
  plot(
  wardRange,
  stat = "noCH",       # tous les critères sauf CH (souvent instable)
  norm = "none",
  withlegend = TRUE,
  xlab = "Nombre de clusters",
  ylab = "Indicateurs de qualité"
  )
}

if (interactive()) plot(wardRange, stat = c("ASWw", "HG", "PBC", "HC"))

if (interactive()) plot(wardRange, stat = c("ASWw", "HG", "PBC", "HC"), norm = "zscore")

asw <- wardRange$stats$ASWw
best_k <- which.max(asw)
best_k

# Plot sequence clusters
#if (interactive()) seqdplot(weighted_seq, group = wardRange$clustering$cluster39, border = NA)

#===========================================================
# 13. Clustering around medoids
#===========================================================

pamRange <- wcKMedRange(dist_uniq, kvals = 2:50,
  weights = attr(weighted_seq, "weights")
)

summary(pamRange, max.rank = 2)

pam_asw <- pamRange$stats$ASWw
best_k_pam <- which.max(pam_asw)
best_k_pam



#save.image("out/Rdata/graph/clustering/leiden/submarkets_plus_sequence_analysis.RData")

#load("out/Rdata/graph/clustering/leiden/submarkets_plus_sequence_analysis.RData")

















#########################################################################################################
# BEGIN ANALYSING
#########################################################################################################


# Plot clustering dendogram

plot(as.dendrogram(seq.distMD), leaflab = "none")


# Plot clustering heatmap
seq_heatmap(MDseq, seq.distMD)


# Representation of inertia jump to help identify a reasonable number of groups

plot(sort(seq.distMD$height, decreasing = TRUE)[1:20], type = "s", xlab = "Number of classes", ylab = "Inertia")


# If not running in SSH mode, prompt for user input
if (nzchar(Sys.getenv("SSH_CONNECTION")) == FALSE) {
  SEQUENCES_CLASSES_NUMBER <- get_numeric_input("Please enter a numeric value: ")
} else {
  # If running in SSH mode, skip the input prompt
  cat("Execution is through SSH. The input prompt is skipped.\n")
}

SEQUENCES_CLASSES_NUMBER <- get_numeric_input("Please enter a numeric value: ")


seq.partitions <- cutree(seq.distMD, SEQUENCES_CLASSES_NUMBER)

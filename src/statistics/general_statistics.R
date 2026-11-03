#########################################################################################
# Functions used for general statistics.
# 
# 2024/11 Raphaël Biawa Diegamounoua
#
# source("src/statistics/general_statistics.R")
#########################################################################################






#############################################################
# Computes a complementary distribution of a given vector.
#
# graph_list: the input graph list
# 
# returns: a dataframe of calculated statistics.
#############################################################

# Complementary ECDF Function----

complementary_ecdf <- function(x) {
  1 - ecdf(x)(x)
}


#############################################################
# Computes a correlation matrix p-values.
#
# mat: a numeric matrix or dataframe
# 
# returns: a p-values matrix.
# source : https://www.sthda.com/french/wiki/visualiser-une-matrice-de-correlation-par-un-correlogramme?title=visualiser-une-matrice-de-correlation-par-un-correlogramme
#############################################################


cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], ...)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
    }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

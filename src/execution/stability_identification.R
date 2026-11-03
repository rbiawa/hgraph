#########################################################################################
# A script to identify stability in a curve
# 
# 2026/05 Raphaël Biawa Diegamounoua
#
# source("src/execution/stability_identification.R")
#########################################################################################

retrieve_package("strucchange")

# Suppose your data frame is summary_eta2 with columns:
# resolution, eta2

model <- lm(eta2 ~ resolution, data = summary_eta2)

# CUSUM test
cusum_test <- strucchange::efp(eta2 ~ resolution, data = summary_eta2, type = "Rec-CUSUM")

if (interactive()) plot(cusum_test)

cusum_test$coefficients

sctest(cusum_test)


# Bai–Perron breakpoints
bp_test <- strucchange::breakpoints(eta2 ~ resolution, data = summary_eta2)

summary(bp_test)
plot(bp_test)

bp_test$breakpoints

summary_eta2$resolution[bp_test$breakpoints]

if (interactive()) View(summary_eta2)

retrieve_package("segmented")

# Base model
m0 <- lm(eta2 ~ resolution, data = summary_eta2)

# Breakpoints from Bai–Perron
bp_test_est <- summary_eta2$resolution[bp_test$breakpoints]

# Segmented regression
m_seg <- segmented::segmented(m0, seg.Z = ~ resolution, psi = bp_test_est)

summary(m_seg)



# Plot segmented regression
if (interactive()){
    plot(summary_eta2$resolution, summary_eta2$eta2,
     pch = 16, col = "grey50",
     xlab = "Resolution", ylab = "Eta²")

plot(m_seg, add = TRUE, col = "blue", lwd = 2)

abline(v = bp_test_est, col = "red", lty = 2)
}





#======================================
#
#
#======================================

# Extract fitted values for plotting
df_fit <- data.frame(
  resolution = summary_eta2$resolution,
  eta2_fit = fitted(m_seg)
)

# Extract breakpoints from the model
bp <- m_seg$psi[, "Est."]
bp
# Should be close to: 2.991 and 13.187

# Compute slopes in each segment
b1  <- coef(m_seg)["resolution"]
u1  <- coef(m_seg)["U1.resolution"]
u2  <- coef(m_seg)["U2.resolution"]

slope_seg1 <- b1
slope_seg2 <- b1 + u1
slope_seg3 <- b1 + u1 + u2

{
  sqm_res_slop_plot <-  ggplot(summary_eta2, aes(x = resolution, y = eta2)) +
    # Raw data
    geom_point(color = "grey60", size = 1.5, alpha = 0.7) +
    geom_line(color = "grey70", linewidth = 0.8) +
    
    # Plateau zone (between first and second breakpoint)
    annotate("rect",
            xmin = bp[1], xmax = bp[2],
            ymin = -Inf, ymax = Inf,
            alpha = 0.10, fill = "skyblue") +
    
    # Segmented regression fit
    geom_line(data = df_fit,
              aes(x = resolution, y = eta2_fit),
              color = "blue", linewidth = 1.2) +
    
    # Breakpoints
    geom_vline(xintercept = bp, linetype = "dashed", color = "red", linewidth = 1) +
    
    # Annotate slopes
    annotate("text", x = bp[1] / 2, y = max(summary_eta2$eta2) * 0.95,
            label = paste0("Slope 1 = ", round(slope_seg1, 4)),
            color = "black", size = 4) +
    
    annotate("text", x = (bp[1] + bp[2]) / 2, y = max(summary_eta2$eta2) * 0.90,
            label = paste0("Slope 2 = ", round(slope_seg2, 4)),
            color = "black", size = 4) +
    
    annotate("text", x = (bp[2] + max(summary_eta2$resolution)) / 2,
            y = max(summary_eta2$eta2) * 0.85,
            label = paste0("Slope 3 = ", round(slope_seg3, 4)),
            color = "black", size = 4) +
      scale_y_continuous(
      limits = 0:1,
      expand = expansion(mult = 0.02)
    ) +
  
    labs(
      title = "Segmented regression of Eta² vs Leiden resolution",
      subtitle = "Breakpoints at ~3.0 and ~13.2 (segmented regression)",
      x = "Resolution parameter",
      y = "Eta² (explained price variance)"
    ) +

    theme_minimal(base_size = 14)
}

if (interactive()) sqm_res_slop_plot

pdf(file = paste(GRAPH_PDF_SAVING_DIRECTORY,
                paste(LOCALITY_NAME_OUT,
                      ANALYSE_SCALE_OUT,
                      "sqm_price_gradient_slop_plot.pdf",
                      sep = "_"),
                sep = "/"),
width = 15, height = 15
)

sqm_res_slop_plot

dev.off()



retrieve_package("purrr")
retrieve_package("broom")

window_size <- 5

slopes <- purrr::map_df(
  1:(nrow(summary_eta2) - window_size),
  function(i) {
    window <- summary_eta2[i:(i + window_size), ]
    model <- lm(eta2 ~ resolution, data = window)
    tibble(
      start = summary_eta2$resolution[i],
      end = summary_eta2$resolution[i + window_size],
      slope = coef(model)[["resolution"]],
      p_value = broom::tidy(model)$p.value[2]
    )
  }
)


if (interactive()) View(slopes)

#print(slopes, n = nrow(slopes))
# Visualize slope vs resolution
if (interactive()) {
    plot(slopes$start, slopes$slope, type = "l")
abline(h = 0, col = "red")
}

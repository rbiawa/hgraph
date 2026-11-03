#########################################################################################
# Housing price GWR modeling
# 
# 2025/09 Raphaël Biawa Diegamounoua
#
# source("src/descriptive_statitiscs/univariate_analysis.R")
#########################################################################################


# Save plotly graphics into pdf
install.packages(c("htmlwidgets", "webshot"))
webshot::install_phantomjs()  # une seule fois


library(htmlwidgets)
library(webshot)

# Distribution normalization

retrieve_package("bestNormalize")


export_plotly_pdf <- function(fig, html_path, pdf_path, width = 800, height = 600) {
  # Sauvegarde HTML non auto-contenu
  htmlwidgets::saveWidget(fig, html_path, selfcontained = FALSE)
  
  # Export PDF via webshot
  webshot::webshot(html_path, pdf_path, vwidth = width, vheight = height)
  
  # Supprimer le dossier de dépendances
  dep_folder <- sub("\\.html$", "_files", html_path)
  if (dir.exists(dep_folder)) {
    unlink(dep_folder, recursive = TRUE, force = TRUE)
  }
}




#(price_cv, area_cv, room_cv, type_simpson, city_simpson, dep_simpson)


## Price_cv

plot_ly(user_metrics, x = ~price_cv) %>% add_histogram()
plot_ly(user_metrics, x = ~sqrt(price_cv)) %>% add_histogram()

fig_sqrt_price_cv <- plot_ly(user_metrics, x = ~sqrt(price_cv)) %>% add_histogram()

export_plotly_pdf(fig_sqrt_price_cv, "out/pdf/serious_search/sqrt_price_cv.html", "out/pdf/serious_search/sqrt_price_cv.pdf")






## Area_cv

plot_ly(user_metrics, x = ~area_cv) %>% add_histogram()
plot_ly(user_metrics, x = ~sqrt(area_cv)) %>% add_histogram()

fig_sqrt_area_cv <- plot_ly(user_metrics, x = ~sqrt(area_cv)) %>% add_histogram()

#saveWidget(fig, "out/pdf/serious_search/histogram.html", selfcontained = FALSE)


#webshot("out/pdf/serious_search/histogram.html", "out/pdf/serious_search/histogram.pdf", vwidth = 800, vheight = 600)



export_plotly_pdf(fig_sqrt_area_cv, "out/pdf/serious_search/sqrt_area_cv.html", "out/pdf/serious_search/sqrt_area_cv.pdf")



## Type_sampson

plot_ly(user_metrics, x = ~type_simpson) %>% add_histogram()

plot_ly(user_metrics, x = ~log(type_simpson)) %>% add_histogram()

plot_ly(user_metrics, x = ~sqrt(type_simpson)) %>% add_histogram()

plot_ly(user_metrics, x = ~(type_simpson)^2) %>% add_histogram()

norm_type_simpson <- bestNormalize(user_metrics$type_simpson)
simpson_transformed <- norm_type_simpson$x.t

plot_ly(as.data.frame(simpson_transformed), x=~simpson_transformed)%>% add_histogram()


## city_simpson

plot_ly(user_metrics, x = ~city_simpson) %>% add_histogram()

plot_ly(user_metrics, x = ~sqrt(city_simpson)) %>% add_histogram()

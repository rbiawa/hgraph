#########################################################################################
# Plot Online search intensity by city
# 
# 2025/06 Raphaël Biawa Diegamounoua
#
# source("src/plot/map_plot.R")
#########################################################################################



#============================
#
# References : 
#
#============================

# https://r-graph-gallery.com/

# https://r-graph-gallery.com/330-bubble-map-with-ggplot2.html



if(!require("patchwork")) install.packages("patchwork"); library(patchwork)
if(!require("xtable")) install.packages("xtable"); library(xtable)
if(!require("ggspatial")) install.packages("ggspatial"); library(ggspatial)
if(!require("ggrepel ")) install.packages("ggrepel "); library(ggrepel)

source("src/packages_loading.R") # Loading packages
source("src/function_retrieving.R") #Retrieve functions



#============================
#
# Data processing : 
#
#============================



events <- events %>% 
  filter(id_listing %in% features$id_listing)

if (exists("events_year")) rm(events_year)

# nb_listings_features 


df_listings_features <- features %>%
  filter(!is.na(city_ID)) %>%
  group_by(city_ID) %>%
  summarise(
    nb_listings_features = n_distinct(id_listing),
    INSEE_COM = first(INSEE_COM)
  )


# nb_listings_events

df_listings_events <- events %>%
  filter(!is.na(id_listing)) %>%
  left_join(features %>% 
              dplyr::select(id_listing, city_ID), by = "id_listing") %>%
  group_by(city_ID) %>%
  summarise(
    nb_listings_events = n_distinct(id_listing)
  )



# nb_consultations

df_consults <- events %>%
  filter(!is.na(id_listing)) %>%
  left_join(features %>% 
              dplyr::select(id_listing, city_ID), by = "id_listing") %>%
  group_by(city_ID) %>%
  summarise(
    nb_consultations = n()
  )




# Merge + compute ratio

df_city_stats <- df_listings_events %>%
  left_join(df_listings_features, by = "city_ID") %>%
  left_join(df_consults, by = "city_ID") %>%
  mutate(
    intensity_features = nb_consultations / nb_listings_features,
    intensity_events   = nb_consultations / nb_listings_events
  )


# Join city shapefile

geom_cities_stats <- geom_sf_cities %>%
  left_join(df_city_stats, by = "city_ID") %>% 
  filter(INSEE_REG %in% features$INSEE_REG)



# Context


bbox_extended <- geom_cities_stats %>%
  st_union() %>% 
  st_buffer(30000)   # 30 km around, ajustable


geom_dep_context <- geom_sf_departements[bbox_extended, ]

geom_reg_context <- geom_sf_regions[bbox_extended, ]


rm(events, features)


gc()



#============================
#
# Plot : 
#
#============================




geom_cities_stats_filt <- geom_cities_stats %>% 
  filter(! INSEE_DEP %in% c("02", "60", "80")) %>% 
  arrange(desc(nb_consultations)) %>% 
  left_join(
    geom_sf_departements %>% 
      st_drop_geometry() %>% 
      dplyr::select(INSEE_DEP, dep_Name),
    by = "INSEE_DEP"
  )




my_blue <- c("#498ea5", "#94d3e7", "#1c61b6")[1] 

my_red  <- c("#742C08", "#e6550d")[1]


map_reg <- function(x, scale=FALSE, city_label = FALSE){
  g <- ggplot() +
    geom_sf(data=geom_cities_stats_filt %>% filter(INSEE_REG==x), linewidth=.3, color="white", 
            fill="gray95") +
    geom_sf(data = filter(st_point_on_surface(geom_cities_stats_filt), INSEE_REG == x),
            aes(size = nb_consultations), shape=21, alpha=.7,
            fill="black", color="white") +
    scale_size_area(max_size = 5, 
                    limits=c(min(geom_cities_stats_filt$nb_consultations, na.rm = T), 
                             max(geom_cities_stats_filt$nb_consultations, na.rm = T))
    ) +
 
    geom_sf(data=geom_cities_stats_filt %>% filter(INSEE_REG==x) %>% group_by(INSEE_DEP) %>% summarise(), 
            color="black", 
            fill=NA)+
    ggrepel::geom_text_repel(
      data = geom_cities_stats_filt %>% filter(INSEE_REG==x) %>% group_by(INSEE_DEP, dep_Name) %>% summarise(.groups = "drop_last"),
      aes(label = dep_Name, geometry = geometry),
      stat = "sf_coordinates",
      col  = my_blue,
      size = 3,
      alpha = 0.7,
      min.segment.length = 0) + labs(x = NULL, y = NULL) + 

    guides(fill = FALSE) +
    annotation_scale()+
    coord_sf(datum = NA)
  
  if (city_label) {
    g <- g +
      ggrepel::geom_text_repel(
        data = head(geom_cities_stats_filt %>% filter(!is.na(nb_consultations)& INSEE_REG==x), 8),
        aes(label = city_Name, geometry = geometry),
        stat = "sf_coordinates",
        col  = my_red,
        size = 2.1,
        alpha = 0.9,
        min.segment.length = 0) + labs(x = NULL, y = NULL)
  }
  
  if(!scale){
    g <- g+theme_bw()+theme(legend.position = "none", 
                            panel.grid = element_blank(),
                            panel.border = element_blank())}
  else{g + theme_bw()+theme(panel.grid = element_blank(),
                            panel.border = element_blank())+
      annotation_north_arrow(
        location = "tr",
        which_north = "true",
        style = north_arrow_fancy_orienteering,
        height = unit(1, "cm"),
        width = unit(1, "cm")
      )+
      labs(size="Number of\nviews")}
  
  
}





gg <- cowplot::plot_grid(map_reg("11", FALSE, city_label = FALSE), 
                   map_reg("32", TRUE, city_label = TRUE), align = "h", rel_widths = c(1,1.4),
                   labels = c("Île-de-France", "Nord-Pas-de-Calais"),
                   label_fontface = "plain")+
  theme(plot.background = element_rect(fill="white", color=NA))

if (interactive()) gg

ggsave("out/png/serious_search/consultation_map_plot.png", gg, dpi = 400, width = 8, height = 4)













ggplot(geom_cities_stats_filt) +
  geom_sf() +
  ggrepel::geom_label_repel(
    data = tail(geom_cities_stats_filt %>% filter(!is.na(nb_consultations ))),
    aes(label = city_Name, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0
  )


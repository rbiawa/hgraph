#########################################################################################
# Execution of cartography data processing.
# 
# 2024/11 Raphaël Biawa Diegamounoua
#
#########################################################################################




# Load base spatial entities (municipalities) and department data
geom_tab_paris_com <- load_and_filter_spatial_data("in/carto/COMMUNE.shp", region_code = "11")
geom_tab_dep <- load_and_filter_spatial_data("in/carto/DEPARTEMENT.shp", region_code = "11", population_column = NULL)

# Remove Paris municipality to keep only its districts
geom_tab_sans_paris <- geom_tab_paris_com %>%
  filter(ID != 'COMMUNE_0000000009736048') %>%
  dplyr::select(ID, NOM, POPULATION)

# Load Paris municipal districts and merge with base data
geom_arrond_mun <- load_districts("Carto/ARRONDISSEMENT_MUNICIPAL.shp", "INSEE_COM", 75056)
geom_tab_arr_paris <- rbind(geom_tab_sans_paris, geom_arrond_mun)

# Join attribute table with geometry data
Attri_Ann <- join_spatial_data(Attri_Ann, geom_tab_arr_paris)
Attri_Ann <- join_spatial_data(Attri_Ann, geom_tab_dep)
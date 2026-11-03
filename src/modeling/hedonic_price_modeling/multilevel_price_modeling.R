#########################################################################################
# A script to perform a multilevel price modeling
# 
# 2026/08 Raphaël Biawa Diegamounoua
#
# source("src/modeling/hedonic_price_modeling/multilevel_price_modeling.R")
#########################################################################################


#========================
# Packages used
#========================

retrieve_package("lme4")
retrieve_package("performance")
retrieve_package("MuMIn")
retrieve_package("insight")

#=========================
# Variable distribution
#=========================

if (interactive()) {
  plot_continuous_variables(pts %>%
    dplyr::select(sqm_price, log_sqm_price),
    transformation = "identity")
}



#=========================
# Null model
#=========================

freq(pts$optim_submarket)

if (FALSE) {
  pts_multilevel <- pts %>%
    filter(optim_submarket < 47)
} else {
   pts_multilevel <- pts 
}


#====================================
# MAILLE HABITAT
#====================================


#=========================
# Null model
#=========================


model_0_mh <- lme4::lmer(log(sqm_price) ~ 1 + (1 | maille_habitat), data = pts_multilevel)


summary(model_0_mh)

performance::icc(model_0_mh)



#================================
# Model with variables at lavel 1
#================================

model_1_mh <- lme4::lmer(log(sqm_price) ~ 1 + log_area + room_count + item_type +
    (1 | maille_habitat),
  data = pts_multilevel
)

summary(model_1_mh)

performance::icc(model_1_mh)

MuMIn::r.squaredGLMM(model_1_mh)

performance::r2_nakagawa(model_1_mh)





#====================================
# MUNICIPALITY
#====================================


#=========================
# Null model
#=========================


model_0_city <- lme4::lmer(log(sqm_price) ~ 1 + (1 | sl_insee_city_id), data = pts_multilevel)


summary(model_0_city)

performance::icc(model_0_city)



#================================
# Model with variables at lavel 1
#================================

model_1_city <- lme4::lmer(log(sqm_price) ~ 1 + log_area + room_count + item_type +
    (1 | sl_insee_city_id),
  data = pts_multilevel
)

summary(model_1_city)

performance::icc(model_1_city)

MuMIn::r.squaredGLMM(model_1_city)

performance::r2_nakagawa(model_1_city)





#====================================
# LEIDEN SUBMARKETS
#====================================


#=========================
# Null model
#=========================

# Now with our method

model_conconsult_0 <- lme4::lmer(log(sqm_price) ~ 1 + (1 | optim_submarket), data = pts_multilevel)

#model_conconsult_0 <- lme4::lmer(log_price ~ 1 + (1 | optim_submarket), data = pts_multilevel)

summary(model_conconsult_0)


performance::icc(model_conconsult_0)

performance::r2_nakagawa(model_conconsult_0)

#================================
# Model with variables at lavel 1
#================================

model_conconsult_1 <- lme4::lmer(log(sqm_price) ~ 1 + log_area + room_count + item_type +
    (1 | optim_submarket),
  data = pts_multilevel
)

summary(model_conconsult_1)

performance::icc(model_conconsult_1)

MuMIn::r.squaredGLMM(model_conconsult_1)

performance::r2_nakagawa(model_conconsult_1)

nb_clust <- insight::n_grouplevels(model_conconsult_1)

nb_clust$Group

nb_clust$N_levels

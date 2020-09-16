# https://uc-r.github.io/assumptions_homogeneity
# https://rpsychologist.com/r-guide-longitudinal-lme-lmer
# https://www.gerkovink.com/miceVignettes/Multi_level/Multi_level_data.html
# https://stefvanbuuren.name/fimd/sec-mlfcs.html
# https://stackoverflow.com/questions/47950304/random-effects-in-longitudinal-multilevel-imputation-models-using-mice
# https://bookdown.org/mwheymans/bookmi/multiple-imputation-models-for-multilevel-data.html#missing-data-in-continuous-variables
# https://cran.r-project.org/web/packages/miceadds/miceadds.pdf
# https://simongrund1.github.io/posts/multiple-imputation-for-three-level-and-cross-classified-data/




names(county_mice)

small_data <- county_mice %>% 
  dplyr::select(rowid:year,
                poor_or_fair_health,
                income_inequality,
                social_associations:violent_crime,
                air_pollution_particulate_matter,
                severe_housing_problems:food_insecurity,
                median_household_income:population,
                percent_not_proficient_in_english:ltpa_percent,
                obesity_percent:year_num)

str(small_data)

library(mice)
library(miceadds)

small_data$ltpa_percent <- as.vector(scale(small_data$ltpa_percent,
                                           scale = FALSE))

pred_matrix <- make.predictorMatrix(data = small_data)
imp_method <- make.method(data = small_data)

pred_matrix[, c("county_fips_code", 'state_fips_code')] <- 0
pred_matrix['state_fips_code', c('ltpa_percent',
                                 'access_pa_percent')] <- -2


imp_cran <- mice(small_data, 
                 maxit = 100, 
                 m = 20, 
                 method = '2l.lmer',
                 predictorMatrix = pred_matrix)

# 2l.lmer
# 2l.continuous
# 2l.norm for normal and heterscedastic data
# 2l.pmm predictive mean modeling, homoscedastic data





preliminary_ltpa_long <- lmer(ltpa_percent ~ year_num +
                                (1 | state_fips_code) +
                                (1 | state_fips_code:county_fips_code), 
                              data = drop,
                              REML = FALSE,
                              control = lmerControl(optimizer = 'Nelder_Mead'))

summary(preliminary_ltpa_long)

ltpa_null_icc <- as_tibble(VarCorr(preliminary_ltpa_long))
ltpa_null_icc

library(mitml)

fit <- with(imp_cran, lme4::lmer(ltpa_percent ~ year_num +
                                   (1 | state_fips_code) +
                                   (1 | state_fips_code:county_fips_code), 
                                 REML = FALSE,
                                 control = lmerControl(optimizer = 'Nelder_Mead')))
summary(pool(fit))

testEstimates(as.mitml.result(fit), var.comp = TRUE)$var.comp


anova(preliminary_ltpa_long, fit)
# listwise seems appropriate.


ltpa_long_controls <- lmer(ltpa_percent ~ year_num +
                             violent_crime +
                             obesity_percent +
                             median_household_income +
                             air_pollution_particulate_matter +
                             rural_percent + 
                             access_pa_percent +
                             (1 | state_fips_code) +
                             (1 | state_fips_code:county_fips_code), 
                           data = drop,
                           REML = FALSE,
                           control = lmerControl(optimizer = 'Nelder_Mead'))

summary(ltpa_long_controls)

ltpa_controls_icc <- as_tibble(VarCorr(ltpa_long_controls))
ltpa_controls_icc


fit_controls <- with(imp_cran, lme4::lmer(ltpa_percent ~ year_num +
                                            violent_crime +
                                            obesity_percent +
                                            median_household_income +
                                            air_pollution_particulate_matter +
                                            rural_percent + 
                                            access_pa_percent +
                                   (1 | state_fips_code) +
                                   (1 | state_fips_code:county_fips_code), 
                                 REML = FALSE,
                                 control = lmerControl(optimizer = 'Nelder_Mead')))
summary(pool(fit_controls))

testEstimates(as.mitml.result(fit_controls), var.comp = TRUE)$var.comp




ltpa_long_access_random <- lmer(ltpa_percent ~ year_num + violent_crime +
                                  obesity_percent +
                                  median_household_income + air_pollution_particulate_matter +
                                  rural_percent + 
                                  access_pa_percent + (access_pa_percent | state_fips_code) + (access_pa_percent | state_fips_code:county_fips_code), 
                                data = drop,
                                REML = FALSE,
                                control = lmerControl(optimizer = 'Nelder_Mead'))

summary(ltpa_long_access_random)

ltpa_random_icc <- as_tibble(VarCorr(ltpa_long_access_random))
ltpa_random_icc

anova(ltpa_long_controls, ltpa_long_access_random)




fit_random <- with(imp_cran, lme4::lmer(ltpa_percent ~ year_num +
                                            violent_crime +
                                            obesity_percent +
                                            median_household_income +
                                            air_pollution_particulate_matter +
                                            rural_percent + 
                                            access_pa_percent +
                                            (access_pa_percent | state_fips_code) +
                                            (access_pa_percent | state_fips_code:county_fips_code), 
                                          REML = FALSE,
                                          control = lmerControl(optimizer = 'Nelder_Mead')))
summary(pool(fit_random))

testEstimates(as.mitml.result(fit_random), var.comp = TRUE)$var.comp






ltpa_long_access_int <- lmer(ltpa_percent ~ year_num + violent_crime +
                               obesity_percent +
                               air_pollution_particulate_matter +
                               rural_percent + 
                               access_pa_percent*median_household_income + 
                               (access_pa_percent | state_fips_code) + (access_pa_percent | state_fips_code:county_fips_code), 
                             data = drop,
                             REML = FALSE,
                             control = lmerControl(optimizer = 'Nelder_Mead'))

summary(ltpa_long_access_int)

anova(ltpa_long_access_random, ltpa_long_access_int)




ltpa_long_access_latino_int <- lmer(ltpa_percent ~ year_num + violent_crime +
                                      obesity_percent +
                                      air_pollution_particulate_matter +
                                      rural_percent + 
                                      median_household_income +
                                      access_pa_percent*latino_percent +
                                      (access_pa_percent | state_fips_code) + (access_pa_percent | state_fips_code:county_fips_code), 
                                    data = drop,
                                    REML = FALSE,
                                    control = lmerControl(optimizer = 'Nelder_Mead'))

summary(ltpa_long_access_latino_int)

anova(ltpa_long_access_random, ltpa_long_access_latino_int)

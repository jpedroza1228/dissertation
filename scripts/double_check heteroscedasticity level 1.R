library(nlme)

hetero_level1_both <- lme(ltpa_percent ~ year,
                          random = list(state_fips_code = ~ 1,
                                        county_fips_code = ~ 1),
                          data = county,
                          na.action = na.omit,
                          method = 'ML')

hetero_level1_both_weight <- lme(ltpa_percent ~ year,
                                 random = list(state_fips_code = ~ 1,
                                               county_fips_code = ~ 1),
                                 weights = varIdent(form = ~ 1 | year),
                                 data = county,
                                 na.action = na.omit,
                                 method = 'ML')

anova(hetero_level1_both, hetero_level1_both_weight)



hetero_level1_null_state <- lme(ltpa_percent ~ year,
                                random = ~ 1 | state_fips_code,
                                data = county,
                                na.action = na.omit,
                                method = 'ML')

summary(hetero_level1_null_state)
# variance is squared stdDev
3.74^2

hetero_level1_null_weight_state <- lme(ltpa_percent ~ year,
                                       random = ~ 1 | state_fips_code,
                                       weights = varIdent(form = ~ 1 | year),
                                       data = county,
                                       na.action = na.omit,
                                       method = 'ML')

summary(hetero_level1_null_weight_state)
3.44^2

anova(hetero_level1_null_state, hetero_level1_null_weight_state)


hetero_level1_null <- lme(ltpa_percent ~ year,
                          random = ~ 1 | county_fips_code,
                          data = county,
                          na.action = na.omit,
                          method = 'ML')

summary(hetero_level1_null)
# variance is squared stdDev
5.24^2

hetero_level1_null_weight <- nlme::lme(ltpa_percent ~ year,
                                       random = ~ 1 | county_fips_code,
                                       weights = varIdent(form = ~ 1 | year),
                                       data = county,
                                       na.action = na.omit,
                                       method = 'ML')

summary(hetero_level1_null_weight)
5.32^2

anova(hetero_level1_null, hetero_level1_null_weight)


prelim_ltpa_lme4 <- lmer(ltpa_percent ~ year +
                           (1 | county_fips_code),
                         data = county,
                         REML = FALSE,
                         control = lmerControl(optimizer = 'Nelder_Mead'))

summary(prelim_ltpa_lme4)
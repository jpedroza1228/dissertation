library(lme4)
library(optimx)
library(lmerTest)

county_icc_2level <- function(multi_model){
  between <- multi_model$vcov[1]
  total <- multi_model$vcov[1] + multi_model$vcov[2]
  
  between/total
}

preliminary_ltpa_long <- lmer(ltpa_percent ~ year_num +(1 | county_fips_code),data = ca,REML = FALSE,
                                    control = lmerControl(optimizer = 'Nelder_Mead'))

summary(preliminary_ltpa_long)

ltpa_null_icc <- as_tibble(VarCorr(preliminary_ltpa_long))
ltpa_null_icc


county_icc_2level(ltpa_null_icc)

ltpa_long_access <- lmer(ltpa_percent ~ year_num + violent_crime +obesity_percent +median_household_income + rural_percent + latino_percent +
                           access_pa_percent + (1 | county_fips_code), data = ca,
                         REML = FALSE,control = lmerControl(optimizer = 'Nelder_Mead'))

summary(ltpa_long_access)

anova(preliminary_ltpa_long, ltpa_long_access)

ltpa_access_icc <- as_tibble(VarCorr(ltpa_long_access))
ltpa_access_icc

county_icc_2level(ltpa_access_icc)

ca$main_effects <- predict(ltpa_long_access, newdata = ca)

ca %>% 
ggplot(aes(access_pa_percent, main_effects)) +
geom_point(aes(color = as.factor(year))) +
geom_smooth(aes(color = as.factor(year)),
            method = 'lm', se = FALSE) +
viridis::scale_color_viridis(discrete = TRUE) +
theme_dark()


main_effects_var <- ranef(ltpa_long_access, condVar = TRUE)
main_effects_var <- as.data.frame(main_effects_var)

main_effects_var <- main_effects_var %>% 
  rename(main_effects_term = term,
         county_fips_code = grp,
         main_effects_diff = condval,
         main_effects_se = condsd) %>% 
  mutate(county_fips_code = as.numeric(county_fips_code))

main_effects_var$county_name <- unique(ca$county_name)

main_effects_var %>% 
ggplot(aes(fct_reorder(county_name, main_effects_diff), main_effects_diff)) +
geom_errorbar(aes(ymin = main_effects_diff + qnorm(0.025)*main_effects_se,
                  ymax = main_effects_diff + qnorm(0.975)*main_effects_se)) +
geom_point(aes(color = county_name)) +
coord_flip() +
labs(x = ' ',
     y = 'Differences in Leisure-time Physical Activity',
     title = 'Variation in Leisure-time Physical Activity\nAcross California Counties') +
viridis::scale_color_viridis(discrete = TRUE) +
theme_dark() +
theme(legend.position = 'none')
      
      
      
ltpa_long_int_latino <- lmer(ltpa_percent ~ year_num + violent_crime +
                               obesity_percent +
                               median_household_income + rural_percent + latino_percent + access_pa_percent +
                               latino_percent:access_pa_percent +
                               (1 | county_fips_code),
                             data = ca,
                             REML = FALSE,
                             control = lmerControl(optimizer = 'Nelder_Mead'))

summary(ltpa_long_int_latino)

anova(ltpa_long_access, ltpa_long_int_latino)


ltpa_long_int_income <- lmer(ltpa_percent ~ year_num + violent_crime +
                                obesity_percent +
                                median_household_income + 
                                rural_percent + latino_percent + access_pa_percent +
                                median_household_income:access_pa_percent +
                                (1 | county_fips_code), 
                              data = ca,
                              REML = FALSE,
                              control = lmerControl(optimizer = 'Nelder_Mead'))

summary(ltpa_long_int_income)

anova(ltpa_long_access, ltpa_long_int_income)
    




states <- c('united_states',
            'Alabama','Alaska','Arizona','Arkansas',
            'California','Colorado','Connecticut',
            'Delaware',
            'Florida',
            'Georgia',
            'Hawaii',
            'Idaho','Illinois','Indiana','Iowa',
            'Kansas','Kentucky',
            'Louisiana',
            'Maine','Maryland','Massachusetts','Michigan','Minnesota','Mississippi','Missouri','Montana',
            'Nebraska','Nevada','New_Hampshire','New_Jersey','New_Mexico','New_York','North_Carolina','North_Dakota',
            'Ohio','Oklahoma','Oregon',
            'Pennsylvania',
            'Rhode_Island',
            'South_Carolina','South_Dakota',
            'Tennessee','Texas',
            'Utah',
            'Vermont','Virginia',
            'Washington','West_Virginia','Wisconsin','Wyoming')
states <- str_to_lower(states)

class(states)

county_sub <- county_sub %>%
  filter(!county_name %in% states)
library(tidyverse)
library(inspectdf)
library(psych)
library(lme4)
library(lmerTest)
library(optimx)

options(max.print = 99999)
options(scipen = 999)
theme_set(theme_minimal())


getwd()

county_icc_2level <- function(multi_model){
  between <- multi_model$vcov[1]
  total <- multi_model$vcov[1] + multi_model$vcov[2]
  
  between/total
}


counties <- function(years){
  
  link <- glue::glue('C:/Users/cpppe/OneDrive/Desktop/github_shared_folders/dissertation/final_data/county{years}_sub.csv')
  
  rio::import(link, setclass = 'tibble')
  
}


county <- map_df(16:20, ~counties(.x))


county <- county %>% 
  dplyr::select(rowid,
                state_fips_code:release_year,
                poor_or_fair_health:access_to_exercise_opportunities,
                preventable_hospital_stays,
                some_college:driving_alone_to_work,
                food_insecurity:uninsured_children,
                median_household_income:percent_rural) %>% 
  rename(year = release_year,
         state = state_abbreviation) %>% 
  mutate(phyact_percent = (physical_inactivity*100),
         ltpa_percent = (100 - phyact_percent)) %>% 
  rename(access_pa = access_to_exercise_opportunities) %>% 
  mutate(smoking_percent = adult_smoking*100,
         obesity_percent = adult_obesity*100,
         access_pa_percent = access_pa*100,
         college_percent = some_college*100,
         unemployment_percent = unemployment*100,
         driving_alone_percent = driving_alone_to_work*100,
         percent_65plus = percent_65_and_older*100,
         latino_percent = percent_hispanic*100,
         rural_percent = percent_rural*100) %>% 
  dplyr::select(-adult_smoking,
                -adult_obesity,
                -access_pa,
                -some_college,
                -unemployment,
                -driving_alone_to_work,
                -percent_65_and_older,
                -percent_hispanic,
                -percent_rural,
                -phyact_percent,
                -physical_inactivity)

county <- county %>%
  filter(!county_name %in% state)

county$year_num <- as.numeric(county$year)

ca <- county %>% 
  filter(state == 'CA')

preliminary_ltpa_long <- lmer(ltpa_percent ~ year_num +(1 | county_fips_code),data = ca,
                              REML = FALSE,
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


library(ggmap)
library(maps)
library(googleway)


us <- map_data(map = 'county')


us <- us %>% 
  rename(county = subregion,
         state = region)

us$county <- str_to_lower(us$county)
us$county <- str_replace_all(us$county, pattern = " ",
                               replacement = "_")

library(gganimate)

us <- us %>% 
  filter(state == 'california') %>% 
  mutate(state = recode(state, 'california' = 'CA'))



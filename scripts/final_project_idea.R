# Working with county data
library(tidyverse)
library(inspectdf)
library(psych)
library(lme4)
library(lmerTest)

options(max.print = 99999)
options(scipen = 999)

getwd()

set.seed(20200304)


county15 <- read.csv("E:/UO/R Projects/dissertation/final_data/county15_sub.csv")
county16 <- read.csv("E:/UO/R Projects/dissertation/final_data/county16_sub.csv")
county17 <- read.csv("E:/UO/R Projects/dissertation/final_data/county17_sub.csv")
county18 <- read.csv("E:/UO/R Projects/dissertation/final_data/county18_sub.csv")
county19 <- read.csv("E:/UO/R Projects/dissertation/final_data/county19_sub.csv")
city500 <- read_csv('E:/UO/R Projects/dissertation/data/500_Cities__Local_Data_for_Better_Health__2019_release.csv')


city500 <- city500 %>%
  janitor::clean_names()

city <- city500 %>% 
  group_by(measure, measure_id) %>% 
  count()

city_sub <- city500 %>% 
  dplyr::select(year,
                state_abbr,
                city_name,
                geographic_level,
                unique_id,
                data_value_type,
                data_value,
                population_count,
                geo_location,
                measure_id:tract_fips) %>% 
  rename(state = state_abbr,
         geo_level = geographic_level,
         id = unique_id)


city_sub$measure_id <- str_to_lower(city_sub$measure_id)

city_sub <- city_sub %>% 
  filter(measure_id == 'casthma' |
           measure_id == 'access2' |
           measure_id == 'csmoking' |
           measure_id == 'mhlth' |
           measure_id == 'lpa' |
           measure_id == 'obesity' |
           measure_id == 'phlth' |
           measure_id == 'sleep')


city_sub %>% 
  names()

city_sub %>% 
  group_by(geo_level, data_value_type) %>% 
  count()

city_wide <- city_sub %>% 
  spread(c(-1:-6, -8:-9, -12:-13), key = measure_id, value = data_value)


# county health ranking data
ex15 <- county15 %>% 
  inspect_na()

ex16 <- county16 %>% 
  inspect_na()

ex17 <- county17 %>% 
  inspect_na()

ex18 <- county18 %>% 
  inspect_na()

ex19 <- county19 %>% 
  inspect_na()


county15_sub <- county15[, -which(colMeans(is.na(county15)) >= 0.40)]
county16_sub <- county16[, -which(colMeans(is.na(county16)) >= 0.40)]
county17_sub <- county17[, -which(colMeans(is.na(county17)) >= 0.40)]
county18_sub <- county18[, -which(colMeans(is.na(county18)) >= 0.40)]
county19_sub <- county19[, -which(colMeans(is.na(county19)) >= 0.40)]

county <- full_join(county15_sub, county16_sub)
county <- full_join(county, county17_sub)
county <- full_join(county, county18_sub)
county <- full_join(county, county19_sub)

ex <- county %>% 
  inspect_na()

county <- county[, -which(colMeans(is.na(county)) >= .21)]

county <- county %>% 
  dplyr::select(X,
                state_fips_code:release_year,
                poor_or_fair_health:access_to_exercise_opportunities,
                uninsured:mental_health_providers,
                preventable_hospital_stays,
                some_college:violent_crime,
                air_pollution_particulate_matter,
                severe_housing_problems:long_commute_driving_alone,
                diabetes_prevalence:limited_access_to_healthy_foods,
                uninsured_adults:health_care_costs,
                median_household_income:insufficient_sleep) %>% 
  rename(row = X,
         state = state_abbreviation,
         year = release_year)
         
county %>% 
  names()

county %>% 
  inspect_na() %>% 
  show_plot()


# may want to think about removing 2015
county %>% 
  filter(year != 2015) %>% 
  inspect_na()


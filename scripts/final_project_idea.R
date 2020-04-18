# Working with county data
library(tidyverse)
library(inspectdf)
library(psych)
library(lavaan)
library(semPlot)
library(semTools)

options(max.print = 99999)
options(scipen = 999)

getwd()

set.seed(04092020)

# county15 <- read_csv("C:/Users/cpppe/OneDrive/Desktop/github shared projects/dissertation/final_data/county15_sub.csv")
county16 <- read_csv("C:/Users/cpppe/OneDrive/Desktop/github shared projects/dissertation/final_data/county16_sub.csv")
county17 <- read_csv("C:/Users/cpppe/OneDrive/Desktop/github shared projects/dissertation/final_data/county17_sub.csv")
county18 <- read_csv("C:/Users/cpppe/OneDrive/Desktop/github shared projects/dissertation/final_data/county18_sub.csv")
county19 <- read_csv("C:/Users/cpppe/OneDrive/Desktop/github shared projects/dissertation/final_data/county19_sub.csv")
county20 <- read_csv('C:/Users/cpppe/OneDrive/Desktop/github shared projects/dissertation/final_data/county20_sub.csv')
# city500 <- read_csv('E:/UO/R Projects/dissertation/data/500_Cities__Local_Data_for_Better_Health__2019_release.csv')


# county20_sub <- county20 %>% 
#     dplyr::select(-contains('numerator'),
#                   -contains('denominator'),
#                   -contains('low'),
#                   -contains('high')) %>% 
#   janitor::clean_names()

# county20_sub <- county20_sub %>% 
#     rename(fips_code = x5_digit_fips_code,
#            county_name = name) %>%
#     rowid_to_column() %>%
#     filter(rowid != 1) %>%
#     # filter(str_detect(county_name, 'County')) %>%
#     rename_at(vars(matches('_raw_value')), ~ str_remove(., '_raw_value'))
  
# county20_sub$county_name <- str_to_lower(county20_sub$county_name)
# county20_sub$county_name <- str_replace_all(county20_sub$county_name, 
#                                             pattern = " ",
#                                             replacement = "_")

# write.csv(county20_sub, 'county20_sub.csv')



# city500 <- city500 %>%
#   janitor::clean_names()

# city <- city500 %>% 
#   group_by(measure, measure_id) %>% 
#   count()

# city_sub <- city500 %>% 
#   dplyr::select(year,
#                 state_abbr,
#                 city_name,
#                 geographic_level,
#                 unique_id,
#                 data_value_type,
#                 data_value,
#                 population_count,
#                 geo_location,
#                 measure_id:tract_fips) %>% 
#   rename(state = state_abbr,
#          geo_level = geographic_level,
#          id = unique_id)


# city_sub$measure_id <- str_to_lower(city_sub$measure_id)

# city_sub <- city_sub %>% 
#   filter(measure_id == 'casthma' |
#            measure_id == 'access2' |
#            measure_id == 'csmoking' |
#            measure_id == 'mhlth' |
#            measure_id == 'lpa' |
#            measure_id == 'obesity' |
#            measure_id == 'phlth' |
#            measure_id == 'sleep')


# city_sub %>% 
#   names()

# city_sub %>% 
#   group_by(geo_level, data_value_type) %>% 
#   count()

# city_wide <- city_sub %>% 
#   spread(c(-1:-6, -8:-9, -12:-13), key = measure_id, value = data_value)


# county health ranking data

# ex15 <- county15 %>% 
  # inspect_na()

# ex16 <- county16 %>% 
  # inspect_na()

# ex17 <- county17 %>% 
  # inspect_na()

# ex18 <- county18 %>% 
  # inspect_na()

# ex19 <- county19 %>% 
  # inspect_na()

# county20_sub %>% 
#   inspect_na() %>% 
#   show_plot()


# county15_sub <- county15[, -which(colMeans(is.na(county15)) >= 0.40)]
county16_sub <- county16[, -which(colMeans(is.na(county16)) >= 0.40)]
county17_sub <- county17[, -which(colMeans(is.na(county17)) >= 0.40)]
county18_sub <- county18[, -which(colMeans(is.na(county18)) >= 0.40)]
county19_sub <- county19[, -which(colMeans(is.na(county19)) >= 0.40)]
county20_sub <- county20[, -which(colMeans(is.na(county20)) >= 0.40)]

# county <- full_join(county15_sub, county16_sub)
county <- full_join(county16_sub, county17_sub)
county <- full_join(county, county18_sub)
county <- full_join(county, county19_sub)
county <- full_join(county, county20_sub)


county %>% 
  inspect_na() %>% 
  show_plot()

county <- county[, -which(colMeans(is.na(county)) >= .25)]

county %>% 
  names()

county <- county %>% 
  dplyr::select(rowid,
                state_fips_code:release_year,
                poor_or_fair_health:access_to_exercise_opportunities,
                preventable_hospital_stays,
                some_college:driving_alone_to_work,
                food_insecurity:uninsured_children,
                median_household_income:percent_rural) %>% 
  rename(state = state_abbreviation,
         year = release_year)

county %>% 
  inspect_na() %>% 
  show_plot()


# may want to think about removing 2015

english <- county %>% 
  filter(year == 2016 |
           year == 2020) %>% 
  group_by(year, state) %>% 
  summarize(mean_per = mean(percent_not_proficient_in_english)) %>% 
  ungroup()

english %>% 
  ggplot(aes(fct_reorder(state, mean_per), mean_per)) +
  geom_col(color = 'white', fill = 'dodgerblue') +
  coord_flip() +
  facet_wrap(~year) +
  theme_minimal()


latino <- county %>% 
  filter(year == 2016 |
           year == 2020) %>% 
  group_by(year, state, county_name) %>% 
  summarize(mean_lat = mean(percent_hispanic)) %>% 
  ungroup()

latino %>% 
  ggplot(aes(fct_reorder(county_name, mean_lat), mean_lat)) +
  geom_point(aes(color = as.factor(year))) +
  coord_flip() +
  facet_wrap(~state) +
  theme_minimal()


ex<- county %>% 
  filter(year == 2020) %>% 
  group_by(state) %>% 
  tally(n()) 

ex %>% 
  summarize(sum_n = sum(n))
            


county %>% 
  dplyr::select(state, county_name, physical_inactivity) %>% 
  mutate(not_active_per = (physical_inactivity*100),
         active = (100 - not_active_per)) %>% 
  head()


# county %>% 
#   pivot_wider(names_from = c(state, county_name, year) , values_from = 8:47)

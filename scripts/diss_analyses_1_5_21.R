
pacman::p_load(tidyverse, sf, RColorBrewer, ggrepel, psych, inspectdf, tidycensus, leaflet, mapview, spdep, spatialreg, rgdal, rgeos)

display.brewer.all()

options(scipen = 999)

set.seed(12152020)
getwd()

# data loading
counties <- function(years){
  
  read_csv(glue::glue('C:/Users/cpppe/Desktop/github_projects/dissertation/census_data/analytic_data20{years}.csv'))
  
}

# counties(19)

county <- map_df(16:20, ~counties(.x)) %>% 
  janitor::clean_names() %>% 
  filter(state_fips_code != 'statecode')

county_sub <- county %>% 
  rename(state = state_abbreviation,
         year = release_year,
         county_name = name) %>% 
  mutate(year = as.numeric(year)) %>% 
  dplyr::select(1:6,
                48,
                53,
                151,
                272,
                297,
                352)

county16 <- county_sub %>% 
  filter(year == 2016) %>% 
  mutate(county_crime = as.numeric(violent_crime_raw_value),
         inactivity = as.numeric(physical_inactivity_raw_value),
         median_house_income = as.numeric(median_household_income_raw_value),
         pop = as.numeric(population_raw_value),
         rural = as.numeric(percent_rural_raw_value),
         rec_resource = as.numeric(access_to_exercise_opportunities_raw_value)) %>% 
  dplyr::select(1:6,
                13:18)

names(county16)

county16$state_fips_code <- str_remove(county16$state_fips_code, "^0+")
county16$county_fips_code <- str_remove(county16$county_fips_code, "^0+")

county18 <- county_sub %>% 
  filter(year == 2018) %>%
  mutate(county_crime = as.numeric(violent_crime_raw_value),
         inactivity = as.numeric(physical_inactivity_raw_value),
         median_house_income = as.numeric(median_household_income_raw_value),
         pop = as.numeric(population_raw_value),
         rural = as.numeric(percent_rural_raw_value),
         rec_resource = as.numeric(access_to_exercise_opportunities_raw_value)) %>% 
  dplyr::select(1:6,
                13:18)

names(county18)

county18$state_fips_code <- str_remove(county18$state_fips_code, "^0+")
county18$county_fips_code <- str_remove(county18$county_fips_code, "^0+")

county20 <- county_sub %>% 
  filter(year == 2020) %>%
  mutate(county_crime = as.numeric(violent_crime_raw_value),
         inactivity = as.numeric(physical_inactivity_raw_value),
         median_house_income = as.numeric(median_household_income_raw_value),
         pop = as.numeric(population_raw_value),
         rural = as.numeric(percent_rural_raw_value),
         rec_resource = as.numeric(access_to_exercise_opportunities_raw_value)) %>% 
  dplyr::select(1:6,
                13:18)

county20$state_fips_code <- str_remove(county20$state_fips_code, "^0+")
county20$county_fips_code <- str_remove(county20$county_fips_code, "^0+")

# write.csv(county16, 'county16.csv')
# write.csv(county18, 'county18.csv')
# write.csv(county20, 'county20.csv')

# https://www.census.gov/programs-surveys/cbp/data/datasets.html

cbp16 <- read_csv('C:/Users/cpppe/Desktop/github_projects/dissertation/census_data/cbp16co.txt') %>% 
  rename(state_fips_code = fipstate,
         county_fips_code = fipscty) %>% 
  mutate(year = 2016) %>% 
  dplyr::select(1:3,
                est,
                year) %>% 
  filter(naics == "61162/" | #Sports/Rec Instruction
           naics == "611620" |
           naics == "71394/" | #rec sports centers
           naics == "713940" |
           naics == "71399/" | # all other amusement/rec industries
           naics == "713990" |
           naics == "7212//" | #recreational camps
           naics == "72121/" |
           naics == "721211" | #parks and campgrounds
           naics == "721214" | #rec and vacation camps (no campgrounds)
           naics == "71213/" | #zoo
           naics == "712130" |
           naics == "71219/" | #parks
           naics == "712190" |
           naics == "71391/" | #golf courses
           naics == "713910" |
           naics == "71311/" | #amusement parks
           naics == "713110" |
           naics == "71392/" | #skiing
           naics == "713920" |
           naics == "71395/" | #bowling
           naics == "713950")

cbp16$state_fips_code <- str_remove(cbp16$state_fips_code, "^0+")
cbp16$county_fips_code <- str_remove(cbp16$county_fips_code, "^0+")

cbp18 <- read_csv('C:/Users/cpppe/Desktop/github_projects/dissertation/census_data/cbp18co.txt') %>% 
  rename(state_fips_code = fipstate,
         county_fips_code = fipscty) %>% 
  mutate(year = 2018) %>% 
  dplyr::select(1:3,
                est,
                year) %>% 
  filter(naics == "61162/" | #Sports/Rec Instruction
           naics == "611620" |
           naics == "71394/" | #rec sports centers
           naics == "713940" |
           naics == "71399/" | # all other amusement/rec industries
           naics == "713990" |
           naics == "7212//" | #recreational camps
           naics == "72121/" |
           naics == "721211" | #parks and campgrounds
           naics == "721214" | #rec and vacation camps (no campgrounds)
           naics == "71213/" | #zoo
           naics == "712130" |
           naics == "71219/" | #parks
           naics == "712190" |
           naics == "71391/" | #golf courses
           naics == "713910" |
           naics == "71311/" | #amusement parks
           naics == "713110" |
           naics == "71392/" | #skiing
           naics == "713920" |
           naics == "71395/" | #bowling
           naics == "713950")

cbp18$state_fips_code <- str_remove(cbp18$state_fips_code, "^0+")
cbp18$county_fips_code <- str_remove(cbp18$county_fips_code, "^0+")


# write.csv(cbp16, 'cbp16.csv')
# write.csv(cbp18, 'cbp18.csv')

census_key <- 'KEY' 

census_api_key(census_key)

# variable names need to be changed

variables_acs <- load_variables(2016, 'acs5')
  # filter(stringr::str_detect(label, 'Median household income'))


acs_var <- c('B02001_002', #estimate total, white alone
             'B02001_003', #estimate total, black/african american alone
             'B02001_005', #estimate total, asian alone
             'B03003_002', #estimate total, not hispanic or latino
             'B03003_003', #estimate total, hispanic or latino
             # all median household income variables
             "B19013_001",
             "B19013A_001",
             "B19013B_001",
             "B19013C_001",
             "B19013D_001",
             "B19013E_001",
             "B19013F_001",
             "B19013G_001",
             "B19013H_001",
             "B19013I_001",
             "B19049_001",
             "B19049_002",
             "B19049_003",
             "B19049_004",
             "B19049_005",
             "B25099_001",
             "B25099_002", 
             "B25099_003", 
             "B25119_001", 
             "B25119_002", 
             "B25119_003",
             'B06011_001'
             )

acs_year <- get_acs(
    geography = "county",
    variables = acs_var,
    year = 2016,
    survey = "acs5",
    geometry = TRUE,
    shift_geo = TRUE,
    output = 'wide'
    ) %>% 
  janitor::clean_names()

acs_year <- separate(acs_year, col = name, into = c('county_name', 'state_name'), sep = ', ') %>% 
  janitor::clean_names() %>% 
  rename(white_est = b02001_002e,
         black_est = b02001_003e,
         asian_est = b02001_005e,
         latino_est = b03003_002e,
         acs_geometry = geometry) %>% 
  dplyr::select(1:4,
                6, 8, 10,
                b25099_002e,
                b19049_003e,
                b25119_003e,
                b19049_005e,
                b19013h_001e,
                b25099_001e,
                b25099_003e,
                b25119_002e,
                b19049_004e,
                b03003_003e,
                b19013_001e,
                b19013a_001e,
                b19049_001e,
                b25119_001e,
                b06011_001e,
                acs_geometry) 

str(acs_year)
names(acs_year)

acs_year$state_name <- str_to_lower(acs_year$state_name)

acs_year <- acs_year %>% 
  mutate(state = recode(state_name, 'alabama' = 'AL','alaska' = 'AK','arizona' = 'AZ','arkansas' = 'AR',
                        'california' = 'CA','colorado' = 'CO','connecticut' = 'CT',
                        'delaware' = 'DE',
                        'florida' = 'FL',
                        'georgia' = 'GA',
                        'hawaii' = 'HI',
                        'idaho' = 'ID','illinois' = 'IL','indiana' = 'IN','iowa' = 'IA',
                        'kansas' = 'KS','kentucky' = 'KY',
                        'louisiana' = 'LA',
                        'maine' = 'MA','maryland' = 'MD','massachusetts' = 'MA','michigan' = 'MI','minnesota' = 'MN','mississippi' = 'MS','missouri' = 'MO','montana' = 'MT',
                        'nebraska' = 'NE','nevada' = 'NV','new_hampshire' = 'NH','new_jersey' = 'NJ','new_mexico' = 'NM','new_york' = 'NY','north_carolina' = 'NC','north_dakota' = 'ND',
                        'ohio' = 'OH','oklahoma' = 'OK','oregon' = 'OR',
                        'pennsylvania' = 'PA',
                        'rhode_island' = 'RI',
                        'south_carolina' = 'SC','south_dakota' = 'SD',
                        'tennessee' = 'TN','texas' = 'TX',
                        'utah' = 'UT',
                        'vermont' = 'VT','virginia' = 'VA',
                        'washington' = 'WA','west_virginia' = 'WV','wisconsin' = 'WI','wyoming' = 'WY'),
         year = 2016)


# https://www.fbi.gov/services/cjis/ucr
# https://crime-data-explorer.app.cloud.gov/

crime <- read_csv('C:/Users/cpppe/Desktop/github_projects/dissertation/census_data/estimated_crimes_1979_2019.csv') %>% 
  janitor::clean_names() %>% 
  filter(year >= 2008) %>% 
  rename(state = state_abbr)

crime16 <- crime %>% 
  filter(year == 2016) %>% 
  dplyr::select(year:homicide,
                robbery:motor_vehicle_theft) %>% 
  filter(state != 'NA')

names(crime16)
str(crime16)

crime18 <- crime %>% 
  filter(year == 2018) %>% 
  dplyr::select(year:homicide,
                robbery:motor_vehicle_theft) %>% 
  filter(state != 'NA')

# write.csv(crime16, 'crime16.csv')
# write.csv(crime18, 'crime18.csv')

# FINAL DATA

# 16
# need to figure out the fips codes in the cbp16 and county16
need_crime16 <- left_join(county16, cbp16, by = c("state_fips_code", "county_fips_code", "year"))
need_acs16 <- left_join(need_crime16, crime16, c("year", "state"))
final16 <- left_join(need_acs16, acs_year, by = c('county_name', 'state', 'year'))

names(final16)
names(wide16)
wide16 <- final16 %>%
  rowid_to_column() %>% 
  pivot_wider(names_from = year,
              values_from = c(8:13,
                              15,
                              17:47)) %>% 
  dplyr::select(-rowid)


# 18
need_crime18 <- left_join(county18, cbp18, by = c("state_fips_code", "county_fips_code", "year"))
need_acs18 <- left_join(need_crime18, crime18, c("year", "state"))

names(need_acs18)
wide18 <- need_acs18 %>%
  rowid_to_column() %>% 
  pivot_wider(names_from = year,
              values_from = c(8:13,
                              15,
                              17:25)) %>% 
  dplyr::select(-rowid) 

# 20
names(county20)
wide20 <- county20 %>% 
  rowid_to_column() %>% 
  pivot_wider(names_from = year,
              values_from = c(8:13)) %>% 
  dplyr::select(-rowid)
  
wide1618 <- left_join(wide16, wide18)

# BELOW IS THE FINAL DATASET TO USE
wider <- left_join(wide1618, wide20)

# full_join(county_aim12, cbp_aim12)
# aim12_needcrime <- full_join(county_aim12, cbp_aim12, by = c('year', 'county_fips_code', 'state_fips_code'))
# left_join(aim12_needcrime, crime_aim12)
# aim12_needacs <- left_join(aim12_needcrime, crime_aim12, by = c('state', 'year'))
# aim12 <- left_join(aim12_needacs, acs_year, by = c('county_name', 'state', 'year'))

# REMOVE ANY UNNECESSARY DATA FOR MEMORY
rm(counties)
rm(county)
rm(county_sub)
rm(county16)
rm(county18)
rm(county20)
rm(cbp)
rm(cbp16)
rm(cbp18)
rm(census_key)
rm(variables)
rm(acs_year)
rm(crime)
rm(crime16)
rm(crime18)
rm(final16)
rm(need_acs16)
rm(need_acs18)
rm(need_crime16)
rm(need_crime18)
rm(other)
rm(wide16)
rm(wide18)
rm(wide1618)
rm(wide20)

names(wider)

library(tictoc)

str(wider)
# FIXING ATOMIC VECTORS
wide_data <- wider

str(wide_data)
names(wide_data)

wide_data %>% 
  drop_na(rec_resource_2018, inactivity_2020) %>% 
  ggplot(aes(rec_resource_2018, inactivity_2020)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  theme_minimal()

wide_data %>% 
  drop_na(county_crime_2020, rec_resource_2018, inactivity_2020) %>% 
  ggplot(aes(rec_resource_2018, inactivity_2020)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, aes(color = cut(county_crime_2020, breaks = 3))) +
  theme_minimal()

imputed <- wide_data %>% 
  mutate_if(is.numeric, funs(replace(., is.na(.), median(., na.rm = TRUE))))


poly_data <- imputed %>% 
  drop_na(acs_geometry_2016) %>% 
  rowid_to_column() %>% 
  st_sf()

poly_data %>%
  mapview(zcol = 'rural_2020', homebutton = FALSE)


str(poly_data)
summary(poly_data)
class(poly_data)

acs_poly <- as(poly_data, 'Spatial')

class(acs_poly)
str(acs_poly)

poly_nb_queen <- poly2nb(acs_poly, row.names = acs_poly$rowid, queen = TRUE)
poly_nb_rook <- poly2nb(acs_poly, row.names = acs_poly$rowid, queen = FALSE)

rm(acs_poly)
rm(imputed)
rm(wider)
rm(wide_data)
rm(poly_nb_rook)
rm(variables_acs)

str(poly_nb)

poly_listw_queen <- nb2listw(poly_nb_queen, style = "W",
                       zero.policy = TRUE)

# moran_inactivity <- moran.test(poly_data$inactivity_2020, poly_listw)
# moran_inactivity

# not working, missing data
# moran_rec <- moran.test(poly_data$rec_resource_2018, poly_listw_queen)
# moran_rec

model_variables <- inactivity_2020 ~ rec_resource_2018 + rural_2020 +
  county_crime_2020 + median_house_income_2016 +
  black_est_2016 + white_est_2016 + latino_est_2016

int_variables <- inactivity_2020 ~ rec_resource_2018*county_crime_2020 +
  rural_2020 +
  median_house_income_2016 +
  black_est_2016 + white_est_2016 + latino_est_2016
  

# psych::describe(imputed, na.rm = TRUE)



model <- lm(model_variables, data = poly_data)
summary(model)
lm.beta::lm.beta(model)


# model_moran <- lm.morantest(model, poly_listw_queen)
# model_moran

# int_moran <- lm.morantest(interaction, poly_listw_queen)
# int_moran

lm_spa_tests <- lm.LMtests(model,
                           poly_listw_queen,
                           test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lm_spa_tests

# RLMlag is lowest

model_slx <- lmSLX(model_variables, poly_data, poly_listw_queen)
summary(model_slx)
summary(impacts(model_slx, poly_listw_queen, R = 500), zstats = TRUE)


tic()
model_sar <- lagsarlm(model_variables, poly_data, poly_listw_queen)
toc()
summary(model_sar)
summary(impacts(model_sar, poly_listw_queen, R = 500), zstats = TRUE)


tic()
model_sem <- errorsarlm(model_variables, poly_data, poly_listw_queen)
toc()
summary(model_sem)
hausman_test <- Hausman.test(model_sem)
hausman_test


tic()
model_sem_mixed <- errorsarlm(model_variables, poly_data,
                              poly_listw_queen, etype = "emixed")
toc()
summary(model_sem_mixed)
summary(impacts(model_sem_mixed, poly_listw_queen, R = 500), zstats = TRUE)




interaction <- lm(int_variables, data = poly_data)
summary(interaction)
lm.beta::lm.beta(interaction)



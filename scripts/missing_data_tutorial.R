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

set.seed(07122020)
# may need OneDrive before Desktop
# C:/Users/cpppe/OneDrive/

counties <- function(years){
  
  link <- glue::glue('C:/Users/cpppe/OneDrive/Desktop/github_shared_folders/dissertation/final_data/county{years}_sub.csv')
  
  rio::import(link, setclass = 'tibble')
  
}

counties(19)
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
                -physical_inactivity) %>% 
  filter(fips_code != '0')
  
county <- county %>% 
  filter(str_detect(fips_code, '000$', negate = TRUE))


county$year_num <- as.numeric(county$year)

names(county)

county_mice <- county[, -which(colMeans(is.na(county)) >= 0.10)]

inspect_na(county_mice)

county_mice %>% 
  inspect_na() %>% 
  show_plot()

county_mice %>% 
  dplyr::select(county_name, state, violent_crime, access_pa_percent, year_num, ltpa_percent) %>% 
  filter(state == 'AK') %>%
  naniar::gg_miss_var(facet = county_name, show_pct = TRUE)

drop <- county_mice %>% 
  drop_na('violent_crime')

# state_nest <- county_mice %>% 
#   dplyr::select(county_name, state, violent_crime, access_pa_percent, year_num, ltpa_percent) %>% 
#   group_by(state) %>% 
#   nest() %>% 
#   mutate(state_missing_plots = map(data, 
#                                    ~naniar::gg_miss_var(.x, facet = county_name, show_pct = TRUE)))

# state_nest$state_missing_plots[[26]]

# fs::dir_create(here::here("missing_plots", "states"))

# files_missing <- str_replace_all(tolower(state_nest$state_missing_plots), " ", "-")
# paths_missing <- here::here("missing_plots", "states", glue::glue("{files_missing}.png"))
# paths_missing

# walk2(paths_missing, state_nest$state_missing_plots, ggsave,
#       width = 9.5, 
#       height = 6.5,
#       dpi = 500)


# cnty_miss <- county_mice %>% 
#   dplyr::select(county_name, state, violent_crime, access_pa_percent, year_num, ltpa_percent) %>% 
#   group_by(state) %>% 
#   nest() %>% 
#   mutate(state_missing_plots = map(data, 
#                                    ~naniar::gg_miss_var(.x, show_pct = TRUE)))

# cnty_miss$state_missing_plots[[26]]
# States we know are missing data:
# AK, CO, CT, GA, HI, IA, IN, MA, MS, MT, NC, NE, NM, OR, SD, UT, WV, WY

library(mice)
library(miceadds)


# small_data <- county_mice %>%
#   sample_frac(.1)

# md.pattern(small_data)

# small_data %>% 
#   inspect_na() %>% 
#   show_plot()

# str(small_data)

# small_data <- small_data %>% 
#   dplyr::select(year_num, county_fips_code, state_fips_code,
#                 access_pa_percent,
#                 ltpa_percent, violent_crime, rural_percent,
#                 obesity_percent) %>% 
#   mutate(year_num = as.integer(year_num))


# IMPUTATION PRACTICE 
# only two level, year and county

names(small_data)

str(small_data)


small_data$ltpa_percent <- as.vector(scale(small_data$ltpa_percent,
                                           scale = FALSE))

variables_levels <- miceadds:::mice_imputation_create_type_vector(colnames(small_data),
                                                                  value = "")
# leave variables at lowest level blank (i.e., "")
variables_levels[c('access_pa_percent', 'ltpa_percent',
                    'violent_crime', 'rural_percent',
                    'obesity_percent')] <- "county_fips_code"




pred_matrix <- make.predictorMatrix(data = small_data)
imp_method <- make.method(data = small_data)

pred_matrix[, c("fips_code", 'state_fips_code')] <- 0
pred_matrix['year_num', ] <- c(0, 0, -2, 1, 1, 1, 1, 1)
pred_matrix['access_pa_percent', ] <- c(1, 0, -2, 0, 4, 1, 1, 1)
pred_matrix['ltpa_percent', ] <- c(1, 0, -2, 4, 0, 1, 1, 1)
pred_matrix['violent_crime', ] <- c(1, 0, -2, 1, 1, 0, 1, 1)
pred_matrix['rural_percent', ] <- c(1, 0, -2, 1, 1, 1, 0, 1)
pred_matrix['obesity_percent', ] <- c(1, 0, -2, 1, 1, 1, 1, 0)

pred_matrix


imp_method[c('access_pa_percent', 'ltpa_percent',
             'violent_crime', 'rural_percent',
             'obesity_percent')] <- '2l.pmm'

# ml.lmer is used for 3 levels or more

levels_id <- list()

levels_id[['violent_crime']] <- c('state_fips_code')
levels_id[['ltpa_percent']] <- c('state_fips_code')
levels_id[['access_pa_percent']] <- c('state_fips_code')
levels_id[['rural_percent']] <- c('fips_code', 'state_fips_code')
levels_id[['obesity_percent']] <- c('fips_code', 'state_fips_code')

# random_slopes <- list()
# random_slopes[['access_pa_percent']] <- list('fips_code' = c())

imp_cran <- mice(small_data, 
                 maxit = 10, 
                 m = 5, 
                 method = imp_method,
                 predictorMatrix = pred_matrix)




library(mitml)

fit <- with(imp_cran, lme4::lmer(ltpa_percent ~ access_pa_percent +
                                     year_num + violent_crime +
                                     rural_percent + obesity_percent +
                                     (access_pa_percent | state_fips_code), 
                                   REML = FALSE,
                                   control = lmerControl(optimizer = 'Nelder_Mead')))
summary(pool(fit))

testEstimates(as.mitml.result(fit), var.comp = TRUE)$var.comp
















level <- character(ncol(small_data))
names(level) <- colnames(small_data)


level['violent_crime'] <- 'county_fips_code'
level['ltpa_percent'] <- 'county_fips_code'
level['access_pa_percent'] <- 'county_fips_code'
level['rural_percent'] <- 'county_fips_code'
level['obesity_percent'] <- 'county_fips_code'






imp_simple <- mice(small_data,
                   pred = pred_matrix,
                   method = imp_method,
                   m = 20,
                   maxit = 20,
                   print = TRUE,
                   seed = 300)

# install.packages('mitml')



completed <- complete(imp_simple, action = 'long')

inspect_na(completed) %>% 
  show_plot()

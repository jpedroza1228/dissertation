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



counties <- function(years){
  
  link <- glue::glue('C:/Users/cpppe/OneDrive/Desktop/github shared projects/dissertation/final_data/county{years}_sub.csv')
  
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
                -physical_inactivity)

county <- county %>%
  filter(!county_name %in% state)

county$year_num <- as.numeric(county$year)

names(county)

county_mice <- county[, -which(colMeans(is.na(county)) >= 0.10)]

inspect_na(county_mice)

county_mice %>% 
  dplyr::select(county_name, state, violent_crime, access_pa_percent, year_num, ltpa_percent) %>%
  filter(state == 'AK') %>%
  naniar::gg_miss_var(facet = county_name, show_pct = TRUE)

county_mice %>% 
  dplyr::select(county_name, state, violent_crime, access_pa_percent, year_num, ltpa_percent) %>%
  naniar::gg_miss_var(facet = state, show_pct = TRUE)

# States we know are missing data:
# AK, CO, CT, GA, HI, IA, IN, MA, MS, MT, NC, NE, NM, OR, SD, UT, WV, WY

library(mice)
library(miceadds)

pred_matrix <- make.predictorMatrix(data = county_mice)
imp_method <- make.method(data = county_mice)

imp_method[c('violent_crime', 'ltpa_percent', 'access_pa_percent')] <- 'ml.lmer'
imp_method['w'] <- '2lonly.norm'

pred_matrix[, c("county_name", "state")] <- 0
# pred_matrix["w", "state"] <- -2

level <- character(ncol(county_mice))
names(level) <- colnames(county_mice)

level['w'] <- 'state'
level['z'] <- 'county_name'

cluster <- list()

cluster[['x']] <- c('county_name', 'state')
cluster[['y']] <- c('county_name', 'state')
cluster[['z']] <- c('state')

imp <- mice(county_name, method = imp_method,
            predictorMatrix = pred_matrix,
            maxit = 20,
            m = 5, levels_id = cluster,
            variables_level = level)

# install.packages('mitml')
library(mitml)



# impute practice
county_mice <- county[, -which(colMeans(is.na(county)) >= 0.10)]

names(county_mice)


county_mice %>% 
  group_by(state) %>% 
  naniar::gg_miss_var(facet = county_name,
                      show_pct = TRUE)

county_mice %>% 
  filter(state_fips_code == 1) %>% 
  naniar::gg_miss_var(facet = county_fips_code,
                      show_pct = TRUE)

county_mice %>% 
  inspect_na() %>% 
  show_plot()

county_mice %>% 
  dplyr::select(county_fips_code, state_fips_code, year_num, violent_crime, access_pa_percent, ltpa_percent) %>% 
  inspect_na() %>% 
  show_plot()

library(mice)
library(miceadds)

small_data <- county_mice %>% 
  sample_frac(.1)

md.pattern(small_data)

# mice.impute.ml.lme


model <- lmer(ltpa_percent ~ year_num + (1 | county_fips_code) + (1 | state_fips_code:county_fips_code), 
              data = county_mice,
              REML = FALSE,
              control = lmerControl(optimizer = 'Nelder_Mead'))
summary(model)


model2 <- lmer(ltpa_percent ~ year_num + violent_crime + access_pa_percent +
                 (1 | county_fips_code) + (1 | state_fips_code:county_fips_code), 
               data = county_mice,
               REML = FALSE,
               control = lmerControl(optimizer = 'Nelder_Mead'))
summary(model2)

anova(model, model2)


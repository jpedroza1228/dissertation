# https://towardsdatascience.com/combining-actual-data-with-simulated-data-in-machine-learning-fa07a68b9640
# https://clavelresearch.wordpress.com/2019/04/17/simulating-correlated-multivariate-data/


# COMBINING REAL DATA WITH SIMULATED DATA - DATA SCIENCE

library(tidyverse)


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
  
  link <- glue::glue('https://raw.githubusercontent.com/jpedroza1228/dissertation/master/final_data/county{years}_sub.csv')
  
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


county$year_num <- as.numeric(county$year)

ca <- county %>% 
  filter(state == 'CA')







library(broom)
library(caret)

practice_ca <- ca %>% 
  dplyr::select(access_pa_percent, ltpa_percent) 

inspectdf::inspect_na(practice_ca)

practice_ca %>% 
  ggplot(aes(access_pa_percent, ltpa_percent)) +
  geom_point(alpha = .3)

summary(lm(ltpa_percent ~ access_pa_percent, data = practice_ca))$sigma
summary(lm(ltpa_percent ~ access_pa_percent, data = practice_ca))

cor.test(practice_ca$access_pa_percent, practice_ca$ltpa_percent)

set.seed(9282020)

cov_practice <- cov(practice_ca) 

psych::describe(practice_ca)

library(MASS)

n <- 295
mu <- c(81.27, 81.47)
sigma_created <- cov_practice 
sigma_model <- 3.3
reps <- 100

ca_fake_data <- mvrnorm(n = n, mu = mu, Sigma = sigma_created)

ca_fake_data <- ca_fake_data %>% 
  as_tibble()

cor(ca_fake_data)

b0 <- 72.64175
b1 <- 0.10868
x1 <- ca_fake_data$access_pa_percent

y <- rnorm(n = n, mean = b0 + b1*x1, sd = sigma_model)

fake_model <- lm(y ~ x1)
summary(fake_model)

do_it <- for(j in 1:reps){
  ca_fake_data <- mvrnorm(n = n, mu = mu, Sigma = sigma_created)
  x1 <- ca_fake_data[, 1]
  
  y <- rnorm(n = n, mean = b0 + b1*x1, sd = sigma_model)
  
  fake_model <- lm(y ~ x1)
  summary(fake_model)
}

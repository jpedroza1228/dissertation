practice <- county %>% 
  dplyr::select(access_pa_percent, ltpa_percent, year, county_fips_code) 

practice %>% 
  psych::describe()

psych::describeBy(practice, group = 'year')

practice %>% 
  group_by(county_fips_code) %>% 
  count()

practice$county_fips_code[1:59]


# https://aosmith.rbind.io/2018/01/09/simulate-simulate-part1/

set.seed(9262020)


real_model <- lm(ltpa_percent ~ access_pa_percent, data = ca)
summary(real_model)
sigma(real_model)


nrep <- 10
b0 <- 72.64175
b1 <- .10868
sigma <- 3.297511


access <- rnorm(295, 81.27, 16)
pa <- rnorm(295, 81.47, 3.72)
years <- c(2016, 2017, 2018, 2019, 2020)
counties <- c(0, 1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49, 51, 53, 55, 57, 59, 61, 63, 65, 67, 69, 71, 73, 75,
              77, 79, 81, 83, 85, 87, 89, 91, 93, 95, 97, 99, 101, 103, 105, 107, 109, 111, 113, 115)

(eps = rnorm(n = access*nrep, mean = 0, sd = sigma))

fake_outcome <- b0 + b1*access + eps

sim <- tibble(access,
              pa,
              bigger = rep(years, c(59, 59, 59, 59, 59)),
              bigger_counties = rep(counties, 5),
              fake_outcome)

fake_model <- lm(fake_outcome ~ access, data = sim)
fake_model


sim_lm_fun <- function(nrep = 10, b0 = 72.64175, b1 = .10868, sigma = 3.297511){
  access <- rnorm(295, 81.27, 16)
  pa <- rnorm(295, 81.47, 3.72)
  
  years <- c(2016, 2017, 2018, 2019, 2020)
  counties <- c(0, 1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49, 51, 53, 55, 57, 59, 61, 63, 65, 67, 69, 71, 73, 75,
                77, 79, 81, 83, 85, 87, 89, 91, 93, 95, 97, 99, 101, 103, 105, 107, 109, 111, 113, 115)

  (eps = rnorm(n = access*nrep, mean = 0, sd = sigma))
  
  fake_outcome <- b0 + b1*access + eps
  
  sim <- tibble(access,
                pa,
                bigger = rep(years, c(59, 59, 59, 59, 59)),
                bigger_counties = rep(counties, 5))
  
  fake_model <- lm(fake_outcome ~ access, data = sim)
  fake_model
  
}

set.seed(9262020)
sim_lm_fun(sigma = 3)

# replicate
many_sims <- replicate(n = 1000, sim_lm_fun(), simplify = FALSE)

# returns as a list
many_sims[[4]]

library(broom)

tidy(fake_model)


many_sims %>% 
  map_df(tidy) %>% 
  filter(term == 'access') %>% 
  ggplot(aes(x = estimate)) +
  geom_density(fill = 'dodgerblue', alpha = .3) +
  geom_vline(xintercept = .11)

many_sims %>%
  map_dbl(~summary(.x)$sigma) %>%
  data.frame(sigma = .) %>%
  ggplot(aes(x = sigma) ) +
  geom_density(fill = "dodgerblue", alpha = .3) +
  geom_vline(xintercept = 3.30)

many_sims %>%
  map_dbl(~summary(.x)$sigma) %>%
  {. < 3.3} %>%
  mean()


many_sims %>%
  map_df(tidy) %>%
  filter(term == "access") %>%
  pull(p.value) %>%
  {. <  0.05} %>%
  mean()


many_sims %>%
  map_dbl(~summary(.x)$r.squared) %>%
  data.frame(r2 = .) %>%
  ggplot(aes(x = r2)) +
  geom_density(fill = "dodgerblue", alpha = .3) +
  geom_vline(xintercept = .2182)


fake <- sim %>% 
  ggplot(aes(access, fake_outcome)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  xlim(10, 125) +
  ylim(10, 100)

real <- county %>%
  filter(state == 'CA') %>% 
  ggplot(aes(access_pa_percent, ltpa_percent)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  xlim(10, 125) +
  ylim(10, 100)

library(gridExtra)
grid.arrange(fake, real, ncol = 2)

library(tidyverse)

imputed$income_factor <- cut(imputed$med_inc_2016_thousand, 3)

# https://rpubs.com/quarcs-lab/tutorial-spatial-regression

# Create neighbors and then try running that as a three-way interaction between access, income, neighbors on LTPA


# lagged interaction will look like this but the facet will be of the neighbors/lagged neighbors
imputed %>% 
  ggplot(aes(rec_resource_2018, ltpa_2020)) +
  geom_point(aes(color = state_fips_code), alpha = .3) +
  geom_smooth(color = 'black', method = 'lm', se = FALSE, size = 1.25) +
  theme_minimal() +
  theme(legend.position = 'none') +
  facet_wrap(~state_fips_code)
  # scale_color_manual(values = c('#669b3e', '#d72d2b', '#398b98'))

imputed %>% 
  ggplot(aes(rec_resource_2018, ltpa_2020)) +
  geom_point(aes(color = state_fips_code), alpha = .3) +
  geom_smooth(aes(linetype = crime_factor), color = 'black', method = 'lm', se = FALSE, size = 1.25) +
  theme_minimal() +
  theme(legend.position = 'none')

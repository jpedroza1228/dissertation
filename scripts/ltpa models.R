preliminary_ltpa_long <- lmer(ltpa_percent ~ year + (1 | state_fips_code) +
                                (1 | state_fips_code:county_fips_code), 
                              data = county,
                              REML = FALSE,
                              control = lmerControl(optimizer = 'Nelder_Mead'))

summary(preliminary_ltpa_long)

ltpa_null_icc <- as_tibble(VarCorr(preliminary_ltpa_long))
ltpa_null_icc

total_icc <- function(df){
  between <- df[1, 4] + df[2, 4]
  total <- df[1, 4] + df[2, 4] + df[3, 4]
  
  between/total
}

state_icc <- function(df){
  between <- df[2, 4]
  total <- df[1, 4] + df[2, 4] + df[3, 4]
  
  between/total
}

county_state_icc <- function(df){
  between <- df[1, 4]
  total <- df[1, 4] + df[2, 4] + df[3, 4]
  
  between/total
}

total_icc(ltpa_null_icc)
state_icc(ltpa_null_icc)
county_state_icc(ltpa_null_icc)


ltpa_long_controls <- lmer(ltpa_percent ~ year + violent_crime +
                             adult_obesity + some_college +
                             median_household_income + air_pollution_particulate_matter +
                             driving_alone_to_work + percent_rural +
                             (1 | state_fips_code) + (1 | state_fips_code:county_fips_code), 
                           data = county,
                           REML = FALSE,
                           control = lmerControl(optimizer = 'Nelder_Mead'))

summary(ltpa_long_controls)

ltpa_controls_icc <- as_tibble(VarCorr(ltpa_long_controls))
ltpa_controls_icc

total_icc(ltpa_controls_icc)
state_icc(ltpa_controls_icc)
county_state_icc(ltpa_controls_icc)

# anova(preliminary_ltpa_long, ltpa_long_controls)


ltpa_long_access <- lmer(ltpa_percent ~ year + violent_crime +
                           adult_obesity + some_college +
                           income_inequality + air_pollution_particulate_matter +
                           driving_alone_to_work + percent_rural + 
                           access_pa + (1 | state_fips_code) + (1 | state_fips_code:county_fips_code), 
                         data = county,
                         REML = FALSE,
                         control = lmerControl(optimizer = 'Nelder_Mead'))

summary(ltpa_long_access)

anova(ltpa_long_controls, ltpa_long_access)


ltpa_long_access_random <- lmer(ltpa_percent ~ year + violent_crime +
                                  adult_obesity + some_college +
                                  income_inequality + air_pollution_particulate_matter +
                                  driving_alone_to_work + percent_rural + 
                                  access_pa + (year || state_fips_code) + (year || state_fips_code:county_fips_code), 
                                data = county,
                                REML = FALSE,
                                control = lmerControl(optimizer = 'Nelder_Mead'))

summary(ltpa_long_access_random)

anova(ltpa_long_access, ltpa_long_access_random)
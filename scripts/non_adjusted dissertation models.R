names(drop)

county_by_year <- function(year){
  drop %>% 
    filter(year == {{year}}) %>% 
    group_by(state_fips_code, year, county_fips_code) %>% 
    summarize(n = n()) %>% 
    ungroup(county_fips_code) %>% 
    count(county_fips_code) %>% 
    summarize(counties = sum(n))
}

year16 <- county_by_year('2016')
year17 <- county_by_year('2017')
year18 <- county_by_year('2018')
year19 <- county_by_year('2019')
year20 <- county_by_year('2020')


# https://stats.stackexchange.com/questions/242109/model-failed-to-converge-warning-in-lmer

# testing assumptions
# https://ademos.people.uic.edu/Chapter18.html#61_assumption_1_-_linearity



model0_ac <- lm(access_pa_percent ~ year_num,
                data = drop)
summary(model0_ac)


prelim <- lmer(access_pa_percent ~ year_num +
                 (1 | state_fips_code) +
                 (1 | state_fips_code:county_fips_code), 
                      data = drop,
                      REML = FALSE)
summary(prelim)
sundry::aic_weights(model0_ac, prelim)


h1_2 <- lmer(access_pa_percent ~ year_num + 
                  latino_percent + median_household_income +
                 (1 | state_fips_code) +
                 (1 | state_fips_code:county_fips_code), 
               data = drop,
               REML = FALSE)
summary(h1_2)

anova(prelim, h1_2)
sundry::aic_weights(prelim, h1_2)


model0_pa <- lm(ltpa_percent ~ year_num,
               data = drop)
summary(model0_pa)


h3 <- lmer(ltpa_percent ~ year_num +
                 (1|state_fips_code) +
                                (1 | state_fips_code:county_fips_code), 
                              data = drop,
                              REML = FALSE)
summary(h3)
sundry::aic_weights(model0, h3)


h4 <- lmer(ltpa_percent ~ year_num +
                             access_pa_percent +
                 (1 | state_fips_code) +
                             (1 | state_fips_code:county_fips_code), 
                           data = drop,
                           REML = FALSE)
summary(h4)
anova(h3, h4)
sundry::aic_weights(h3, h4)


h5 <- lmer(ltpa_percent ~ 
                 year_num + access_pa_percent +
             rural_percent +
             violent_crime + air_pollution_particulate_matter +
             social_associations +
             obesity_percent + latino_percent + median_household_income +
             percent_65plus + poor_or_fair_health +
                 (1 | state_fips_code) +
                 (1 | state_fips_code:county_fips_code), 
               data = drop,
               REML = FALSE)

summary(h5)
anova(h4, h5)
sundry::aic_weights(h4, h5)


h6 <- lmer(ltpa_percent ~ year_num*access_pa_percent +
             rural_percent +
             violent_crime + air_pollution_particulate_matter +
             social_associations +
             obesity_percent + latino_percent + median_household_income +
             percent_65plus + poor_or_fair_health +
             (1 | state_fips_code) +
             (1 | state_fips_code:county_fips_code), 
           data = drop,
           REML = FALSE)
summary(h6)
sundry::aic_weights(h5, h6)

drop %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot(aes(access_pa_percent, ltpa_percent)) +
  # geom_point(color = 'gray90', alpha = .9) +
  geom_smooth(method = lm, se = FALSE, aes(color = year), size = 1.25) +
  theme_minimal() +
  viridis::scale_color_viridis(discrete = TRUE)

h7 <- lmer(ltpa_percent ~ access_pa_percent*latino_percent +
             year_num +
             rural_percent +
             violent_crime + air_pollution_particulate_matter +
             social_associations +
             obesity_percent + median_household_income +
             percent_65plus + poor_or_fair_health +
             (1 | state_fips_code) +
             (1 | state_fips_code:county_fips_code),
           data = drop,
           REML = FALSE)
summary(h7)
sundry::aic_weights(h6, h7)


h8 <- lmer(ltpa_percent ~ access_pa_percent*median_household_income +
              year_num +
              rural_percent +
              violent_crime + air_pollution_particulate_matter +
              social_associations +
              obesity_percent + latino_percent +
              percent_65plus + poor_or_fair_health +
              (1 | state_fips_code) +
              (1 | state_fips_code:county_fips_code),
            data = drop,
            REML = FALSE)
summary(h8)
sundry::aic_weights(h7, h8)




# per year analysis

prelim_sem <- '
access_pa_percent ~ c(c16, c17, c18, c19, c20)*median_household_income +
c(d16, d17, d18, d19, d20)*latino_percent
'

library(lavaan)
prelim_fit <- sem(prelim_sem,
                  data = drop,
                  missing = 'fiml.x',
                  group = 'year')
summary(prelim_fit)
fitMeasures(prelim_fit)

library(semTools)
measurementInvariance(model = prelim_sem, 
                      data = drop, 
                      group = "year")



drop16 <- drop %>% 
  filter(year == 2016)

drop18 <- drop %>% 
  filter(year == 2018)

drop20 <- drop %>% 
  filter(year == 2020)


prelim16 <- lmer(access_pa_percent ~ median_household_income +
                 (1 | state_fips_code), 
               data = drop16,
               REML = FALSE)
prelim18 <- lmer(access_pa_percent ~ median_household_income +
                   (1 | state_fips_code), 
                 data = drop18,
                 REML = FALSE)
prelim20 <- lmer(access_pa_percent ~ median_household_income +
                   (1 | state_fips_code), 
                 data = drop20,
                 REML = FALSE)
summary(prelim16)
summary(prelim18)
summary(prelim20)

var16 <- as_tibble(VarCorr(prelim16))
var18 <- as_tibble(VarCorr(prelim18))
var20 <- as_tibble(VarCorr(prelim20))

state_icc_2level <- function(df){
  between <- df[1, 4]
  total <- df[1, 4] + df[2, 4]
  
  between/total
}

state_icc_2level(var16)
state_icc_2level(var18)
state_icc_2level(var20)




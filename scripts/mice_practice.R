small_data <- county_mice %>% 
  sample_frac(.07)

md.pattern(small_data)

small_data <- small_data %>% 
  dplyr::select(year_num, violent_crime, access_pa_percent,
                ltpa_percent, county_fips_code, state_fips_code)

names(small_data)

pred_matrix <- make.predictorMatrix(data = small_data)
imp_method <- make.method(data = small_data)

pred_matrix[, c("county_fips_code", "state_fips_code")] <- 0
pred_matrix["violent_crime", c(1, 3, 4, 5, 6)] <- c(1, 1, 1, -2, -2)
pred_matrix["access_pa_percent", c(1, 4, 5, 6)] <- c(1, 2, -2, -2)
pred_matrix["ltpa_percent", c(1, 5, 6)] <- c(1, -2, -2)

pred_matrix

imp_method[c('violent_crime', 'ltpa_percent', 'access_pa_percent')] <- 'ml.lmer'

level <- character(ncol(small_data))
names(level) <- colnames(small_data)


level['violent_crime'] <- 'county_fips_code'
level['ltpa_percent'] <- 'county_fips_code'
level['access_pa_percent'] <- 'county_fips_code'

cluster <- list()

cluster[['violent_crime']] <- c('county_fips_code', 'state_fips_code')
cluster[['ltpa_percent']] <- c('county_fips_code', 'state_fips_code')
cluster[['access_pa_percent']] <- c('county_fips_code', 'state_fips_code')

imp <- mice(small_data, method = imp_method,
            predictorMatrix = pred_matrix,
            maxit = 2,
            m = 2,
            levels_id = cluster,
            variables_level = level)

imp_single <- mice(small_data, m = 2, maxit = 2, print = FALSE)
imp_single

library(mitml)

single_list <- mids2mitml.list(imp_single)

single_fit <- with(single_list, lmer(ltpa_percent ~ violent_crime + access_pa_percent +
                                      (1 | county_fips_code) +
                                      (1 | state_fips_code:county_fips_code),
                                    REML = FALSE))

testEstimates(single_fit, var.comp = TRUE)
summary(pool(single_fit))

str(small_data)

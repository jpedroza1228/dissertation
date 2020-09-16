psych::describe(small_data, na.rm = TRUE)

access <- rnorm(n = 15703, mean = 61.49, sd = 23.72)
pa <- rnorm(n = 15703, mean = 0, sd = 5.38)
lat <- rnorm(n = 15703, mean = 9.32, sd = 13.67)
money <- rnorm(n = 15703, mean = 49825.51, sd = 13109.03)
ind <- as.integer(round(runif(15703, 1, 840)))
group <- as.integer(round(runif(15703, 1, 56)))
bigger <- as.integer(round(runif(15703, 1, 4)))

df <- tibble(access, pa, lat, money, ind, group, bigger)


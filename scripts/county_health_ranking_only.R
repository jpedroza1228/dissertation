library(tidyverse)
library(inspectdf)
library(psych)
library(lme4)
library(lmerTest)

options(max.print = 99999)
options(scipen = 999)

getwd()

set.seed(20200304)


county15 <- read.csv("E:/UO/R Projects/dissertation/county_data/county15_sub.csv")
county16 <- read.csv("E:/UO/R Projects/dissertation/county_data/county16_sub.csv")
county17 <- read.csv("E:/UO/R Projects/dissertation/county_data/county17_sub.csv")
county18 <- read.csv("E:/UO/R Projects/dissertation/county_data/county18_sub.csv")

colnames(county15)

ca15 <- county15 %>%
  filter(state_abbreviation == 'CA')

ca16 <- county16 %>%
  filter(state_abbreviation == 'CA')

ca17 <- county17 %>%
  filter(state_abbreviation == 'CA')

ca18 <- county18 %>%
  filter(state_abbreviation == 'CA')

ca_county <- full_join(ca15, ca16)
ca_county <- full_join(ca_county, ca17)
ca_county <- full_join(ca_county, ca18)

# write.csv(ca_county, 'ca_county.csv')

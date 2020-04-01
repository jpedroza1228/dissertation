library(tidyverse)
library(inspectdf)
library(psych)

cities500 <- read_csv('E:/UO/R Projects/dissertation/data/500_Cities__Local_Data_for_Better_Health__2019_release.csv')

cities500 <- cities500 %>% 
  janitor::clean_names()

cities500_ca <- cities500 %>% 
  filter(state_abbr == 'CA')

# write.csv(cities500_ca, 'cities500_ca.csv')

cities500_ca %>% 
  count(year)


gis500 <- read_csv('E:/UO/R Projects/dissertation/data/500_Cities__Census_Tract-level_Data__GIS_Friendly_Format___2019_release.csv')

gis500 <- gis500 %>% 
  janitor::clean_names()

gis500_ca <- gis500 %>% 
  filter(state_abbr == 'CA')

# write.csv(gis500_ca, 'gis500_ca.csv')

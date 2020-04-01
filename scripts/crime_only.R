library(tidyverse)
library(inspectdf)
library(psych)

hate <- read_csv('E:/UO/R Projects/dissertation/data/HATE_2001-2018.csv')

hate_sub <- hate %>% 
  janitor::clean_names() %>% 
  filter(closed_year >= 2015)

ex <- hate_sub %>% 
  group_by(most_serious_location, closed_year, county) %>% 
  count()

exx <- hate_sub %>% 
  count(most_serious_location)

hate_sub <- hate_sub %>% 
  filter(most_serious_location == 'Church/Synagogue/Temple' |
           most_serious_location == 'Community Center' |
           most_serious_location == 'Convenience Store' |
           most_serious_location == 'Field/Woods/Park' |
           most_serious_location == 'Grocery/Supermarket' |
           most_serious_location == 'Highway/Road/Alley/Street' |
           most_serious_location == 'Lake/Waterway/Beach' |
           most_serious_location == 'Liquore Store' |
           most_serious_location == 'Park/Playground' |
           most_serious_location == 'Residence/Home/Driveway' |
           most_serious_location == 'School-College/University' |
           most_serious_location == 'School-Elementary/Secondary')

cops <- read_csv('E:/UO/R Projects/dissertation/data/LE_and_CJ_Personnel_2003-2018.csv')

cops_sub <- cops %>% 
  janitor::clean_names() %>% 
  filter(year >= 2015) %>% 
  dplyr::select(year:st_le_total,
              cnty_total:cnty_le_total)

arrest <- read_csv('E:/UO/R Projects/dissertation/data/OnlineArrestData1980-2018.csv')

arrest_sub <- arrest %>% 
  janitor::clean_names() %>% 
  filter(year >= 2015)


# write.csv(hate_sub, 'hate_sub.csv')
# write.csv(cops_sub, 'cops_sub.csv')
# write.csv(arrest_sub, 'arrest_sub.csv')

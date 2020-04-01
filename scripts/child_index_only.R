library(tidyverse)
library(inspectdf)
library(psych)

child_index <- read_csv('E:/UO/R Projects/dissertation/data/child_opp_index.csv')

ca_child_index <- child_index %>% 
  janitor::clean_names() %>% 
  filter(stateusps == 'CA')

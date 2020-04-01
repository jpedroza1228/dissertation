library(tidyverse)
library(inspectdf)
library(psych)

getwd()

brfss18 <- Hmisc::sasxport.get("E:/UO/R Projects/dissertation/data/LLCP2018.xpt")
brfss17 <- Hmisc::sasxport.get("E:/UO/R Projects/dissertation/data/LLCP2017.xpt")
brfss16 <- Hmisc::sasxport.get("E:/UO/R Projects/dissertation/data/LLCP2016.xpt")

brfss18_ca <- brfss18 %>%
  janitor::clean_names() %>% 
  filter(x_state == 6)

brfss17_ca <- brfss17 %>%
  janitor::clean_names() %>% 
  filter(x_state == 6)

brfss16_ca <- brfss16 %>%
  janitor::clean_names() %>%
  filter(x_state == 6)

# write.csv(brfss18_ca, 'E:/UO/R Projects/dissertation/final_data/brfss18_ca.csv')
# write.csv(brfss17_ca, 'E:/UO/R Projects/dissertation/final_data/brfss17_ca.csv')
# write.csv(brfss16_ca, 'E:/UO/R Projects/dissertation/final_data/brfss16_ca.csv')


smart17 <- Hmisc::sasxport.get("E:/UO/R Projects/dissertation/data/MMSA2017.xpt")
smart16 <- Hmisc::sasxport.get("E:/UO/R Projects/dissertation/data/MMSA2016.xpt")


smart17_sub <- smart17 %>%
  filter(stringr::str_detect(mmsaname, 'CA'))

smart16_sub <- smart16 %>%
  filter(stringr::str_detect(mmsaname, 'CA'))


# write.csv(smart17_sub, 'E:/UO/R Projects/dissertation/final_data/smart17_sub.csv')
# write.csv(smart16_sub, 'E:/UO/R Projects/dissertation/final_data/smart16_sub.csv')






# smart15 <- Hmisc::sasxport.get("E:/UO/R Projects/dissertation/data/MMSA2015.xpt")
# smart14 <- Hmisc::sasxport.get("E:/UO/R Projects/dissertation/data/MMSA2014.xpt")
# smart13 <- Hmisc::sasxport.get("E:/UO/R Projects/dissertation/data/MMSA2013.xpt")
# smart12 <- Hmisc::sasxport.get("E:/UO/R Projects/dissertation/data/CNTY2012.xpt")
# smart11 <- Hmisc::sasxport.get("E:/UO/R Projects/dissertation/data/CNTY2011.xpt")


# brfss15 <- Hmisc::sasxport.get("E:/UO/R Projects/dissertation/data/LLCP2015.xpt")
# brfss14 <- Hmisc::sasxport.get("E:/UO/R Projects/dissertation/data/LLCP2014.xpt")
# brfss13 <- Hmisc::sasxport.get("E:/UO/R Projects/dissertation/data/LLCP2013.xpt")
# brfss12 <- Hmisc::sasxport.get("E:/UO/R Projects/dissertation/data/LLCP2012.xpt")
# brfss11 <- Hmisc::sasxport.get("E:/UO/R Projects/dissertation/data/LLCP2011.xpt")


# brfss15_ca <- brfss15 %>%
#   janitor::clean_names() %>% 
#   filter(x_state == 6)

# brfss14_ca <- brfss14 %>%
#   janitor::clean_names() %>% 
#   filter(x_state == 6)

# brfss13_ca <- brfss13 %>%
#   janitor::clean_names() %>% 
#   filter(x_state == 6)

# brfss12_ca <- brfss12 %>%
#   janitor::clean_names() %>% 
#   filter(x_state == 6)

# brfss11_ca <- brfss11 %>%
#   janitor::clean_names() %>% 
#   filter(x_state == 6)

library(tidyverse)

df <- read_csv(here::here("08_animals", "global-living-planet-index.csv")) %>% 
  janitor::clean_names() %>% 
  filter(entity == "Freshwater") %>% 
  select(-code, -entity) %>% 
  rename("Living Planet Index" = living_planet_index,
         "Upper CI" = upper_ci_living_planet_index_95_percent_ci,
         "Lower CI" = lower_ci_living_planet_index_95_percent_ci)

write_csv(df, here::here("08_animals", "global-living-planet-index-freshwater.csv"))

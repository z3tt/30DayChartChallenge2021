library(tidyverse)
library(lubridate)
library(gggibbous)

Sys.setlocale("LC_ALL","English")

df_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')

df <- df_raw %>% 
  filter(!is.na(date_added)) %>% 
  mutate(
    date_added = as_date(date_added, format = "%B %e, %Y"),
    quarter = quarter(date_added),
    year = year(date_added),
    year_quarter = glue::glue("{year}–Q{quarter}"),
    year_numeric = year + quarter/4,
    is_american = if_else(str_detect(country, "United States"), TRUE, FALSE)
  ) %>% 
  filter(year_numeric != 2021.25) %>% 
  count(year_quarter, year_numeric, type) %>% 
  group_by(year_quarter, year_numeric) %>%
  mutate(total = sum(n, na.rm = TRUE)) %>% 
  group_by(year_quarter, year_numeric, type) %>% 
  mutate(prop = n / total) %>% 
  ungroup %>% 
  mutate(right = type == "Movie") 

pal <- colorRampPalette(c("white", "#E50914", "black"))

df %>% 
  group_by(year_quarter, year_numeric) %>%
  summarize(total = sum(n, na.rm = TRUE)) %>% 
  ggplot(aes(year_numeric, total)) +
  geom_line() +
  geom_moon(
    data = df,
    aes(ratio = prop, right = right, fill = type),
    color = "transparent", size = 17
  ) +
  scale_fill_manual()
  
  

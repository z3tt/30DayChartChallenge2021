library(tidyverse)
library(rvest)

url <-  "https://www.goal.com/en/news/the-100-most-expensive-football-transfers-of-all-time/ikr3oojohla51fh9adq3qkwpu"

list <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = TRUE)

df <- list[[2]] %>% 
  as_tibble %>% 
  #slice(1:5) %>% 
  mutate(
    Fee = as.numeric(str_extract(Fee, "(\\d+)")),
    year_deal = Year,
    Club = str_trim(sub('.*\\-', '', Clubs))
  ) %>% 
  complete(Year, nesting(Player, Fee, year_deal)) %>% 
  group_by(Player, year_deal) %>% 
  fill(Fee, Club, .direction = "down") %>% 
  mutate(
    Fee = if_else(year_deal > Year, NA_real_, Fee),
    Club = Club,
    Country = case_when(
      str_detect(Club, "Inter|Lazio|Juventus|Napoli") ~ "Italy",
      str_detect(Club, "Madrid|Barcelona") ~ "Spain",
      str_detect(Club, "Manchester|Chelsea|Arsenal|Liverpool|Everton|West Ham|Tottenham|Leicester") ~ "England",
      str_detect(Club, "Bayern") ~ "Germany",
      str_detect(Club, "Monaco|PSG") ~ "France",
      str_detect(Club, "Jiangsu|Shanghai") ~ "China"
    ),
    Player = paste0(Player, " (", Club, ")")
  ) %>% 
  group_by(Player, Year, Club) %>% 
  filter(Fee == max(Fee), Year > 2000) %>% 
  dplyr::select(Player, Year, Fee, Club, Country) %>%
  pivot_wider(id_cols = c("Player", "Club", "Country"), 
              names_from = Year, values_from = "Fee")

write_csv(df, here::here("22_animation", "players.csv"))
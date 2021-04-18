library(tidyverse)

df_episodes <- read_rds(here::here("17_pop_culture", "top250_allratings.Rds"))

df_title_basics <- 
  read_tsv(
    here::here("17_pop_culture", "title.basics.tsv.gz"), 
    col_types = list(runtimeMinutes = col_double())
  ) %>% 
  dplyr::select(tconst, originalTitle, runtimeMinutes, genres)

df_ratings <- read_rds(here::here("17_pop_culture", "top250_ratings.Rds"))

df_top250 <- df_ratings %>% 
  left_join(df_episodes, by = c("code" = "Code")) %>% 
  mutate(
    ## clean some titles manually first
    rating = as.numeric(rating),
    Rating = as.numeric(Rating),
    rating_count = as.numeric(str_remove(rating_count, ",")),
    year = as.numeric(str_sub(year, 2, 5)),
    title = str_replace_all(title, "MisteRogers", "Mister Rogers'"),
    title = if_else(year == 2010 & title == "Spartacus", 
                    "Spartacus: Blood and Sand", title),
    title_id = glue::glue("{title} ({year})")
  ) %>% 
  group_by(title_id) %>% 
  mutate(total = n()) %>% 
  group_by(title_id, code, year, Season, rating, rating_count, poster_url, total) %>% 
  summarize(
    rating_season = mean(Rating, na.rm = TRUE),
    n = n()
  ) %>% 
  group_by(title_id, Season) %>% 
  mutate(
    year = year + Season - 1,
    year = if_else(year > 2020, 2020, year)
  ) %>% 
  ungroup() %>% 
  dplyr::select(
    title_id, code,
    year, 
    season = Season,
    rating_season,
    rating_avg = rating,
    rating_count
  ) %>% 
  left_join(df_title_basics, by = c("code" = "tconst")) %>% 
  ## add missing genres and runtimes
  mutate(
    genres = case_when(
      ## ACTION
      title_id %in% c("Spartacus: Gods of the Arena (2011)") ~ 
        "Action,Adventure,Biography",
      title_id %in% c("Justified (2010)") ~ 
        "Action,Crime,Drama",
      title_id %in% c("Aranyélet (2015)") ~ 
        "Action,Drama,Thriller",
      ## ANIMATION
      title_id %in% c("Attack on Titan (2013)", "Vinland Saga (2019)", "Justice League Unlimited (2004)") ~ 
        "Animation,Action,Adventure",
      title_id %in% c("One Punch Man (2015)") ~ 
        "Animation,Action,Comedy",
      title_id %in% c("Demon Slayer: Kimetsu No Yaiba (2019)", "Archer (2009)") ~ 
        "Animation,Action,Comedy",
      title_id %in% c("Rick and Morty (2013)") ~ 
        "Animation,Adventure,Comedy",
      title_id %in% c("Over the Garden Wall (2014)") ~ 
        "Animation,Adventure,Drama",
      title_id %in% c("The Bugs Bunny Show (1960)", "The Bugs Bunny/Road Runner Hour (1968)") ~ 
        "Animation,Comedy",
      title_id %in% c("Death Note (2006)") ~ 
        "Animation,Crime,Drama",
      title_id %in% c("Erased (2016)") ~ 
        "Animation,Drama,Fantasy",
      ## BIOGRAPHY
      title_id %in% c("SCAM 1992: The Harshad Mehta Story (2020)", "Narcos (2015)") ~ 
        "Biography,Crime,Drama",
      title_id %in% c("Bose: Dead/Alive (2017)") ~ 
        "Biography,History,Mystery",
      ## COMEDY
      title_id %in% c("Monty Python's Flying Circus (1969)", "Fawlty Towers (1975)", "Detectorists (2014)", "Silicon Valley (2014)") ~ 
        "Comedy",
      title_id %in% c("Alfred Hitchcock Presents (1955)") ~ 
        "Comedy,Crime,Drama",
      title_id %in% c("TVF Pitchers (2015)", "Fleabag (2016)", "The Marvelous Mrs. Maisel (2017)", "Louie (2010)", "Horace and Pete (2016)") ~ 
        "Comedy,Drama",
      title_id %in% c("Inside No. 9 (2014)") ~ 
        "Comedy,Drama,Horror",
      title_id %in% c("This Is Us (2016)", "Flames (2018)") ~ 
        "Comedy,Drama,Romance",
      title_id %in% c("The Grand Tour (2016)") ~ 
        "Comedy,TV-Show",
      ## CRIME
      title_id %in% c("Twin Peaks (2017)") ~ 
        "Crime,Drama,Fantasy",
      title_id %in% c("Persona (2018)", "Endeavour (2012)", "Big Little Lies (2017)") ~ 
        "Crime,Drama,Mystery",
      title_id %in% c("Breaking Bad (2008)", "Senke nad Balkanom (2017)", "Gomorrah (2014)", "Mindhunter (2017)", "Mr. Robot (2015)", "Ezel (2009)") ~ 
        "Crime,Drama,Thriller",
      ## DOCUMENTARY
      title_id %in% c("Planet Earth II (2016)", "Blue Planet II (2017)", "Life (2009)", "Chef's Table (2015)") ~ 
        "Documentary",
      title_id %in% c("The Defiant Ones (2017)") ~ 
        "Documentary,Biography,Crime",
      title_id %in% c("The Last Dance (2020)") ~ 
        "Documentary,Biography,History",
      title_id %in% c("The Jinx: The Life and Deaths of Robert Durst (2015)") ~ 
        "Documentary,Crime",
      title_id %in% c("Content Cop (2015)") ~ 
        "Documentary,Comedy,Crime",
      title_id %in% c("The World at War (1973)") ~ 
        "Documentary,History,War",
      ## DRAMA
      title_id %in% c("Succession (2018)") ~ 
        "Drama",
      title_id %in% c("The Twilight Zone (1959)") ~ 
        "Drama,Fantasy,Horror",
      title_id %in% c("The Haunting of Hill House (2018)") ~ 
        "Drama,Horror,Mystery",
      title_id %in% c("Skam (2015)", "Normal People (2020)") ~ 
        "Drama,Romance",
      ## MISC
      title_id %in% c("Mister Rogers' Neighborhood (1968)") ~ 
        "Family,Fantasy,Music",
      title_id %in% c("Queer Eye (2018)") ~
        "Reality-TV",
      TRUE ~ genres
    )
  ) %>% 
  group_by(code) %>% 
  mutate(genre = stringr::word(genres)) %>% 
  arrange(title_id, season)

df_rect <- df_top250 %>% 
  ungroup() %>% 
  summarize(
    year = mean(year, na.rm = TRUE),
    rating = mean(rating_season, na.rm = TRUE)
  )

df_text <- tibble(
  x = c(1954.4, 1954.4, 2020.6, 2020.6, df_rect$year, 1962, 1980),
  y = c(9.7, 0.3, 9.7, 0.3, 4, df_rect$rating, 3.3),
  hjust = c(0, 0, 1, 1, .5, .5, .5),
  vjust = c(0, 1, 0, 1, .1, .1, .5),
  angle = c(0, 0, 0, 0, 270, 0, 0),
  text = c("Old Series<br>High Rating", "Old Series<br>Low Rating",
           "New Series<br>High Rating", "New Series<br>Low Rating",
           glue::glue("<span style='font-family:Overpass;font-size:17pt;'>Mean Year: {round(df_rect$year, 0)}</span>"), glue::glue("<span style='font-family:Overpass;font-size:17pt;'>Mean Rating: {round(df_rect$rating, 2)}</span>"),
           "<b style='font-size:38pt;'>“People are sheep. TV is the shepherd.”</b><br><i style='font-size:22pt;'>Jess C. Scott — Literary Heroin (Gluttony)</i><br><br><br><span style='font-family:Overpass;font-size:14pt;'>Average rating of the Top 250 TV shows as rated by IMDb users per season, shown across years.<br>Bubble size indicates number of votes, bubble color the genre.</span>")
)

ggplot(df_top250, aes(year, rating_season, size = rating_count)) +
  annotate("rect", 
           xmin = -Inf, xmax = df_rect$year,
           ymin = -Inf, ymax = df_rect$rating,
           fill = "#009380") +
  annotate("rect", 
           xmin = df_rect$year, xmax = Inf,
           ymin = -Inf, ymax = df_rect$rating,
           fill = "#EF8900") +
  annotate("rect", 
           xmin = -Inf, xmax = df_rect$year,
           ymin = df_rect$rating, ymax = Inf,
           fill = "#FABA00") +
  annotate("rect", 
           xmin = df_rect$year, xmax = Inf,
           ymin = df_rect$rating, ymax = Inf,
           fill = "#A5037D") +
  #geom_hline(yintercept = df_rect$rating, color = "white", size = 2) +
  #geom_vline(xintercept = df_rect$year, color = "white", size = 2) +
  geom_point(shape = 21, fill = "white", color = "white", stroke = 1) + 
  geom_point(aes(fill = genre, 
                 fill = after_scale(colorspace::darken(fill, .1, space = "HLS"))),
             shape = 21, color = "transparent", stroke = 0, alpha = .8) + 
  #geom_point(shape = 21, color = "#ffffff1A", fill = "transparent", stroke = .1) + 
  geom_richtext(data = df_text, aes(x, y, label = text, 
                                    vjust = vjust, hjust = hjust, angle = angle), 
            inherit.aes = FALSE, color = "white", size = 11,
            family = "Passion One", lineheight = .63,
            fill = NA, label.colour = NA) +
  scale_x_continuous(expand = c(0, 0), limits = c(1954, 2021)) +
  scale_y_continuous(limits = c(0, 10)) +
  scale_color_discrete(guide = "none") +
  scale_fill_discrete(guide = "none") +
  scale_size(range = c(3, 8), guide = "none") +
  labs(caption = "Visualization: Cédric Scherer | Data: IMDb.com (2020–11–01) | #30DayChartChallenge 2021 | Day 17: Pop Culture") +
  theme_void(base_size = 18) +
  theme(
    plot.background = element_rect(fill = "#281F55", color = "#281F55"),
    plot.margin = margin(60, 60, 20, 60),
    plot.caption = element_text(color = "grey80", family = "Overpass", 
                                size = 12, margin = margin(t = 15, b = 0), hjust = .5)
  )

file <- here::here("17_pop_culture", "17_pop_culture.pdf")

ggsave(file, width = 16.5, height = 13, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = file, 
  filenames = glue::glue("{str_remove(file, '.pdf')}.png"),
  format = "png", dpi = 200
)




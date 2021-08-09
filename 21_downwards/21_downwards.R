library(tidyverse)
library(ggtext)
library(gganimate)
library(ragg)
library(pdftools)

register_variant(
  name = "Vision Heavy",
  family = "Vision",
  weight = "heavy"
)

theme_set(theme_minimal(base_family = "Vision"))

theme_update(
  plot.background = element_rect(fill = "grey94", color = "grey94"),
  plot.title.position = "plot",
  plot.title = element_markdown(family = "Vision Heavy", size = 30, 
                                hjust = 0, margin = margin(b = 17)),
  plot.subtitle = element_markdown(family = "Vision Heavy", size = 15, 
                                   color = "grey40", lineheight = 1.3,
                                   hjust = 0, margin = margin(t = 0, b = 60)),
  plot.caption = element_text(size = 12, color = "grey60", hjust = 1,
                              margin = margin(t = 5)),
  axis.title.x = element_text(size = 19, color = "grey40", family = "Vision Heavy",
                              margin = margin(t = 12, b = 5)),
  axis.title.y = element_text(margin = margin(l = 15)),
  axis.text.x = element_text(size = 15, color = "grey40",
                             margin = margin(t = 2)),
  axis.text.y = element_text(size = 18, color = "grey40",
                             margin = margin(r = 5)),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_line(size = 0.6, color = "grey80",
                                    linetype = "dotted"),
  legend.position = "none",
  plot.margin = margin(25, 65, 25, 45))

## http://football-data.co.uk/germanym.php
df_bl <- readr::read_csv(here::here("21_downwards", "D1.csv"))

df_bl_days <-
  df_bl %>%
  mutate(
    Date = lubridate::dmy(Date),
    Match = glue::glue("{HomeTeam} - {AwayTeam}"),
    Result = glue::glue("{FTHG} - {FTAG}"),
    HT = HomeTeam,
    AT = AwayTeam
  ) %>% 
  dplyr::select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HS, AS, Match, Result, HT, AT) %>% 
  gather(Team, Club, -c(Date, FTHG, FTAG, FTR, HS, AS, Match, Result, HT, AT)) %>% 
  mutate(
    Goals = case_when(Team == "HomeTeam" ~ FTHG, 
                      Team == "AwayTeam" ~ FTAG),
    Diff = case_when(Team == "HomeTeam" ~ FTHG - FTAG, 
                     Team == "AwayTeam" ~ FTAG - FTHG),
    Score = case_when(FTR == "D" ~ 1,
                      Team == "HomeTeam" & FTR == "H" ~ 3,
                      Team == "AwayTeam" & FTR == "A" ~ 3,
                      TRUE ~ 0),
    Shots = case_when(Team == "HomeTeam" ~ HS,
                      Team == "AwayTeam" ~ AS),
    Opponent = case_when(Team == "HomeTeam" ~ AT, 
                         Team == "AwayTeam" ~ HT)
  ) %>% 
  dplyr::select(Date, Match, Result, Club, Score, Goals, Shots, Diff, Opponent) %>% 
  group_by(Club) %>% 
  arrange(Date) %>% 
  mutate(
    Day = row_number(),
    Score_rel = cumsum(Score) / Day,
    Diff_sum = cumsum(Diff),
    Goals_sum = cumsum(Goals)
  ) %>%
  ungroup() %>% 
  group_by(Day) %>% 
  arrange(desc(Score_rel), desc(Diff_sum), desc(Goals_sum)) %>%
  group_by(Club) %>% 
  mutate(max_score = max(Score_rel)) %>% 
  ungroup() %>% 
  mutate(Club = fct_reorder(Club, max_score)) %>% 
  arrange(Day, Club, Score_rel)


##### TOP CLUBS #############################################################
df_bl_days_shift <-
  df_bl_days %>% 
  mutate(
    Day = case_when(
      Club == "Bayern Munich" ~ Day + .025,
      Club == "RB Leipzig" ~ Day - .025,
      TRUE ~ as.numeric(Day)
    ),
    Score_rel = case_when(
      Club == "Bayern Munich" ~ Score_rel + .006,
      Club == "RB Leipzig" ~ Score_rel - .006,
      TRUE ~ Score_rel
    )
  )

df_bl_days_diff <-
  df_bl_days %>% 
  filter(Club %in% c("Bayern Munich", "RB Leipzig")) %>% 
  group_by(Day) %>% 
  summarize(
    ymin = min(Score_rel),
    ymax = max(Score_rel),
    Club = if_else(Score_rel[which(Club == "Bayern Munich")] > Score_rel[which(Club == "RB Leipzig")], "Bayern Munich", "RB Leipzig")
  ) %>% 
  group_by(Club) %>% 
  filter(Day < max(df_bl_days$Day)) %>% 
  ungroup



## Top Clubs and Schalke's way down
g <- df_bl_days_shift %>% 
  ggplot(aes(Day, Score_rel, group = Club)) +
  ## other teams
  geom_step(
    data = filter(df_bl_days_shift, !Club %in% c("Bayern Munich", "RB Leipzig", "Schalke 04")),
    color = "grey60", size = .8, alpha = .3, show.legend = FALSE
  ) + 
  geom_point(
    data = filter(df_bl_days_shift, !Club %in% c("Bayern Munich", "RB Leipzig", "Schalke 04")),
    size = 2.3, color = "grey80", shape = 21, fill = "grey94", stroke = .4
  ) +
  ## Top Clubs
  geom_rect(
    data = df_bl_days_diff,
    aes(xmin = Day, xmax = Day + 1, 
        ymin = ymin, ymax = ymax,
        fill = Club,
        fill = after_scale(colorspace::lighten(fill, .5))),
    inherit.aes = FALSE, color = NA, alpha = .6
  ) +
  geom_step(
    data = filter(df_bl_days_shift, Club %in% c("Bayern Munich", "RB Leipzig", "Schalke 04")),
    aes(color = Club,
        color = after_scale(colorspace::lighten(color, .2, space = "HLS"))),
    size = 1.4, show.legend = FALSE
  ) + 
  geom_point(
    data = filter(df_bl_days_shift, Club %in% c("Bayern Munich", "RB Leipzig", "Schalke 04")),
    aes(color = Club), size = 3
  ) +
  geom_text(
    data = filter(df_bl_days, Club %in% c("Bayern Munich", "RB Leipzig", "Schalke 04"), Day > 33),
    aes(color = Club, label = sprintf("% 1.2f", Score_rel)),
    size = 4.5, family = "Vision", fontface = "bold",
    hjust = 0, nudge_x = .25, show.legend = FALSE
  ) +
  annotate(
    "text", x = 5, y = 2.73, label = "RB Leipzig",
    color = "#000060", size = 8.5, family = "Bangers"
  ) +
  annotate(
    "text", x = 19.5, y = 2.55, label = "FC Bayern Munich",
    color = "#dc052d", size = 8.5, family = "Bangers"
  ) +
  annotate(
    "text", x = 25, y = 0.26, label = "FC Schalke 04",
    color = "#364e67", size = 6.5, family = "Bangers"
  ) +
  annotate(
    "text", x = 3.1, y = 3, label = "FC Augsburg and\nTSG 1899 Hoffenheim",
    color = "grey65", size = 4.7, family = "Vision Heavy", hjust = 0,
    lineheight = .78
  ) +
  annotate(
    "text", x = 12, y = 2.48, label = "Bayer 04\nLeverkusen",
    color = "grey65", size = 4.7, family = "Vision Heavy", lineheight = .78
  ) +
  annotate(
    "text", x = .8, y = 3.03, label = "Points per\nMatch\n",
    color = "grey40", size = 6, family = "Vision Heavy", lineheight = .78, vjust = 0
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(.7, NA), breaks = 1:34, expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 3, by = 1), expand = c(.01, .01), 
                     limits = c(NA, 3.1)) +
  scale_color_manual(values = c("#364e67", "#dc052d", "#000060"), name = NULL) +
  scale_fill_manual(values = c("#dc052d", "#000060"), guide = FALSE) +
  guides(color = guide_legend(override.aes = list(size = 7))) +
  labs(x = "Match Day", y = "", 
       title = "Bayern Munich's Road to the 31<sup>st</sup> Championship — and Schalke's Way Down",
       subtitle = "<b style='color:#dc052d;'>FC Bayern Munich</b> is the champion of the Bundesliga season 2020/21 with incredible 2.29 points per match, more than 0.3 points more than<br>the best chasers. <b style='color:#000060;'>RB Leipzig</b> (1.91 points per match) was getting close several times but has lost the trace again in the last matches.<br><b style='color:#364e67;'>FC Schalke 04</b>, on the other hand, played one of their worst seasons ever scoring only 0.47 points per match.",
       caption = "Visualization: Cédric Scherer") +
  theme(plot.subtitle = element_markdown(family = "Vision"),
        plot.title = element_markdown(size = 26))

ggsave(here::here("21_downwards", "21_downwards.pdf"), g, 
       width = 14, height = 9.5, device = cairo_pdf)

pdf_convert(pdf = here::here("21_downwards", "21_downwards.pdf"), 
            filenames = here::here("21_downwards", "21_downward.png"),
            format = "png", dpi = 500)




## Top Clubs — static plot without Schalke
g <- df_bl_days_shift %>% 
  ggplot(aes(Day, Score_rel, group = Club)) +
  ## other teams
  geom_step(
    data = filter(df_bl_days_shift, !Club %in% c("Bayern Munich", "RB Leipzig")),
    color = "grey60", size = .8, alpha = .3, show.legend = FALSE
  ) + 
  geom_point(
    data = filter(df_bl_days_shift, !Club %in% c("Bayern Munich", "RB Leipzig")),
    size = 2.3, color = "grey80", shape = 21, fill = "grey94", stroke = .4
  ) +
  ## Top Clubs
  geom_rect(
    data = df_bl_days_diff,
    aes(xmin = Day, xmax = Day + 1, 
        ymin = ymin, ymax = ymax,
        fill = Club,
        fill = after_scale(colorspace::lighten(fill, .5))),
    inherit.aes = FALSE, color = NA, alpha = .6
  ) +
  geom_step(
    data = filter(df_bl_days_shift, Club %in% c("Bayern Munich", "RB Leipzig")),
    aes(color = Club,
        color = after_scale(colorspace::lighten(color, .3, space = "HLS"))),
    size = 1.4, show.legend = FALSE
  ) + 
  geom_point(
    data = filter(df_bl_days_shift, Club %in% c("Bayern Munich", "RB Leipzig")),
    aes(color = Club), size = 3
  ) +
  geom_text(
    data = filter(df_bl_days, Club %in% c("Bayern Munich", "RB Leipzig"), Day > 33),
    aes(color = Club, label = sprintf("% 1.2f", Score_rel)),
    size = 4.5, family = "Vision", fontface = "bold",
    hjust = 0, nudge_x = .25, show.legend = FALSE
  ) +
  annotate(
    "text", x = 5, y = 2.73, label = "RB Leipzig",
    color = "#294e88", size = 8.5, family = "Bangers"
  ) +
  annotate(
    "text", x = 19.5, y = 2.55, label = "FC Bayern Munich",
    color = "#dc052d", size = 8.5, family = "Bangers"
  ) +
  annotate(
    "text", x = 25, y = 0.28, label = "FC Schalke 04",
    color = "grey65", size = 4.7, family = "Vision Heavy"
  ) +
  annotate(
    "text", x = 3.1, y = 3, label = "FC Augsburg and\nTSG 1899 Hoffenheim",
    color = "grey65", size = 4.7, family = "Vision Heavy", hjust = 0,
    lineheight = .78
  ) +
  annotate(
    "text", x = 12, y = 2.48, label = "Bayer 04\nLeverkusen",
    color = "grey65", size = 4.7, family = "Vision Heavy", lineheight = .78
  ) +
  annotate(
    "text", x = .8, y = 3.03, label = "Points per\nMatch\n",
    color = "grey40", size = 6, family = "Vision Heavy", lineheight = .78, vjust = 0
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(.7, NA), breaks = 1:34, expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 3, by = 1), expand = c(.01, .01), 
                     limits = c(NA, 3.1)) +
  scale_color_manual(values = c("#dc052d", "#294e88"), name = NULL) +
  scale_fill_manual(values = c("#dc052d", "#294e88"), guide = FALSE) +
  guides(color = guide_legend(override.aes = list(size = 7))) +
  labs(x = "Match Day", y = "", 
       title = "Bayern Munich's Road to the 31<sup>st</sup> Championship", 
       subtitle = "<b style='color:#dc052d;'>FC Bayern Munich</b> is the champion of the Bundesliga season 2020/21 with incredible 2.29 points per match, more than 0.3 points more than<br>the best chasers. <b style='color:#294e88;'>RB Leipzig</b> (1.91 points per match) was getting close several times but has lost the trace again in the last matches.", 
       caption = "Visualization: Cédric Scherer") +
  theme(plot.subtitle = element_markdown(family = "Vision"))

ggsave(here::here("21_downwards", "21_downwards_not_really_umpf.pdf"), g, 
       width = 14, height = 9, device = cairo_pdf)



pdf_convert(pdf = here::here("21_downwards", "21_downwards_not_really_umpf.pdf"), 
            filenames = here::here("21_downwards", "21_downwards_not_really_umpf.png"),
            format = "png", dpi = 500)

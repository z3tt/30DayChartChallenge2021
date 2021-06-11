## source: https://www.businessofapps.com/data/spotify-statistics/
## viz: https://1z1euk35x7oy36s8we4dr6lo-wpengine.netdna-ssl.com/wp-content/uploads/2018/12/spotify-users-and-subs.png

library(tidyverse)
library(ggtext)
library(ragg)

register_variant(
  name = "Vision Heavy",
  family = "Vision",
  weight = "heavy"
)

df <- 
  tibble(
    quartal = rep(1:4, 6),
    year = rep(2015:2020, each = 4),
    subscribers = c(18, 22, 24, 28, 30, 36, 40, 48, 52, 59, 62, 71, 75, 
                    83, 87, 96, 100, 108, 113, 124, 130, 138, 144, 155),
    users = c(68, 77, 82, 91, 96, 104, 113, 123, 131, 138, 150, 160, 170, 
              180, 191, 207, 217, 232, 248, 271, 286, 299, 320, 345)
  ) %>% 
  mutate(
    step = paste0(year, "-Q", quartal),
    non_subscribers = users - subscribers
  )
  
offset <- .15

df_rect <-
  tibble(
    year = 2015:2020,
    xmin = c(125, 175, 225, 275, 325, 355)
  )

ggplot(df, aes(quartal, users)) +
  geom_col(width = .9, fill = "#525252") +
  geom_linerange(aes(quartal - offset, ymin = 0, ymax = subscribers), 
                 color = "#1DB954", alpha = .8, size = 1.4) +
  geom_linerange(aes(quartal + offset, ymin = 0, ymax = non_subscribers), 
                 color = "white", alpha = .8, size = 1.4) +
  geom_point(aes(quartal - offset, subscribers), 
             color = "#1DB954", size = 4) +
  geom_point(aes(quartal + offset, non_subscribers), 
             color = "white", size = 4.5) +
  geom_text(aes(quartal - offset, subscribers, label = subscribers), 
            color = "#1DB954", size = 5.2, family = "Vision", alpha = .85, nudge_y = 6) +
  geom_text(aes(quartal + offset, non_subscribers, label = non_subscribers), 
            color = "white", size = 5.2, family = "Vision", alpha = .85, nudge_y = 6) +
  geom_text(aes(quartal, users, label = users), 
            color = "black", size = 6, family = "Vision Heavy", nudge_y = -4) +
  geom_rect(data = df_rect, aes(xmin = -Inf, xmax = Inf, ymin = xmin, ymax = Inf),
            inherit.aes = FALSE, fill = "black") +
  facet_wrap(~ year, nrow = 1, strip.position = "bottom") +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(labels = function(x) paste0("Q", x)) +
  scale_y_continuous(breaks = seq(0, 350, by = 50), limits = c(0, 360), position = "right", 
                     labels = function(x) ifelse(x == 350, paste(x, "Millions"), paste(x))) +
  labs(x = NULL, y = NULL, 
       title = "Spotify  <span style='color:#6D6D6D;'>Users</span> • <span style='color:#1DB954;'>Subscribers</span> • <span style='color:#f0f0f0;'>Non-Subscribers</span> in Millions",
       caption = "<span style='color:#f0f0f0;'>Visualization: Cédric Scherer</span>  •  <span style='color:#1DB954;'>Data: Spotify</span>  •  <span style='color:#6D6D6D;'>#30DayChartChallenge | Day 20: Upwards</span>") +
  theme_minimal(base_size = 20, base_family = "Vision") +
  theme(
    text = element_text(color = "grey90"),
    axis.text.x = element_text(color = "grey45", margin = margin(t = 7, b = 5)),
    axis.text.y = element_text(color = "grey45", margin = margin(l = 7)),
    plot.title = element_markdown(family = "Vision Heavy", face = "italic", 
                                  color = "grey70", size = 44, hjust = .5,
                                  margin = margin(t = 0, b = 10)),
    plot.title.position = "plot",
    plot.caption = element_markdown(family = "Vision Heavy", size = 14, hjust = 0,
                                    color = "grey35", margin = margin(t = 35, b = 0)),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.spacing = unit(.4, "lines"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey15", size = .3),
    strip.background = element_rect(fill = "grey10", color = "transparent"),
    strip.text = element_text(color = "grey70", size = 25, family = "Vision Heavy",
                              margin = margin(rep(5, 4))), 
    strip.placement = "outside",
    plot.margin = margin(45, 45, 30, 45)
  ) 

file <- here::here("20_upwards", "20_upwards.pdf")

ggsave(file, width = 20, height = 14, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = file, 
  filenames = glue::glue("{str_remove(file, '.pdf')}.png"),
  format = "png", dpi = 450
)


## REMIX VERSION

ggplot(df, aes(quartal, users)) +
  geom_col(width = .9, fill = "#525252") +
  geom_linerange(aes(quartal - offset, ymin = 0, ymax = subscribers), 
                 color = "#1DB954", alpha = .8, size = 1.4) +
  geom_linerange(aes(quartal + offset, ymin = 0, ymax = non_subscribers), 
                 color = "white", alpha = .8, size = 1.4) +
  geom_point(aes(quartal - offset, subscribers), 
             color = "#1DB954", size = 4) +
  geom_point(aes(quartal + offset, non_subscribers), 
             color = "white", size = 4.5) +
  geom_text(aes(quartal - offset, subscribers, label = subscribers), 
            color = "#1DB954", size = 7, family = "Vision Heavy", nudge_y = 7) +
  geom_text(aes(quartal + offset, non_subscribers, label = non_subscribers), 
            color = "white", size = 7, family = "Vision Heavy", nudge_y = 7) +
  geom_text(aes(quartal, users, label = users), 
            color = "#525252", size = 9, family = "Vision Heavy", nudge_y = 6) +
  geom_rect(data = df_rect, aes(xmin = -Inf, xmax = Inf, ymin = xmin, ymax = Inf),
            inherit.aes = FALSE, fill = "black") +
  facet_wrap(~ year, nrow = 1, strip.position = "bottom") +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(labels = function(x) paste0("Q", x)) +
  scale_y_continuous(breaks = seq(0, 350, by = 50), limits = c(0, 360), position = "right", 
                     labels = function(x) ifelse(x == 350, paste(x, "Millions"), paste(x))) +
  labs(x = NULL, y = NULL, 
       title = "Spotify  <span style='color:#6D6D6D;'>Users</span> • <span style='color:#1DB954;'>Subscribers</span> • <span style='color:#f0f0f0;'>Non-Subscribers</span> in Millions",
       caption = "<span style='color:#f0f0f0;'>Visualization: Cédric Scherer</span>  •  <span style='color:#1DB954;'>Data: Spotify</span>  •  <span style='color:#6D6D6D;'>#30DayChartChallenge | Day 20: Upwards</span>") +
  theme_minimal(base_size = 25, base_family = "Vision") +
  theme(
    text = element_text(color = "grey90"),
    axis.text.x = element_text(color = "grey45", margin = margin(t = 7, b = 5)),
    axis.text.y = element_text(color = "grey45", margin = margin(l = 7)),
    plot.title = element_markdown(family = "Vision Heavy", face = "italic", 
                                  color = "grey70", size = 46, hjust = .5,
                                  margin = margin(t = 0, b = 10)),
    plot.title.position = "plot",
    plot.caption = element_markdown(family = "Vision Heavy", size = 14, hjust = 0,
                                    color = "grey35", margin = margin(t = 35, b = 0)),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.spacing = unit(.4, "lines"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey20", size = .4),
    strip.background = element_rect(fill = "grey10", color = "transparent"),
    strip.text = element_text(color = "grey70", size = 28, family = "Vision Heavy",
                              margin = margin(rep(8, 4))), 
    strip.placement = "outside",
    plot.margin = margin(45, 45, 30, 45)
  ) 

file <- here::here("20_upwards", "20_upwards_alt.pdf")

ggsave(file, width = 24, height = 14, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = file, 
  filenames = glue::glue("{str_remove(file, '.pdf')}.png"),
  format = "png", dpi = 450
)


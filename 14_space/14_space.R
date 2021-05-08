## packages
library(tidyverse)
library(ggblur)
library(ggtext)
library(colorspace)
library(ragg)
library(pdftools)

theme_set(theme_void())

theme_update(
  plot.background = element_rect(fill = "black"),
  plot.margin = margin(80, 180, 40, 200),
  plot.caption = element_text(family = "InputSansCondensed", color = "grey50", 
                              size = 18,  hjust = .5, lineheight = 1.1, 
                              margin = margin(t = 90)),
  plot.caption.position = "plot"
)

df_astro <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

df_missions <-
  df_astro %>% 
  group_by(name) %>% 
  summarize(
    hours = sum(hours_mission),
    year = min(year_of_mission),
    max_year = max(year_of_mission)
  ) %>% 
  ungroup() %>% 
  #mutate(year = -year) %>% 
  arrange(year) %>% 
  mutate(id = row_number())

df_labs <-
  df_missions %>% 
  filter(year %in% c(1961, 197:201*10, 2019)) %>% 
  group_by(year) %>% 
  filter(id == min(id))

df_text <-
  df_missions %>% 
  arrange(-hours) %>% 
  slice(c(1:4, 8, 11, 28, 29, 60)) %>% 
  mutate(
    first_name = str_remove(name, ".*, "),
    last_name = str_remove(name, "(?<=),.*"),
    name_short = glue::glue("{str_sub(first_name, 0, 1)}. {last_name}"),
    era = glue::glue("{year}–{max_year}"),
    label = glue::glue("<b style='font-size:18pt;'>{name_short}</b><br><span style='font-size:11pt;'>({era})</span><br><i style='color:#646464;'>{format(floor(hours), big.mark = ',')} hours ~ {round(hours / 8760, 1)} years</i>"),
    off = c(10, 8.5, 7.5, 7, 5, 4.2, 3.2, 2.6, 2.5),
    id = id + off
  ) 

## colorful blurs
p <-
  df_missions %>% 
  ggplot(aes(
    x = id, 
    y = hours + 5, 
    color = year, 
    fill = year
  )) +
  ## tick marks years
  geom_text(
    data = df_labs,
    aes(y = -1.5, label = "|"),
    family = "Changa",
    fontface = "bold",
    size = 4,
    vjust = 1
  ) +
  ## sparkling points
  geom_point_blur(
    aes(
      y = hours + 5, 
      size = hours,
      blur_size = hours,
      color = year,
      color = after_scale(lighten(color, .4, space = "HLS"))
    ),
    blur_steps = 150
  ) +
  geom_linerange(
    aes(
      x = id,
      xmax = id,
      ymin = 0,
      ymax = hours + 5,
      color = year,
      color = after_scale(desaturate(color, .3)),
      alpha = hours
    ),
    size = .25
  ) +
  ## triangles
  geom_point(
    aes(y = 0), 
    shape = 17, 
    size = .3
  ) +
  ## points
  geom_point(
    aes(
      y = hours + 5, 
      size = hours
    )
  ) +
  ## labels years
  geom_richtext(
    data = df_labs,
    aes(y = 0, label = glue::glue("<br>{year}"),
        color = year, color = after_scale(colorspace::lighten(color, .25))),
    size = 8,
    family = "InputSansCondensed",
    fontface = "bold",
    fill = NA,
    label.color = NA,
    vjust = .85
  ) +
  ## title
  geom_textbox(
    data = tibble(
      id = 0, hours = 14600,
      label = "<span style='font-size:52pt;line-height:2pt;'>Travelling to Outer Space</span><br><br><span style='font-family:InputSansCondensed;'>Cumulative time in outer space for all 565 cosmonauts and astronauts who participated in space missions between April 23, 1961 and January 15, 2020, sorted by the year of their first mission.</span>"
    ),
    aes(
      x = id,
      y = hours,
      label = label
    ),
    inherit.aes = F,
    size = 8,
    family = "Neutraface 2 Display Titling",
    color = "grey70",
    lineheight = 1.7,
    width = unit(6.2, "inch"),
    hjust = .2,
    vjust = 0,
    fill = NA,
    box.colour = NA
  ) +
  ## labels astronauts
  geom_richtext(
    data = df_text,
    aes(label = label),
    size = 3.2,
    family = "InputSansCondensed",
    lineheight = 1.4,
    hjust = 0,
    nudge_x = 3.5,
    fill = NA,
    label.color = NA
  )  +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(.002, .002)) + 
  #scico::scale_color_scico(palette = "hawaii", guide = FALSE) +
  #scico::scale_fill_scico(palette = "hawaii", guide = FALSE) +
  scale_color_gradientn(colors = viridis::turbo(100), guide = FALSE) +
  scale_fill_gradientn(colors = viridis::turbo(100), guide = FALSE) +
  scale_blur_size_continuous(range = c(1, 15), guide = FALSE) +
  scale_size(range = c(.001, 3), guide = FALSE) +
  scale_alpha(range = c(.33, 1), guide = FALSE) +
  labs(caption = "Visualization by Cédric Scherer  •  #30DayChartChallenge 2021 | Day 14: Space\nData by Stavnichuk & Corlett 2020 (DOI: 10.17632/86tsnnbv2w.1)")

ggsave(here::here("14_space", "14_space_turbo.pdf"), 
       width = 21, height = 35, device = cairo_pdf, limitsize = FALSE)

## mixed colors (turbo)
p <-
  df_missions %>% 
  ggplot(aes(
    x = id, 
    y = hours + 5, 
    color = hours, 
    fill = hours
  )) +
  ## tick marks years
  geom_text(
    data = df_labs,
    aes(y = -1.5, label = "|"),
    family = "Changa",
    color = "grey65",
    fontface = "bold",
    size = 4,
    vjust = 1
  ) +
  ## sparkling points
  geom_point_blur(
    aes(
      y = hours + 5, 
      size = hours,
      blur_size = hours,
      color = hours,
      color = after_scale(lighten(color, .4, space = "HLS"))
    ),
    blur_steps = 150
  ) +
  geom_linerange(
    aes(
      x = id,
      xmax = id,
      ymin = 0,
      ymax = hours + 5,
      color = hours,
      color = after_scale(desaturate(color, .3))
    ),
    size = .25
  ) +
  ## triangles
  geom_point(
    aes(y = 0), 
    shape = 17, 
    size = .3
  ) +
  ## points
  geom_point(
    aes(
      y = hours + 5, 
      size = hours
    )
  ) +
  ## labels years
  geom_richtext(
    data = df_labs,
    aes(y = 0, label = glue::glue("<br>{year}")),
    color = "grey85",
    size = 8,
    family = "InputSansCondensed",
    fontface = "bold",
    fill = NA,
    label.color = NA,
    vjust = .85
  ) +
  ## title
  geom_textbox(
    data = tibble(
      id = 0, hours = 14600,
      label = "<span style='font-size:52pt;line-height:2pt;'>Travelling to Outer Space</span><br><br><span style='font-family:InputSansCondensed;'>Cumulative time in outer space for all 565 cosmonauts and astronauts who participated in space missions between April 23, 1961 and January 15, 2020, sorted by the year of their first mission.</span>"
    ),
    aes(
      x = id,
      y = hours,
      label = label
    ),
    inherit.aes = F,
    size = 8,
    family = "Neutraface 2 Display Titling",
    color = "grey70",
    lineheight = 1.7,
    width = unit(6.2, "inch"),
    hjust = .24,
    vjust = 0,
    fill = NA,
    box.colour = NA
  ) +
  ## labels astronauts
  geom_richtext(
    data = df_text,
    aes(label = label),
    size = 3.2,
    family = "InputSansCondensed",
    lineheight = 1.4,
    hjust = 0,
    nudge_x = 3.5,
    fill = NA,
    label.color = NA
  )  +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(.002, .002)) + 
  scale_color_gradientn(colors = viridis::turbo(100, end = .9), guide = FALSE) +
  scale_fill_gradientn(colors = viridis::turbo(100, end = .9), guide = FALSE) +
  scale_blur_size_continuous(range = c(1, 15), guide = FALSE) +
  scale_size(range = c(.001, 3), guide = FALSE) +
  scale_alpha(range = c(.33, 1), guide = FALSE) +
  labs(caption = "Visualization by Cédric Scherer  •  #30DayChartChallenge 2021 | Day 14: Space\nData by Stavnichuk & Corlett 2020 (DOI: 10.17632/86tsnnbv2w.1)")

ggsave(here::here("14_space", "14_space_turbo_mixed.pdf"), 
       width = 21, height = 35, device = cairo_pdf, limitsize = FALSE)



## mixed colors
p <-
  df_missions %>% 
  ggplot(aes(
    x = id, 
    y = hours + 5, 
    color = hours, 
    fill = hours
  )) +
  ## tick marks years
  geom_text(
    data = df_labs,
    aes(y = -1.5, label = "|"),
    family = "Changa",
    color = "grey55",
    fontface = "bold",
    size = 4,
    vjust = 1
  ) +
  ## sparkling points
  geom_point_blur(
    aes(
      y = hours + 5, 
      size = hours,
      blur_size = hours,
      color = hours,
      color = after_scale(lighten(color, .4, space = "HLS"))
    ),
    blur_steps = 150
  ) +
  geom_linerange(
    aes(
      x = id,
      xmax = id,
      ymin = 0,
      ymax = hours + 5,
      color = hours,
      color = after_scale(desaturate(color, .3))
    ),
    size = .25
  ) +
  ## triangles
  geom_point(
    aes(y = 0), 
    shape = 17, 
    size = .3
  ) +
  ## points
  geom_point(
    aes(
      y = hours + 5, 
      size = hours
    )
  ) +
  ## labels years
  geom_richtext(
    data = df_labs,
    aes(y = 0, label = glue::glue("<br>{year}")),
    color = "grey55",
    size = 8,
    family = "InputSansCondensed",
    fontface = "bold",
    fill = NA,
    label.color = NA,
    vjust = .85
  ) +
  ## title
  geom_textbox(
    data = tibble(
      id = 0, hours = 14600,
      label = "<span style='font-size:52pt;line-height:2pt;'>Travelling to Outer Space</span><br><br><span style='font-family:InputSansCondensed;'>Cumulative time in outer space for all 565 cosmonauts and astronauts who participated in space missions between April 23, 1961 and January 15, 2020, sorted by the year of their first mission.</span>"
    ),
    aes(
      x = id,
      y = hours,
      label = label
    ),
    inherit.aes = F,
    size = 8,
    family = "Neutraface 2 Display Titling",
    color = "grey70",
    lineheight = 1.7,
    width = unit(6.2, "inch"),
    hjust = .24,
    vjust = 0,
    fill = NA,
    box.colour = NA
  ) +
  ## labels astronauts
  geom_richtext(
    data = df_text,
    aes(label = label),
    size = 3.2,
    family = "InputSansCondensed",
    lineheight = 1.4,
    hjust = 0,
    nudge_x = 3.5,
    fill = NA,
    label.color = NA
  )  +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(.002, .002)) + 
  scale_color_gradientn(colors = viridis::mako(100, begin = .1), guide = FALSE) +
  scale_fill_gradientn(colors = viridis::mako(100, begin = .1), guide = FALSE) +
  scale_blur_size_continuous(range = c(1, 15), guide = FALSE) +
  scale_size(range = c(.001, 3), guide = FALSE) +
  scale_alpha(range = c(.33, 1), guide = FALSE) +
  labs(caption = "Visualization by Cédric Scherer  •  #30DayChartChallenge 2021 | Day 14: Space\nData by Stavnichuk & Corlett 2020 (DOI: 10.17632/86tsnnbv2w.1)")

ggsave(here::here("14_space", "14_space_mako_mixed.pdf"), 
       width = 21, height = 35, device = cairo_pdf, limitsize = FALSE)

## monochrome
p <-
  df_missions %>% 
  ggplot(aes(
    x = id, 
    y = hours + 5, 
    color = hours, 
    fill = hours
  )) +
  ## tick marks years
  geom_text(
    data = df_labs,
    aes(y = -1.5, label = "|"),
    family = "Changa",
    fontface = "bold",
    color = "grey65",
    size = 4,
    vjust = 1
  ) +
  ## sparkling points
  geom_point_blur(
    aes(
      y = hours + 5, 
      size = hours,
      blur_size = hours,
      color = hours,
      color = after_scale(lighten(color, .4, space = "HLS"))
    ),
    blur_steps = 150
  ) +
  geom_linerange(
    aes(
      x = id,
      xmax = id,
      ymin = 0,
      ymax = hours + 5,
      color = hours,
      color = after_scale(desaturate(color, .1))
    ),
    size = .25
  ) +
  ## triangles
  geom_point(
    aes(y = 0), 
    shape = 17, 
    size = .4
  ) +
  ## points
  geom_point(
    aes(
      y = hours + 5, 
      size = hours
    )
  ) +
  ## labels years
  geom_richtext(
    data = df_labs,
    aes(y = 0, label = glue::glue("<br>{year}")),
    size = 8,
    color = "grey65",
    family = "InputSansCondensed",
    fontface = "bold",
    fill = NA,
    label.color = NA,
    vjust = .85
  ) +
  ## title
  geom_textbox(
    data = tibble(
      id = 0, hours = 14600,
      label = "<span style='font-size:52pt;line-height:2pt;'>Travelling to Outer Space</span><br><br><span style='font-family:InputSansCondensed;'>Cumulative time in outer space for all 565 cosmonauts and astronauts who participated in space missions between April 23, 1961 and January 15, 2020, sorted by the year of their first mission.</span>"
    ),
    aes(
      x = id,
      y = hours,
      label = label
    ),
    inherit.aes = F,
    size = 8,
    family = "Neutraface 2 Display Titling",
    color = "grey70",
    lineheight = 1.7,
    width = unit(6.2, "inch"),
    hjust = .2,
    vjust = 0,
    fill = NA,
    box.colour = NA
  ) +
  ## labels astronauts
  geom_richtext(
    data = df_text,
    aes(label = label),
    size = 3.2,
    family = "InputSansCondensed",
    lineheight = 1.4,
    hjust = 0,
    nudge_x = 3.5,
    fill = NA,
    label.color = NA
  )  +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(.002, .002)) + 
  scale_color_gradient(low = "grey20", high = "white", guide = FALSE) +
  scale_fill_gradient(low = "grey20", high = "white", guide = FALSE) +
  scale_blur_size_continuous(range = c(1, 15), guide = FALSE) +
  scale_size(range = c(.001, 3), guide = FALSE) +
  scale_alpha(range = c(.33, 1), guide = FALSE) +
  labs(caption = "Visualization by Cédric Scherer  •  #30DayChartChallenge 2021 | Day 14: Space\nData by Stavnichuk & Corlett 2020 (DOI: 10.17632/86tsnnbv2w.1)")

ggsave(here::here("14_space", "14_space_mono.pdf"), 
       width = 23, height = 30, device = cairo_pdf, limitsize = FALSE)

## convert PDFs to PNGs
pdfs <- list.files(here::here("14_space"), pattern = ".*pdf", full.names = TRUE)
for(pdf in pdfs) {
  pdf_convert(pdf = glue::glue("{pdf}"), 
              filenames = glue::glue("{str_remove(pdf, '.pdf')}.png"),
              format = "png", dpi = 200)
}

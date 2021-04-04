library(tidyverse)
library(zoo)
library(colorspace)
library(ggfx)
library(ggtext)
library(ragg)

df <- 
  readr::read_csv("https://raw.githubusercontent.com/Z3tt/30DayChartChallenge/main/04_magical/wizard-witch.csv", skip = 2) %>% 
  janitor::clean_names() %>% 
  pivot_longer(cols = -monat, names_to = "query", values_to = "interest") %>% 
  mutate(
    query = str_replace(query, "_weltweit", ""),
    query = fct_rev(query),
    monat = as.Date(paste(monat, "-01", sep = ""))
  ) %>% 
  group_by(query) %>% 
  mutate(roll = rollmean(interest, 24, align = "right", fill = NA)) %>% 
  filter(!is.na(roll)) 

df_ribbon <-
  df %>% 
  group_by(monat) %>% 
  summarize(
    min = min(roll),
    max = max(roll),
    diff = max - min,
    query = max == roll
  ) %>% 
  slice(1) %>% 
  mutate(
    query = if_else(isTRUE(query), "wizard", "witch"),
    query = fct_rev(query)
  )

cols <- c("#a34fde", "#fbc200")

ggplot(df, aes(monat, roll, color = query)) +
  geom_ribbon(
    data = df_ribbon,
    aes(x = monat, ymin = max, ymax = Inf),
    fill = "grey5", color = "grey5", size = 5, inherit.aes = FALSE
  ) +
  geom_ribbon(
    data = df_ribbon,
    aes(x = monat, ymin = min, ymax = max, color = query,
        fill = after_scale(darken(desaturate(color, .1), .4, space = "HLS"))),
    alpha = .7, inherit.aes = FALSE
  ) +
  with_blur(
    geom_line(data = filter(df, query == "witch"), 
              color = cols[2], size = 2.5),
    colour = lighten(cols[2], .1), sigma = 3
  ) +
  with_blur(
    geom_line(data = filter(df, query == "wizard"), 
              color = cols[1], size = 2.5),
    colour = lighten(cols[1], .1), sigma = 3
  ) +
  geom_line(size = 1.5) +
  geom_richtext(
    aes(x = as.Date("2013-06-01"), y = 45, 
        label = "Google Search Trends of<br><span style='font-family:cinzel;'><b style='color:#a34fde;font-size:30pt;'>Wizard</b> & <b style='color:#fbc200;font-size:30pt;'>Witch</b></span>"),
    color = "grey80", size = 7, lineheight = 1.5, family = "Work Sans",
    stat = "unique", fill = NA, label.color = NA
  ) +
  scale_x_date(date_breaks = "year", date_labels = "%Y", expand = c(0, 0)) +
  scale_color_manual(values = cols) +
  labs(caption = "Visualization: Cédric Scherer  |  Data: Global Google Search Trends (derived 2021–04–02) as rolling average  •  #30DayChartChallenge 2021  |  Day 4: Magical") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "transparent", color = "transparent"),
    plot.background = element_rect(fill = "grey5"),
    axis.text.x = element_text(family = "Cinzel", color = "grey80",
                               size = 10, face = "bold", margin = margin(t = 6)),
    panel.grid.major.x = element_line(color = "grey50", linetype = "13", size = .4),
    plot.margin = margin(15, 30, 10, 30),
    plot.caption = element_text(family = "Work Sans", color = "grey50", size = 8,
                                hjust = .5, margin = margin(t = 30, b = 0)),
    legend.position = "none"
  )

file <- here::here("04_magical", "04_magical.pdf")

ggsave(file, width = 12, height = 8, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = file, 
  filenames = glue::glue("{str_remove(file, '.pdf')}.png"),
  format = "png", dpi = 200
)

## packages
library(tidyverse)
library(geofacet)
library(treemapify)
library(ggtext)
library(colorspace)
library(ragg)
library(cowplot)
library(pdftools)

theme_set(theme_void(base_family = "Playfair Display"))

theme_update(
  plot.title = element_textbox(hjust = 0, face = "bold", color = "grey35", 
                               lineheight = 1.2,
                               size = 27, margin = margin(b = 12, t = 0),
                               width = unit(5.5, "inches")),
  plot.caption = element_textbox(color = "grey30", size = 14, hjust = 0,
                                 lineheight = 1.4,
                                 margin = margin(b = 0, t = -5),
                                 width = unit(5.5, "inches")),
  strip.text = element_blank(),
  panel.spacing = unit(.25, "lines"),
  plot.margin = margin(rep(20, 4)),
  plot.background = element_rect(color = "white", fill = "white"),
  legend.position = "none"
)

## data
df_energy <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv') %>% 
  mutate(
    country = case_when(
      country == "EL" ~ "GR", 
      country == "UK" ~ "GB", 
      TRUE ~ country
    )
  ) %>% 
  filter(
    level == "Level 1", 
    type != "Other"
  ) %>% 
  mutate(
    category = case_when(
      type == "Conventional thermal" ~ "Conventional\nthermal",
      type == "Nuclear" ~ "Nuclear",
      TRUE ~ "Renewable"
    ),
    category = factor(category)
  ) %>% 
  group_by(country, country_name, category) %>% 
  summarize(`2018` = sum(`2018`, na.rm = TRUE)) %>% 
  group_by(category) %>% 
  mutate(total_cat = sum(`2018`, na.rm = TRUE)) %>% 
  group_by(country) %>% 
  mutate(
    total_country = sum(`2018`, na.rm = TRUE),
    rel = `2018` / total_cat
  )
  
  
## add Georgia to geofacet
my_grid <- 
  europe_countries_grid1 %>% 
  filter(!code %in% c("IS", "BY", "RU", "MD", "CH")) %>% 
  add_row(row = 6, col = 10, code = "GE", name = "Georgia") %>% 
  mutate(
    row = case_when(
      code == "IE" ~ 2, 
      TRUE ~ row
    ),
    col = if_else(code %in% c("IE", "PT", "ES"),col + 1, col),
    col = col - 1
  ) 

## colors
pal <- c("grey60", "#f7d577", "#0dd891")

## geofacet
grid <- 
  ggplot(df_energy, aes(area = `2018`, fill = category,  subgroup = country)) +
  geom_treemap(
    aes(alpha = rel),
    color = NA, start = "topleft", layout = "scol"
  ) +
  geom_text(
    aes(x = 0, y = .1, label = glue::glue("{country}"), size = total_country),
    alpha = .1, family = "Goffik-Outline",
    color = "grey50", size = 30, stat = "unique"
  ) +
  facet_geo(~ country, grid = my_grid) +
  scale_fill_manual(values = pal) +
  scale_size(range = c(2, 20)) +
  scale_alpha(range = c(.2, 1)) +
  theme(plot.margin = margin(7, 180, 7, 7))

## legend
df_legend <-
  tibble(
    category = rep(factor(unique(df_energy$category), levels = levels(df_energy$category)), 11),
    rel = rep(seq(0, 1, by = .1), each = 3)
  )

legend <- 
  ggplot(df_legend, aes(rel, fct_rev(category))) +
  geom_tile(
    aes(fill = category, color = after_scale(darken(fill, .15, space = "HLS")), 
        alpha = rel),
    size = .2
  ) +
  geom_hline(
    data = tibble(y = -.5:5.5), aes(yintercept = y),
    color = "white", size = .7
  ) +
  geom_segment(
    data = tibble(x = c(.22, .78), xend = c(.05, .95)),
    aes(x = x, xend = xend, y = 3.85, yend = 3.85),
    inherit.aes = FALSE, color = "grey30", size = .4,
    arrow = arrow(length = unit(.2, "lines"), type = "closed")
  ) +
  ## some strange transfromation because I used the custom legend from a 
  ## TidyTuesday contribution that was scaled to 10-100%
  geom_point(data = df_energy, aes(x = rel * (1 / .45), 
                                   y = as.numeric(fct_rev(category))), 
             shape = 21, color = "transparent", fill = "white", size = 3) +
  geom_point(data = df_energy, aes(x = rel * (1 / .45), 
                                   y = as.numeric(fct_rev(category))), 
             shape = 1, size = 3, alpha = .3) +
  geom_text(
    data = tibble(x = c(-.025, 1.05), h = c(0, 1), 
                  label = c("0%", "45%")),
    aes(x = x, y = 3.85, label = label, hjust = h),
    inherit.aes = FALSE, family = "Oswald", color = "grey30", size = 4.2
  ) +
  geom_text(
    data = tibble(x = c(.418, 1.007, .343, .275), y = c(3.28, 2.28, 1.28, 1.28),
                  label = c("DE", "FR", "DE", "NO")),
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE, family = "Oswald", color = "black", size = 3.2,
    fontface = "bold"
  ) +
  geom_text(
    data = tibble(x = c(.418, 1.007, .343, .275), y = c(2.68, 1.68, .68, .68),
                  label = c("19%", "45%", "15%", "12%")),
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE, family = "Oswald", color = "black", size = 2.7
  ) +
  labs(
    title = "How European Countries Generated Electricity in 2018",
    caption = "<span style='font-family:Oswald;'><b>Germany</b> is the largest energy producing country in Europe. It generates the most renewable and conventional thermal energy, representing 15.4% and 18.8% of Europe's total production, respectively.<br><br><b>France</b> is the second largest European energy producer and by far the largest nuclear energy provider: it produces 45.3% of all of Europe's nuclear energy.</span><br><br><i style='font-size:9pt;color:black;'>Visualization: Cédric Scherer | Data: Eurostat | #30DayChartChallenge 2021 | Day 6: Experimental</i>"
  ) +
  annotate(
    "text", x = .5, y = 3.85, label = "Proportion of European Total",
    family = "Oswald",  color = "grey45", size = 5.2
  ) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  scale_x_continuous(expand = c(.006, .006)) +
  scale_y_discrete(expand = c(.5, .5), position = "right") +
  scale_alpha(range = c(.2, 1), limits = c(0, 1)) +
  theme(plot.margin = margin(rep(0, 4)),
        axis.text.y = element_text(size = 14, family = "Oswald", face = "bold",
                                   color = rev(darken(pal, .2)), hjust = 0))

ggdraw(grid) +
  draw_plot(legend, .845, .76, .28, .42, hjust = .5, vjust = .5) 

file <- here::here("06_experimental", "06_experimental.pdf")

ggsave(file, width = 20, height = 12, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = file, 
  filenames = glue::glue("{str_remove(file, '.pdf')}.png"),
  format = "png", dpi = 250
)

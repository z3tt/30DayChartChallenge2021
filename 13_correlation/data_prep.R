library(tidyverse)
library(ggtext)
library(babynames)

ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

ufo <- ufo_sightings %>% 
  mutate(
    date_time = lubridate::as_datetime(date_time, format = "%d/%m/%Y %H:%M"),
    year = lubridate::year(date_time)
  ) %>% 
  count(year, name = "UFO sightings") %>% 
  mutate(`UFO sightings` = `UFO sightings` * 5.2)

comb <- 
  babynames %>% 
  filter(name == "Mia", sex == "F") %>% 
  dplyr::select(year, `Babies named "Mia"` = n) %>% 
  left_join(ufo) 
  
comb %>% filter(year %in% 1945:2010) -> comb_lim

write_csv(comb_lim, here::here("13_correlation", "data.csv"))

comb %>%
  pivot_longer(
    cols = -year,
    names_to = "group",
    values_to = "n"
  ) %>% 
  filter(year %in% 1945:2010) %>% 
  ggplot(aes(year, n, color = group)) + 
    geom_line() +
    geom_point(aes(shape = group), size = 2) +
    scale_x_continuous(
      limits = c(1945, 2012), expand = c(.02, .02),
      breaks = seq(1950, 2010, by = 10)
    ) +
    scale_y_continuous(
      name = 'Babies named "Mia"', expand = c(.02, .02),
      sec.axis = sec_axis(trans = ~ . / 5.5, name = "UFO sightings")
    ) +
    scale_color_manual(values = c("#C84053", "black")) + 
    scale_shape_manual(values = c(18, 20)) +
    labs(
      title = '<b style="color:#C84053;font-size:24pt;">Number of Babies named "Mia" in the U.S.</b><br>correlates with<br><b style="color:black;font-size:24pt;">Number of UFO Sightings Around the World</b>'
    ) +
    theme_minimal(base_size = 14, base_family = "Open Sans") +
    theme(axis.line.x = element_line(color = "grey75"),
          axis.ticks.x = element_line(color = "grey75"),
          axis.text.x = element_text(color = "grey60", margin = margin(t = 7)),
          axis.title.x = element_blank(),
          axis.text.y.left = element_text(color = "#C84053"),
          axis.title.y.left = element_text(color = "#C84053", margin = margin(r = 10)),
          axis.title.y.right = element_text(margin = margin(l = 10)),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.key.width = unit(4, "lines"),
          plot.title = element_markdown(hjust = .5, size = 20, color = "grey60",
                                        family = "Lora", lineheight = 1.2))

  
file <- here::here("13_correlation", "13_correlation.pdf")

ggsave(file, width = 12, height = 8, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = file, 
  filenames = glue::glue("{str_remove(file, '.pdf')}.png"),
  format = "png", dpi = 200
)

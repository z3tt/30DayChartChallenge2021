library(tidyverse)
library(lubridate)
library(zoo)
library(viridis)
library(ggtext)
library(ragg)

Sys.setlocale("LC_ALL","English")

register_variant(
  name = "Produkt Regular",
  family = "Produkt",
  weight = "normal"
)

register_variant(
  name = "Produkt Light",
  family = "Produkt",
  weight = "light"
)

register_variant(
  name = "Produkt Medium",
  family = "Produkt",
  weight = "medium"
)


## data: https://covid19.apple.com/mobility
df <- 
  read_csv(here::here("23_tiles", "applemobilitytrends-2021-04-21.csv")) %>% 
  pivot_longer(
    cols = starts_with("202"),
    names_to = "day",
    values_to = "traffic"
  ) %>% 
  mutate(
    day = ymd(day), traffic = log2(traffic / 100),
    region = if_else(region == "Saint Petersburg - Russia", "Saint Petersburg", region)
  )

## all largest cities contained in the data 
# cities <- c("Tokyo", "Delhi", "Sao Paulo", "Mexico City", "Cairo", "Mumbai", "Osaka", "New York City", "Buenos Aires", "Istanbul", "Manila", "Rio de Janeiro", "Los Angeles", "Moscow", "Bangalore", "Paris", "Jakarta", "Hyderabad", "London", "Chicago", "Ho Chi Minh City", "Kuala Lumpur", "Hong Kong", "Riyadh", "Santiago", "Madrid", "Pune", "Houston", "Dallas", "Toronto", "Miami", "Belo Horizonte", "Singapore", "Philadelphia", "Atlanta", "Fukuoka", "Barcelona", "Johannesburg", "Saint Petersburg", "Washington DC", "Guadalajara", "Berlin")

## all largest cities contained in the data with traffic data
cities <- c("Tokyo", "Sao Paulo", "Mexico City", "Osaka", "New York City", "Manila", "Rio de Janeiro", "Los Angeles", "Paris", "London", "Chicago", "Madrid", "Houston", "Dallas", "Toronto", "Miami", "Singapore", "Philadelphia", "Atlanta", "Fukuoka", "Barcelona", "Washington DC", "Berlin")

df_plot <- df %>% 
  filter(region %in% cities, !is.na(traffic)) %>% 
  mutate(
    region = factor(region, levels = rev(cities)),
    transportation_type = str_to_title(transportation_type)
  ) %>% 
  group_by(region) %>% 
  mutate(traffic = rollmean(traffic, 7, align = "right", fill = NA)) %>% 
  filter(day > "2020-01-18") %>%
  ungroup

ggplot(df_plot, aes(day, as.integer(region), fill = traffic, color = traffic)) +
  geom_tile() +
  geom_point(data = df_plot %>% group_by(region, transportation_type) %>%
               filter(traffic == min(traffic)),
             aes(y = as.integer(region) + .325),
             color = "white", fill = "#000000B3", shape = 25, 
             size = 3, stroke = .6) +
  geom_hline(data = tibble(y = 0:23 + 0.5), aes(yintercept = y),
             color = "white", size = .5) +
  geom_text(data = df_plot %>% group_by(region, transportation_type) %>% slice(1),
            aes(x = as_date("2019-11-17"), label = region), 
            family = "Produkt Medium", color = "white", size = 4.3,
            nudge_x = 3, hjust = 0) +
  facet_wrap(~transportation_type, ncol = 1, scales = "free_x") +
  coord_cartesian(expand = FALSE) +
  scale_x_date(breaks = seq(ymd("2020-02-01"), ymd("2021-04-01"), 
                            by = '1 month'), 
               date_labels = "%b%e<sup>st</sup><br>%Y",
               date_minor_breaks = "1 week",
               limits = c(as_date("2019-11-17"), NA)) +
  scale_fill_gradientn(colors = viridis::rocket(100, direction = -1, end = .97),
                       #breaks = log2(seq(-1.25, .5, by = .25)),
                       #breaks = c(seq(-4, 0, by = 1), .5, 1),
                       breaks = log2(c(.0625, .125, .25, .5, 1, 2)),
                       labels = c("6.25%", "12.5%", "25%", "50%", "100%<br>Baseline", "200%"),
                       #limits = c(NA, 4),
                       name = "Relative volume of directions requests for the largest cities compared to a baseline volume on January 13<sup>th</sup> 2020") +
  scale_color_gradientn(colors = viridis::rocket(100, direction = -1, end = .97),
                        breaks = seq(-.6, 2.6, by = .2), guide = "none") +
  guides(fill = guide_colorsteps(barwidth = unit(60, "lines"),
                                 barheight = unit(.7, "lines"),
                                 title.position = "top",
                                 title.hjust = .5, label.vjust = 1)) +
  labs(title = "Change in Apple Routing Requests during the COVID19 Pandemic",
       caption = "Visualization: Cédric Scherer | Data: Apple Maps Mobility Trends (accessed on April 23<sup>rd</sup> 2020).<br>Change in traffic requests is shown as 7-day rolling average. Data for May 11<sup>th</sup>-12<sup>th</sup>, 2020 and March 12<sup>th</sup>, 2021 are not available. Shown are all largest cities, sorted<br>by population, that are contained in the dataset and include transit request data. Downward traingles indicate the lowest value within the visualized time period.<br><br><span style='color:#9f9f9f;'>#30DayChartChallenge 2021 | Day 23: Tiles</span>") +
  theme_minimal(base_size = 17, base_family = "Produkt Regular") +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "grey35", color = "transparent"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_markdown(size = 10.5, color = "grey35",
                                   lineheight = 1.05),
    axis.text.y = element_blank(),
    axis.ticks.x = element_line(color = "grey35", size = .5),
    legend.position = "top",
    legend.title = element_markdown(family = "Produkt Light", size = 16.5,
                                color = "grey35",
                                margin = margin(t = 0, b = 10)),
    legend.text = element_markdown(family = "InputMono", size = 13,
                                   color = "grey35", lineheight = 1.1,
                                   margin = margin(t = 1, b = 0)),
    legend.margin = margin(10, 0, -15, 0),
    strip.text = element_text(family = "Produkt Medium", size = 23, hjust = 0),
    panel.spacing.y = unit(.7, "lines"),
    plot.title = element_text(family = "Produkt Medium", color = "black",
                              size = 27.3, hjust = .5, 
                              margin = margin(t = 20, b = 0)),
    plot.caption = element_markdown(family = "InputSerifCondensed", 
                                    color = "grey35", size = 9, lineheight = 1.4,
                                    hjust = 0, margin = margin(t = 25, b = 5)),
    plot.margin = margin(20, 30, 20, 30)
  )

file <- here::here("23_tiles", "23_tiles.pdf")

ggsave(file, width = 13, height = 22.5, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = file, 
  filenames = glue::glue("{str_remove(file, '.pdf')}.png"),
  format = "png", dpi = 320
)

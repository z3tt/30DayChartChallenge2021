library(tidyverse)
library(colorspace)
library(ggtext)
library(ggrepel)

df_slope <-
  read_csv("https://raw.githubusercontent.com/Z3tt/30DayChartChallenge/main/05_slope/worldbank_urban_percentag.csv", skip = 4) %>% 
  dplyr::select(`Country Name`, `1960`, `2019`) %>%
  mutate(change = `2019` - `1960`) %>% 
  pivot_longer(
    cols = -c(`Country Name`, change),
    names_to = "year",
    values_to = "population"
  ) %>% 
  filter(
    !is.na(change),
    !str_detect(`Country Name`, "Europe|income|East Asia|Euro area|situations|poor countries|small states|IBRD|IDA|Latin America|UN class|demographic|North Africa|OECD|Other small|Sub-Saharan Africa|Arab World")
  )

df_slope %>% 
  filter(`Country Name` != "World", change < 0) %>% 
  ggplot(aes(year, population, group = `Country Name`)) +
  geom_line(data = df_slope %>% filter(change >= 0), 
            aes(color = change), alpha = .33, size = .9) +
  geom_line(data = df_slope %>% filter(`Country Name` == "World"), 
            color = "grey15", size = 1.5) +
  geom_line(aes(color = change * 4, 
                color = after_scale(darken(color, .2, space = "HLS"))), 
            size = 1.3) +
  geom_text_repel(data = df_slope %>% filter(change < 0, year == 1960), 
                  aes(label = `Country Name`),
                  hjust = 1, nudge_x = -.11, size = 4.5, family = "Overpass",
                  direction = "y", force = .5, min.segment.length = 0, 
                  segment.size = .5, segment.curvature = -0.15, segment.ncp = 3, 
                  segment.angle = 90, segment.inflect = FALSE, box.padding = .15) +
  geom_text_repel(data = df_slope %>% filter(change < 0, year == 2019), 
                  aes(label = glue::glue("{format(abs(change), digits = 3)}% ↓")),
                  hjust = 0, nudge_x = .07, size = 4.5, family = "Overpass Mono",
                  direction = "y", force = .5,  min.segment.length = 0, 
                  segment.size = .5) +
  geom_point(data = df_slope %>% filter(change >= 0), 
             aes(color = change), size = 1.5) +
  geom_point(shape = 21, size = 4, stroke = 2, fill = "white", 
             color = "transparent") +
  geom_point(data = df_slope %>% filter(`Country Name` == "World"), size = 5) +
  geom_point(aes(color = change * 4, 
                 color = after_scale(darken(color, .2, space = "HLS"))), 
             shape = 21, stroke = 2, fill = NA, size = 4) +
  geom_richtext(data = df_slope %>% filter(`Country Name` == "World"),
            aes(x = "2019", y = 40.5, label = "<b style='font-size:21pt;font-family:overpass;'>World</b><br>22.1% ↑"), 
            color = "grey15", nudge_x = -.45, size = 4.5, fontface = "bold",
            family = "Overpass Mono", stat = "unique", inherit.aes = FALSE,
            fill = NA, label.colour = NA) +
  geom_point(aes(y = 0), stat = "unique", shape = "-", size = 8, color = "grey65") +
  annotate("text", x = "2019", y = 100, label = " 100% urban", size = 4.5,
           family = "Overpass Mono SemiBold", color = "grey65", hjust = 0, vjust = .7) +
  annotate("text", x = "2019", y = 0, label = "   0% urban", size = 4.5, 
           family = "Overpass Mono SemiBold", color = "grey65", hjust = 0, vjust = .3) +
  geom_textbox(aes(x = "1960", y = 100, label = "Only a Dozen Countries Worldwide have Declining Urban Population Figures"),
               family = "Overpass ExtraBold", size = 5.5, box.colour = NA, fill = NA,
               nudge_x = -.5, vjust = .6, width = unit(3.1, "inch"), 
               lineheight = 1.25, stat = "unique", inherit.aes = FALSE) +
  geom_textbox(aes(x = "1960", y = 94.5, label = "According to the <i>World Urbanization Prospects</i> by the United Nations Population Division, only twelve out of 218 countries listed have a declining trend when it comes to changes in urban population between 1960 and 2019.<br><br>Surprinsingly—at least for a European citizen—Austria and Liechtenstein are amog these exhibiting a decrease in urban population by 6.2% and 6.07% within six decades, respectively.</span>"),
               family = "Overpass", size = 4.3, box.colour = NA, fill = NA,
               nudge_x = -.5, vjust = 1, width = unit(3.1, "inch"), 
               lineheight = 1.4, stat = "unique", inherit.aes = FALSE) +
  coord_cartesian(clip = "off") +
  scale_x_discrete(expand = c(.3, .3), position = "top") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  rcartocolor::scale_color_carto_c(palette = "Geyser", direction = -1, 
                                   limits = c(-73, 73), guide = "none") +
  labs(caption = "Visualization by Cédric Scherer | Data: United Nations Population Division. World Urbanization Prospects: 2018 Revision. | #30DayChartChallenge 2021 Day 5: Slope             ") +
  theme_void() +
  theme(panel.grid.major.x = element_line(color = "grey65", size = 1),
        axis.text.x = element_text(color = "grey50", size = 14, 
                                   family = "Overpass ExtraBold",
                                   margin = margin(t = 5, b = 10)),
        plot.margin = margin(15, 0, 15, 100),
        plot.caption = element_text(family = "Overpass", size = 9, 
                                    color = "grey50", hjust = 1,
                                    margin = margin(t = 35, b = 0, r = 50)),
        plot.caption.position = "plot")

file <- here::here("05_slope", "05_slope.pdf")

ggsave(file, width = 10, height = 13.5, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = file, 
  filenames = glue::glue("{str_remove(file, '.pdf')}.png"),
  format = "png", dpi = 250
)


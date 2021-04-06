library(tidyverse)
library(ggtext)
library(ragg)

ticks <-
  tibble(
    x = 0:30,
    ymax = 1,
    ymin = .88
  )

ticks_major <-
  tibble(
    x = seq(0, 30, 5),
    ymax = 1,
    ymin = .73
  )

ticks_vert <-
  tibble(
    x = c(0, 10, 20, 30),
    y = .712
  ) %>% 
  mutate(xmin = x - .22, xmax = x + .22)

labs_x <-
  tibble(
    x = c(9.5, 19.45, 29.45),
    y = .8,
    lab = c("<span style='font-size:23pt;'>1o</span>", "2O", "30")
  )

labs_vert <-
  tibble(
    x = c(0, 17.6, 19.7, 20.5, 21.1, 21.8, 25.3, 26, 26.67, 27.6, 28.2, 29.65, 30.2),
    y = 1.42,
    lab = c("<b style='font-size:32pt;'>TOLEDO.</b>", "<i>G. Ianfonius.</i>", "<i>G. Mercator", "<i>I. Schonerus.</i>", "<i>P. Lantsbergius.</i>", "<i>T. Brabz.</i>", "<i>I. Regiomontanus.</i>", "<i>Orontius.</i>", "<i>C. Clavius.</i>", "<i>C. Ptolomaus.</i>", "<i>A. Argelius.</i>", "<i>A. Maginus.</i>", "<i>D. Origanus.</i>")
  )

marker <-
  tibble(
    x = c(0, 17.6, 19.7, 20.7, 21.1, 21.5, 25.4, 26, 26.4, 27.75, 28.1, 29.7, 30.1),
    y = 1.42
  )

labs_hor <-
  tibble(
    x = c(23.5, 3.5, 7.3, 9, 13.3),
    y = c(1.2, 2, 2, 2, 2),
    lab = c("ROMA", "G<span style='font-size:25pt;'> R A D O S</span>",  "<span style='font-size:25pt;'>D E</span>", "<span style='font-size:25pt;'>L A</span>", "L<span style='font-size:25pt;'> O N G I T U D</span>.")
  )

ggplot() +
  geom_linerange(aes(xmin = -.24, xmax = 30.05, y = 1), size = 1.4) + 
  geom_linerange(data = ticks, aes(x = x, ymin = ymin, ymax = ymax), 
                 size = 1.4) +
  geom_linerange(data = ticks_major, aes(x = x, ymin = ymin, ymax = ymax), 
                 size = 1.6) +
  geom_linerange(data = ticks_vert, aes(xmin = xmin, xmax = xmax, y = y), 
                 size = 1.4) +
  geom_point(data = marker, aes(x, y = 1.26),
             shape = "|", size = 4.5, stroke = 4) +
  geom_point(data = marker, aes(x, y = 1.22),
             shape = 22, size = 2.5, stroke = 1.5, fill = "white") +
  geom_point(data = marker, aes(x, y = 1.12),
             shape = 21, size = 4.5, stroke = 1.5, fill = "white") +
  geom_point(data = marker, aes(x, y = 1.12),
             size = .5) +
  geom_richtext(data = labs_x, aes(x, y, label = lab), size = 6.2, 
                family = "Kaushan Script", fill = NA, label.colour = NA) +
  geom_text(data = labs_hor[1,], aes(x, y, label = lab), 
            size = 10, family = "Averia Serif Libre", fontface = "bold") + #
  geom_richtext(data = labs_hor[2:5,], aes(x, y, label = lab), 
            size = 14, family = "Averia Serif Libre", fontface = "bold", fill = NA, label.colour = NA) + #Playfair Display SC
  geom_richtext(data = labs_vert, aes(x, y, label = lab), 
               angle = 90, hjust = 0, size = 6, family = "Averia Serif Libre", 
               fontface = "bold", fill = NA, label.colour = NA) + #Faustina Historical
  scale_x_continuous(expand = c(.03, .03)) +
  scale_y_continuous(limits = c(.4, 3.4), expand = c(0, 0)) +
  theme_void() +
  theme(plot.caption = element_markdown(family = "Averia Serif Libre", size = 19, color = "grey15", hjust = 0, lineheight = 1.1, margin = margin(35, 5, 10, 20))) +
  labs(caption = "Remake of <span style='color:#828282;'>(one of?)</span> the first visual representation of statistical data by Michael Florent Van Langren, drawn in 1644.<br>The Flemish astronomer illustrated the twelve known estimates in longitude between Toledo and Rome at that time.<span style='font-size:15pt;'><br><br><i>Created with ggplot2 by Cédric Scherer</i> | <i>#30DayChartChallenge 2021</i> | <i>Day 3: Historical</i></span>")

ggsave(here::here("03_historical", "03_historical.png"), 
       width = 17600, height = 6000, res = 1200, 
       device = agg_png, limitsize = FALSE)


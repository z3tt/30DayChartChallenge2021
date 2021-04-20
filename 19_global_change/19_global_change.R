library(tidyverse)
library(ggsankey)

## data via https://ourworldindata.org/grapher/total-agricultural-area-over-the-long-term

df <- 
  read_csv(here::here("19_global_change", "cropland-extent-over-the-long-run.csv")) %>% 
  filter(!Entity %in% c("World", "Greenland"), Year >= 0) %>% 
  mutate(Entity = case_when(
    #str_detect(Entity, "Brazil|United States|Canada") ~ "Americas and the Carribean",
    str_detect(Entity, "Brazil") ~ "Latin America and\nthe Caribbean",
    str_detect(Entity, "Canada|United States") ~ "Northern America",
    str_detect(Entity, "Russia") ~ "Europe",
    str_detect(Entity, "Rest of Asia") ~ "Asia (excluding\nIndia and China)",
    #str_detect(Entity, "India|China") ~ "Asia",
    TRUE ~ Entity
  )) %>% 
  group_by(Entity, Year) %>% 
  summarize(cropland = sum(`Cropland (HYDE (2017))`, na.rm = TRUE)) %>%
  #mutate(timestep = Year) %>%
  #mutate(timestep = (Year %/% 100) * 100) %>%
  mutate(timestep = ((Year * 2) %/% 100) * 100 / 2) %>%
  group_by(Entity, timestep) %>% 
  summarize(cropland = max(cropland, na.rm = TRUE)) %>%
  group_by(Entity) %>% 
  mutate(last = cropland[which(timestep == max(timestep))]) %>% 
  ungroup() %>% 
  mutate(Entity = fct_reorder(Entity, -last))

df_labs <- 
  tibble(
    Entity = factor(levels(df$Entity), levels = levels(df$Entity)),
    cropland =  c(720000000, 400000000, 125000000, -125000000, -340000000, 
                  -540000000, -710000000, -815000000, -872000000)
  )

cols <- c("firebrick", "tan", "#F39530", "#778e99", "grey74", 
          "#003b4c", "#bbcad2", "grey81", "grey88")

set.seed(1)

ggplot(df, 
       aes(x = timestep, value = cropland, node = Entity, fill = Entity)) +
  geom_sankey_bump(space = 15000000, smooth = 12) +
  geom_text(
    data = df_labs, 
    aes(x = 2003, y = cropland, label = Entity, 
        color = Entity, color = after_scale(colorspace::darken(color, .2))), 
    hjust = 0, size = 6.5, family = "Bebas Neue", lineheight = .75
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(NA, 2100),
                     breaks = c(500, 1000, 1500, 1600, 1700, 1800, 1900)) +
  scale_y_continuous(expand = c(.01, .01)) +
  scale_color_manual(values = cols, guide = FALSE) +
  scale_fill_manual(values = cols, guide = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#FAF5F0"),
        panel.grid.major.x = element_line(color = "#E8E1DB"))

ggsave(here::here("19_global_change", "19_global_change_raw.pdf"), 
       width = 40, height = 12, device = cairo_pdf)

ggsave(here::here("19_global_change", "19_global_change_raw.svg"), 
       width = 40, height = 12)

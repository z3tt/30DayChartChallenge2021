library(rnaturalearth)
library(sf)
library(terra)
library(stars)
library(ragg)
library(pdftools)

sf_urban <- 
  rnaturalearth::ne_download(scale = 10, 
                             category = "cultural", 
                             type = "urban_areas",
                             returnclass = "sf") %>% 
  st_transform(crs = "+proj=cea +lon_0=0 +lat_ts=45")

v <- terra::vect(sf_urban)
r <- terra::rast(v, resolution = 10000)
raster_urban <- terra::rasterize(v, r)
distance <- terra::distance(raster_urban)
sf_distance <- stars::st_as_stars(distance)

g <- ggplot() +
  geom_stars(data = sf_distance) +
  coord_cartesian(expand = FALSE) +
  labs(caption = "Cédric Scherer  |  #30DayChartChallenge 2021  |  Day 10: Abstract") +
  theme_void() +
  theme(
    panel.border = element_rect(fill = "transparent", color = "grey96", size = 1.3),
    plot.background = element_rect(fill = "black"),
    plot.margin = margin(35, 35, 0, 35),
    plot.caption = element_text(family = "Playfair Display SC", color = "grey96", 
                                size = 15, hjust = .5, 
                                margin = margin(t = 15, b = 25))
  )

g +
  scale_fill_gradientn(colors = viridis::rocket(100), guide = "none") +
  ggsave(here::here("10_abstract", "10_abstract_rocket.pdf"), 
         width = 18, height = 12, device = cairo_pdf)

g +
  scale_fill_gradientn(colors = viridis::mako(100), guide = "none") +
  labs(caption = "Cédric Scherer |  #3ODayChartChallenge 2O21  |  Day 1O: Abstract") +
  theme(plot.caption = element_text(family = "Graduate")) +
  ggsave(here::here("10_abstract", "10_abstract_mako.pdf"), 
         width = 18, height = 12, device = cairo_pdf)

g +
  scale_fill_gradientn(colors = viridis::turbo(100), guide = "none") +
  theme(plot.caption = element_text(family = "Luckiest Guy")) +
  ggsave(here::here("10_abstract", "10_abstract_turbo.pdf"), 
         width = 18, height = 12, device = cairo_pdf)

pdfs <- list.files(here::here("10_abstract"), pattern = "*.pdf", recursive = TRUE)
for(pdf in pdfs) {
  pdf_convert(pdf = glue::glue("{here::here('10_abstract')}/{pdf}"), 
              filenames = glue::glue("{here::here('10_abstract')}/{str_remove(pdf, '.pdf')}.png"),
              format = "png", dpi = 400)
}




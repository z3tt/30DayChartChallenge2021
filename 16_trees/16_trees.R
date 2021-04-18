library(tidyverse)
library(ggraph)

csv <- read_csv(here::here("16_trees", "2015StreetTreesCensus_TREES.csv"))

csv <- csv %>% 
  mutate(spc_latin = case_when(
    str_detect(spc_latin, "var. inermis") ~ "Gleditsia triacanthos", 
    str_detect(spc_latin, "Acer platanoides") ~ "Acer platanoides",
    TRUE ~ spc_latin
  ))

csv %>% 
  filter(!is.na(spc_common), !spc_latin %in% c("Prunus", "Acer")) %>% 
  count(spc_common, sort = T) %>% 
  slice(1:20) %>% 
  pull(spc_common) -> species



dat <- csv %>% 
  filter(spc_common %in% species) %>% 
  count(zipcode, spc_latin) %>% 
  mutate(zipcode = as.character(zipcode),
         spc_latin = stringr::str_wrap(spc_latin, 10))

V <- crossprod(table(dat[1:2]))
diag(V) <- 0

graph <- igraph::graph_from_adjacency_matrix(V, weighted = TRUE)

V(graph)$group <- degree(graph, mode = 'in')
V(graph)$group <- sub("\\\n.*$", "", V(graph)$name)

set.seed(12345)
pal <- sample(c(rcartocolor::carto_pal(12, name = "Prism"), "firebrick"))

g <- ggraph(graph, layout = 'linear') + 
  geom_edge_arc(aes(edge_width = weight, edge_color = weight),
                fold = TRUE, alpha = .35) +
  geom_node_point(shape = 15, size = 21, color = "white") +
  geom_node_text(aes(label = name, color = group), 
                 family = "Ostrich Sans", size = 7, lineheight = .8) +
  scale_color_manual(values = pal, guide = "none") +
  scale_edge_color_gradientn(colours = rcartocolor::carto_pal(7, name = "Emrld"), guide = "none") +
  scale_edge_width(range = c(.5, 2.5), guide = "none") +
  labs(title = "What Tree Species Occur Together in New York City?",
       subtitle = "The arc diagram shows the 15 most common tree species in NYC and how often they occur in close the same area.\nThe more often pairs of these tree species were recorded in the same ZIP code area, the wider and darker the arcs.",
       caption = "Visualization: Cédric Scherer | Data: 2015 Street Tree Census, NYC Parks & Recreation | #30DayChartChallenge | Day 16: Trees") +
  theme_void() +
  theme(plot.margin = margin(20, 10, 15, 10),
        plot.title = element_text(family = "Ostrich Sans", size = 59, hjust = .5),
        plot.subtitle = element_text(family = "Century Gothic", size = 17, color = "grey55", 
                                     hjust = .5, margin = margin(t = 10, b = 0)),
        plot.caption = element_text(family = "Century Gothic", size = 13, color = "grey55", 
                                    hjust = .5, margin = margin(t = 15, b = 0)))

file <- here::here("16_trees", "16_trees.pdf")
       
ggsave(file, width = 25, height = 14, device = cairo_pdf)

pdftools::pdf_convert(
 pdf = file, 
 filenames = glue::glue("{str_remove(file, '.pdf')}.png"),
 format = "png", dpi = 180
)

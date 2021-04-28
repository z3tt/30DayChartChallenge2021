library(tidyverse)
library(ragg)
library(colorspace)

register_variant(
  name = "Gotham Black",
  family = "Gotham",
  weight = "bold"
)

## data ´: https://www.the-numbers.com/movies/production-company/DreamWorks-Animation
df <- data.frame(
  stringsAsFactors = FALSE,
  check.names = FALSE,
  date = c("Sep 27, 2019","Feb 22, 2019","Jun 2, 2017",
           "Mar 31, 2017","Nov 4, 2016","Jan 29, 2016",
           "Mar 27, 2015","Nov 26, 2014","Jun 13, 2014",
           "Mar 7, 2014","Jul 17, 2013","Mar 22, 2013",
           "Nov 21, 2012","Jun 8, 2012","Oct 28, 2011","May 26, 2011",
           "Nov 5, 2010","May 21, 2010","Mar 26, 2010",
           "Mar 27, 2009","Nov 7, 2008","Jun 6, 2008",
           "Nov 2, 2007","May 17, 2007","Nov 3, 2006",
           "May 19, 2006","Oct 5, 2005","May 27, 2005","Oct 1, 2004",
           "May 19, 2004","Jul 2, 2003","May 24, 2002",
           "May 18, 2001","Jun 21, 2000","Mar 31, 2000",
           "Dec 18, 1998","Oct 2, 1998"),
  title = c("Abominable","How to Train Your Dragon: T…",
            "Captain Underpants: The Fir…","The Boss Baby",
            "Trolls","Kung Fu Panda 3","Home",
            "Penguins of Madagascar","How to Train Your Dragon 2",
            "Mr. Peabody & Sherman","Turbo","The Croods",
            "Rise of the Guardians","Madagascar 3: Europe's Most…",
            "Puss in Boots","Kung Fu Panda 2","Megamind",
            "Shrek Forever After","How to Train Your Dragon",
            "Monsters vs. Aliens","Madagascar: Escape 2 Africa",
            "Kung Fu Panda","Bee Movie","Shrek the Third",
            "Flushed Away","Over the Hedge",
            "Wallace & Gromit: The Curse…","Madagascar","Shark Tale",
            "Shrek 2","Sinbad: Legend of the Seven…",
            "Spirit: Stallion of the Cim…","Shrek","Chicken Run",
            "The Road to El Dorado","The Prince of Egypt","Antz"),
  budget = c("$75,000,000",
             "$129,000,000","$38,000,000","$125,000,000",
             "$125,000,000","$140,000,000","$130,000,000",
             "$132,000,000","$145,000,000","$145,000,000",
             "$135,000,000","$135,000,000","$145,000,000",
             "$145,000,000","$130,000,000","$150,000,000",
             "$130,000,000","$165,000,000","$165,000,000",
             "$175,000,000","$150,000,000","$130,000,000",
             "$150,000,000","$160,000,000","$149,000,000","$80,000,000",
             "$30,000,000","$75,000,000","$75,000,000",
             "$70,000,000","$60,000,000","$80,000,000",
             "$50,000,000","$42,000,000","$95,000,000","$60,000,000",
             "$60,000,000"),
  gross_domestic = c("$60,761,390","$160,799,505","$73,921,000","$175,003,033",
                     "$153,707,064","$143,528,619","$177,397,510",
                     "$83,350,911","$177,002,924","$111,506,430",
                     "$83,028,130","$187,168,425","$103,412,758",
                     "$216,391,482","$149,260,504","$165,249,063",
                     "$148,415,853","$238,736,787","$217,581,232",
                     "$198,351,526","$180,174,880","$215,434,591","$126,631,277",
                     "$322,719,944","$64,665,672","$155,019,340",
                     "$56,068,547","$193,595,521","$161,412,000",
                     "$441,226,247","$26,483,452","$73,215,310",
                     "$267,655,011","$106,793,915","$50,802,661",
                     "$101,413,188","$90,757,863"),
  gross_world = c("$188,125,727","$522,587,135","$126,422,949",
                  "$527,965,936","$343,242,613","$521,170,825",
                  "$385,997,896","$366,942,531","$614,586,270",
                  "$269,806,430","$286,896,578","$573,068,425",
                  "$306,900,902","$746,921,271","$554,987,477",
                  "$664,837,547","$321,887,208","$756,244,673","$494,870,992",
                  "$381,687,380","$599,680,774","$631,910,531",
                  "$287,594,577","$807,330,936","$179,357,126",
                  "$343,397,247","$197,593,152","$556,559,566",
                  "$371,741,123","$935,253,978","$80,767,884",
                  "$106,515,310","$491,812,794","$227,793,915",
                  "$65,700,000","$218,613,188","$152,457,863")
  ) %>% 
  mutate(
    budget = as.numeric(str_remove_all(budget, "\\$|,")),
    gross_domestic = as.numeric(str_remove_all(gross_domestic, "\\$|,")),
    gross_world = as.numeric(str_remove_all(gross_world, "\\$|,")),
    date = lubridate::mdy(date),
    type = if_else(str_detect(title, "Shrek"), '"Shrek" Quadrilogy', "Other Movies"),
    type = fct_rev(fct_inorder(type)),
    type_num = as.integer(type)
  )

theme_set(theme_minimal(base_size = 38, base_family = "Impact"))

theme_update(
  plot.margin = margin(rep(15, 4)),
  axis.title = element_blank(),
  axis.text.x = element_text(margin = margin(7, 0, 0, 0), size = 38,
                             lineheight = .75),
  axis.text.y = element_text(margin = margin(0, 3, 0, 0), family = "Lato"),
  plot.title = element_text(margin = margin(0, 0, 50, 0), hjust = .5, 
                            size = 70, lineheight = .8),
  plot.title.position = "plot",
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = "grey87", size = .7),
  panel.grid.minor = element_blank(),
  axis.line.x = element_line(color = "grey60", size = 1.1),
  legend.position = "none"
)

pal <- c("#668300", "#004da0")

ggplot(df, aes(type, gross_domestic, fill = type)) +
  stat_summary(geom = "bar", width = .75,
               aes(fill = type, fill = after_scale(lighten(fill, .4)))) +
  stat_summary(geom = "errorbar", width = .2, size = 1.3) +
  scale_x_discrete(expand = c(.23, .23)) +
  scale_y_continuous(expand = c(0, 0), 
                     labels = function(x) paste0("$", x / 10^6, "M")) +
  scale_fill_manual(values = pal) +
  coord_cartesian(clip = "off", ylim = c(0, 400*10^6)) +
  ggtitle("Domestic Box Office of\nDreamWorks Movies")

ggsave(here::here("27_educational", "barbarplot.png"), 
       width = 1100, height = 1200, res = 100,
       device = agg_png, limitsize = FALSE)

ggplot(df, aes(type_num, gross_domestic, color = type, fill = type)) +
  ggdist::stat_halfeye(
    aes(fill = type, fill = after_scale(lighten(fill, .4))),
    adjust = .5, width = .7, .width = 0, justification = -.14, point_colour = NA
  ) + 
  geom_boxplot(
    aes(fill = type, fill = after_scale(desaturate(lighten(fill, .8), .4))),
    width = .08, outlier.shape = NA, size = 1.4
  ) +
  gghalves::geom_half_point(
    aes(fill = type, fill = after_scale(darken(fill, .2))),
    side = "l", size = 5, range_scale = .65, alpha = .5, 
    width = .8, shape = 21, color = "white", stroke = 1.3) +
  scale_x_continuous(breaks = 1:2, labels = rev(unique(df$type)), 
                     expand = c(.001, .001)) +
  scale_y_continuous(expand = c(0, 0), 
                     labels = function(x) paste0("$", x / 10^6, "M")) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  coord_cartesian(clip = "off", ylim = c(0, 430*10^6)) +
  ggtitle("Domestic Box Office of\nDreamWorks Movies")

ggsave(here::here("27_educational", "raincloudplot.png"), 
       width = 1100, height = 1140, res = 100,
       device = agg_png, limitsize = FALSE)

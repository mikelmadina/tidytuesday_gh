library(tidyverse)
library(maps)

# DATUAK EKARRI

## Tenperaturak

zonann_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/zonann_temps.csv')

## Mapa osatzeko datuak

world <- map_data("world")

# DATUAK PRESTATU

datuak <- zonann_temps %>%
  select(-c(Glob, NHem, SHem, `24N-90N`, `24S-24N`, `90S-24S`)) %>%
  pivot_longer(-Year,
               names_to = "eremua",
               values_to = "tenperatura")
  # mutate(
  #   eremua = forcats::fct_relevel(
  #     as.factor(eremua),
  #     "64N-90N",
  #     "44N-64N",
  #     "24N-44N",
  #     "EQU-24N",
  #     "24S-EQU",
  #     "44S-24S",
  #     "64S-44S",
  #     "90S-64S"
  #   )
  #)

datuak_mapa <- datuak %>% 
  mutate(ymin = stringr::str_extract(eremua, "-(\\d{2}\\w{1})", group = 1),
         ymax = stringr::str_extract(eremua, "^\\d{2}\\w{1}"),
         ymin = stringr::str_replace(ymin, "(\\d{2})S", "-\\1"),
         ymin = stringr::str_replace(ymin, "(\\d{2})N", "\\1"),
         ymax = stringr::str_replace(ymax, "(\\d{2})S", "-\\1"),
         ymax = stringr::str_replace(ymax, "(\\d{2})N", "\\1"),
         ymin = as.numeric(ymin),
         ymax = as.numeric(ymax),
         xmin = ((Year - 1950) * 2.5) - 1.25,
         xmax = ((Year - 1950) * 2.5) + 1.25,
         x = ((Year - 1950) * 2.5),
         temp_lat = case_when(
           eremua == "64N-90N" ~ tenperatura * 2 + 64 + 13,
           eremua == "44N-64N" ~ tenperatura * 2 + 44 + 13,
           eremua == "24N-44N" ~ tenperatura * 2 + 24 + 10,
           eremua == "EQU-24N" ~ tenperatura * 2 + 13,
           eremua == "24S-EQU" ~ tenperatura * 2 - 13,
           eremua == "44S-24S" ~ tenperatura * 2 - 24 - 10,
           eremua == "64S-44S" ~ tenperatura * 2 - 44 - 10,
           eremua == "90S-64S" ~ tenperatura * 2 - 64 - 13
         )) %>% 
  replace(is.na(.), 0)
  

# Kolore eskaletarako balioak, https://en.wikipedia.org/wiki/Warming_stripes -tik hartuak

koloreak <- c("#2171b5", "#4292c6", "#6baed6", "#9ecae1", "#c6dbef","#deebf7","white", "#fee0d2", "#fcbba1", "#fc9272", "#fb6a4a", "#ef3b2c", "#cb181d", "#a50f15", "#67000d")

ggplot(datuak_mapa) +
  # mapa
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey75", color = "grey75", linewidth = 0.25) +
  
  # tenperatura adierazten duten marra koloreztatuak
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = tenperatura, color = NULL), alpha = 0.75) +
  
  # Eremukako 0 graduko lerroa
  annotate(geom = "rect", xmin = -172, xmax = 180, ymin = c(77, 57, 34, 13, -13, -34, -54, -77), ymax = c(77, 57, 34, 13, -13, -34, -54, -77), color = "black", linewidth = 0.15) +
  
  # Tenperaturen lerroa
  geom_line(aes(group = eremua, x = x, y = temp_lat), linewidth = 0.75, alpha = 0.7, color = "grey20") +
  
  # Eremuak bereizteko lerroak
  annotate(geom = "segment", x = -Inf, xend = Inf, y = c(64,44,24,0,-24,-44,-64), yend = c(64,44,24,0,-24,-44,-64), color = "gray75", linewidth = 0.15) +
  
  # Ardatzetako etiketak
 
  annotate(geom = "rect", xmin = -172, xmax = 180, ymin = -3, ymax = 3, fill = "white", alpha = 0.3) +
  annotate(geom = "rect", xmin = -170, xmax = -160, ymin = -90, ymax = 90, fill = "white", alpha = 0.3) +
  annotate(geom = "text", x = seq(from = -150, to = 175, by = 25), y = 0, label = seq(from = 1890, to = 2020, by = 10), size = 3.5, fontface = "bold", family = "Atkinson Hyperlegible") +
  annotate(geom = "text", x = -163, y = c(-87, -64, -44, -24, 0, 24, 44,64,87), label = c("90º S", "64º S", "44º S", "24º S", "0º", "24º N", "44º N","64º N","90º N"), size = 3.5, fontface = "bold", family = "Atkinson Hyperlegible") +
  
  # Eskalak
  scale_color_gradientn(colors = koloreak)  +
  scale_fill_gradientn(colors = koloreak) +
  scale_x_continuous(limits = c(-172,180), expand = expansion(add = c(0,0))) +
  scale_y_continuous(expand = expansion(add = c(0,0))) +
  
  # Testuak
  labs(title ="Global surface temperatures by latitude zones", 
       subtitle ="The values in the tables are deviations from the corresponding 1951-1980 means.", 
       caption ="#tidytuesday 2023-28\nMikel Madina\nGISTEMP Team, 2023: GISS Surface Temperature Analysis (GISTEMP), version 4. NASA Goddard Institute for Space Studies") + 
  
  # Bestelako egokitzapen grafikoak
  theme_void() +
  theme(text = element_text(family = "Atkinson Hyperlegible"),
        plot.margin = margin(t = 0.5, r = 1, b = 0.5, l = 1, unit = "cm"),
        plot.title = element_text(face = "bold", size = 24),
        plot.subtitle = element_text(margin = margin(t = 0.5, r = 0, b = 0.5, l = 0, unit = "cm")),
        plot.caption = element_text(hjust = 0, margin = margin(t = 0.25, unit = "cm")),
        legend.position = "none")
        
# IRUDIA GORDE

ggsave("2023-07-11_temperatures/plot.png",
       device = ragg::agg_png,
       dpi = 300,
       width = 29,
       height = 18,
       units = "cm",
       bg = "white")


















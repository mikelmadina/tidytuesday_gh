# tidytuesday 2022-20 (2022-05-17

## Trabajo con datos y otras funcionalidades
library(tidyverse)
library(tidygraph)
library(systemfonts)
library(here)
library(svglite)

# gráficos y tablas
library(ggraph)
library(ggflags)
library(ggbump)
library(gt)


##################
###   DATOS   ####
##################

# Obtener de los datos

## Datos Eurovision

eurovision <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv')
votes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision-votes.csv')

## Datos de las capitales (para iso2 con ggflag)
### Datos descargados de: https://simplemaps.com/data/world-cities

ciudades <- read_csv(here("2022-05-17_Eurovision/worldcities.csv"))

# Preparar datos

capitales <- ciudades %>% 
  filter(capital == "primary") %>% 
  mutate(country = case_when(
    country == "Czechia" ~ "Czech Republic",
    country == "Macedonia" ~ "North Macedonia",
    TRUE ~ country)) %>% 
  select(country, city, iso3, iso2)

votes_2022 <- votes %>% 
  filter(year == 2022 & semi_final == "f") %>% 
  left_join(capitales, by = c("from_country" = "country")) %>% 
  left_join(capitales, by = c("to_country" = "country")) %>% 
  filter(points > 0) %>% 
  select(from_country, to_country, jury_or_televoting, points, iso2.y)

paises_votantes <- votes_2022 %>% 
  group_by(from_country) %>% 
  summarise() %>% 
  add_row(from_country = "Serbia")

nodes <- paises_votantes %>% 
  left_join(capitales, by = c("from_country" = "country")) %>% 
  # En el dataset de ciudades hay dos cappitales para Países Bajos, y no hay datos para República Checa
  filter(city != "The Hague") %>% 
  arrange(from_country) %>% 
  mutate(id = row_number())

votes_2022 <- votes_2022 %>% 
  left_join(nodes, by = c("from_country" = "from_country")) %>% 
  rename(from = id) %>% 
  left_join(nodes, by = c("to_country" = "from_country")) %>% 
  rename(to = id) %>% 
  select(from, to, jury_or_televoting, points, iso2.y)

pesos <- votes_2022 %>% 
  group_by(to, jury_or_televoting) %>% 
  summarise(puntos = sum(points)) %>% 
  pivot_wider(names_from = jury_or_televoting, values_from = puntos)

nodes <- nodes %>% 
  left_join(pesos, by = c("id" = "to")) %>% 
  mutate(iso2 = tolower(iso2)) 

datos <- tbl_graph(nodes = nodes, edges = votes_2022, node_key = "id") %>% 
  mutate(Popularity = centrality_degree(mode = "in"),
         vota_a = centrality_degree(mode="out"))

##################
### HAIRBALLS ####
##################

# Variables

width_segment <- 0.5
size_text <- 3
colores <- c("T" = "#15b8bd", "J" = "#f5911f")

# Red jurado + televoto

hb_nagusia <- ggraph(datos) +
  geom_node_point(
    # si uno de los dos es NA, no aparece el círculo
    aes(size = ifelse(is.na(J), 0, J) + ifelse(is.na(T), 0, T)),
    color = "gray75",
    alpha = 0.5
  ) +
  geom_edge_diagonal(
    aes(colour = jury_or_televoting),
    edge_width = width_segment,
    arrow = arrow(length = unit(1.5, 'mm'), type = "closed"),
    alpha = 0.5
  ) +
  geom_node_text(aes(label = paste0(
    from_country, "\n",
    # si uno de los dos es NA, aparece NA
    ifelse(is.na(J) & is.na(T),
           "",
           ifelse(is.na(J), 0, J)
           +
           ifelse(is.na(T), 0, T))
  )),
  color = "white",
  fontface = "bold") +
  scale_size_continuous(range = c(2, 45)) +
  scale_edge_color_manual(values = colores) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none")


# Guardar PDF

ggsave(here("2022-05-17_Eurovision/hb_nagusia.pdf"),
       plot = hb_nagusia,
       height = 11.665,
       width = 12.5,
       device = cairo_pdf
)

# Red televoto

hb_tele <-
  ggraph(filter(activate(datos, edges), jury_or_televoting == "T")) +
  geom_node_point(aes(size = T), color = "grey75", alpha = 0.5) +
  geom_edge_diagonal(
    aes(colour = jury_or_televoting),
    edge_width = width_segment,
    arrow = arrow(length = unit(2, 'mm'), type = "closed"),
    alpha = 0.5
  ) +
  geom_node_text(
    aes(label = paste0(from_country, "\n", ifelse(is.na(
      T
    ), "", T))),
    color = "white",
    fontface = "bold",
    size = size_text
  ) +
  scale_size_continuous(range = c(3, 25)) +
  scale_edge_color_manual(values = colores) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none")


# Guardar PDF

ggsave(here("2022-05-17_Eurovision/hb_tele.pdf"),
       plot = hb_tele,
       height = 11.665/2,
       width = 12.5/1.95,
       device = cairo_pdf
)

# Red jurado

hb_jury <-
  ggraph(filter(activate(datos, edges), jury_or_televoting == "J")) +
  geom_node_point(aes(size = J), color = "gray75", alpha = 0.5) +
  geom_edge_diagonal(
    aes(colour = jury_or_televoting),
    edge_width = width_segment,
    arrow = arrow(length = unit(2, 'mm'), type = "closed"),
    alpha = 0.5
  ) +
  
  geom_node_text(
    aes(label = paste0(from_country, "\n", ifelse(is.na(
      J
    ), "", J))),
    color = "white",
    fontface = "bold",
    size = size_text
  ) +
  scale_size_continuous(range = c(3, 25)) +
  scale_edge_color_manual(values = colores) +
  # facet_edges( ~ jury_or_televoting) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none")


# Guardar PDF

ggsave(here("2022-05-17_Eurovision/hb_jury.pdf"),
       plot = hb_jury,
       height = 11.665/2,
       width = 12.5/1.95,
       device = cairo_pdf
)

##################
###  BARRAS   ####
##################

# Datos

votos_total <- votes_2022 %>%
  group_by(to, jury_or_televoting) %>%
  summarise(subtotal = sum(points),
            iso2 = str_to_lower(max(iso2.y))) %>% 
  mutate(
    subtotal = case_when(jury_or_televoting == "J" ~ -subtotal,
                         TRUE ~ subtotal),
    total = abs(subtotal) + abs(lag(subtotal))
  ) %>% 
  fill(total, .direction = "up") %>% 
  ungroup %>%
  arrange(desc(total)) %>%
  mutate(ranking = row_number())

votos_j <- votes_2022 %>%
  filter(jury_or_televoting == "J") %>%
  group_by(to) %>%
  summarise(total_j = sum(points),
            iso2 = str_to_lower(max(iso2.y))) %>%
  arrange(desc(total_j)) %>%
  mutate(orden = row_number())

votos_t <- votes_2022 %>%
  filter(jury_or_televoting == "T") %>%
  group_by(to) %>%
  summarise(total_t = sum(points),
            iso2 = str_to_lower(max(iso2.y))) %>%
  arrange(desc(total_t)) %>%
  mutate(orden = row_number())

# Gráficos

ranking_total <- ggplot(votos_total,
                        aes(y = fct_reorder(as.character(to), abs(subtotal), .fun = sum),
                            x = subtotal)) +
  geom_col(aes(fill = jury_or_televoting), width = 0.5) +
  geom_flag(
    data = ~ filter(.x, jury_or_televoting == "T"),
    aes(
      x = ifelse(ranking %% 2 == 0, 15,-15),
      y = as.character(to),
      country = iso2
    ),
    size = 5.5
  ) +
  scale_fill_manual(values = c("T" = "#15b8bd", "J" = "#f5911f")) +
  theme_void() +
  theme(legend.position = "none")


ggsave(
  here("2022-05-17_Eurovision/ranking_total.pdf"),
  plot = ranking_total,
  width = (max(votos_j$total_j) + max(votos_t$total_t)) / 150,
  height = 3.84,
  device = cairo_pdf
)  

ranking_j <- ggplot(votos_j, aes(y = -total_j,
                                 x = orden)) +
  geom_col(fill = "#f5911f", width = 0.5) +
  geom_flag(aes(y = 0, x = orden, country = iso2), size = 4) +
  coord_flip() +
  scale_x_reverse() +
  theme_void()


ggsave(here("2022-05-17_Eurovision/ranking_j.pdf"),
       plot = ranking_j,
       height = 3.95,
       width = max(votos_j$total_j) / 150,
       device = cairo_pdf)

ranking_t <- ggplot(votos_t, aes(y = total_t,
                                 x = orden)) +
  geom_col(fill = "#15b8bd", width = 0.5) +
  geom_flag(aes(y = 0, x = orden, country = iso2), size = 4) +
  coord_flip() +
  scale_x_reverse() +
  theme_void()


ggsave(
  here("2022-05-17_Eurovision/ranking_t.pdf"),
  plot = ranking_t,
  height = 3.84,
  width = max(votos_t$total_t) / 150,
  device = cairo_pdf
)


##################
### SIGMOIDES ####
##################

# Datos

bump_jury <- votos_total %>% 
  ungroup() %>% 
  filter(jury_or_televoting == "J") %>% 
  arrange(subtotal) %>% 
  mutate(y = row_number(),
         x = -300) %>% 
  rename(xend = subtotal) %>% 
  arrange(desc(total)) %>% 
  mutate(yend = row_number()) 

bump_tele <- votos_total %>% 
  ungroup() %>% 
  filter(jury_or_televoting == "T") %>% 
  arrange(desc(subtotal)) %>% 
  rename(x = subtotal) %>% 
  mutate(yend = row_number(),
         xend = 460) %>% 
  arrange(desc(total)) %>% 
  mutate(y = row_number())

# asier <- bump_jury %>% 
#   select(to, iso2, y) %>% 
#   left_join(bump_tele, by = c("to", "iso2")) %>% 
#   mutate(diff = y.x - yend) %>% 
#   arrange(diff) %>% 
#   ggplot(aes(diff, fct_reorder(iso2, diff))) +
#            geom_col()

# Gráficos

fun_color_range_j <- colorRampPalette(c("#f5911f", "darkorange4"))
my_colors_j <- fun_color_range_j(20)  

(p_bump_jury <- ggplot(bump_jury) +
    geom_sigmoid(aes(x = x, xend = xend, 
                     y = y * 0.985, 
                     yend = yend, 
                     group = to, color = yend),
                 alpha = 0.8) +
    scale_y_reverse() +
    scale_x_continuous(expand = c(0,0)) +
    scale_colour_gradientn(colors = my_colors_j) +
    theme_void() +
    theme(legend.position = "none")
)


ggsave(here("2022-05-17_Eurovision/bump_jury.pdf"),
       plot = p_bump_jury,
       height = 3.95,
       width = 2,
       device = cairo_pdf
)


fun_color_range_t <- colorRampPalette(c("#15b8bd", "darkslategray"))
my_colors_t <- fun_color_range_t(20)  

(p_bump_tele <- ggplot(bump_tele) +
    geom_sigmoid(aes(x = x, xend = xend, 
                     y = y, 
                     yend = yend * 0.985, 
                     group = to, 
                     color = y),
                 alpha = 0.8) +
    scale_y_reverse() +
    scale_x_continuous(expand = c(0,0)) +
    scale_colour_gradientn(colors = my_colors_t) +
    theme_void() +
    theme(legend.position = "none")
)

ggsave(here("2022-05-17_Eurovision/bump_tele.pdf"),
       plot = p_bump_tele,
       height = 3.84,
       width = 2,
       device = cairo_pdf
)

#####################
### CONCIDENCIAS ####
#####################

# Datos

coinc_dif <- votes %>% 
  filter(year == 2022 & semi_final == "f") %>% 
  group_by(from_country, to_country) %>% 
  pivot_wider(names_from = jury_or_televoting, values_from = points) %>% 
  mutate(coinciden = ifelse(J == T, TRUE, FALSE),
         difieren = ifelse(abs(J - T) > 6, TRUE, FALSE),
         diferencia = (abs(J - T))) %>% 
  ungroup() %>% 
  rename(De = from_country,
         A = to_country)

coinciden <- coinc_dif %>% 
  replace_na(list(J = 0, T = 0)) %>% 
  filter(coinciden == TRUE & J == 12) %>% 
  select(De, A, J, T) %>% 
  arrange(desc(T))

difieren_JT  <- coinc_dif %>% 
  filter(difieren == TRUE & diferencia == 12 & J == 12) %>% 
  select(De, A, J, T) %>% 
  arrange(J, De)

difieren_TJ  <- coinc_dif %>% 
  filter(difieren == TRUE & diferencia == 12 & T == 12) %>% 
  select(De, A, J, T) %>% 
  arrange(J, De)



# Gráficos

tabla_dif_JT <- gt(difieren_JT) %>% 
  cols_label(
    J = html("<span style='color:#f5911f'>Jurado</span>"),
    T = html("<span style='color:#15b8bd'>Televoto</a>")
  ) %>% 
  data_color(
    columns= J,
    apply_to = "text",
    colors = c("#f5911f")
  ) %>% 
  data_color(
      columns= T,
      apply_to = "text",
      colors = c("#15b8bd")
    ) %>% 
  tab_options(
    table.font.names = c("Gotham"),
    table.font.color = "white",
    table.background.color = "transparent",
    table.font.size = 12,
    container.width = pct(35),
    table.width = pct(30)
  ) 


gtsave(tabla_dif_JT, here("2022-05-17_Eurovision/tabla_dif_JT.pdf"))

tabla_dif_TJ <- gt(difieren_TJ) %>% 
    cols_label(
      J = html("<span style='color:#f5911f'>Jurado</span>"),
      T = html("<span style='color:#15b8bd'>Televoto</a>")
    ) %>% 
    data_color(
      columns= J,
      apply_to = "text",
      colors = c("#f5911f")
    ) %>% 
    data_color(
      columns= T,
      apply_to = "text",
      colors = c("#15b8bd")
    ) %>% 
    tab_options(
      table.font.names = c("Gotham"),
      table.font.color = "white",
      table.background.color = "transparent",
      table.font.size = 12,
      container.width = pct(35),
      table.width = pct(30)
    ) 


gtsave(tabla_dif_TJ, here("2022-05-17_Eurovision/tabla_dif_TJ.pdf"))


tabla_coinc <- gt(coinciden) %>% 
    cols_label(
      J = html("<span style='color:#f5911f'>Jurado</span>"),
      T = html("<span style='color:#15b8bd'>Televoto</a>")
    ) %>% 
    data_color(
      columns= J,
      apply_to = "text",
      colors = c("#f5911f")
    ) %>% 
    data_color(
      columns= T,
      apply_to = "text",
      colors = c("#15b8bd")
    ) %>% 
    tab_options(
      table.font.names = c("Gotham"),
      table.font.color = "white",
      table.background.color = "transparent",
      table.font.size = 12,
      container.width = pct(35),
      table.width = pct(30)
    ) 


gtsave(tabla_coinc, here("2022-05-17_Eurovision/tabla_coinc.pdf"))

## paketeak kargatu
library(tidyverse)
library(systemfonts)
library(here)
library(lubridate)
library(showtext)
library(ggtext)
library(shadowtext)
library(ggpattern)

showtext_auto()
eskubirako_asteak <- 35
windowsFonts(`Lora` = windowsFont("Lora"))

# tema presatu

theme_set(theme_minimal(base_size = 12, base_family = "Lora"))
theme_update(
  text = element_text(color = "#101010"),
  plot.background = element_rect(fill = "#e1fffe"),
  plot.margin = margin(60, 50, 30, 50),
  plot.title = element_markdown(
    size = 40,
    lineheight = 1,
    margin = margin(t = 0, b = 5),
    padding = margin(b = 20)
  ),
  plot.title.position = "plot",
  plot.subtitle = element_textbox(
    size = 20,
    color = "grey35",
    lineheight = 1.15,
    margin = margin(t = 5, b = 10),
    padding = margin(b = 20)
  ),
  plot.caption = element_markdown(
    size = 16,
    color = "grey35",
    hjust = 0.5,
    margin = margin(t = 10, b = 0)
  ),
  plot.caption.position = "plot",
  axis.title = element_blank(),
  axis.text.x = element_text(size = 10),
  axis.text.y = element_blank(),
  axis.ticks.x = element_line(color = "grey55", size = .35),
  axis.ticks.length.x = unit(.5, "lines"),
  axis.line.x = element_line(color = "grey55", size = .35),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_line(color = "lightpink1", size = .15),
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.spacing.x = unit(3.5, "lines"),
  strip.text = element_blank(),
  legend.position = "none"
)

###################################################
## Datuak ekarri
##################################################

nyt_titles <-
  readr::read_tsv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv'
  )
nyt_full <-
  readr::read_tsv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv'
  )

###################################################
## Datuak prestatu
###################################################

# NYT zerrendan aste gehien agertu diren 10 liburuak (laburpena)

liburuak_top10_laburpena <- nyt_titles %>%
  arrange(desc(total_weeks)) %>%
  head(10) %>%
  mutate(id_f = as.factor(id),
         id_f = fct_reorder(id_f, total_weeks, .desc = TRUE),)

# NYT zerrendan aste gehien agertu diren 10 liburuak (asteroko datuak)

liburuak_top10_astero <- nyt_full %>%
  #top10 ean ez daudenak iragazi
  semi_join(liburuak_top10_laburpena, by = c("title_id" = "id")) %>%
  # Astearen zenbakia, urtearekin berriro 1etik hasi gabe
  # https://stackoverflow.com/a/19780628
  mutate(total_week = week(week) + 53 * (year(week) - min(year(week)))) %>%
  arrange(title, week) %>%
  # Jarraiak diren asteentzako identifikatzaile bat sortu sortu,
  # aurrerago geom_path en group aes-ekin erabili ahal izateko
  mutate(group = ifelse(week(lag(week)) + 1 == week(week), NA, row_number())) %>%
  fill(group, .direction = "down") %>%
  mutate(group = replace_na(group, 1)) %>%
  group_by(group) %>%
  # Poligonoa zuzen itxi ahal izateko
  mutate(orden = row_number())

# poligonoa itxi ahal izateko beste bi puntu behar ditugu
## lehen puntua, aste-taldearen hasieran

polygon_points_first <- liburuak_top10_astero %>%
  group_by(group) %>%
  summarise(
    year = max(year),
    week = min(week),
    rank = 16,
    orden = 0,
    title = max(title),
    title_id = max(title_id),
    author = max(author),
    total_week = max(total_week)
  )

## azken puntua, aste-taldearen amaieran

polygon_points_last <- liburuak_top10_astero %>%
  group_by(group) %>%
  summarise(
    year = max(year),
    week = max(week),
    rank = 16,
    orden = Inf,
    title = max(title),
    title_id = max(title_id),
    author = max(author),
    total_week = max(total_week)
  )

## poligonoen puntu guztien datubasea sortu

liburuak_top10_astero_poli <-
  bind_rows(liburuak_top10_astero,
            polygon_points_first,
            polygon_points_last) %>%
  arrange(week, orden)

# Liburu jakin bat zein astetan lehen aldiz zerrendaren lehen tokian?

lehen_aldiz_1 <- liburuak_top10_astero %>%
  filter(rank == 1) %>%
  group_by(title_id) %>%
  summarise(lehen_1 = min(week),
            total_week = min(total_week))

# Lehen aldiz 1. postuan zerrendan agertu eta zenbat astera?

lehen_astea <- liburuak_top10_astero %>%
  group_by(title_id) %>%
  filter(week == min(week)) %>% 
  select(title_id, total_week)

zenbatera_1_zenbakia <- bind_rows(lehen_astea, lehen_aldiz_1) %>%
  group_by(title_id) %>%
  arrange(total_week) %>%
  mutate(zenbatera = total_week - lag(total_week)) %>%
  filter(!is.na(zenbatera)) %>%
  select(title_id, lehen_1, zenbatera)

## aldagai berria datubase nagusian atxiki
liburuak_top10_laburpena <- liburuak_top10_laburpena %>%
  left_join(zenbatera_1_zenbakia, by = c("id" = "title_id"))

# Liburuan oinarritutako filmaren estreinaldiaren astea
# (Wikipediatik hartutako datuak)

estreinaldiak <- tribble(
  ~ title_id,   ~ estreinaldia,
  "381",   NA,
  "549", "1958-12-04",
  "3343", NA,
  "4723", "1995-06-02",
  "4749", "1954-06-24",
  "4796", "2006-04-21",
  "4918", "2006-05-19",
  "5381", "2011-08-10",
  "6157", "1953-09-16",
  "7239", "2022-07-15"
) %>%
  mutate(across(title_id, as.integer),
         across(estreinaldia, as.Date)) %>%
  group_by(title_id) %>%
  transmute(estreinaldia = floor_date(estreinaldia, "weeks"))

## aldagai berria datubase nagusian atxiki
liburuak_top10_laburpena <- liburuak_top10_laburpena %>%
  left_join(estreinaldiak, by = c("id" = "title_id"))

# Liburuen azalak

img <-
  as_tibble(list.files(
    here("2022-05-10-nyt_bestsellers", "img"),
    pattern = "jpg",
    full.names = TRUE
  ))

colnames(img) <- "non"

azalak <- img %>%
  mutate(title_id = str_extract(non, "\\d+.jpg"),
         title_id = as.integer(str_extract(title_id, "\\d+")))

## aldagai berria datubase nagusian atxiki
liburuak_top10_laburpena <- liburuak_top10_laburpena %>%
  left_join(azalak, by = c("id" = "title_id")) %>%
  mutate(azala = paste0('<img src="', non, '" width="55" height="83" />'))

### Liburuen azaletako koloreak

koloreak <- tribble(
  ~ id, ~ kolorea,
  381,  "#1c81aa",
  549,  "#cb4a60",
  3343, "#3dc9c6",
  4723, "#386385",
  4749, "#535c59",
  4796, "#376155",
  4918, "#8e0201",
  5381, "#886b9b",
  6157, "#d7bc85",
  7239, "#fca16c"
)

## aldagai berria datubase nagusian atxiki
liburuak_top10_laburpena <- liburuak_top10_laburpena %>%
  left_join(koloreak)

# Idazleen argazkiak

argazkiak <- tribble(
  ~ id, ~ argazkia,
  381, "anthony_doerr.png",
  549, "patrick_dennis.png",
  3343, "dr_seuss.png",
  4723, "robert_james_waller.png",
  4749, "herman_wouk.png",
  4796, "james_redfield.png",
  4918, "dan_brown.png",
  5381, "kathryn_stockett.png",
  6157, "lloyd_c_douglas.png",
  7239, "delia_owens.png"
)

## aldagai berria datubase nagusian atxiki
liburuak_top10_laburpena <- liburuak_top10_laburpena %>%
  left_join(argazkiak) %>%
  mutate(argazkia = paste0(
    '<img src="',
    paste0(here("2022-05-10-nyt_bestsellers/img/"), "/", argazkia),
    '" width="70" height="105" />'
  ))

# Bi taulak elkartu

liburuak_top10_astero_all <- liburuak_top10_astero_poli %>%
  select(!c(author, title, year)) %>%
  left_join(liburuak_top10_laburpena, by = c("title_id" = "id"))

# Ordenak ondo jartzeko

liburuak_top10_astero_all <- liburuak_top10_astero_all %>%
  group_by(title_id) %>%
  ## hainbat geom-etan datuak erraz iragazteko aldagaia
  mutate(iragazteko = row_number())

# Asteak: liburu bakoitza zerrendan lehen eta azken aldiz noiz agertu den

aste_tartea <- liburuak_top10_astero_all %>%
  group_by(title_id) %>%
  summarise(astea_lehena = min(week),
            astea_azkena = max(week)) %>%
  mutate(
    aldea = interval(astea_lehena, astea_azkena),
    aldea = aldea %/% weeks(1)
  ) %>%
  arrange(desc(aldea)) %>%
  head(1)

# Liburu bakoitzaren X ardatzeko denbora tartea lehen liburuaren denbora tartearekin berdindu

denbora_eskalaren_mugak <- liburuak_top10_laburpena %>%
  mutate(
    hasiera = first_week - weeks(50),
    amaiera = first_week + weeks(aste_tartea$aldea) + weeks(eskubirako_asteak)
  ) %>%
  pivot_longer(c("hasiera", "amaiera")) %>%
  select(-name) %>%
  rename(week = "value") %>%
  mutate(
    amaiera = week
  ) %>%
  select(c("id_f", "week"))

## datu berriak datubase nagusian atxiki

liburuak_top10_astero_all <- liburuak_top10_astero_all %>%
  bind_rows(denbora_eskalaren_mugak)

# Etorkizuna adieraziko duen karratua marraztu ahal izateko datuak kalkulatu

datuetako_azken_astea <- max(nyt_full$week)

etorkizuna <- liburuak_top10_astero_all %>%
  group_by(id_f) %>%
  filter(!is.na(first_week)) %>%
  summarise(
    azken_astea = max(week),
    xmax = max(first_week) + weeks(max(aste_tartea$aldea)) + weeks(eskubirako_asteak),
    xmin = datuetako_azken_astea,
    ymin = -Inf,
    ymax = Inf,
    iragazteko = 1000
  ) %>%
  filter(year(azken_astea) > 2010) %>%
  pivot_longer(starts_with("x"), names_to = "ezabatu", values_to = "week") %>%
  pivot_longer(starts_with("y"), names_to = "ezabatu2", values_to = "rank") %>%
  select(-(starts_with("ezabatu") | azken_astea)) %>%
  group_by(id_f) %>%
  # laukia lortu ahal izateko hurrenkera zuzena zehaztu
  mutate(
    orden = case_when(
      week == min(week) & rank == -Inf ~ 1,
      week == min(week) & rank == Inf ~ 2,
      week == max(week) & rank == Inf ~ 3,
      week == max(week) & rank == -Inf ~ 4,
    )
  ) %>%
  arrange(id_f, orden)

## datu berriak datubase nagusian atxiki

liburuak_top10_astero_all <- liburuak_top10_astero_all %>%
  bind_rows(etorkizuna)


###################################################
## Oharrak
###################################################

oharrak <- tribble(
  ~ id_f, ~ week, ~ rank, ~ testua, ~ hjust,
  7239, ymd("2022-09-01") + weeks(30), 1, "Zer ekarriko du etorkizunak?<br />
  2022ko uztailean ***<span style='color: #fca16c'>Where the Crawdads sing</span>*** filma estreinatuko dute.<br />
  Beste hainbat liburuen kasuan, filmari esker liburuak berriro salduenen zerrendan agertu dira.<br />
  Baina ez da beti gertatzen", 0,
  4723, ymd("1995-06-02") + weeks(30), 11, "Liburuan oinarritutako 1995eko ekaineko filmak<br />liburuaren salmentak piztu zituen", 0,
  3343, ymd("1990-05-01") + weeks(2), -1 ,  "Liburua zerrendan agertu eta lehen postua lortu arte igarotako aste kopurua", 0,
  3343, ymd("1997-01-01"), 3, "Top 5", 1
)

oharren_geziak <- tribble(
  ~ id_f, ~ x, ~ xend, ~ y, ~ yend, ~ curvature,
  7239, ymd("2022-09-01") + weeks(30), ymd("2022-07-15"), 1, 6, 0.2,
  4723, ymd("1995-06-02") + weeks(30), ymd("1995-06-02"), 8, 7, 0.2,
  3343, ymd("1990-05-01") + weeks(2),  ymd("1990-04-01"), -1 , 0, 0.2,
  3343, ymd("1997-01-01"), ymd("1997-02-01"), 3, 5, -0.2
)

###################################################
## Grafikoak
###################################################

nagusia <- ggplot(data = liburuak_top10_astero_all,
                  aes(week, rank)) +
  # etorkizuneko asteak
  geom_polygon_pattern(
    data = ~ filter(.x, iragazteko == 1000),
    aes(x = week,
        y = rank,
        group = id_f),
    fill = "transparent",
    pattern = "stripe",
    pattern_fill = "grey90",
    pattern_colour = "grey90",
    pattern_density = 0.0025,
    alpha = 0.5
  ) +
  # liburuaren azala
  geom_richtext(
    data = ~ filter(.x, iragazteko == 1),
    aes(x = as.Date(-Inf),
        y = 8,
        label = azala),
    hjust = 0,
    fill = NA,
    label.color = NA
  ) +
  # idazlearen argazkia
  geom_richtext(
    data = ~ filter(.x, iragazteko == 1),
    aes(
      x = first_week + weeks(aste_tartea$aldea) + weeks(eskubirako_asteak) - weeks(20),
      y = 7,
      label = argazkia
    ),
    fill = NA,
    label.color = NA
  ) +
  # liburua zerrendan (azalera)
  geom_polygon(aes(
    x = week,
    y = rank,
    group = group,
    fill = kolorea
  ),
  alpha = 0.5) +
  # liburua zerrendan (marra)
  geom_path(
    data = ~ filter(.x,!orden %in% c(0, Inf)),
    aes(
      x = week,
      y = rank,
      group = group,
      color = kolorea
    ),
    size = 0.75,
    lineend = "round"
  ) +
  # liburuan oinarritutako filma noiz estrenatu zen
  geom_vline(
    data = ~ filter(.x, iragazteko == 1),
    aes(xintercept = estreinaldia),
    color = "grey15",
    size = 0.75,
    linetype = "11",
    alpha = 0.75
  ) +
  # liburuaren izenburua eta zenbat aste
  geom_richtext(
    data = ~ filter(.x, iragazteko == 1),
    aes(
      x = first_week - weeks(50),
      y = -5.5,
      label = paste0("**", stringr::str_to_title(title), "** (", total_weeks, " aste zerrendan)"),
      color = kolorea
    ),
    family = "Lora",
    hjust = 0,
    size = 6,
    fill = "#e1fffe",
    label.color = NA
  ) +
  # idazlearen izena
  geom_text(
    data = ~ filter(.x, iragazteko == 1),
    aes(
      x = first_week + weeks(aste_tartea$aldea) + weeks(eskubirako_asteak),
      y = -5.5,
      label = stringr::str_to_title(author),
      color = kolorea
    ),
    family = "Lora",
    hjust = 1,
    size = 6
  ) +
  # lehen aldiz lehen postuan (atzeko zirkulua)
  geom_point(
    data = ~ filter(.x, iragazteko == 1),
    aes(x = lehen_1, 
        y = 1),
    size = 8,
    color = "black",
    alpha = 0.2
  ) +
  # lehen aldiz lehen postuan (pasatako aste kopurua)
  geom_shadowtext(
    data = ~ filter(.x, iragazteko == 1),
    aes(x = lehen_1, y = 1, label = zenbatera),
    color = "black",
    bg.colour = "white",
    size = 3.5,
    fontface = "bold"
  ) +
  # lehen aldiz lehen postuan (gezia)
  geom_segment(
    data =  ~ filter(.x, iragazteko == 1 & zenbatera > 10),
    aes(
      x = first_week,
      xend = lehen_1 - weeks(5),
      y = 1,
      yend = 1
    ),
    arrow = arrow(
      angle = 20,
      type = "open",
      ends = "last",
      length = unit(0.2, "cm")
    )
  ) +
  # kameraren ikonoa
  geom_richtext(
    data = ~ filter(.x, iragazteko == 1),
    aes(
      x = estreinaldia,
      y = 12,
      label = paste0(
        '<img src="',
        here("2022-05-10-nyt_bestsellers/img/video_camera.png"),
        '" width="30" height="40" />'
      )
    ),
    hjust = 0.5,
    fill = NA,
    label.color = NA
  ) +
  # oharrak
  geom_richtext(
    data = oharrak,
    aes(week, rank, label = testua, hjust = hjust),
    size = 4.5,
    color = "#101010",
    fill = NA,
    label.colour = NA
  ) +
  # oharren geziak (ezkerrera)
  geom_curve(
    data = filter(oharren_geziak, curvature == 0.2),
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    curvature = 0.2,
    arrow = arrow(length = unit(0.03, "npc"))
  ) +
  # oharren geziak (eskuinera)
  geom_curve(
    data = filter(oharren_geziak, curvature == -0.2),
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    curvature = -0.2,
    arrow = arrow(length = unit(0.03, "npc"))
  ) +
  # testuak
  labs(title = "NYTeko liburu salduenen zerrendan<br />aste gehien azaldu diren 10 liburuak",
       subtitle = "Dr. Seussen ***<span style='color: #3dc9c6'>Oh, the Places you'll go</span>*** da zerrendan aste gehien agertu den liburua.
        Baina zerrendako beste liburuek<br />ez bezalako patroia erakusten du: 
       agerpen gehienak zerrendan agertu eta hurrengo asteetan bildu beharrean,<br />
       urtero hainbat astez berragertzen da. Patroia garbiago antzeman dadin, liburu bakoitzaren denbora-lerroa<br />
       zerrendan lehen aldiz agertu eta hamar urte bitarteko epera zabaldu da.",
       caption = "#tidytuesday 2022-19 | Datuak: Post45 Data | Datu gehigarriak eta irudiak: Wikipedia | Diseinua: Mikel Madina @neregauzak") +
  
  # ranking-a denez, Y ardatza goitik behera jarri, eta 5. tokiari dagokion marra gehitu
  scale_y_reverse(
    limits = c(16, -8),
    expand = c(0, 0),
    breaks = c(5)
  ) +
  scale_x_date(expand = c(0, 0)) +
  scale_size_identity() +
  # datubasean ezarritako koloreen kodeak zuzenean hartzeko
  scale_color_identity() +
  scale_fill_identity() +
  # free_x liburu bakoitzaren X ardatza liburuaren datei egokitzeko
  facet_wrap(vars(as.factor(id_f)), ncol = 1, scales = "free_x")

# A2 tamainako PDFa gorde

ggsave(here("2022-05-10-nyt_bestsellers/nagusia.pdf"),
  plot = nagusia,
  width = 16.54,
  height = 23.4,
  device = cairo_pdf
)


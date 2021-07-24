##------------------------------------------------------------##
##  DataFeast                                                 ##
##  Facebook:                                                 ##
##  Email: analisisyvisualizacion@gmail.com                   ##
##                                                            ##
##  Anime Genres (Horror) Feast Visualizations                ##
##------------------------------------------------------------##


# Set-up ------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggtext))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(patchwork))

## GGplot custom theme
source("../DataFeast_utilities/DataFeast_theme.R")

# Colors
colors_type <- c("Movie"="#00b0a2",
                 "TV" = "#e8ff00", 
                 # "OVA" = "#ff8ba6",
                 # "Music" = "#6096f2",
                 "Special" = "#35e400"
                 # "ONA" = "#c5d9ea"
)

sakura_colors <- c("#e9ebe7","#f0e1d8","#ba7176","#c4957a","#dfd3c3","#e4d0a5","#efb57a","#EBBCB0")
sakura <- colorRampPalette(colors = sakura_colors)

## Particular fonts
font_add("Blood", "../DataFeast_utilities/bloodlustrotal.ttf")
font_add("Sakura", "../DataFeast_utilities/garton.normal.ttf")
showtext_auto()

# Data Preparation --------------------------------------------------------

## Horror
df_horror <- read_csv("WebScrapping/dataset_anime_Horror.csv") %>% 
  mutate(
    ranked = as.numeric(str_replace(ranked, "Ranked #", "")),
    popularity = as.numeric(str_replace(popularity, "Popularity #", "")),
    links = str_replace(links, "https://myanimelist.net/anime/", ""),
    scored_by = as.numeric(scored_by),
    score = as.numeric(score)
  ) %>% 
  separate(col = links, into = c("ID_Anime", "Anime_name"), sep = "/") %>% 
  mutate(Anime_name = str_replace_all(Anime_name, "_", " "))

# Studios that make horror/Gore Anime
df_gore_studio <- df_horror %>% 
  group_by(Studio) %>% 
  summarise(unique_titles = n_distinct(ID_Anime)) %>% 
  filter(unique_titles > 5, Studio != "-") %>% 
  ungroup() %>% 
  mutate(Studio = fct_reorder(Studio, unique_titles)) %>% 
  na.omit(Studio) %>% 
  arrange(desc(unique_titles))

# Scores and Source
df_horror_group <- df_horror %>% 
  group_by(ID_Anime) %>% 
  summarise_all(first) %>% 
  mutate(title = paste0(Title_japanese, "\n", Anime_name),
         Source_class = case_when(
            Anime_source %in% c('Manga', 'Visual novel', 'Web manga') ~ 'Manga',
            Anime_source %in% c('Novel', 'Light novel', 'Book') ~ 'Novel',
            Anime_source %in% c('Game') ~ 'Game',
            Anime_source %in% c('Original') ~ 'Original',
            Anime_source %in% c('Other', 'Unknown', 'Picture book', 'Music') ~ 'Others'),
         Studio = case_when(
           Studio %in% c("Studio Pierrot,             Pierrot Plus", "Studio Pierrot,             Pierrot Plus") ~ "Studio Pierrot",
           Studio == "Madhouse,             Satelight,             Graphinica" ~ "Madhouse",
           Studio == "Millepensee,             GEMBA" ~ "Millepensee",
           TRUE ~ Studio
  ))

## MadHouse
df_madhouse <- read_csv("WebScrapping/dataset_anime_Madhouse.csv") %>% 
  mutate(
    ranked = as.numeric(str_replace(ranked, "Ranked #", "")),
    popularity = as.numeric(str_replace(popularity, "Popularity #", "")),
    links = str_replace(links, "https://myanimelist.net/anime/", ""),
    scored_by = as.numeric(scored_by),
    score = as.numeric(score)
  ) %>% 
  separate(col = links, into = c("ID_Anime", "Anime_name"), sep = "/") %>% 
  mutate(Anime_name = str_replace_all(Anime_name, "_", " ")) %>% 
  separate_rows(Genres, sep = ";") 

df_madhouse_genres <- df_madhouse %>% 
  group_by(Genres) %>% 
  count() %>% 
  filter(n > 45) %>% 
  ungroup() %>% 
  mutate(Genres = fct_reorder(Genres, n))

df_madhouse_unique <- df_madhouse %>% 
  group_by(ID_Anime) %>% 
  summarize_all(first)

## Shojo

# Visualizations ----------------------------------------------------------

## Horror

### Loliplot
img_mad <- png::readPNG("img/Mad_House.png")
img_mad <- grid::rasterGrob(img_mad, interpolate = TRUE)

gore_loliplot <- ggplot(df_gore_studio, aes(x = Studio, y = unique_titles)) +
  geom_segment(aes(x = Studio, y = 0, xend = Studio, yend = unique_titles), 
               color = "#b0c9d5", linetype = "twodash", size = 0.8, alpha = 0.9) +
  geom_point(color = redBright, size = 8, alpha = 0.9) +
  geom_text(aes(label = unique_titles), color = "black", size = 10,stat = "unique") +
  coord_flip(clip = "off") +
  scale_y_continuous(limits = c(0,50), expand = c(0.01,0)) +
  guides(fill = "none") +
  labs(x = "Estudios de Anime", y = "Número De Animes Genero Horror",
       caption = "Visualization: <b style='font-family:Norsebold;font-size:20pt'>DATA FEAST</b> | Creditos Imagén: Studio MAD HOUSE") +
  # annotation_custom(feast, xmin=-0.5, xmax=-1.5, ymin=-5, ymax=-20) +
  annotation_custom(img_mad, xmin = 0.8, xmax = 9.75, ymin = 15, ymax = 45) +
  theme(axis.text.x = element_text(family = "Lato", size = 30),
        axis.text.y = element_text(family = "Lato", size = 30),
        plot.margin = unit(c(1, 1, 1, 1), "lines"))

ggsave(plot = gore_loliplot, paste0(Sys.Date(),"FeastViz_Estudios_Horror.png"), dpi = 300)

## Scores
img_alucard <- png::readPNG("img/Hellsing.png")
img_alucard <- grid::rasterGrob(img_alucard, interpolate = TRUE)

gore_scores <- df_horror_group %>% 
  ggplot(aes(x = score, y = scored_by)) +
  geom_point(aes(size = members), color = wheat, alpha = 0.6) +
  geom_text_repel(data = filter(df_horror_group, scored_by > 3.0e05), 
                  aes(label = Anime_name), size = 10, 
                  #family = "noto", 
                  color = redBright,
                  segment.size = 0.6, xlim = c(3.5, 8.5), box.padding = 0.5, force = 2,) +
  scale_x_continuous(limits = c(2,11)) +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0,1.5e6),expand = c(0.01, 0.01)) +
  scale_size_continuous( name = "Número de veces selecionado por usuarios de MAL:",
                         breaks = c(1000, 10000, 100000, 250000, 500000),
                         labels = c("  1,000", " 10,000", "100,000", "250,000", "500,000")) +
  guides(size = guide_legend(override.aes = list(alpha = 1))) +
  labs( x = "Puntuación Promedio del Usuario", y = "Número de evaluaciones", 
        caption = "Visualization: <b style='font-family:Norsebold;font-size:20pt'>DATA FEAST</b>| Créditos imagen: Hellsing") +
  annotation_custom(img_alucard, xmin = 8.0, xmax = 12, ymin=550000, ymax = 1550000) +
  theme(axis.text = element_text(family = "Lato"),
        legend.position = c(0.15,0.5),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_text(size = 32),
        legend.text = element_text(family = "Lato", size = 30))

ggsave(plot = gore_scores, paste0(Sys.Date(),"FeastViz_Puntuacion_Horror.png"), dpi = 300, width = 10)

## Source
img_another <- png::readPNG("img/another_per.png")
img_another <- grid::rasterGrob(img_another, interpolate = TRUE)

colors_source <- c("Manga" = "#e8e3e2", "Game" = "#006ead", "Original" = "#ffda86",
                   "Novel" = "#dd003e", "Others" = "#5ac624")

gore_scores_bysource <- df_horror_group %>% 
  ggplot(aes(x = score, y = scored_by)) +
  geom_point(aes(color = Source_class), alpha = 0.3) +
  geom_text_repel(data = filter(df_horror_group, scored_by > 0.9e05 & Source_class == "Novel"), 
                  aes(label = Anime_name), size = 12,
                  #family = "Lato",
                  color = waterRal,
                  segment.size = 0.4, xlim = c(4.0, 9.0), box.padding = 0.9, force = 5) +
  scale_x_continuous(limits = c(4,9)) +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0,1.5e6), expand = c(0.01, 0.01)) +
  scale_color_manual(values = colors_source) +
  guides(color = guide_legend(title = "Fuente del Anime", override.aes = list(alpha = 1), ncol = 2)) +
  labs( x = "Puntuación Promedio del Usuario", y = "Número de evaluaciones", 
        caption = "Visualization: <b style='font-family:Norsebold;font-size:20pt'>DATA FEAST</b> | Créditos imagen: Another") +
  annotation_custom(img_another, xmin = 4.1, xmax = 7.3, ymin= 10000, ymax = 2400000) +
  theme(axis.text = element_text(family = "Lato"),
        legend.position = c(0.20,0.40),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_text(size = 35, hjust = 0.5, color = greyLight),
        legend.text = element_text(family = "Lato", size = 30))

ggsave(plot = gore_scores_bysource, paste0(Sys.Date(),"FeastViz_Fuente_Horror.png"), dpi = 300)

## Berserk
img_guts <- png::readPNG("img/Berserk.png")
img_guts <- grid::rasterGrob(img_guts, interpolate = TRUE)

gore_scores_berserk <- df_horror_group %>% 
  ggplot(aes(x = score, y = scored_by)) +
  geom_point(data = filter(df_horror_group, !str_detect(Anime_name, "Berserk")), alpha = 0.3, color = greyMetal) +
  geom_point(data = filter(df_horror_group, str_detect(Anime_name, "Berserk")), aes(color = Type), alpha = 0.9) +
  geom_text_repel(data = filter(df_horror_group, str_detect(Anime_name, "Berserk")), 
                  aes(label = Anime_name), size = 11,
                  #family = "Lato",
                  color = redBright,
                  segment.size = 0.6, xlim = c(5.5, 10.0), box.padding = 0.8, force = 5, nudge_y = 1e4) +
  scale_x_continuous(limits = c(3,9.5)) +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0,1.5e6), expand = c(0.01, 0.01)) +
  scale_color_manual(values = colors_type) +
  guides(color = guide_legend(title = "Tipo De Anime", override.aes = list(alpha = 1), ncol = 1)) +
  labs( x = "Puntuación Promedio del Usuario", y = "Número de evaluaciones", 
        caption = "Visualization: <b style='font-family:Norsebold;font-size:20pt'>DATA FEAST</b> | Créditos imagen: Berserk (RIP Kentaro Miura)") +
  annotation_custom(img_guts, xmin = 2.5, xmax = 5.5, ymin=1.4e5, ymax = 1.5e6) +
  theme(axis.text = element_text(family = "Lato"),
        legend.position = c(0.50,0.85),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_text(size = 35, hjust = 0.5, color = greyLight, face = "bold"),
        legend.text = element_text(family = "Lato", size = 30, face = "bold"))

ggsave(plot = gore_scores_berserk, paste0(Sys.Date(),"FeastViz_Horror_AnimeBerserk.png"), dpi = 300, width = 10)

## Best Horror Animes by Studio
gore_studios_score <- df_horror_group %>% 
  filter(score > 6 & scored_by > 100000)  %>% 
  mutate(Studio = fct_reorder(Studio, score)) %>% 
  ggplot(aes(x = Studio, y = scored_by)) +
  geom_jitter(color = wheat, show.legend = FALSE, size = 3, width = 0.1) +
  geom_text_repel(data = df_horror_group %>%
                    filter(score > 6 & scored_by > 100000) %>%
                    group_by(Studio) %>% 
                    slice_max(order_by = scored_by, n = 1),
                  aes(label = Anime_name), size = 11, 
                  #family = "Lato",
                  color = greyMetal,
                  segment.size = 0.3, box.padding = 0.2, force = 5, nudge_y = 1e5) +
  labs(x= "Estudios de Anime", y = "Número de evaluaciones", 
       caption = "Visualization: <b style='font-family:Norsebold;font-size:20pt'>DATA FEAST</b> | Data Source: My Anime List (MAL) 2021 Genre: Horror", 
       title = "Animes mejor evaluados por Estudio") +
  guides(fill = "none") +
  # coord_flip(clip = "off") +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, 1.5e6)) +
  theme(axis.text = element_text(family = "Lato"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 100, hjust = 0.5))

ggsave(plot = gore_studios_score, paste0(Sys.Date(),"FeastViz_AnimesHorror_Studios.png"), dpi = 300, width = 15)

### FullPanel
infogra <- (gore_loliplot | gore_scores_bysource | gore_scores_berserk) / 
  gore_studios_score + plot_annotation(title = "A n i m e s   d e l   G é n e r o   H o r r o r ", theme = 
                                         theme(plot.title = element_text(size = 200, face = "bold", hjust = 0.5, color = "#8a0303", 
                                                                         family = "Blood")))

ggsave(plot = infogra, paste0(Sys.Date(),"FeastViz_AnimesHorror.png"), dpi = 300, width = 22, height = 16)

## Madhouse (Sakura)
img_sak <- png::readPNG("img/Sakura_1.png")
img_sak <- grid::rasterGrob(img_sak, interpolate = TRUE)

madhouse <- ggplot(df_madhouse_genres, aes(x = Genres, y = n)) +
  geom_col(aes(fill = Genres), color = "#ab9560", size = 1.5) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 150), expand = c(0.01, 0)) +
  scale_fill_manual(values = sakura(9)) +
  guides(fill = "none") +
  labs(x ="Generos populares", y = "Numero de Animes") +
  annotation_custom(img_mad, xmin = 0.5, xmax = 2.75, ymin = 75, ymax = 145) +
  annotation_custom(img_sak, xmin = 2.25, xmax = 5.25, ymin = 80, ymax = 150) +
  theme(
    text = element_text(color = "#9f2148", family = "Sakura", face = "bold"),
    panel.border = element_rect(size = 0.75),
    plot.background = element_rect(color = "#dedfcf", fill = "#dedfcf"),
    legend.background = element_rect(colour = NA, fill = "#dedfcf"),
    legend.key = element_rect(color = "#dedfcf", fill = "#dedfcf"),
    legend.box.background = element_blank(),
    legend.position = c(0.2,0.25),
    legend.text = element_text(family = "Lato", size = 20),
    legend.title = element_text(family = "Lato", size = 25),
    axis.text = element_text(color = "#9f2148", size = 50),
    axis.title.x = element_text(size = 75),
    axis.title.y = element_text(size = 75),
    plot.caption = element_markdown(size = 25, 
                                    hjust = 1, vjust = 0, margin = margin(t = 10), colour = "#001b1e", family = "Lato")
  )
ggsave(plot = madhouse, "FeastViz_MadHouse_studio.png", dpi = 300)

## Sakura
img_sak <- png::readPNG("img/Sakura_2.png")
img_sak <- grid::rasterGrob(img_sak, interpolate = TRUE)

plot_scores_sakura<- df_madhouse_unique %>% 
  ggplot(aes(x = score, y = scored_by)) +
  geom_point(aes(size = members), color = "#9f2148", alpha = 0.35) +
  geom_text_repel(data = df_madhouse_unique %>% filter(scored_by > 5e5 | str_detect(Title_english, "Cardcaptor Sakura")),
                  aes(label = Anime_name),
                  force = 5, box.padding = 0.5, xlim = c(7,10.0), color = "#21cbfb", size = 12, family = "Lato") +
  guides(size = guide_legend(override.aes = list(alpha = 1.0))) +
  labs(x = "Puntuacion Promedio del Usuario", y = "Numero de puntuaciones",
       caption = "Visualization: <b style='font-family:Norsebold;font-size:35pt'>DATA FEAST</b> | Creditos imagen: Sakura CardCaptor | Data Source: My Anime List (MAL) July 2021 Genero Shojo y Animes de Madhouse' Studios"
  ) +
  # Scales 
  scale_y_continuous(expand = c(0.01, 0.01), limits = c(0, 2.1e6), labels = scales::comma) +
  scale_x_continuous(limits = c(5,10)) +
  scale_size_continuous(name = "Times listed by MAL users:",
                        breaks = c(1000, 10000, 100000, 250000, 1000000), 
                        labels = c("  1,000", " 10,000", "100,000", "250,000", "1,000,000")) +
  annotation_custom(img_sak, xmin = 4.0, xmax = 8.0, ymin = 0.9e6, ymax = 2.1e6) +
  coord_cartesian(clip = "off") +
  theme(
    text = element_text(color = "#9f2148", family = "Sakura", face = "bold"),
    panel.border = element_rect(size = 0.75),
    plot.background = element_rect(color = "#dedfcf", fill = "#dedfcf"),
    legend.background = element_rect(colour = NA, fill = "#dedfcf"),
    legend.key = element_rect(color = "#dedfcf", fill = "#dedfcf"),
    legend.box.background = element_blank(),
    legend.position = c(0.2,0.25),
    legend.text = element_text(family = "Lato", size = 20),
    legend.title = element_text(family = "Lato", size = 25),
    axis.text = element_text(color = "#9f2148", size = 50),
    axis.title.x = element_text(size = 75),
    axis.title.y = element_text(size = 75),
    plot.caption = element_markdown(size = 25, 
                                    hjust = 1, vjust = 0, margin = margin(t = 10), colour = "#001b1e", family = "Lato")
  )
ggsave(plot = plot_scores_sakura, "FeastViz_Madhouse_Animes.png", dpi = 300)

## FullPanel
sakura_madhouse <- madhouse + plot_scores_sakura + plot_annotation(title = "Animes Madhouse' Studios y del Genero Shojo (Sakura CardCaptor)",
                                                                   theme = theme(
                                                                     plot.background = element_rect(color = "#dedfcf", fill = "#dedfcf"),
                                                                     plot.title = element_text(size = 135, face = "bold", hjust = 0.5,
                                                                                               family = "Sakura", color = "#9f2148")
                                                                   ))

ggsave(plot = sakura_madhouse, paste0(Sys.Date(),"FeastViz_MadhouseSakura.png"), dpi = 300, width = 15, height = 7)

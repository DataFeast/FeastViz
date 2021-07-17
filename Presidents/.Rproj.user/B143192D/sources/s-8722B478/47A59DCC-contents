##------------------------------------------------------------##
##  Omar Rosas Bringas                                        ##
##  analisisyvisualizacion@gmail.com                          ##
##  DataFeast                                                 ##
##  Presidents and Ministers incomes visualization            ##
##------------------------------------------------------------##

# Set-up ------------------------------------------------------------------
library(tidyverse)
library(showtext)
library(patchwork)
library(ggtext)

regions_colors <- c("Europe"="#005eb4", 
                    "South America"="#d30080", 
                    "Asia"="#ffcf00", 
                    "Africa"="#ff6e00", 
                    "Oceania"="#ff0015", 
                    "North America"="#00a63d")

## Fonts
font_add_google(name = "Poppins", family = "Poppins")
font_add_google("Roboto Mono", "Roboto Mono")
font_add("Norsebold", "../DataFeast_utilities/Norsebold.otf")
showtext_auto()

# Custom Theme ------------------------------------------------------------
theme_set(theme_light(base_size = 25, base_family = "Poppins"))

# Data --------------------------------------------------------------------
df_incomes <- read.csv("https://raw.githubusercontent.com/DataFeast71/Data_explorations/main/Salaries_HeadGovernment/data/Presidents_data_clean.csv", header = TRUE, stringsAsFactors = FALSE)

## World tiles
df_world_tile <- read_csv("https://gist.githubusercontent.com/maartenzam/787498bbc07ae06b637447dbd430ea0a/raw/9a9dafafb44d8990f85243a9c7ca349acd3a0d07/worldtilegrid.csv") %>% 
  mutate(
    ## Namibias two-digit country code is handled as `NA` 
    alpha = if_else(name == "Namibia", "NA", alpha.2),
    ## We are oing to split America into "North" and South"
    region = if_else(region == "Americas", sub.region, region),
    region = if_else(region %in% c("Northern America", "Central America", "Caribbean"), "North America", region),
    region = if_else(region == "Southern America", "South America", region)
  )

## Complete data set
df_all <- df_incomes %>% 
  full_join(df_world_tile, by = c("Country" = "name")) %>% 
  filter(!is.na(Country))

## Map tiles
map_regions <- df_all %>% 
  ggplot(aes(x = x, y = y, fill = region, color = region)) +
  geom_tile(color = "white", show.legend = FALSE) +
  scale_y_reverse() +
  scale_fill_manual(values = regions_colors) +
  #scale_fill_uchicago(guide = "none") +
  coord_equal() +
  theme(line = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background =  element_rect(fill = "transparent", color = "transparent"),
        panel.border = element_rect(color = "transparent"),
        strip.background = element_rect(color = "gray20"),
        axis.text = element_blank(),
        plot.margin = margin(0,0,0,0)) +
  labs(x = NULL, y = NULL)

## President data
df_president <- df_all %>% 
  filter(!is.na(region), !is.na(HeadOfState)) %>% 
  group_by(region) %>% 
  mutate(president_income_median = median(HeadOfState, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(Country, HeadOfState, Status, Continent, region, president_income_median) %>%
  mutate(region = fct_reorder(region, -president_income_median))

world_president  <- df_president %>% 
  summarize(median_presidents = median(HeadOfState, na.rm = TRUE)) %>% 
  pull(median_presidents)

arrows_president <- tibble(
  x1 = c(6.0, 4.7, 2.5, 1.8, 1.6, 5.8),
  x2 = c(5.6, 5.0, 2.95, 2.2, 0.9, 6),
  y1 = c(3e6, 1e4, 2e9, 6e3, 6e3, 1e4),
  y2 = c(world_president, 61392, 9.6e+09, 39924,33600, 67903)
)

## Ministers Data
df_ministers <- df_all %>% 
  filter(!is.na(region), !is.na(HeadGoverment)) %>% 
  group_by(region) %>% 
  mutate(goverment_income_median = median(HeadGoverment, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(Country, HeadGoverment, StatusGoverment, Continent, region, goverment_income_median) %>% 
  mutate(region = factor(region, levels = c("Europe", "South America", "Asia", "Africa", "Oceania", "North America")))

world_ministers  <- df_ministers %>% 
  summarize(median_ministers = median(HeadGoverment, na.rm = TRUE)) %>% 
  pull(median_ministers)

arrows_ministers <- tibble(
  x1 = c(1.7, 4.55, 2.7),
  x2 = c(2.5, 4.0, 2.95),
  y1 = c(2e4, 1e4, 1e6), # stard
  y2 = c(world_ministers, 4.5e4,1.55e6) # end
)

# Visualization -----------------------------------------------------------
gplot_president <- ggplot(df_president, aes(x = region, y = HeadOfState, color = region))+
  ## custom
  coord_flip() +
  scale_y_continuous(trans = "log10", limits = c(1e3,1.5e10) ,labels = scales::dollar, expand = c(0.005,0.005)) +
  scale_color_manual(values = regions_colors) +
  scale_x_discrete(labels = c("Europa", "América del Sur", "Asia", "Africa", "Oceania", "América del Norte")) +
  labs(x = NULL, y = "Ingreso Anual (USD)", title = "Representantes de Estado (Presidentes, Realeza, Sultanes, etc.)") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 40, family = "Roboto Mono", hjust = 0.5),
    axis.title = element_text(size = 30),
    axis.text.x = element_text(family = "Roboto Mono", size = 28, color = "black"),
    axis.text.y = element_text(family = "Roboto Mono", size = 28, color = "black"),
    panel.grid = element_blank()
  ) + 
  ## Geoms
  geom_segment(aes(x = region, xend = region, y = world_president, yend = president_income_median), size = 0.8) +
  geom_hline(aes(yintercept = world_president), color = "grey60", size = 0.6) +
  geom_jitter(position = position_jitter(seed = 2021, width = 0.2), size = 2, alpha = 0.3) +
  stat_summary(fun = median, geom = "point", size = 5) +
  ## Annotations
  annotate("text", label = glue::glue("Ingreso Mundial:\n{ scales::dollar(round(world_president, 1))} dólares"),
           x = 6.2, y = 3e6, family= "Poppins", size = 7, color = "gray10", lineheight=0.5) +
  annotate("text", label = "Mediana Continental",
           x = 4.55, y = 5e3, family= "Poppins", size = 7, color = "gray10") +
  annotate("text", label = "Países por Continente",
           x = 1.7, y = 6e3, family= "Poppins", size = 7, color = "gray10") +
  annotate("text", label = glue::glue("Mexico\n{scales::dollar(67903)}"),
           x = 5.6, y = 1e4, family= "Poppins", size = 7, color = "gray10", lineheight = 0.5) +
  annotate("text", label = glue::glue("Arabia Saudita es el país con el\nrepresentante que tiene el ingreso más grande\n{scales::dollar(9600000000)}"),
           x = 2.25, y = 5e8, family= "Poppins", size = 7, color = "gray10", lineheight = 0.5) +
  ### Arrows
  geom_curve(data = arrows_president, aes(x = x1, y = y1, xend=x2, yend=y2),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.5, color = "gray20", curvature = -0.4) +
  ## Map Tiles
  annotation_custom(ggplotGrob(map_regions), xmin = 2.8, xmax = 7.8, ymin = 7.3, ymax=10.1)

ggsave(plot = gplot_president, "FeastViz_Presidentes.png")

### Ministers

gplot_ministers <- ggplot(df_ministers, aes(x = region, y = HeadGoverment, color = region))+
  ## custom
  coord_flip() +
  scale_y_continuous(trans = "log10", limits = c(1e3,2e6) ,labels = scales::dollar, expand = c(0.005,0.005)) +
  scale_x_discrete(labels = c("Europa", "América del Sur", "Asia", "Africa", "Oceania", "América del Norte")) +
  scale_color_manual(values = regions_colors) +
  labs(x = NULL, y = "Ingreso Anual (USD)", caption = "Visualization: <b style='font-family:Norsebold;font-size:30pt;'>DATA FEAST</b> | Data: Head of State and Government",
       title = "Representantes de Gobierno (Primer Ministro, Canciller, etc)") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 40, family = "Roboto Mono", hjust = 0.5),
    axis.title = element_text(size = 30),
    axis.text.x = element_text(family = "Roboto Mono", size = 28, color = "black"),
    axis.text.y = element_text(family = "Roboto Mono", size = 28, color = "black"),
    plot.caption = element_markdown(size = 25, hjust = 1, vjust = 1, colour = "black", 
                                family = "Poppins"),
    panel.grid = element_blank()
  ) + 
  ## Geoms
  geom_segment(aes(x = region, xend = region, y = world_ministers, yend = goverment_income_median), size = 0.8) +
  geom_hline(aes(yintercept = world_ministers), color = "grey60", size = 0.6) +
  geom_jitter(position = position_jitter(seed = 2021, width = 0.2), size = 2, alpha = 0.3) +
  stat_summary(fun = median, geom = "point", size = 5) +
  ## Annotations
  annotate("text", label = glue::glue("Ingreso Mundial:\n{ scales::dollar(round(world_ministers, 1))} dólares"),
           x = 1.5, y = 2.1e4, family= "Poppins", size = 7, color = "gray10", lineheight=0.5) +
  annotate("text", label = "Mediana Continental",
           x = 4.45, y = 1e4, family= "Poppins", size = 7, color = "gray10") +
  annotate("text", label = glue::glue("Singapur is el país con el\nPrimer ministro con mayor ingreso\n{scales::dollar(1610000)} dólares"),
           x = 2.51, y = 7e5, family= "Poppins", size = 7, color = "gray10", lineheight = 0.5) +
  ### Arrows
  geom_curve(data = arrows_ministers, aes(x = x1, y = y1, xend=x2, yend=y2),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.5, color = "gray20", curvature = -0.4)

ggsave(plot = gplot_ministers, "FeastViz_Ministros.png")

## inforgraphic

infografic <- gplot_president + gplot_ministers + plot_layout(nrow = 1)
ggsave(plot = infografic, "FeastViz_IngresosGobierno.png", width = 15)


df_ministers %>%
  filter(StatusGoverment == "President")
 
unique(df_ministers$StatusGoverment)

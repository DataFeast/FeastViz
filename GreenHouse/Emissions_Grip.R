##------------------------------------------------------------##
##  DataFeast                                                 ##
##  Facebook:                                                 ##
##  Email: analisisyvisualizacion@gmail.com                   ##
##                                                            ##
##  GreenHouse Emissions America Continent                    ##
##------------------------------------------------------------##


# Set-up ------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(treemapify))
suppressPackageStartupMessages(library(geofacet))
suppressPackageStartupMessages(library(colorspace))
suppressPackageStartupMessages(library(ggtext))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(showtext))

## Fonts
font_add_google(name = "Montserrat",family = "Montserrat")
font_add_google(name = "PT Sans", family = "PT")
font_add("Norsebold", "../DataFeast_utilities/Norsebold.otf")
showtext_auto()

## Colors 
emissions_Sector <- c("Agriculture" = "#368e11", "Construction" = "#7c5c2b", "Energy\nElectricity" = "#0093ff", "Industry" = "#da8100", "Transportation" = "#d20047")

# Custom Theme
theme_set(theme_void())

theme_update(
  strip.text = element_blank(),
  panel.spacing = unit(0.25, "lines"),
  plot.margin = margin(rep(20, 4)),
  plot.background = element_rect(color = "#fffbf1", fill = "#fffbf1"),
  legend.position = "none"
)


# World TIle Data prearation ----------------------------------------------

df_world_tile <- read_csv("https://gist.githubusercontent.com/maartenzam/787498bbc07ae06b637447dbd430ea0a/raw/9a9dafafb44d8990f85243a9c7ca349acd3a0d07/worldtilegrid.csv") %>% 
  mutate(
    ## Namibias two-digit country code is handled as `NA` 
    alpha = if_else(name == "Namibia", "NA", alpha.2),
    ## We are oing to split America into "North" and South"
    region = if_else(region == "Americas", sub.region, region),
    region = if_else(region %in% c("Northern America", "Central America", "Caribbean"), "North America", region),
    region = if_else(region == "Southern America", "South America", region),
    ## to join both data set, we need a Id column
    country_code = alpha.3
  ) %>% 
  filter(str_detect(region,"America"))

America_grid <- df_world_tile %>% 
  select(x, y, alpha.2, name) %>% 
  rename("col" = x,
         "row" = y,
         "code" = alpha.2)

# Emissions Data Preparation --------------------------------------------------------

emisions <- read_csv("Data/historical_emissions.csv") %>% 
  filter(Gas == "All GHG", !Sector %in% c("Total including LUCF", "Total excluding LUCF")) %>% 
  select(Country, Sector, `2018`) %>% 
  mutate(Country = case_when(
    Country == "Antigua and Barbuda" ~ "Antigua & Barbuda",
    Country == "Saint Kitts and Nevis" ~ "St. Kitts & Nevis",
    Country == "Saint Lucia" ~ "St. Lucia",
    Country == "Saint Vincent and the Grenadines" ~ "St. Vincent & the Grenadines",
    Country == "Trinidad and Tobago" ~ "Trinidad & Tobago",
    Country == "United States" ~ "United States of America",
    TRUE ~ Country
  )) 

emissions_grip <- emisions %>% 
  filter(Country %in% df_world_tile$name) %>% 
  mutate(Sector_New = case_when(
    Sector %in% c("Agriculture", "Land-Use Change and Forestry") ~ "Agriculture",
    Sector %in% c("Industrial Processes", "Fugitive Emissions", "Waste", "Other Fuel Combustion", "Bunker Fuels") ~ "Industry",
    Sector %in% c("Manufacturing/Construction", "Building") ~ "Construction",
    Sector %in% c("Energy", "Electricity/Heat") ~ "Energy\nElectricity",
    TRUE ~ Sector
  ),
  Sector_New = factor(Sector_New)) %>% 
  group_by(Country, Sector_New) %>% 
  summarize(`2018` = sum(`2018`, na.rm = TRUE)) %>% 
  group_by(Sector_New) %>% 
  mutate(total_emm = sum(`2018`, na.rm = TRUE)) %>% 
  group_by(Country) %>% 
  mutate(
    total_country = sum(`2018`, na.rm = TRUE),
    rel = `2018` / total_emm
  ) %>% 
  left_join(df_world_tile[,c(1:3)], by = c("Country"="name")) %>% 
  rename("country" = alpha.2)


# Visualization -----------------------------------------------------------

grid <- ggplot(emissions_grip, aes(area = `2018`, fill = Sector_New, subgroup = country)) +
  geom_treemap(aes(alpha = rel), color = NA, start = "topleft", layout = "scol") +
  geom_text(aes(x = 0, y = 1, label = glue::glue("{country}")),
            alpha = 0.25, color = "grey20", stat = "unique", size = 24) +
  facet_geo(~ country, grid = America_grid) +
  scale_fill_manual(values = emissions_Sector) +
  # scale_size(range = c(2,20)) +
  scale_alpha(range = c(0.3, 1)) +
  theme(
    plot.margin = margin(t = 7, r = 20, b = 7, l = 200)
  )

## Legend
df_legend <- tibble(
  category = rep(factor(unique(emissions_grip$Sector_New)), 10),
  rel = rep(seq(-0.1, 0.8, by = 0.1), each = 5)
)

max_country <-  emissions_grip %>% 
  group_by(Sector_New) %>% 
  filter(rel == max(rel))

text_caption <- "<span style='font-family:Montserrat;'><b>Estados Unidos de América</b> es el país con la emisión de gases más grande de todo el continente Americano. Emitieron más del 50% de gases invernadero del total del continenente en todos los sectores con excepción de Agricultura. Se observa que del Sector <i style='color:#0093ff;'>Energia y Electricidad</i>, los EEUU emiten el 70.2% de los gases totales y un 69.0% por el sector <i style='color:#d20047;'>Transporte</i>. <b>Brasil</b>, por otro lado, es uno de los países con mayor emision en el sector <i style='color:#368e11;'>Agricultura, uso de Suelo y silvicultura</i> con 42.8% -por la deforestación en mi opinión.<br>En el caso particular de <b>México</b>, emite <b>9.3 veces menos</b> que EEUU y su contribución en la emision de gases es menor del 10% en cada uno de los Sectores siendo el de la <i style='color:#da8100;'>Industria</i> el más alto. </span><br><br><span>Visualization: <b style='font-family:Norsebold;font-size:16pt;'>Data Feast</b> | Data: Climate Data Explorer (Greenhouse gasses emissions)</span>"

legend <- ggplot(df_legend, aes(x = rel, y = fct_rev(category))) +
  # Each category
  geom_tile(aes(fill = category, color = after_scale(darken(fill, 0.15, space = "HLS")),
                alpha = rel), size = 0.25) +
  #Horizontal lines for sector
  geom_hline(data = tibble(y = -0.5:5.5), aes(yintercept = y), color = "white", size = 0.8) +
  # Arrows for Realtive Scale
  geom_segment(data = tibble(x = c(0.15, 0.6), xend = c(-0.05, 0.80)),
               aes(x = x, xend = xend, y = 5.85, yend = 5.85),
               inherit.aes = FALSE, color = "grey30",
               size = 0.4, arrow = arrow(length = unit(0.2, "lines"), type = "closed")) +
  geom_text(data = tibble(x = c(-0.13, 0.83), h = c(0,0.3), label = c("-.05%", "80%")),
            aes(x = x, y = 5.80, label = label, hjust = h),
            inherit.aes = FALSE, family = "Montserrat", color = "grey30", size = 4.2) +
  # Add  points for each country
  geom_point(data = emissions_grip, aes(x = rel, y = as.numeric(fct_rev(Sector_New))),
             shape = 21, color = "transparent", fill = "white", size = 3) +
  geom_point(data = emissions_grip, aes(x = rel, y = as.numeric(fct_rev(Sector_New))),
             shape = 1, size = 3, alpha = 0.3) +
  geom_text(data = max_country, aes(x = rel, y = Sector_New, label = country),
            inherit.aes = FALSE, family = "Montserrat", color = "black", size = 4.0, nudge_y = 0.25, fontface = "bold") +
  geom_text(data = max_country, aes(x = rel, y = Sector_New, label = glue::glue("{format(rel*100, digits = 3)}%")),
            inherit.aes = FALSE, family = "Montserrat", color = "black", size = 3.5, nudge_y = -0.25) +
  labs(title = "¿Cuáles Países del Contienent Americano emitieron más Gases Invernadero en 2018?",
       caption = text_caption) +
  annotate("text", x = 0.35, y = 6.4, label = "Emisiones Relativas de los Países Americanos",
           family = "PT", color = "grey45", size = 5.2) +
  scale_color_manual(values = emissions_Sector) +
  scale_fill_manual(values = emissions_Sector) +
  scale_alpha(range = c(0.3, 1)) +
  scale_x_continuous(expand = c(0.005, 0.005)) +
  scale_y_discrete(expand = c(0.5, 0.5), position = "right", labels= c("Agriculture" = "Agricultura", "Construction" = "Construcción", "Energy\nElectricity" = "Energía\nElectricidad", "Industry" = "Industria", "Transportation" = "Transporte")) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(rep(0,4)),
    plot.title = element_textbox(hjust = 0.5, face = "bold", color = "grey35",
                                 lineheight = 1.2, size = 27, margin = margin(b = 0, t = 0),
                                 width = unit(6.5, "inches"), family = "Montserrat"),
    panel.spacing = unit(0, "lines"),
    plot.caption = element_textbox(color = "grey30", size = 14, hjust = 0,
                                   lineheight = 1.4, margin = margin(b = 0, t = -10),
                                   width = unit(7.5, "inches")),
    axis.text.y = element_text(size = 12, family = "Montserrat", face = "bold", color = rev(darken(emissions_Sector, .2)), hjust = 0)
  )

America <- ggdraw(grid) +
  draw_plot(legend, x = 0.245, y = 0.24, width = 0.45, height = 0.47, hjust = 0.5, vjust = 0.5)

filename <- paste0(Sys.Date(), "_FeastViz_GreenHouseEmissions.pdf")

ggsave(plot = America, filename = filename, width = 17, height = 15, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = filename,
  filenames = str_replace(filename, "pdf", "png"),
  dpi = 300, format = "png"
)
  

##------------------------------------------------------------##
##  DataFeast                                                 ##
##  Facebook:                                                 ##
##  Email: analisisyvisualizacion@gmail.com                   ##
##                                                            ##
##  Fossils Fuel grow use                                     ##
##------------------------------------------------------------##



# Set-up ------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(colorspace))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(ggtext))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(showtext))
suppressPackageStartupMessages(library(patchwork))

# Colors
scale_fossils <- colorRampPalette(colors = c("#88b489", "#a9bf9c", "#b0bcac","#a0aa9d", "#899b98", "#836c62", "#673c24", "#583218"))

# Fonts
font_add_google("Raleway", "Raleway")
font_add_google("Lato", "Lato")
font_add_google(name = "Lato", family = "Lato-Bold", bold.wt = 700)
font_add("Norsebold", "../DataFeast_utilities/Norsebold.otf")
showtext_auto()

# Functions to plots
theme_viz <- function(caption) {
  list(
    ## Annotations
    annotate("text", x = 3.05, y = 40000, label = "40000 TWh", size = 4.5, 
             family = "Raleway", color = "grey65", hjust = 0, vjust = 0.3),
    annotate("text", x = 3.05, y = 0.5, label = "0 TWh", size = 4.5, 
             family = "Raleway", color = "grey65", hjust = 0, vjust = 0.3),
    ## Add a base point
    geom_point(aes(y = 0.5), shape = "-", color = "grey65", size = 8, stat = "unique"),
    ## Scales
    coord_cartesian(clip = "off"),
    scale_y_continuous(trans = "log10", expand = c(0,0), limits = c(0.5, 40000)),
    scale_x_discrete(expand = c(0.3,0.3), position = "top"),
    scale_color_gradientn(trans = "log10", colours = scale_fossils(100), guide = "none"),
    ## Caption
    labs(caption = caption),
    ## Theme
    theme_void(),
    theme(
      # Panel
      panel.grid.major.x = element_line(color = "grey85", size = 1),
      # Plot
      plot.margin = margin(t = 15, r = 0, b = 15, l = 150),
      plot.caption = element_markdown(family = "Lato", size = 9, color = "grey40", hjust = 1,
                                      margin = margin(t = 25, b = 0, r = 50, )),
      plot.caption.position = "plot",
      # Axis
      axis.text.x = element_text(color = "grey50", size = 14, family = "Raleway", margin = margin(t=5, b=10))
  ))
}


# Data Preparation --------------------------------------------------------

df <- read_csv("Data/fossil-fuel-primary-energy.csv") %>% 
  pivot_wider(id_cols = c(Entity, Code), names_from = Year, values_from = `Fossil Fuels (TWh)`) %>% 
  select(Entity, `1965`, `1992`, `2019`) %>% 
  mutate(
    relative_first = abs(((`1992`- `1965`)/`1965`)*100),
    relative_second = abs(((`2019`- `1992`)/`1992`)*100),
    relative_second_sign = ((`2019`- `1992`)/`1992`)*100,
    relative_total = ((`2019`- `1965`)/`1965`)*100,
    relative_total = if_else(is.na(relative_total), ((`2019`- `1992`)/`1992`)*100, relative_total)
  ) %>% 
  pivot_longer(cols = -c(Entity, relative_first, relative_second, relative_second_sign, relative_total),
               names_to = "year",
               values_to = "Fossils") %>% 
  filter(
    !is.na(Fossils),
    !str_detect(Entity, "World|Europe|North America")
  )

# Visualization (Countries that reduce) -----------------------------------

# TextBox
title <- "Doce Países han reducido el uso de combustibles fosiles como fuente de energía a niveles inferiores registrados en 1965"

text_box <- "De acuerdo a los datos de Energia por Combustibles Fosiles de <i>BP Statistical Review of World Energy</i>, solamente una docena de paises de lo 80 registrados presentaron una reduccion en el uso de estos combustibles.<br><br><br> Esta reduccion significa que presentan menores niveles de usos combustibles (en TWh) al primer registro el cual es en 1965, o el año intermedio, 1992. <br><br><br><br> De estos paises, cinco redujeron el uso de combustibles a niveles inferiores que en 1965 y son Europeos. Mientras que los siete restantes países mostraron una reduccion a comparación de los niveles en 1992. <br><br><br><br> En el caso particular de México -que es de mi interes- se observa un incremento de <b>365%</b> en el primer periodo mientras que en el segundo solo crecio un <b>58.63%</b> <br><br><br><br>Incluso si algunos países muestran una reducción en el uso de estos combustibles, se ve opacado con el los paises que tienen un incremento grande o que producen una gran cantidad de energía por esta fuente."

fossils_reduce <- df %>% 
  filter(Entity != "Mexico", relative_total < 0) %>% 
  ggplot(aes(x = year, y = Fossils, group = Entity)) +
  # Plotting those that increase in the first half
  geom_line(data = df %>% filter(relative_total >= 0, year <= 1992),
            aes(color = relative_first), alpha = 0.3, size = 0.95) +
  # Plotting those that increase in the second half
  geom_line(data = df %>% filter(relative_total >= 0, year >= 1992),
            aes(color = relative_second), alpha = 0.3, size = 0.95) +
  # PLotting those that decrease
  geom_line(data = df %>% filter(relative_total < 0, year <= 1992),
            aes(color = relative_first *4, 
                color = after_scale(darken(color, 0.2, space = "HLS"))),
            size = 1.5) +
  # Plotting those that decrease in second half
  geom_line(data = df %>% filter(relative_total < 0, year >= 1992),
            aes(color = relative_second *4,
                color = after_scale(darken(color, 0.2, space = "HLS"))),
            size = 1.5) +
  # PLotting Mexico
  geom_line(data = df %>% filter(Entity == "Mexico"), color = "#006e44", size = 2) +
  ## Adding points
  # Points that increase
  geom_point(data = df %>% filter(relative_total >= 0, year < 1992),
             aes(color = relative_first), size = 2.0) +
  geom_point(data = df %>% filter(relative_total >= 0, year >= 1992),
             aes(color = relative_second), size = 2) +
  # Points that descrease
  geom_point(shape = 21, fill = "white", color = "transparent", size = 4, stroke = 2) +
  geom_point(data = df %>% filter(relative_total < 0, year < 1992),
             aes(color = relative_first*4,
                 color = after_scale(darken(color, 0.2, space = "HLS"))),
             shape = 21, stroke = 2, fill = NA, size = 4) +
  geom_point(data = df %>% filter(relative_total < 0, year >= 1992),
             aes(color = relative_second*4,
                 color = after_scale(darken(color, 0.2, space = "HLS"))),
             shape = 21, stroke = 2, fill = NA, size = 4) +
  # Plotting Mexico
  geom_point(data = df %>% filter(Entity == "Mexico"), size = 5, color = "#006e44") +
  ## Label countries that decrease
  geom_text_repel(data = df %>%  filter(relative_total < 0, year == 1965),
                  aes(label = Entity), hjust = 1, nudge_x = -0.11, size = 4.5, family = "Lato", direction = "y",
                  force = 0.5, min.segment.length = 0, segment.size = 0.5, fontface = "bold") +
  # Right Side
  geom_text_repel(data = df %>%  filter(relative_total < 0, year == 2019, !is.na(relative_first)),
                  aes(label = glue::glue("{format(abs(relative_total), digits = 3)}% ↓")),
                  hjust = 0, nudge_x = 0.1, nudge_y = 0.02, size = 4.5, direction = "y", 
                  force = 0.5, min.segment.length = 0, segment.size = 0.5, fontface = "bold") +
  # Label Countries that not hace data from 1965
  geom_text_repel(data = df %>% filter(relative_total < 0, year == 2019, is.na(relative_first)),
                  aes(label = glue::glue("{Entity} {format(abs(relative_total), digits = 3)}% ↓")),
                  hjust = 0, nudge_x = 0.15, nudge_y = -0.01, size = 4.5, 
                  direction = "y", force = 0.5, min.segment.length = 0, segment.size = 0.5) +
  # Label for Mexico
  geom_richtext(aes(x = "1992", y = 1200,
                    label = "<b style='font-size:21pt;font-family:Lato;color:#006e44;'>Mexico</b><br><b style='color:#600000';>365.01% ↑</b>"),
                color = "grey15", nudge_x = -0.45, size = 4.5, fontface = "bold", stat = "unique",
                inherit.aes = FALSE, fill = NA, label.colour = NA) +
  geom_richtext(aes(x = "2019", y = 1500, label = "<br><b style='color:#600000';>58.63% ↑</b>"),
                color = "grey15", nudge_x = -0.35, size = 4.5, fontface = "bold", stat = "unique",
                inherit.aes = FALSE, fill = NA, label.colour = NA) +
  ## Add Box text
  geom_textbox(aes(x = "1965", y = 3.5e4, label = title), 
               family = "Raleway", size = 5.5, box.colour = NA, fill = NA, nudge_x = -1.1, vjust = 0.7, 
               width = unit(3.1, "inch"), lineheight = 1.25, stat = "unique", inherit.aes = FALSE) +
  geom_textbox(aes(x = "1965", y = 1e4, 
                   label = text_box),
               family = "Raleway", size = 4.0, box.colour = NA, fill = NA, nudge_x = -1.1, vjust = 1,
               width = unit(3.1, "inch"),lineheight = 1.4, stat = "unique", inherit.aes = FALSE) +
  theme_viz(caption = "Visualization: <span style='font-family:Norsebold;'><b style='font-size:14pt'>Data Feast </b></span> | Fuente Datos: <i>Fossil fuel consumption</i>. Our World in Data 12-Jul-2021")

file_name <- paste0(Sys.Date(), "_FeastViz_Fossils_reduce.pdf")
ggsave(fossils_reduce, filename = file_name, height = 13.5, width = 10, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = file_name,
  filenames = str_replace(file_name, "pdf", "png"),
  format = "png", dpi = 300
)

# Visualization (Countries that reduce in second period) ------------------

countries_reduced <- df %>% filter(relative_total < 0) %>% distinct(Entity) %>% pull()

title <-   "Por otro lado, una docena de paises presentaron crecimiento en el uso de combustibles Fosiles en un inicio pero luego ha disminuido en el perido de 1992 a 2019."

text_box <- "Doce paises presentaron un crecimeinto en el uso de combustibles fosiles en el periodo de 1965 a 1992 y posteriormente redujeron su uso para 2019. Sin embargo, esta reducción no fue suficente en comparación con lo reportado para 1965. <br><br> Con excepción de Venezuela -que disminuyo en <b>10.88%</b> -, todos los paises son del contienente Europeo. No obstante, aun con una reducción en el uso de estos combustibles fosiles, la producción de energia proveniente de ellos lo hace aun dependientes."

fossils_second <- df %>% 
  filter(!(Entity %in% countries_reduced), relative_second_sign < 0) %>% 
  ggplot(aes(x = year, y = Fossils, group = Entity)) +
  ## Plotting those that increase in both periods, Each Line with their respective value
  geom_line(data = df %>% filter((relative_second_sign >= 0 & year <= 1992) | (Entity %in% countries_reduced)),
            aes(color = relative_first), alpha = 0.3, size = 0.95) +
  # Second period
  geom_line(data = df %>% filter((relative_second_sign >= 0 & year >= 1992) | (Entity %in% countries_reduced)),
            aes(color = relative_second), alpha = 0.3, size = 0.95) +
  ## Ploting those that reduce in second periord
  geom_line(data = df %>% filter(relative_second_sign < 0, year <= 1992, !(Entity %in% countries_reduced)),
            aes(color = relative_first*4,
                color = after_scale(darken(color, 0.1, space = "HLS"))), size = 1.5) +
  geom_line(data = df %>% filter(relative_second_sign < 0, year >= 1992, !(Entity %in% countries_reduced)),
            aes(color = relative_second*4,
                color = after_scale(darken(color, 0.1, space = "HLS"))), size = 1.5) +
  ## Adding Points
  # Points that increase in both periods
  geom_point(data = df %>% filter((relative_second_sign >= 0 & year <= 1992) | (Entity %in% countries_reduced)),
             aes(color = relative_first), size = 1.5) +
  geom_point(data = df %>% filter((relative_second_sign >= 0 & year >= 1992) | (Entity %in% countries_reduced)),
             aes(color = relative_second), size = 1.5) +
  # Points that reduce in second period
  geom_point(shape = 21, fill= "white", color = "transparent", size = 4, stroke = 2) +
  geom_point(data = df %>% filter(relative_second_sign < 0, year <= 1992, !(Entity %in% countries_reduced)),
             aes(color = relative_first*4,
                 color = after_scale(darken(color, 0.1, space = "HLS"))), shape = 21, stroke = 2, fill = NA, size = 4) +
  geom_point(data = df %>% filter(relative_second_sign < 0, year >= 1992, !(Entity %in% countries_reduced)),
             aes(color = relative_second*4,
                 color = after_scale(darken(color, 0.1, space = "HLS"))), shape = 21, stroke = 2, fill = NA, size = 4) +
  ## Labels countries
  geom_text_repel(data = df %>% filter(relative_second_sign < 0, year == 1965, !(Entity %in% countries_reduced)),
                  aes(label = Entity), hjust = 1, nudge_x = -0.10, direction  = "y", min.segment.length = 0, 
                  segment.size = 0.5, force = 0.5, fontface = "plain", size = 4.5) +
  geom_text_repel(data = df %>% filter(relative_second_sign < 0, year == 2019, !(Entity %in% countries_reduced), !is.na(relative_first)),
                  aes(label = glue::glue("{format(relative_second, digits = 3)}% ↓")),
                  hjust = 1, nudge_x = 0.5, direction  = "y", min.segment.length = 0, 
                  segment.size = 0.5, force = 0.5, fontface = "plain", size = 4.5) +
  ## Adding textBoxes
  geom_textbox(aes(x = "1965", y = 3e1, label = title),
               family = "Raleway", size = 5.5, box.colour = NA, fill = NA, nudge_x = -1.1, vjust = 0.6, width = unit(3.5, "inch"),
               lineheight = 1.25, stat = "unique", inherit.aes = FALSE) +
  geom_textbox(aes(x = "1965", y = 1e1, label = text_box),
               family = "Raleway", size = 3.0, box.colour = NA, fill = NA, nudge_x = -1.1, vjust = 1, width = unit(3.1, "inch"),
               lineheight = 1.4, stat = "unique", inherit.aes = FALSE) +
  theme_viz(caption = "Visualization: <span style='font-family:Norsebold;'><b style='font-size:14pt'>Data Feast </b></span> | Data: Fossil fuel consumption. Our World in Data 12/07/2021")

file_name <- paste0(Sys.Date(), "_FeastViz_Fossils_reduced_second.pdf")

ggsave(plot = fossils_second, filename = file_name, height = 13.5, width = 10, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = file_name,
  filenames = str_replace(file_name, "pdf", "png"),
  format = "png", dpi = 300
)

# Visualization (Countries that show high growth) -------------------------

# Threshold for Fossil use
upperq <- quantile(df$Fossils, na.rm = TRUE)[4]
iqr <- IQR(df$Fossils, na.rm = TRUE)

threshold_fossils <- (iqr*1.5) + upperq

# Threshold for relative change
upperq <- quantile(df$relative_total, na.rm = TRUE)[4]
iqr <- IQR(df$relative_total, na.rm = TRUE)

threshold_relative <- (iqr*1.5) + upperq

countries_high <- df %>% filter(relative_total >= threshold_relative | Fossils >= threshold_fossils, year == "2019") %>%
  distinct(Entity) %>% pull()


title <- "En cambio, menos de 20 paises presentaron un gran crecimiento y uso de los combustibles fosiles"

text_box <- "De los 80 paises, siete destacaron un creciemiento superior a los demás paises, donde incrementaron el uso de los combustibles fosiles arriba de <b>3000%</b> del periodo de 1965 a 2019. Entre ellos, Qatar y los Emiratos Arabes Unidos tuvieron un aumento de <b>39703.6%</b> y <b>155618.1%</b>, respectivamente. <br><br><br> Mientras que otros once paises, no presentan un crecimiento fuera de lo normal a los restantes paises, si destacan por el <b>alto</b> consumo de combustibles fosiles, como es China, EEUU, Rusia, India y Japón que se encuentran en niveles superiores de los 2000 TWh. Por cada TWh son 588427.6 barriles de crudo y cada barril son 160 litros, osea un chingo. <br><br><br>Destaca que ni México ni un pais de Latinoamerica presente un alto uso de combustibles fosiles o creciemiento."


fossils_high <- df %>% 
  filter(Entity %in% countries_high) %>% 
  ggplot(aes(x = year, y = Fossils, group = Entity)) +
  # Show those with not a high increase
  geom_line(data = df %>% filter(!(Entity %in% countries_high)),
            aes(color = abs(relative_total)), alpha = 0.25, size = 0.95) +
  # Show those with higher increase 
  geom_line(aes(color = abs(relative_total)*10,
                color = after_scale(darken(color, 0.1, space = "HLS"))),
            size = 0.95) +
  # Plotting points
  # Countires with not high increase
  geom_point(data = df %>% filter(!(Entity %in% countries_high)),
             aes(color = abs(relative_total)), size = 1.5) +
  # Countries with high increase
  geom_point(shape = 21, fill = "white", color = "transparent", size = 3.0, stroke = 2) +
  geom_point(aes(color = abs(relative_total)*10,
                 color = after_scale(darken(color, 0.1, space = "HLS"))),
             shape = 21, fill = NA, size = 3.0, stroke = 2) +
  ## Adding labels
  geom_text_repel(data = df %>% filter(Entity %in% countries_high, year == "1965"),
                  aes(label = Entity), hjust = 1, nudge_x = -0.11, direction = "y", min.segment.length = 0, segment.size = 0.5,
                  size = 4.0, fontface = "bold") +
  geom_text_repel(data = df %>% filter(Entity %in% countries_high, year == "2019", !is.na(relative_first), relative_total > 0),
                  aes(label = glue::glue("{format(relative_total, digits = 3)}% ↑")), hjust = 1,
                  nudge_x = 0.40, direction = "y", min.segment.length = 0, segment.size = 0.5,
                  size = 3.0) +
  geom_text_repel(data = df %>% filter(Entity %in% countries_high, year == "2019", is.na(relative_first)),
                  aes(label = glue::glue("{Entity} {format(abs(relative_total), digits = 3)}% ↓")), hjust = 1,
                  nudge_x = 0.99, direction = "x", min.segment.length = 0, segment.size = 0.5,
                  size = 4.0, fontface = "bold") +
  geom_text_repel(data = df %>% filter(Entity %in% countries_high, year == "2019", !is.na(relative_first), relative_total < 0),
                  aes(label = glue::glue("{format(abs(relative_total), digits = 3)}% ↓")), hjust = 1,
                  nudge_x = 0.49, nudge_y = -0.045, direction = "y", min.segment.length = 0, segment.size = 0.5,
                  size = 3.0, fontface = "bold") +
  ## Textbox
  ## Adding textbox 
  geom_textbox(aes(x = "2019", y = 2e2, label = title),
               family = "Raleway", size = 5.5, box.colour = NA, fill = NA, nudge_x = 1.15, vjust = 0.6, width = unit(3.5, "inch"),
               lineheight = 1.25, stat = "unique", inherit.aes = FALSE) +
  geom_textbox(aes(x = "2019", y = 9e1, label = text_box),
               family = "Raleway", size = 3.0, box.colour = NA, fill = NA, nudge_x = 1.1, vjust = 1, width = unit(3.5, "inch"),
               lineheight = 1.5, stat = "unique", inherit.aes = FALSE) +
  theme_viz(caption = "Visualization: <span style='font-family:Norsebold;'><b style='font-size:14pt;'>Data Feast</b></span> | Data: Fossil fuel consumption. Our World in Data 12/07/2021") +
  theme(
    plot.margin = margin(t = 15, r = 150, b = 0, l = 0)
  )

file_name <- paste0(Sys.Date(), "_FeastViz_FossilsHigh.pdf")
ggsave(fossils_high, filename = file_name, height = 13.5, width = 10, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = file_name,
  filenames = str_replace(file_name, "pdf", "png"),
  format = "png", dpi = 300
)

# Infographics ------------------------------------------------------------

info_fossils <- fossils_reduce | fossils_second | fossils_high

ggsave(plot = info_fossils, paste0(Sys.Date(), "_FeastViz_FossilsPanel.pdf"), width = 30, height = 13.5, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = "img/2021-07-29_FeastViz_FossilsPanel.pdf",
  filenames = "2021-07-29_FeastViz_FossilsPanel.png",
  format = "png", dpi = 300
)


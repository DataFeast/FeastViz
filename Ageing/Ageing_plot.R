##------------------------------------------------------------##
##  DataFeast                                                 ##
##  Facebook:                                                 ##
##  Email: analisisyvisualizacion@gmail.com                   ##
##                                                            ##
##  Ageing                                                    ##
##------------------------------------------------------------##

# Set-up ------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggblur))
suppressPackageStartupMessages(library(ggtext))
suppressPackageStartupMessages(library(colorspace))
suppressPackageStartupMessages(library(ragg))
suppressPackageStartupMessages(library(showtext))

# Fonts
font_add_google(name = "Neuton", family = "Neuton") #Nautraface 2 Display Titling
font_add_google(name = "Oldenburg", family = "Oldenburg") #InputSansCondensed
font_add_google(name = "Changa", family = "Changa") 
font_add("Norsebold", "../DataFeast_utilities/Norsebold.otf")
showtext_auto()

# Theme
theme_set(theme_void())

theme_update(
  plot.background = element_rect(fill = "black"),
  plot.margin = margin(t=70, r = 100,b = 40, l = 100),
  plot.caption = element_markdown(family = "Neuton", color = "grey70",
                                  size = 18, hjust = 0.5, lineheight = 1.1, margin = margin(t=100)),
  plot.caption.position = "plot"
)

# Data Preparation --------------------------------------------------------

df_Class <- read_delim("Data/anage_data.txt", delim = "\t") %>% 
  rename("Name" = "Common name",
         "Age" = "Maximum longevity (yrs)",
         "Quality" = "Data quality") %>% 
  select(Kingdom, Class, Name, Age, Quality) %>% 
  filter(
    !is.na(Age),
    !Class %in% c("Saccharomycetes", "Chromadorea", "Branchiopoda"),
    Quality %in% c("acceptable", "high")
  ) %>% 
  group_by(Class) %>% 
  top_n(Age, n = 20) %>% 
  ungroup()

## Labels
df_labs <- df_Class %>% 
  group_by(Class) %>% 
  top_n(Age, n = 1) %>% 
  ungroup() %>% 
  mutate(Class = fct_reorder(Class, Age)) %>% 
  plyr::ddply(., c("Class")) %>% 
  mutate(
    id = row_number(),
    label = Class, #if_else(id %% 2 == 0, glue::glue("{Class}"), glue::glue("{Class}"))
  )

Species <- tribble(
  ~Specie, ~Common, ~Img,
  #-------|--------|----,
  "Lampetra fluviatilis", "Lamprea de río", "river_lamprey.png",
  "Protopterus annectens", "Pez pulmonado<br>africano", "",
  "Lasius niger", "Hormiga negra", "garden_ant.png",
  "Polypterus senegalus", "Bichir de Senegal", "GreyBichir.png",
  "Lepisosteus osseus", "Gar de nariz larga", "Longnose_Gar.png",
  "coelacanths", "Celacanto", "Celacanto.png",
  "Lophochroa leadbeateri", "Cacatúa rosa", "Pink_cockatoo.png",
  "Homarus americanus", "Langosta Americana", "American_lobster.png",
  "Proteus anguinus", "Proteo", "Olm2.png",
  "Acipenser fulvescens", "Esturión de lago", "Lake_Sturgeon.png",
  "Chelonoidis nigra", "Tortuga Galápagos", "Galapagos_tortoise.png",
  "Strongylocentrotus<br>franciscanus", "Erizo de Mar Rojo", "Red_sea_urchin.png",
  "Sebastes aleutianus", "Pez de roca<br>de ojos ásperos", "Rougheye_rockfish.png",
  "Balaena mysticetus", "Ballena Boreal", "Bowhead_whale.png",
  "Somniosus microcephalus", "Tiburón de Groenlandia", "Greenland_shark.png",
  "Arctica islandica", "Almeja de Islandia", "quahog clam.png",
  "Pinus longaeva", "Pino longevo, Matusalén", "Pinus_longaeva.png"
)

## Text
df_text <- df_Class %>% 
  group_by(Class) %>% 
  top_n(Age, n = 1) %>% 
  ungroup() %>% 
  mutate(
    Class = fct_reorder(Class, Age),
    label = glue::glue("<b style='font-size:12pt'>{Name}</b><br><i style='color:#b4b1b1'>{scales::comma(Age)} años <br>~{round(Age*12,1)} meses</i>")
  ) %>% 
  arrange(Age) %>% 
  cbind(Species) %>% 
  mutate(
    label2 = glue::glue("<img src='./img/{Img}' width='100'/><br><b style='font-size:12pt'>{Specie}<br>{Common}</b><br><i style='color:#b4b1b1'>{scales::comma(Age)} años <br>~{round(Age*12,1)} meses</i>"),
    Age = if_else(Age > 1000, 800, Age),
    ClassLab = c(0.9, 2,3.1,3.9,5.05, 6:8, 9.0, 10, 11, 12, 13.1,14.25,15,16, 17),
    Age = c(0,20, 29,34,38,48,90,100,105,140,177,200,215,220,400,507,835)
  )

# Visualization -----------------------------------------------------------
position <- position_jitter(0.4, seed = 2021)

title_text <- "<span style='font-size:70pt;line-height:18pt;'>Longevidad de distintas Especies biológicas</span><br><br><span style='font-family:Neuton;'>¿Cuántos años viven algunos de los seres vivos más longevos? Según datos de <i><b>AnAge Database</b></i>, estas son las especies más longevas decada una de las 17 clases (categoría taxonómica) mostradas aqui.<br>Existen reportes de especies como <b><i>Pinus longaeva</i></b> o pino de conos erizados, el cual se estima que puede vivir hasta 5,062 años, 10 veces más que algunos de los animales más longevos de su clase como la almeja de Islandia, <b><i>Arctica islandica</i></b> o el tiburón de Groenlandia (<b><i>Somniosus microcephalus</i></b>)."

# Longevidad maxima en años de especies en veinte diferentes clases taxonómicas con una calidad de datos 'Aceptable' de acuerdo a <i><b>AnAge database</b></i>. Se señalan las especies más longevas de cada clase.<br> Hay que remarcar la especie <b><i>Pinus longaeva</i></b> o pino de conos erizados, que puede vivir <b>5,062</b> años, 10 veces más que el animal con más edad reportado.</span>

p <- df_Class %>% 
  mutate(
    Age = if_else(Age > 1000, 800, Age),
    Class = fct_reorder(Class, Age)
  ) %>% 
  ggplot(aes(x = Class, y = Age, color = Class, fill = Class)) +
  ## Tick mark for Classes
  geom_text(data = df_labs, aes(y = -0.5, label = "|"),
            family = "Changa", fontface = "bold", size = 4, vjust = 1) +
  ## Sparkling points
  geom_point_blur(aes(y = Age, size = Age, blur_size = Age, 
                      color = Class, color = after_scale(lighten(color, 0.4, space = "HLS"))),
                  blur_steps = 150, position = position) +
  ## Add Lines by species
  geom_linerange(aes(x = Class, xmax = Class, ymin = 0, ymax = Age,
                     color = Class, color = after_scale(desaturate(color, 0.3)),
                     alpha = Age),
                 size = 0.25, position = position) +
  ## Points in the base
  geom_point(aes(y = 0), shape = 17, size = 0.5,
             position = position) +
  ## Points for Species
  geom_point(aes(y = Age, size = Age),
             position = position) +
  ## Labels
  geom_richtext(data = df_labs, aes(y = -15, label = label, color = Class,
                                    color = after_scale(lighten(color, 0.25))),
                size = 5, family = "Oldenburg", fontface = "italic", fill = NA, label.color = NA, vjust = 0.5) +
  ## Label Oldest Species
  geom_richtext(data = df_text, aes(x = ClassLab, label = label2, y = Age + 70),
                size = 3.2, family = "Oldenburg", lineheight = 1.4, fill = NA, label.color = NA, nudge_y = 1.5, nudge_x = 0) +
  ## Title
  geom_textbox(data = tibble(
    Class = 6.5, Age = 500, label = title_text),
  aes(x = Class, y = Age, label = label), inherit.aes = FALSE, size = 12, family = "Neuton",
  color = "grey70", lineheight = 1.7, width =  unit(13, "inch"), hjust = 0.5, vjust = 0, fill = NA, box.colour = NA, ) +
  ## Scales 
  coord_cartesian(clip = "off") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0.002, 0.002)) +
  scale_color_manual(values =viridis::turbo(25), guide = "none") +
  scale_fill_manual(values =viridis::turbo(25), guide = "none") +
  scale_size(range = c(0.001, 4), guide = "none") +
  scale_alpha(range = c(0.22, 1), guide = "none") +
  scale_blur_size_continuous(range = c(1,15), guide = "none") +
  labs(caption = "Visualization: <span style='font-family:Norsebold;font-size:18pt;'>Data Feast</span> | Data by: AnAge • The Animal Ageing and Longevity Database")

fileName <- paste0(Sys.Date(), "_FeastViz_Ageing.pdf")

ggsave(plot = p, filename = fileName, width = 30, height = 20, device = cairo_pdf, limitsize = FALSE)

pdftools::pdf_convert(
  pdf = fileName,
  format = "png",
  filenames = str_replace(fileName, "pdf", "png")
)





##------------------------------------------------------------##
##  DataFeast                                                 ##
##  Facebook:                                                 ##
##  Email: analisisyvisualizacion@gmail.com                   ##
##                                                            ##
##  K-Pop Google Trends                                       ##
##------------------------------------------------------------##

# Set-up ------------------------------------------------------------------
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(colorspace))
suppressPackageStartupMessages(library(ggfx))
suppressPackageStartupMessages(library(ggtext))
suppressPackageStartupMessages(library(showtext))
suppressPackageStartupMessages(library(patchwork))
suppressPackageStartupMessages(library(ragg))

## Colors
cols <- c("#f4a7bb", # Blackpink
          "#ff0015", # RedVelvet
          "#ffc496" # TWICE
)

background <- "#000000"

## Particular fonts
font_add("BlackPink", "../DataFeast_utilities/Primetime.ttf")
font_add("Twice", "../DataFeast_utilities/Raisonne DemiBold.ttf")
font_add("RedVelvet", "../DataFeast_utilities/RedVelvet-Regular.otf")
font_add("Norsebold", "../DataFeast_utilities/Norsebold.otf")
font_add_google("Cinzel Decorative", "Cinzel_D")
font_add_google("Work Sans", "Work Sans")
showtext_auto()

## Theme
theme_custom <- function() {
  theme_void() +
  theme(
    # Legend 
    legend.position  = "none",
    # Panel 
    panel.background =  element_rect(fill = "transparent", color = "transparent"),
    panel.grid.major.x = element_line(color = "grey70", linetype = "13", size = 0.5),
    # Plot
    plot.background = element_rect(fill = background),
    plot.margin = margin(t = 10, r = 30, b = 10, l = 30),
    plot.caption = element_markdown(lineheight = 1.5, color = "grey100", hjust = 0.5, margin = margin(t = 30, b = 0), family = "Work Sans"),
    # Axis
    axis.text.x = element_text(family = "Cinzel_D", color = "grey90", size = 12, face = "bold", margin = margin(t = 6))
  )
}

theme_set(theme_custom())

# General Functions -------------------------------------------------------
#Function to read and prepare data using a specific date to filter
google_trend <- function(file, date){
  df <- read_csv(file, skip = 2)
  colnames(df) <- c("Month", "Blackpink", "Twice", "Redvelvet") 
  df <- df %>% 
    gather(key = "Query", value = "interest", -Month) %>% 
    mutate(Month = as.Date(paste0(Month, "-01"))) %>% 
    # Add a specfic date
    filter(Month >= date) %>% 
    mutate(interest = if_else(interest == "<1", "0", interest),
           interest = as.numeric(interest))
  return(df)
}

# This function will be used to prepare data for ribbon plot
data_ribbon <- function(data) {
  df <- data %>% 
    group_by(Month) %>% 
    summarise(
      min = min(interest),
      max = max(interest),
      diff = max - min,
      Query = max == interest
    ) %>% 
    slice(1)
  return(df)
} 

# This function will be used to make GeomLines for each group
lines_group <- function(data){
  list(
    with_blur(
      geom_line(data = filter(data, Query == "Blackpink"),
                colour = cols[1], size = 3.5),
      colour = lighten(cols[1], .1), sigma = 5),
    with_blur(
      geom_line(data = filter(data, Query == "Redvelvet"),
                colour = cols[2], size = 3.5),
      colour = lighten(cols[2], .1), sigma = 5),
    with_blur(
      geom_line(data = filter(data, Query == "Twice"),
                colour = cols[3], size = 3.5),
      colour = lighten(cols[3], .1), sigma = 5)
  )
}

# Data Preparation --------------------------------------------------------
df_kpop <- google_trend(file = "DataSet/BlackPink_Twice_RedVelvet.csv", date = "2014-07-01")

df_ribbon_complete <- df_kpop %>% 
  group_by(Month) %>% 
  summarise(
    min = min(interest),
    max = max(interest),
    diff = max - min,
    Query = if_else(max == interest, Query, "FALSE")    
  ) %>% 
  filter(Query != "FALSE")

## Particular Ribbons
df_ribbon_velvet <- df_kpop %>% 
  filter(Query == "Redvelvet") %>% 
  group_by(Month) %>% 
  summarise(
    min = min(interest),
    max = max(interest),
    diff = max - min,
    Query = max == interest
  ) %>% 
  mutate(Query = "RedVelvet")

df_ribbon_twice <- df_kpop %>% 
  filter(Query %in% c("Twice", "Redvelvet")) %>% 
  group_by(Month) %>% 
  summarise(
    min = min(interest),
    max = max(interest),
    diff = max - min,
    Query = max == interest    
  ) %>% 
  slice(1) %>% 
  mutate(Query = if_else(isTRUE(Query), "Twice", "RedVelvet"))

df_ribbon_blackpink<- df_kpop %>% 
  filter(Query %in% c("Twice", "Blackpink")) %>% 
  group_by(Month) %>% 
  summarise(
    min = min(interest),
    max = max(interest),
    diff = max - min,
    Query = max == interest
  ) %>% 
  slice(1) %>% 
  ## Custom changes
  mutate(Query = if_else(isTRUE(Query), "Blackpink", "Twice"),
         Query = if_else(Month < "2016-08-01", "Twice2", Query),
         Query = if_else(Month >= "2016-08-01" & Month <= "2016-09-01", "Blackpink2", Query),
         Query = if_else(Month >= "2016-10-01" & Month <= "2016-11-01", "Twice", Query),
  ) %>%
  ungroup() %>%
  add_row(Month= as.Date(c("2016-7-15", "2016-7-15", "2016-9-10", "2018-05-12","2018-05-23")),
          min = c(14, 14, 11, 19,38),
          max = c(14, 14, 11, 38, 51),
          diff= c(14, 14, 0, 0, 0),
          Query= c("Twice2", "Blackpink2", "Twice", "Twice", "Blackpink"))


Kpop <- ggplot(df_kpop, aes(x = Month, y = interest, color = Query)) +
  geom_ribbon(data = df_ribbon_complete, 
              aes(x = Month, ymin = max, ymax = Inf),
              fill = "black", color = "black", size = 5, inherit.aes = FALSE) +
  geom_ribbon(data = df_ribbon_twice,
              aes(x = Month, ymin = min, ymax = max, color = Query,
                  fill = after_scale(darken(desaturate(color, .1), .3, space = "HLS"))),
              alpha = 0.8, inherit.aes = FALSE) +
  geom_ribbon(data = df_ribbon_velvet, 
              aes(x = Month, ymin = min, ymax = max, color = Query,
                  fill = after_scale(darken(desaturate(color, .1), .3, space = "HLS"))),
              alpha = 0.8, inherit.aes=FALSE) +
  geom_ribbon(data = df_ribbon_blackpink,
              aes(x = Month, ymin = min , ymax = max, color = Query,
                  fill = after_scale(darken(desaturate(color, .1), .3, space = "HLS"))),
              alpha = 0.8, inherit.aes = FALSE) +
  lines_group(data = df_kpop) +
  geom_line(size = 2.0) +
  # Scales
  scale_x_date(date_breaks = "4 month", date_labels = "%b %y", expand = c(0.005,0.005)) +
  scale_color_manual(values = rep(cols, each = 2)) +
  labs(caption = "<span style='font-family:Norsebold;'>Visualization: <b style='font-size:15pt'>Data Feast</b></span> | Fuente: Global Google Search Trends (Obtenido 2021-07-10) valores de interes")

Kpop_ind <- Kpop +
  geom_richtext(
    aes(x = as.Date("2016-11-01"), y = 95,
        label = "Google Search Trends of <br><span style='font-family:Cinzel_D;'><b style='color:#f4a7bb;font-size:45pt;font-family:BlackPink;'>BlackPink   </b>, <b style='color:#ffc496;font-size:45pt;font-family:Twice;'>   TWICE</b> &     <b style='color:#ff0015;font-family:RedVelvet;font-size:45pt;'>    Red Velvet</b></span>"),
    color = "grey90", size = 12, lineheight = 1.5, family = "Work Sans", stat = "unique", fill = NA, label.color = NA
  ) +
  ## Photos
  geom_richtext(
    aes(x = as.Date("2015-02-01"), y = 72,
        label = "<img src='./img/Blackpink.png' width='180'/>"),
    stat = "unique", fill = NA, label.color = NA) +
  geom_richtext(
    aes(x = as.Date("2016-10-01"), y = 74,
        label = "<img src='./img/Twice.png' width='180'/>"),
    stat = "unique", fill = NA, label.color = NA) +
  geom_richtext(
    aes(x = as.Date("2018-05-01"), y = 74,
        label = "<img src='./img/RedVelvet.png' width='180'/>"),
    stat = "unique", fill = NA, label.color = NA) 

filename_plot <- paste0(Sys.Date(), "_FeastViz_KpopTrends.pdf")
ggsave(Kpop_ind, filename = filename_plot, width = 18, height = 10, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = filename_plot,
  filenames = str_replace(filename_plot, "pdf", "png"),
  format = "png", dpi = 300
)

### By Region
# Japan
df_kpop_Japan <- google_trend(file = "DataSet/Japan_BlackPink_Twice_RedVelvet.csv", date = "2016-01-01")

## Particular Ribbon
df_ribbon_complete <- data_ribbon(df_kpop_Japan)

kpop_Japan <- ggplot(df_kpop_Japan, aes(x = Month, y = interest, color = Query)) +
  geom_ribbon(data = df_ribbon_complete,
              aes(x = Month, ymin = max, ymax=Inf),
              fill = "black", color = "black", size = 5, inherit.aes=FALSE) +
  lines_group(data = df_kpop_Japan) +
  geom_line(size = 2.0) +
  geom_richtext(
    aes(x = as.Date("2020-08-01"), y = 85,
        label = "<br><img src='./img/Japan.png' width='300'/>"),
    color = "grey90", size = 12, lineheight = 1.5, family = "Work Sans",
    stat = "unique", fill = NA, label.color = NA) +
  scale_x_date(date_breaks = "4 month", date_labels = "%b %y", expand = c(0.005, 0.005)) +
  scale_color_manual(values = cols) +
  labs(caption = "<span style='font-family:Norsebold;'>Visualization: <b style='font-size:15pt'>Data Feast</b></span> | Data: Global Google Search Trends Japan (Obtenido 2021-07-10) valores interes") 

filename_plot <- paste0(Sys.Date(), "_FeastViz_KpopJapan.pdf")
ggsave(kpop_Japan, filename = filename_plot, width = 18, height = 10, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = filename_plot,
  filenames = str_replace(filename_plot, "pdf", "png"),
  format = "png", dpi = 300
)

# Korea
df_kpop_Korea <- google_trend(file = "DataSet/Korea_BlackPink_Twice_RedVelvet.csv", date = "2016-01-01")

# Particular Ribbon
df_ribbon_complete <- data_ribbon(df_kpop_Korea)

kpop_Korea <- ggplot(df_kpop_Korea, aes(x = Month, y = interest, color = Query)) +
  geom_ribbon(data = df_ribbon_complete,
              aes(x = Month, ymin = max, ymax = Inf),
              fill = "black", color = "black", size = 5, inherit.aes = FALSE) +
  lines_group(data = df_kpop_Korea) +
  geom_line(size = 2.0) +
  geom_richtext(
    aes(x = as.Date("2020-01-01"), y = 75,
        label = "<br><img src='./img/SouthKorea.png' width='300'/>"),
    color = "grey90", size = 12, lineheight = 1.5, family = "Work Sans",
    stat = "unique", fill = NA, label.color = NA) +
  scale_x_date(date_breaks = "4 month", date_labels = "%b %y", expand = c(0.005, 0.005)) +
  scale_color_manual(values = cols) +
  labs(caption = "<span style='font-family:Norsebold;'>Visualization: <b style='font-size:15pt'>Data Feast</b></span> | Fuente: Global Google Search Trends Korea (Obtenido 2021-07-10) valores de interes")

filename_plot <- paste0(Sys.Date(), "_FeastViz_KpopKorea.pdf")
ggsave(kpop_Korea, filename = filename_plot, width = 18, height = 10, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = filename_plot,
  filenames = str_replace(filename_plot, "pdf", "png"),
  format = "png", dpi = 300
)

# Mexico
df_kpop_Mexico <- google_trend(file = "DataSet/Mexico_BlackPink_Twice_RedVelvet.csv", date = "2016-01-01")

# Particular Ribbon
df_ribbon_complete <- data_ribbon(df_kpop_Mexico)

kpop_Mexico <- ggplot(df_kpop_Mexico, aes(x = Month, y = interest, color = Query)) +
  geom_ribbon(data = df_ribbon_complete,
              aes(x = Month, ymin = max, ymax = Inf),
              fill = "black", color = "black", size = 5, inherit.aes = FALSE) +
  lines_group(data = df_kpop_Mexico) +
  geom_line(size = 2.0) +
  geom_richtext(
    aes(x = as.Date("2018-04-01"), y = 75,
        label = "<br><img src='./img/Mexico.jpg' width='300'/>"),
    color = "grey90", size = 12, lineheight = 1.5, family = "Work Sans",
    stat = "unique", fill = NA, label.color = NA) +
  scale_x_date(date_breaks = "4 month", date_labels = "%b %y", expand = c(0.005, 0.005)) +
  scale_color_manual(values = cols) +
  labs(caption = "<span style='font-family:Norsebold;'>Visualization: <b style='font-size:15pt'>Data Feast</b></span> | Fuente: Global Google Search Trends Korea (Obtenido 2021-07-10) valores de interes")

filename_plot <- paste0(Sys.Date(), "_FeastViz_KpopMexico.pdf")
ggsave(kpop_Mexico, filename = filename_plot, width = 18, height = 10, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = filename_plot,
  filenames = str_replace(filename_plot, "pdf", "png"),
  format = "png", dpi = 300
)

## Full panel

Kpop_full <- Kpop +
  geom_richtext(
    aes(x = as.Date("2016-11-01"), y = 95,
        label = "Google Search Trends of <br><span style='font-family:Cinzel_D;'><b style='color:#f4a7bb;font-size:70pt;font-family:BlackPink;'>BlackPink</b>   ,  <b style='color:#ffc496;font-size:70pt;font-family:Twice;'>  TWICE  </b>  &  <b style='color:#ff0015;font-family:RedVelvet;font-size:70pt;margin-left:5em'> Red  Velvet</b></span>"),
    color = "grey90", size = 15, lineheight = 1.5, family = "Work Sans", stat = "unique", fill = NA, label.color = NA
  ) +
  ## Photos
  geom_richtext(
    aes(x = as.Date("2015-04-01"), y = 52,
        label = "<img src='./img/Blackpink.png' width='300'/>"),
    stat = "unique", fill = NA, label.color = NA) +
  geom_richtext(
    aes(x = as.Date("2016-10-01"), y = 64,
        label = "<img src='./img/Twice.png' width='300'/>"),
    stat = "unique", fill = NA, label.color = NA) +
  geom_richtext(
    aes(x = as.Date("2018-02-01"), y = 70,
        label = "<img src='./img/RedVelvet.png' width='300'/>"),
    stat = "unique", fill = NA, label.color = NA) 

Kpop_panel <- Kpop_full / (kpop_Korea | kpop_Mexico)

ggsave(plot = Kpop_panel, paste0(Sys.Date(), "_FeastViz_KpopPanel.pdf"), width = 28, height = 20, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = paste0(Sys.Date(), "_FeastViz_KpopPanel.pdf"),
  filenames = str_replace(paste0(Sys.Date(), "_FeastViz_KpopPanel.pdf"), "pdf", "png"),
  format = "png", dpi = 300
)






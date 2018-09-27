
library(ggplot2)
library(sf)
options(device = "X11")
X11.options(type = "cairo")
library(tidyverse)
library(eurostat)

donazioni <- read.csv2("docs/differenza_donazioni_16_15.csv", stringsAsFactors = F)

europe <- get_eurostat_geospatial(output_class = "sf", resolution = "60",
                                  nuts_level = "2", year = "2016", cache = TRUE,
                                  update_cache = FALSE, cache_dir = NULL)

donazioni_italia <- subset(europe, europe$CNTR_CODE=="IT")

donazioni_italia <- donazioni_italia %>% 
  mutate(Regione=case_when(
    NUTS_NAME == "Provincia Autonoma di Bolzano/Bozen" ~ "Provincia Autonoma di Bolzano",
    NUTS_NAME == "Valle d'Aosta/Vallée d'Aoste" ~ "Valle d'Aosta",
    TRUE ~ NUTS_NAME
  ))

donazioni_italia <- left_join(donazioni_italia, donazioni)

donazioni_italia <- donazioni_italia %>% 
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))

ggplot(donazioni_italia) + 
  geom_sf(aes(fill=Differenza), color = "white", alpha = .8) +
  scale_fill_distiller(palette = "Reds", direction = -1, "") +
  theme_void() +
  theme(panel.grid.major = element_line(colour="transparent")) +
  labs(title="Differenza nelle donazioni di sangue in Italia",
       subtitle = "tra il 2016 e il 2015",
       caption = "Dati: Osservatorio Nazionale sulla Salute") +
  geom_text(aes(label = case_when(
                  Differenza > 0 ~ paste0("+", Differenza, "%"),
                  TRUE ~ paste0(Differenza, "%")),
                  x = lon, y = lat),
            size = 5)

#ggsave("Donazioni.png", height = 30, width = 20, units = "cm")
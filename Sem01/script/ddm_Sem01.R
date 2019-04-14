# Sem01 - Datos de Miércoles
# Daniel H @gmbeisbol

#### Packages utilizados
library(tidyverse)
library(ggthemes)

#### Importar datos
setwd("~/DatosMiercoles/Sem01")

fifa <- readr::read_delim("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-10/partidos.txt",delim = "\t")
fifa$partido_orden <- as.factor(fifa$partido_orden)

#### Pregunta a responder
### Cuantos goles han anotado cada uno de los países, y cómo ha sido su acumulado en el tiempo?

#### Manipulación de los datos

fifa_1 <- fifa %>%
  select(anio, fecha, equipo_1, equipo_1_final, equipo_2_final) %>%
  rename(anho = anio,
         pais = equipo_1,
         goles_anotados = equipo_1_final,
         goles_recibidos = equipo_2_final)

fifa_2 <- fifa %>%
  select(anio, fecha, equipo_2, equipo_2_final, equipo_1_final) %>%
  rename(anho = anio,
         pais = equipo_2,
         goles_anotados = equipo_2_final,
         goles_recibidos = equipo_1_final)

goles <- rbind(fifa_1, fifa_2) %>%
  arrange(fecha, pais) %>% 
  group_by(pais) %>%
  mutate(acum_anotados = cumsum(goles_anotados),
         acum_recibidos = cumsum(goles_recibidos),
         delta_goles = acum_anotados - acum_recibidos)

goles_summary <- rbind(fifa_1, fifa_2) %>%
  arrange(fecha, pais) %>% 
  group_by(pais) %>%
  summarise(total_anotados = sum(goles_anotados),
            total_recibidos = sum(goles_recibidos),
            delta_total = total_anotados - total_recibidos) %>%
  arrange(desc(total_anotados)) %>% 
  mutate(pais = factor(pais, unique(pais)))
  

## Graficar

p <- ggplot(goles_summary) +
  geom_bar(aes(x = pais, y = total_anotados),
           stat = "identity", fill = "#29c649", alpha = 0.7) +
  geom_bar(aes(x = pais, y = -total_recibidos),
           stat = "identity", fill = "#e80000", alpha = 0.7) +
  geom_path(aes(x = pais, y = delta_total, group = 1), color = "blue", alpha = 0.7) +
  labs(title = "Goles anotados y recibidos por cada país en Mundiales de Fútbol",
       subtitle = "(1930-2018)",
       caption = "#DatosDeMiércoles por: Daniel Hernández.\ twitter.com/gmbeisbol",
       tag = "FIFA",
       x = "Países",
       y = "Goles recibidos / Goles anotados")

p <- p + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

p

ggsave(filename = "Total_Goles.png", width = 16, height = 9)

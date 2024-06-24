# Libs

library(ggplot2) # plot graph
library(dplyr) # data manipulation
library(gganimate) # plot animated graph
library(sf) # download shapefile

# Data

# dados de AIDS no ES
dt <- read.csv("data/aids14a23.csv", sep = ";", dec = ".") # dados de AIDS

# Shapefile do ES, baixado no site do IBGE
shp <- st_read("data/ES_Municipios_2022.shp")

# Data Preprocessing

dt_aux<- dt %>%
  group_by(NU_ANO, ID_MUNICIP) %>%
  summarise(casos = n()) %>%
  mutate(ID_MUNICIP = as.character(ID_MUNICIP))

shp_aux$centro <- st_centroid(shp$geometry)

shp_aux <- shp %>%
  mutate(CD_MUN = as.character(CD_MUN)) %>%  # Converter para character para manipulação de string
  mutate(CD_MUN = substr(CD_MUN, 1, 6)) %>%  # Retirar o sétimo dígito
  rename(ID_MUNICIP = CD_MUN)

dt_shp <- left_join(shp_aux,dt_aux, by = c("ID_MUNICIP" = "ID_MUNICIP"))

dt_shp <- dt_shp %>% arrange(casos)

# Plotting

dt_shp %>% ggplot() +
  geom_sf(fill = "grey", alpha = 0.3) +
  geom_point(aes(size = casos, color = casos, alpha = casos),
             shape = 20, stroke = FALSE
  )

dt_shp %>%
  ggplot() +
  geom_sf(fill = "grey", alpha = 0.3) +
  geom_point(aes(x = st_coordinates(centro)[, "X"], y = st_coordinates(centro)["Y",],
                 size = casos, color = casos, alpha = casos),
             shape = 20, stroke = FALSE
  )





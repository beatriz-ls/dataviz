# Libs

library(ggplot2) # plot graph
library(dplyr) # data manipulation
library(gganimate) # plot animated graph
library(sf) # download shapefile
library(readr) # download data

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

shp_aux <- shp %>%
  mutate(CD_MUN = as.character(CD_MUN)) %>%  # Converter para character para manipulação de string
  mutate(CD_MUN = substr(CD_MUN, 1, 6)) %>%  # Retirar o sétimo dígito
  rename(ID_MUNICIP = CD_MUN)

shp_aux$centro <- st_centroid(shp$geometry)

dt_shp <- left_join(shp_aux,dt_aux, by = c("ID_MUNICIP" = "ID_MUNICIP"))

dt_shp <- dt_shp %>% arrange(casos) %>%
                    mutate(lon = st_coordinates(centro)[, 1], 
                           lat = st_coordinates(centro)[, 2])

# Plotting

p1 <- dt_shp %>%
  ggplot() +
  geom_sf(fill = "grey", alpha = 0.3) +
  geom_point(aes(x = lon, y = lat,
                 size = casos, alpha = casos, color = casos),
             shape = 20, stroke = FALSE
  )+
  scale_size_continuous(range = c(1, 9)) +
  scale_color_viridis_c(option = "inferno", trans = "log") +
  scale_alpha_continuous(trans = "log") +
  theme_void() +
  theme(legend.position = "none")

p3 <- dt_shp %>%
  ggplot() +
  geom_sf(fill = "grey", alpha = 0.3) +
  geom_point(aes(x = lon, y = lat,
                 size = casos, alpha = casos, color = casos),
             shape = 20, stroke = FALSE
  )+
  scale_size_continuous(range = c(1, 9)) + 
  
  transition_states(dt_shp$NU_ANO, 
                    transition_length =  1,
                    state_length = 1)
  
  
  #scale_color_viridis_c(option = "inferno", trans = "log") +
  #scale_alpha_continuous(trans = "log")





library(tidyverse)
library(sf)
library(mapview)



# leer datos --------------------------------------------------------------

# 1. manzanas
# lee layer manzanas poly originales
# st_read(dsn = "Z:/_Carto base/INE.gdb",
#         as_tibble = TRUE,
#         query = "SELECT * FROM \"Variables_C17_Manzana\" WHERE (PROVINCIA = 'SANTIAGO' OR COMUNA = 'SAN BERNARDO'
#         OR COMUNA = 'PUENTE ALTO') AND NOT (DISTRITO = 'LO AGUIRRE' OR DISTRITO = 'LO HERRERA'
#         OR DISTRITO = 'LA RINCONADA' OR DISTRITO = 'PUDAHUEL')") %>%
#   janitor::clean_names() %>% 
#   select(cod_comuna:cod_categoria) %>%
#   rename(manzent = manzent_i) %>% 
#   st_cast("POLYGON") %>% 
#   mutate(id_manzent = row_number()) %>% 
#   select(cod_comuna:manzent, id_manzent, everything()) %>% 
#   st_write(dsn = "datos_input/manzanas_ams.geojson")


manzanas <- st_read(dsn = "datos_input/manzanas_ams.geojson", as_tibble = TRUE)

manzanas_centroides <- st_point_on_surface(manzanas)


# 2. areas verdes
# setwd("Z:/_Carto base/SIEDU.gdb")

temp_wd <- "Z:/_Estudios/2018 IAC/Índice de accesibilidad urbana/GIS.gdb"

# vector with layer names
layers_names <- st_layers(temp_wd)$name %>%
  tibble() %>% 
  filter(
    str_detect(string = .,
               pattern = "_[2|3|4]_AMS$") #"_\\d_AMS$" \\d encuentra cualquier dígito
  ) %>%
  arrange(.) %>% 
  pull(.)


walk(.x = layers_names,
     .f = function (x) {
       layers_names <- tolower(x)
       assign(
         x = layers_names,
         value = sf::st_read(dsn = temp_wd,
                             layer = x,
                             as_tibble = TRUE),
         envir = .GlobalEnv
         )
       }
     )

# limpia capa
list(
  c_av_2_ams,
  c_av_3_ams,
  c_av_4_ams
  ) %>% 
  set_names(tolower(layers_names)) %>% 
  map(.f = ~ st_transform(.x, crs = 32719) %>% 
        select(-c(ESTADO, CLASE, area_Ha, ESTADO_2, AV, ID, layer, OK))) %>%
  list2env(x = ., .GlobalEnv)
  

# exportar centroides av 
st_write(obj = c_av_2_ams, dsn = "datos_input/c_av_2ams.shp", delete_layer = TRUE)
st_write(obj = c_av_3_ams, dsn = "datos_input/c_av_3ams.shp", delete_layer = TRUE)
st_write(obj = c_av_4_ams, dsn = "datos_input/c_av_4ams.shp", delete_layer = TRUE)










# networking --------------------------------------------------------------

   
st_layers("C:/Users/svaldiviar/Desktop/Assignments/02_metro_L7/data/red_pedestrian.gdb")


"C:/Users/svaldiviar/Desktop/Assignments/02_metro_L7/data/eriazos_centroides"



red <- st_read(dsn = "C:/Users/svaldiviar/Desktop/Assignments/02_metro_L7/data/red_pedestrian.gdb",
               layer = "streetsF",
               as_tibble = TRUE) %>% 
  filter(COM_IZQ == "SANTIAGO") %>% 
  st_cast("LINESTRING") %>% 
  rename(geometry = Shape)




red <- red %>% 
  select(1:Shape_Length, oneway2 = ONEWAY, everything()) %>% 
  mutate(oneway = case_when(
    oneway2 == "" ~ "F",
    oneway2 == "FT" ~ "T1",
    oneway2 == "TF"~ "T2")
    )




red_fn <- bind_rows(
  red %>%
    filter(oneway == "F"),
  red %>% 
    filter(oneway == "F") %>% 
    st_reverse(),
  red %>% 
    filter(oneway == "T1"),
  red %>% 
    filter(oneway == "T2") %>% 
    st_reverse()
  )



net <- as_sfnetwork(red_fn)


net <- net %>% 
  convert(to_spatial_subdivision) %>% 
  convert(to_spatial_smooth)




net <- net %>% 
  activate("edges") %>% 
  mutate(weight = edge_length())



paths <- st_network_paths(net, from = 1, to = 1000)


paths %>%
  slice(1) %>%
  pull(edge_paths) %>%
  unlist()

paths %>%
  slice(1) %>%
  pull(edge_paths) %>%
  unlist()

plot_path = function(node_path) {
  net %>%
    activate("nodes") %>%
    slice(node_path) %>%
    plot(cex = 1.5, lwd = 1.5, add = TRUE)
}

colors = sf.colors(3, categorical = TRUE)

plot(net, col = "grey")
paths %>%
  pull(node_paths) %>%
  walk(plot_path)
net %>%
  activate("nodes") %>%
  st_as_sf() %>%
  slice(c(495, 121, 458)) %>%
  plot(col = colors, pch = 8, cex = 2, lwd = 2, add = TRUE)



duplicated <- red %>% 
  filter(oneway == "F") %>% 
  st_reverse()



duplicates = edges[edges$oneway == FALSE, ]
reversed_duplicates = st_reverse(duplicates)

edges = rbind(edges, reversed_duplicates)
net = as_sfnetwork(edges)
activate(net, "edges")


red %>% 
  st_drop_geometry() %>% 
  count(ONEWAY)


net <- as_sfnetwork(red)




nd_test %>%
  activate("edges") %>%
  mutate(weight = edge_length()) %>%
  activate("nodes") %>%
  mutate(bc = centrality_betweenness(weights = weight, directed = FALSE))




nd_test <- nd_test %>%
  activate("nodes") %>%
  mutate(bc = centrality_betweenness())

ggplot() +
  geom_sf(data = st_as_sf(nd_test, "edges"), col = "grey50") +
  geom_sf(data = st_as_sf(nd_test, "nodes"), aes(col = bc, size = bc)) +
  ggtitle("Betweenness centrality in Münster Roxel")


















data <- read_csv("https://raw.githubusercontent.com/ivanMSC/COVID19_Chile/master/Por%20Km%20Cuadrado/_GEOMETRIA.csv")


web <- st_read("https://services3.arcgis.com/CNzkI2T3GmfwkaAR/ArcGIS/rest/services/Cuadrantes_Urbanos_11dias_Vista/FeatureServer/",
               layer = "CUADRANTES_URBANOS")

data %>% 
  filter(COMUNA == "SANTIAGO") %>% 
  view()


data_geo <- data %>% 
  pivot_longer(cols = c(X1, X2, X3, X4),
               names_to = "coord_1",
               values_to = "coord_x") %>% 
  pivot_longer(cols = c(Y1, Y2, Y3, Y4),
               names_to = "coord_2",
               values_to = "coord_y") %>% 
  filter((coord_1 == "X1" & coord_2 == "Y1") |
         (coord_1 == "X2" & coord_2 == "Y2") |
         (coord_1 == "X3" & coord_2 == "Y3") |
         (coord_1 == "X4" & coord_2 == "Y4")) %>% 
  filter(COMUNA == "SANTIAGO") %>% 
  st_as_sf(coords = c("coord_x", "coord_y"),
           crs = 31979)


mapview()

  st_write("cuadr.shp"
  )
  group_by(ID_C_AUC) %>% 
  summarise() %>% 
  st_cast("POLYGON") %>% 
  # select(COMUNA) %>% 
  plot()


data %>% 
  pivot_longer(cols = c(c(X1, X2), c(X3, X4)),
               names_to = "coord_type_1",
               values_to = "coord_X") %>% 
  pivot_longer(cols = c(c(Y1, Y2), c(Y3, Y4)),
               names_to = "coord_type_2",
               values_to = "coord_Y") %>% 
  view()

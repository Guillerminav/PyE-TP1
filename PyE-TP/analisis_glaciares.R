#carga e instalacion paquetes

install.packages("pacman")

pacman::p_load(
  janitor,    # data cleaning and tables
  matchmaker, # dictionary-based cleaning
  tidyverse,   # data management and visualization
  stringi
)

# carga de los datasets

#usar read_delim para manejar puntos dem iles y comas decimales

df_tipo <- read_delim("agg_c_glaciarestipo_pcia.csv",
                      delim = ";",
                      locale = locale(decimal_mark = ",", grouping_mark = ".")) %>%
    
  clean_names() %>%
  distinct()

df_anp <- read_delim("agg_b_glaciaresanp_nuevo_2023.csv",
                      delim = ";") %>%
  clean_names()

# normalizar los nombres de las provincias

normalizar_pcia <- function(texto) {
  texto %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    toupper() %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_squish()
}

#aplicar normalizacion y unir

# se crea una columna temporal 'pcia_match' para hacer el join sin romepr los nombres

dataset_final <- df_tipo %>%
  mutate(pcia_match = normalizar_pcia(provincia)) %>%
  full_join(
    df_anp %>% mutate(pcia_match = normalizar_pcia(provincia)),
    by = "pcia_match",
    suffix = c("_tipo", "_anp")
  ) %>%
  mutate(provincia = coalesce(provincia_tipo, provincia_anp)) %>%
  select(provincia, everything(), -pcia_match, -provincia_tipo, -provincia_anp)

write_csv(dataset_final, "glaciares_consolidado.csv")
view(dataset_final)
  
  
#ajustes finales y analisis descriptivo

# A) formato ancho a largo y creamos 2da varaible cuantitativa
df_analisis <- dataset_final %>%
  pivot_longer(
    # seleccionamos las columnas que tienen las superficies por tipo
    cols = c(superficie_glaciar_descubierto_km2, 
             superficie_glaciar_cubierto_km2, 
             superficie_manchon_nieve_km2, 
             superficie_glaciar_cubierto_glaciar_escombros_km2, 
             superficie_glaciar_escombros_km2),
    names_to = "tipo_glaciar",  # CUALITATIVA
    values_to = "superficie_km2" # CUANTITATIVA
  ) %>%
  # Limpiamos un poco los nombres de la nueva columna para que queden lindos en el gráfico
  mutate(
    tipo_glaciar = str_remove(tipo_glaciar, "superficie_"),
    tipo_glaciar = str_remove(tipo_glaciar, "_km2"),
    tipo_glaciar = str_replace_all(tipo_glaciar, "_", " "),
    tipo_glaciar = str_to_title(tipo_glaciar) # Pone la primera letra en mayúscula
  ) %>%
  # filtramos los que tienen superficie 0 para no ensuciar el análisis
  filter(superficie_km2 > 0)

# Provincia
tabla_provincias <- df_analisis %>%
  group_by(provincia) %>%
  summarise(superficie_total = sum(superficie_km2, na.rm = TRUE)) %>%
  arrange(desc(superficie_total))

# Tipo de Glaciar
tabla_tipos <- df_analisis %>%
  group_by(tipo_glaciar) %>%
  summarise(superficie_total = sum(superficie_km2, na.rm = TRUE)) %>%
  arrange(desc(superficie_total))

print("--- Superficie por Tipo de Glaciar ---")
print(tabla_tipos)

# (Cuali vs Cuali vs Cuanti)
# Muestra la superficie por provincia, coloreada por el tipo de glaciar
ggplot(df_analisis, aes(x = reorder(provincia, superficie_km2, sum), y = superficie_km2, fill = tipo_glaciar)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Superficie de Glaciares por Provincia y Tipo",
    subtitle = "Fuente: Inventario Nacional de Glaciares (2023)",
    x = "Provincia",
    y = "Superficie (km²)",
    fill = "Tipo de Glaciar"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


view(df_analisis)


###analisis areas protegidas

# A) Preparar los datos de conservación
df_proteccion <- dataset_final %>%
  # columnas de interés y quitamos duplicados de filas
  select(provincia, glaciares_en_app, glaciares_en_apn, 
         superficie_glaciares_en_ap, superficie_total_glaciar_por_provincia) %>%
  distinct() %>%
  # calculamos la superficie que se encuentra desprotegida
  mutate(
    superficie_sin_proteccion = superficie_total_glaciar_por_provincia - superficie_glaciares_en_ap,
    # corregimos si por redondeo algún valor da menor a cero
    superficie_sin_proteccion = ifelse(superficie_sin_proteccion < 0, 0, superficie_sin_proteccion)
  ) %>%
  # Pasamos a formato largo las tres categorías para poder colorear el gráfico
  pivot_longer(
    cols = c(glaciares_en_app, glaciares_en_apn, superficie_sin_proteccion),
    names_to = "estado_proteccion",
    values_to = "km2"
  ) %>%
  # etiquetas para la leyenda del gráfico
  mutate(
    estado_proteccion = case_when(
      estado_proteccion == "glaciares_en_app" ~ "Parque/Área Protegida Provincial",
      estado_proteccion == "glaciares_en_apn" ~ "Parque/Área Protegida Nacional",
      estado_proteccion == "superficie_sin_proteccion" ~ "Sin Protección Legal",
      TRUE ~ estado_proteccion
    )
  ) %>%
  filter(km2 > 0) # Eliminamos registros en cero para limpiar el gráfico

# barras apiladas por volumen absoluto
ggplot(df_proteccion, aes(x = reorder(provincia, km2, sum), y = km2, fill = estado_proteccion)) +
  geom_col(color = "white", linewidth = 0.2) +
  scale_fill_manual(values = c(
    "Parque/Área Protegida Nacional" = "#2ca02c",  # Verde
    "Parque/Área Protegida Provincial" = "#bcbd22", # Verde claro/amarillento
    "Sin Protección Legal" = "#d62728"             # Rojo
  )) +
  coord_flip() +
  labs(
    title = "Distribución de Superficie de Glaciares según Estado de Protección",
    subtitle = "Comparativa del tipo de gestión ambiental por provincia (2022)",
    x = "Provincia",
    y = "Superficie de Hielo (km²)",
    fill = "Estado de Conservación"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.direction = "vertical")




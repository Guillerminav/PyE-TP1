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

dataset_final <- dataset_final %>%
  mutate(
    provincia = if_else(
      str_detect(provincia, "Tierra del Fuego"),
      "Tierra del Fuego, Antárt. e IAS",
      provincia
    )
  )

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


# ==============================================================================
# SEGUNDA PARTE - ESTIMACIÓN INFERENCIAL
# ==============================================================================
library(dplyr)

# A) formamos los grupos
# Grupo 1: Glaciares descubiertos
g1 <- df_analisis %>%
  filter(tipo_glaciar == "Glaciar Descubierto")

# Grupo 2: Glaciares cubiertos y de escombros
g2 <- df_analisis %>%
  filter(tipo_glaciar %in% c("Glaciar Cubierto", 
                             "Glaciar Escombros", 
                             "Glaciar Cubierto Glaciar Escombros"))

# B) calculo de parametros muestrales
# Parámetros del Grupo 1
n1 <- nrow(g1)
media1 <- mean(g1$superficie_km2, na.rm = TRUE)
sd1 <- sd(g1$superficie_km2, na.rm = TRUE)

# Parámetros del Grupo 2
n2 <- nrow(g2)
media2 <- mean(g2$superficie_km2, na.rm = TRUE)
sd2 <- sd(g2$superficie_km2, na.rm = TRUE)

cat("--- Resultados Grupo 1 ---\n", "n:", n1, " | Media:", media1, " | SD:", sd1, "\n")
cat("--- Resultados Grupo 2 ---\n", "n:", n2, " | Media:", media2, " | SD:", sd2, "\n\n")

# C) construccion de intervalos de confianza (95%)
nivel_confianza <- 0.95
alpha <- 1 - nivel_confianza

# Intervalo para Grupo 1
t_critico_1 <- qt(1 - alpha/2, df = n1 - 1)
error_estandar_1 <- sd1 / sqrt(n1)
margen_error_1 <- t_critico_1 * error_estandar_1
lim_inf_1 <- media1 - margen_error_1
lim_sup_1 <- media1 + margen_error_1

# Intervalo para Grupo 2
t_critico_2 <- qt(1 - alpha/2, df = n2 - 1)
error_estandar_2 <- sd2 / sqrt(n2)
margen_error_2 <- t_critico_2 * error_estandar_2
lim_inf_2 <- media2 - margen_error_2
lim_sup_2 <- media2 + margen_error_2

cat("--- Intervalos de Confianza al 95% ---\n")
cat("IC Grupo 1 (Descubiertos): [", round(lim_inf_1, 2), " ; ", round(lim_sup_1, 2), "]\n")
cat("IC Grupo 2 (Cubiertos/Escombros): [", round(lim_inf_2, 2), " ; ", round(lim_sup_2, 2), "]\n")




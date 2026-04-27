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
    stringgi::stri_trans_general("Latin-ASCII") %>%
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
  
  









#carga e instalacion paquetes

install.packages("pacman")

pacman::p_load(
  janitor,    # data cleaning and tables
  matchmaker, # dictionary-based cleaning
  tidyverse   # data management and visualization
)

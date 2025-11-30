# CONFIGURACIONES
#-------------------------------------------------------------------------------

# Limpiar entorno
rm(list = ls())

# Configurar opciones globales
options(stringsAsFactors = FALSE)
options(scipen = 999)  # Evitar notación científica

# Librerías del proyecto
library(tidyverse)
library(modeest)
library(readxl)
library(lubridate)
library(scales)
library(WDI)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(skimr)
library(naniar)

# Definir directorios de forma reproducible
if (!exists("proyecto")) {
  proyecto_dir <- here::here() 
}

# Rutas principales
dir_data_raw       <- file.path(proyecto_dir, "data", "raw")
dir_data_processed <- file.path(proyecto_dir, "data", "processed")
dir_data_clean     <- file.path(proyecto_dir, "data", "clean")
dir_output_figures <- file.path(proyecto_dir, "output", "figures")
dir_output_tables  <- file.path(proyecto_dir, "output", "tables")
dir_scripts  <- file.path(proyecto_dir, "scripts")
dir_configuracion  <- file.path(proyecto_dir, "configuracion")
dir_funciones  <- file.path(proyecto_dir, "funciones")

# Crear directorios si no existen
dirs_crear <- c(dir_data_raw, dir_data_processed, dir_data_clean,
                dir_output_figures, dir_output_tables, dir_scripts,dir_configuracion,dir_funciones)

for (dir in dirs_crear) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
}



# 03_modelacion.R
# Modelación estadística
# Autor:César Salazar
# Fecha:18/05/2026

library(here)
library(readr)
library(dplyr)
library(ggplot2)
library(BayesFactor)

# Carga automática de datos:

datos <- here("datos", "procesados", "cardio_procesado.csv")

if (!file.exists(datos)) {
  message("Archivo procesado no encontrado. Ejecutando limpieza...")
  source(here("codigo", "01_limpieza.R"))
}

cardio <- read_csv(datos, show_col_types = FALSE)

# Análisis exploratorio

source(here("codigo/02_descriptivo.R"))

# Factor de Bayes

ap_si  <- cardio$ap_hi[cardio$cardio == "Si"]
ap_no  <- cardio$ap_hi[cardio$cardio == "No"]

bf_resultado <- ttestBF(x = ap_si, y = ap_no)
bf_resultado





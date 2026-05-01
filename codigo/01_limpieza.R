# 01_limpieza.R
# Limpieza y preparación de los datos crudos
# Autor: César Salazar, Gerardo Montero, Gabriel Robles, Óscar Espinoza
# Fecha: 29/04/2026

library(tidyverse)

cardio_raw <- read_delim("datos/originales/cardio_train.csv", delim = ";")

cardio_raw %>% count(id) %>% filter(n > 1)

colSums(is.na(cardio_raw))

glimpse(cardio_raw)

cardio <- cardio_raw %>%  # Pasar a factores
  mutate(
    age = round(age / 365),
    gender     = factor(gender, levels = c(1,2), labels = c("Mujer","Hombre")),
    cholesterol = factor(cholesterol, levels = 1:3, labels = c("Normal","Alto","Muy Alto")),
    gluc        = factor(gluc, levels = 1:3, labels = c("Normal","Alto","Muy Alto")),
    smoke  = factor(smoke,  levels = c(0,1), labels = c("No","Si")),
    alco   = factor(alco,   levels = c(0,1), labels = c("No","Si")),
    active = factor(active, levels = c(0,1), labels = c("No","Si")),
    cardio = factor(cardio, levels = c(0,1), labels = c("No","Si"))
  )

cardio %>%  # Ver si existen datos imposibles en presión y peso
  summarise(
    ap_hi_min = min(ap_hi),
    ap_hi_max = max(ap_hi),
    ap_lo_min = min(ap_lo),
    ap_lo_max = max(ap_lo),
    weight_min = min(weight),
    weight_max = max(weight),
    height_min = min(height),
    height_max = max(height)
  )

cardio <- cardio %>%  # Filtrar por datos reales en presión y peso
  filter(
    ap_hi >= 70, ap_hi <= 250,
    ap_lo >= 40, ap_lo <= 150,
    ap_hi > ap_lo,
    height >= 140, height <= 210,
    weight >= 40, weight <= 200
  )

glimpse(cardio)

write_csv(cardio, "datos/procesados/cardio_procesado.csv")

# 02_descriptivo.R
# Análisis descriptivo y exploratorio de los datos
# Autor: Andrés Montero
# Fecha: 29/4/26
# Librerías
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(showtext)
library(scales)

# Estilo de robles
source("codigo/funciones/04_estilo.R")

# Datos de Cesar
cardio <- read_csv("datos/procesados/cardio_procesado.csv")

# Preparacion de datos
cardio <- cardio %>%
  mutate(
    cardio = factor(cardio, levels = c("No", "Si")),
    smoke = factor(smoke, levels = c("No", "Si")),
    alco = factor(alco, levels = c("No", "Si")),
    active = factor(active, levels = c("No", "Si")),
    gender = factor(gender),
    cholesterol = factor(cholesterol),
    gluc = factor(gluc),
    imc = weight / ((height / 100)^2)
  )

# Paleta para variables binarias tipo Si / No
colores_si_no <- c(
  "No" = unname(colores_factorriesgo["0"]),
  "Si" = unname(colores_factorriesgo["1"])
)
# ============================================================
# Grafico 1: correlacion con enfermedad cardiovascular
# ============================================================

cardio_cor <- cardio %>%
  mutate(
    cardio_num = ifelse(cardio == "Si", 1, 0),
    smoke_num = ifelse(smoke == "Si", 1, 0),
    alco_num = ifelse(alco == "Si", 1, 0),
    active_num = ifelse(active == "Si", 1, 0),
    gender_num = as.numeric(gender),
    cholesterol_num = as.numeric(cholesterol),
    gluc_num = as.numeric(gluc)
  ) %>%
  select(
    cardio_num,
    age,
    gender_num,
    height,
    weight,
    imc,
    ap_hi,
    ap_lo,
    cholesterol_num,
    gluc_num,
    smoke_num,
    alco_num,
    active_num
  )

cor_cardio <- cardio_cor %>%
  summarise(
    across(
      .cols = -cardio_num,
      .fns = ~ cor(.x, cardio_num, use = "complete.obs")
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "correlacion"
  ) %>%
  arrange(desc(abs(correlacion)))

grafico_correlacion <- ggplot(
  cor_cardio,
  aes(x = reorder(variable, correlacion), y = correlacion)
) +
  geom_col(fill = colores_default[1]) +
  coord_flip() +
  labs(
    title = "Correlación de variables con enfermedad cardiovascular",
    x = "Variable",
    y = "Correlación con enfermedad cardiovascular"
  ) +
  set_tipografia()

# ============================================================
# Grafico 2: distribucion de edad segun enfermedad
# ============================================================

grafico_edad <- ggplot(cardio, aes(x = age, fill = cardio)) +
  geom_density(alpha = 0.55) +
  scale_fill_manual(values = colores_si_no) +
  labs(
    title = "Distribución de edad según enfermedad cardiovascular",
    x = "Edad",
    y = "Densidad",
    fill = "Enfermedad cardiovascular"
  ) +
  set_tipografia()

# ============================================================
# Grafico 3: actividad, glucosa y colesterol
# ============================================================

cardio_largo <- cardio %>%
  select(cardio, active, gluc, cholesterol) %>%
  pivot_longer(
    cols = c(active, gluc, cholesterol),
    names_to = "variable",
    values_to = "categoria"
  ) %>%
  group_by(variable, categoria, cardio) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable, categoria) %>%
  mutate(porcentaje = n / sum(n)) %>%
  ungroup()

grafico_factores <- ggplot(
  cardio_largo,
  aes(x = categoria, y = porcentaje, fill = cardio)
) +
  geom_col(position = "fill") +
  facet_wrap(~ variable, scales = "free_x") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = colores_si_no) +
  labs(
    title = "Proporción de enfermedad cardiovascular por actividad, glucosa y colesterol",
    x = "Categoría",
    y = "Porcentaje",
    fill = "Enfermedad cardiovascular"
  ) +
  set_tipografia()

# ============================================================
# Grafico 4: prevalencia por colesterol
# ============================================================

cardio_colesterol <- cardio %>%
  group_by(cholesterol, cardio) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cholesterol) %>%
  mutate(porcentaje = n / sum(n)) %>%
  ungroup()

grafico_colesterol <- ggplot(
  cardio_colesterol,
  aes(x = cholesterol, y = porcentaje, fill = cardio)
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = colores_si_no) +
  labs(
    title = "Prevalencia de enfermedad cardiovascular por colesterol",
    x = "Nivel de colesterol",
    y = "Porcentaje",
    fill = "Enfermedad cardiovascular"
  ) +
  set_tipografia()

# ============================================================
# Grafico 5: distribucion del IMC segun enfermedad
# ============================================================

grafico_imc <- ggplot(cardio, aes(x = imc, fill = cardio)) +
  geom_density(alpha = 0.55) +
  scale_fill_manual(values = colores_si_no) +
  coord_cartesian(xlim = c(10, 60)) +
  labs(
    title = "Distribución del IMC según enfermedad cardiovascular",
    x = "Índice de masa corporal",
    y = "Densidad",
    fill = "Enfermedad cardiovascular"
  ) +
  set_tipografia()


# Tipos de variables
eda_tipos <- tibble(
  variable = names(cardio),
  tipo = sapply(cardio, class)
)

#  Valores faltantes
eda_faltantes <- cardio %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "faltantes"
  ) %>%
  mutate(
    porcentaje_faltantes = faltantes / nrow(cardio)
  )

#  Resumen de variables numericas
eda_numericas <- cardio %>%
  select(where(is.numeric)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  group_by(variable) %>%
  summarise(
    media = mean(valor, na.rm = TRUE),
    mediana = median(valor, na.rm = TRUE),
    sd = sd(valor, na.rm = TRUE),
    minimo = min(valor, na.rm = TRUE),
    q1 = quantile(valor, 0.25, na.rm = TRUE),
    q3 = quantile(valor, 0.75, na.rm = TRUE),
    maximo = max(valor, na.rm = TRUE),
    .groups = "drop"
  )

#  Frecuencias de variables categoricas
eda_categoricas <- cardio %>%
  select(where(is.factor)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "categoria"
  ) %>%
  group_by(variable, categoria) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(porcentaje = n / sum(n)) %>%
  ungroup()

#  Balance de la variable respuesta
eda_balance_cardio <- cardio %>%
  count(cardio) %>%
  mutate(porcentaje = n / sum(n))

#  Resumen por enfermedad cardiovascular
eda_por_cardio <- cardio %>%
  group_by(cardio) %>%
  summarise(
    edad_media = mean(age, na.rm = TRUE),
    edad_mediana = median(age, na.rm = TRUE),
    imc_medio = mean(imc, na.rm = TRUE),
    imc_mediano = median(imc, na.rm = TRUE),
    presion_sistolica_media = mean(ap_hi, na.rm = TRUE),
    presion_diastolica_media = mean(ap_lo, na.rm = TRUE),
    peso_medio = mean(weight, na.rm = TRUE),
    .groups = "drop"
  )

eda_dimensiones <- tibble(
  filas = nrow(cardio),
  columnas = ncol(cardio)
)
# guardar en lista los resultados
list(
  grafico_correlacion = grafico_correlacion,
  grafico_edad = grafico_edad,
  grafico_factores = grafico_factores,
  grafico_colesterol = grafico_colesterol,
  grafico_imc = grafico_imc,
  eda_tipos = eda_tipos,
  eda_faltantes = eda_faltantes,
  eda_numericas = eda_numericas,
  eda_categoricas = eda_categoricas,
  eda_balance_cardio = eda_balance_cardio,
  eda_por_cardio = eda_por_cardio,
  eda_dimensiones = eda_dimensiones
)

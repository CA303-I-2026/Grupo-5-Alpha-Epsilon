# 02_descriptivo.R
# Análisis descriptivo y exploratorio de los datos
# Autor: Andrés Montero
# Fecha: 29/4/26
# Librerías

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(cowplot)
library(ggplot2)

library(showtext)
library(scales)
library(here)

# Estilo de robles
source(here("codigo/04_estilo.R"))

# Datos de Cesar
cardio <- read_csv(here("datos/procesados/cardio_procesado.csv"))

# 30 de abril: cambios por Robles de estilo 
nombres_variables <- c(
  "id" = "ID",
  "cardio_num"     = "Factor De Riesgo",
  "cardio"         = "Factor De Riesgo",
  "age"            = "Edad",
  "gender_num"     = "Sexo",
  "gender"         = "Sexo",
  "height"         = "Altura",
  "weight"         = "Peso",
  "imc"            = "IMC",
  "ap_hi"          = "Presión arterial (alta)",
  "ap_lo"          = "Presión arterial (baja)",
  "cholesterol_num" = "Colesterol",
  "cholesterol"    = "Colesterol",
  "gluc_num"       = "Nivel de Glucosa",
  "gluc"           = "Nivel de Glucosa",
  "smoke_num"      = "Fumador",
  "smoke"          = "Fumador",
  "alco_num"       = "Bebedor de Alcohol",
  "alco"           = "Bebedor de Alcohol",
  "active_num"     = "Actividad física",
  "active"         = "Actividad física"
)


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

# ===========================================================================
# Cuadro resumen 1:  variables numericas estratificadas por enfermedad cardio
# ===========================================================================

eda_por_cardio <- cardio %>%
  group_by(cardio) %>%
  summarise(
    age_r    = paste0(round(mean(age,    na.rm = TRUE), 1), " ± ", round(sd(age,    na.rm = TRUE), 1)),
    imc_r    = paste0(round(mean(imc,    na.rm = TRUE), 1), " ± ", round(sd(imc,    na.rm = TRUE), 1)),
    ap_hi_r  = paste0(round(mean(ap_hi,  na.rm = TRUE), 1), " ± ", round(sd(ap_hi,  na.rm = TRUE), 1)),
    ap_lo_r  = paste0(round(mean(ap_lo,  na.rm = TRUE), 1), " ± ", round(sd(ap_lo,  na.rm = TRUE), 1)),
    weight_r = paste0(round(mean(weight, na.rm = TRUE), 1), " ± ", round(sd(weight, na.rm = TRUE), 1)),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols      = -cardio,
    names_to  = "variable",
    values_to = "valor"
  ) %>%
  pivot_wider(
    names_from  = cardio,
    values_from = valor
  ) %>%
  mutate(Variable = case_when(
    variable == "age_r"    ~ "Edad",
    variable == "imc_r"    ~ "IMC",
    variable == "ap_hi_r"  ~ "Presión sistólica",
    variable == "ap_lo_r"  ~ "Presión diastólica",
    variable == "weight_r" ~ "Peso"
  )) %>%
  select(Variable, `Sin enfermedad` = No, `Con enfermedad` = Si)

# ===========================================================================
# Cuadro resumen 2: variables categoricas por grupo enfermedad
# ===========================================================================

eda_categoricas_por_cardio <- cardio %>%
  select(cardio, gender, cholesterol, gluc, smoke, alco, active) %>%
  pivot_longer(
    cols = -cardio,
    names_to = "variable",
    values_to = "categoria"
  ) %>%
  group_by(variable, categoria, cardio) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable, cardio) %>%
  mutate(porcentaje = paste0(round(n / sum(n) * 100, 1), "%")) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(
    names_from  = cardio,
    values_from = porcentaje
  ) %>%
  mutate(variable = nombres_variables[variable]) %>%
  rename(
    Variable  = variable,
    Categoría = categoria,
    `Sin enfermedad` = No,
    `Con enfermedad` = Si
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
  mutate(variable = nombres_variables[variable]) %>% 
  arrange(desc(abs(correlacion))) 

grafico_correlacion <- ggplot(
  cor_cardio,
  aes(x = reorder(variable, correlacion), y = correlacion)
) +
  #nombres fijos de variables sin tener que cambiar el dataset
  geom_col(fill = colores_default[1]) +
  coord_flip() +
  labs(
    title = "Correlación de variables con enfermedad cardiovascular",
    x = "Variable",
    y = "Correlación con enfermedad cardiovascular"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
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
    title = str_wrap("Proporción de enfermedad cardiovascular por actividad, glucosa y colesterol", width = 60),
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


cardio_presion <- cardio %>%
  mutate(
    categoria_presion = case_when(
      ap_hi < 120 ~ "Normal",
      ap_hi >= 120 & ap_hi < 140 ~ "Elevada",
      ap_hi >= 140 ~ "Alta",
      TRUE ~ NA_character_
    ),
    categoria_presion = factor(
      categoria_presion,
      levels = c("Normal", "Elevada", "Alta")
    )
  ) %>%
  group_by(categoria_presion, cardio) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(categoria_presion) %>%
  mutate(porcentaje = n / sum(n)) %>%
  ungroup()

grafico_presion <- ggplot(cardio_presion, aes(x = categoria_presion, y = porcentaje, fill = cardio)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = colores_si_no) +
  labs(
    title = str_wrap("Prevalencia de enfermedad cardiovascular por presión arterial sistólica", width = 60),
    x = "Categoría de presión sistólica",
    y = "Porcentaje",
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
    names_to = "Variable",
    values_to = "Faltantes"
  ) %>%
  mutate(
    `Porcentaje faltantes` = Faltantes / nrow(cardio),
      Variable = nombres_variables[Variable]
  )

#  Resumen de variables numericas
eda_numericas <- cardio %>%
  select(where(is.numeric), -id) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Valor"
  ) %>%
  group_by(Variable) %>%
  summarise(
    Media = mean(Valor, na.rm = TRUE),
    Mediana = median(Valor, na.rm = TRUE),
    sd = sd(Valor, na.rm = TRUE),
    Mínimo = min(Valor, na.rm = TRUE),
    Q1 = quantile(Valor, 0.25, na.rm = TRUE),
    Q3 = quantile(Valor, 0.75, na.rm = TRUE),
    Máximo = max(Valor, na.rm = TRUE),
    .groups = "drop" 
  ) %>% 
  mutate(
     Variable = nombres_variables[Variable]  
  )

#  Frecuencias de variables categoricas


#  Balance de la variable respuesta
eda_balance_cardio <- cardio %>%
  count(cardio) %>%
  rename( "Enfermedad Cardiovascular" = cardio) %>% 
  mutate(
    Porcentaje = scales::percent( n / sum(n), accuracy = 0.001)
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
  grafico_presion = grafico_presion,
  eda_tipos = eda_tipos,
  eda_faltantes = eda_faltantes,
  eda_numericas = eda_numericas,
  eda_categoricas_por_cardio = eda_categoricas_por_cardio,
  eda_balance_cardio = eda_balance_cardio,
  eda_por_cardio = eda_por_cardio,
  eda_dimensiones = eda_dimensiones
)

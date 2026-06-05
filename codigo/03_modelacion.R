# 03_modelacion.R
# Modelación estadística
# Autor:César Salazar
# Fecha:18/05/2026

library(MASS)     
library(moments) 
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(cowplot)
library(ggplot2)
library(showtext)
library(scales)
library(here)
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

# Distribuciones de ap_hi y ap_lo
# De ap_hi

# 1. Estadísticos de forma
cat("Skewness ap_hi:", skewness(cardio$ap_hi))
cat("Kurtosis ap_hi:", kurtosis(cardio$ap_hi))  

# 2. Ajuste MLE
fit_normal <- fitdistr(cardio$ap_hi, "normal")
fit_gamma  <- fitdistr(cardio$ap_hi, "gamma")

# 3. Visual: superponer ambas curvas y comparar con histograma
ggplot(cardio, aes(x = ap_hi)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60,
                 fill = "grey80", color = "grey60") +
  stat_function(fun = dnorm,
                args = list(mean = fit_normal$estimate["mean"],
                            sd   = fit_normal$estimate["sd"]),
                aes(color = "Normal"), linewidth = 1) +
  stat_function(fun = dgamma,
                args = list(shape = fit_gamma$estimate["shape"],
                            rate  = fit_gamma$estimate["rate"]),
                aes(color = "Gamma"), linewidth = 1) +
  scale_color_manual(values = c(Normal = "#E84855", Gamma = "#2E86AB")) +
  coord_cartesian(ylim = c(0, 0.047)) +          
  labs(title = "Distribución de ap_hi",
       x = "Presión sistólica (mmHg)", y = "Densidad",
       color = "Ajuste")

# De ap_lo

# 1. Estadísticos de forma
cat("Skewness ap_lo:", skewness(cardio$ap_lo))
cat("Kurtosis ap_lo:", kurtosis(cardio$ap_lo))  

# 2. Ajuste MLE
fit_normal <- fitdistr(cardio$ap_lo, "normal")
fit_gamma  <- fitdistr(cardio$ap_lo, "gamma")

# 3. Visual: superponer ambas curvas y comparar con histograma
ggplot(cardio, aes(x = ap_lo)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60,
                 fill = "grey80", color = "grey60") +
  stat_function(fun = dnorm,
                args = list(mean = fit_normal$estimate["mean"],
                            sd   = fit_normal$estimate["sd"]),
                aes(color = "Normal"), linewidth = 1) +
  stat_function(fun = dgamma,
                args = list(shape = fit_gamma$estimate["shape"],
                            rate  = fit_gamma$estimate["rate"]),
                aes(color = "Gamma"), linewidth = 1) +
  scale_color_manual(values = c(Normal = "#E84855", Gamma = "#2E86AB")) +
  coord_cartesian(ylim = c(0, 0.11)) +          
  labs(title = "Distribución de ap_lo",
       x = "Presión distólica (mmHg)", y = "Densidad",
       color = "Ajuste")


# Factor de Bayes

ap_hi_si <- cardio$ap_hi[cardio$cardio == "Si"]
ap_hi_no <- cardio$ap_hi[cardio$cardio == "No"]

ap_lo_si <- cardio$ap_lo[cardio$cardio == "Si"]
ap_lo_no <- cardio$ap_lo[cardio$cardio == "No"]

cardio_num <- as.numeric(cardio$cardio == "Si") # Se ocupa en número

# Se toma k como la cantidad de factores de riesgo

ecv_k0 <- cardio_num[cardio$n_factores == 0] # k = 0
ecv_k3 <- cardio_num[cardio$n_factores >= 3] # k es 3 o más

cat("\nn por grupo (extremos del gradiente):\n")
cat("  k = 0:   n =", length(ecv_k0), "\n")
cat("  k ≥ 3:   n =", length(ecv_k3), "\n")

# Hipótesis primaria

cat("\n====== BF: ap_hi ======\n")
bf_ap_hi <- ttestBF(x = ap_hi_si, y = ap_hi_no, rscale = sqrt(2)/2)
print(bf_ap_hi) # Evidencia extrema

# Hipótesis secundaria

cat("\n====== BF: ap_lo ======\n")
bf_ap_lo <- ttestBF(x = ap_lo_si, y = ap_lo_no, rscale = sqrt(2)/2)
print(bf_ap_lo) # Evidencia extrema

# Hipótesis de correlación acumulada

cat("\n====== BF: correlación gradiente ======\n")
bf_cor <- correlationBF(y = cardio_num, x = cardio$n_factores)
print(bf_cor)

# Este test falla, va en la sección de fallos
# Con datos completos:

r_observado <- cor(cardio$n_factores, cardio_num)
cat("Correlación observada (n completo):", round(r_observado, 4), "\n")

# Hipótesis de correlación acumulada con submuestreo reproducible

set.seed(2002)
idx_sub <- sample(nrow(cardio), size = 5000)

n_factores_sub <- cardio$n_factores[idx_sub]
cardio_num_sub <- cardio_num[idx_sub]

bf_cor <- correlationBF(y = cardio_num_sub, x = n_factores_sub)
print(bf_cor) # Evidencia extrema

# Hipótesis de contraste entre extremos del gradiente

cat("\n====== BF: extremos del gradiente ======\n")
bf_extremos <- ttestBF(x = ecv_k3, y = ecv_k0, rscale = sqrt(2)/2)
print(bf_extremos) # Evidencia extrema

# Tabla resumen de los 4 BF

# Función auxiliar para clasificar en la escala de Jeffreys
clasificar_bf <- function(bf) {
  case_when(
    bf > 100  ~ "Extrema a favor de H\u2081",
    bf > 30   ~ "Muy fuerte a favor de H\u2081",
    bf > 10   ~ "Fuerte a favor de H\u2081",
    bf > 3    ~ "Moderada a favor de H\u2081",
    bf > 1/3  ~ "Anecdótica",
    bf > 1/10 ~ "Moderada a favor de H\u2080",
    bf > 1/30 ~ "Fuerte a favor de H\u2080",
    TRUE      ~ "Muy fuerte a favor de H\u2080"
  )
}

bf_valores <- c(
  extractBF(bf_ap_hi)$bf,
  extractBF(bf_ap_lo)$bf,
  extractBF(bf_cor)$bf,
  extractBF(bf_extremos)$bf
)


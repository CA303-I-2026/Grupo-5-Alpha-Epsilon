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

ap_si  <- cardio$ap_hi[cardio$cardio == "Si"]
ap_no  <- cardio$ap_hi[cardio$cardio == "No"]

bf_resultado <- ttestBF(x = ap_si, y = ap_no)
bf_resultado





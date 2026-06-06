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

tabla_bf <- tibble(
  Prueba = c(
    "Presión sistólica (ap\\_hi)",
    "Presión diastólica (ap\\_lo)",
    "Correlación gradiente ECV",
    "Extremos del gradiente (k=0 vs k\u22653)"
  ),
  H0 = c(
    "\u03bc_ap\\_hi igual entre grupos",
    "\u03bc_ap\\_lo igual entre grupos",
    "\u03c1(n\u00b0 factores, cardio) = 0",
    "P(ECV|k=0) = P(ECV|k\u22653)"
  ),
  BF10      = bf_valores,
  log10_BF  = log10(bf_valores),
  Evidencia = clasificar_bf(bf_valores)
)

cat("\n====== Tabla resumen: Factores de Bayes ======\n")
print(tabla_bf, n = Inf)

# =============================================================================
# Posteriores e intervalos de credibilidad
# =============================================================================

set.seed(2002) # Para un random diferente

# Posterior de ap_hi
cat("\n====== Posterior: ap_hi (10 000 iteraciones MCMC) ======\n")
post_ap_hi <- posterior(bf_ap_hi, iterations = 10000)

# Diferencia de medias en mmHg
delta_ap_hi <- as.numeric(post_ap_hi[, "delta"] * sqrt(post_ap_hi[, "sig2"]))

ic_ap_hi     <- quantile(delta_ap_hi, c(0.025, 0.975))
media_ap_hi  <- mean(delta_ap_hi)
mediana_ap_hi <- median(delta_ap_hi)

cat("Diferencia de medias ap_hi (con ECV − sin ECV):\n")
cat("  Media posterior:  ", round(media_ap_hi, 2), "mmHg\n")
cat("  Mediana posterior:", round(mediana_ap_hi, 2), "mmHg\n")
cat("  IC 95% credibilidad: [", round(ic_ap_hi[1], 2), ",", round(ic_ap_hi[2], 2), "] mmHg\n")

# Gráfico: distribución posterior de la diferencia de medias
grafico_posterior_ap_hi <- ggplot(
  data.frame(delta = delta_ap_hi),
  aes(x = delta)
) +
  geom_density(fill = "#E63946", alpha = 0.4, color = "#E63946", linewidth = 1) +
  geom_vline(xintercept = media_ap_hi,  linetype = "dashed", color = "black") +
  geom_vline(xintercept = ic_ap_hi[1],  linetype = "dotted", color = "#2471A3", linewidth = 1) +
  geom_vline(xintercept = ic_ap_hi[2],  linetype = "dotted", color = "#2471A3", linewidth = 1) +
  labs(
    title    = "Distribución posterior: diferencia de presión sistólica",
    x        = "\u0394\u03bc (mmHg)",
    y        = "Densidad posterior"
  ) +
  set_tipografia()

# Posterior para correlación del gradiente
cat("\n====== Posterior: correlación gradiente (10 000 iteraciones) ======\n")
post_cor <- posterior(bf_cor, iterations = 10000)

rho_muestras <- as.numeric(post_cor[, "rho"])

ic_rho     <- quantile(rho_muestras, c(0.025, 0.975))
media_rho  <- mean(rho_muestras)

cat("Correlación ρ(n_factores, cardio):\n")
cat("  Media posterior:  ", round(media_rho, 4), "\n")
cat("  IC 95% credibilidad: [", round(ic_rho[1], 4), ",", round(ic_rho[2], 4), "]\n")

grafico_posterior_rho <- ggplot(
  data.frame(rho = rho_muestras),
  aes(x = rho)
) +
  geom_density(fill = "#2471A3", alpha = 0.4, color = "#2471A3", linewidth = 1) +
  geom_vline(xintercept = 0,         linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = ic_rho[1], linetype = "dotted", color = "#E63946", linewidth = 1) +
  geom_vline(xintercept = ic_rho[2], linetype = "dotted", color = "#E63946", linewidth = 1) +
  labs(
    title    = "Distribución posterior: correlación gradiente - ECV",
    x        = "\u03c1",
    y        = "Densidad posterior"
  ) +
  set_tipografia()

# =============================================================================
# Prevalencia de ECV: Beta - Bernoulli
# =============================================================================

n_total  <- nrow(cardio)
n_ecv    <- sum(cardio$cardio == "Si")
n_sano   <- n_total - n_ecv

# Parámetros de la posterior
alpha_post <- 1 + n_ecv
beta_post  <- 1 + n_sano

prev_media  <- alpha_post / (alpha_post + beta_post)
prev_moda   <- (alpha_post - 1) / (alpha_post + beta_post - 2)
prev_ic95   <- qbeta(c(0.025, 0.975), alpha_post, beta_post)

cat("\n====== BETA-BERNOULLI: prevalencia de ECV ======\n")
cat("n total:    ", n_total, "\n")
cat("n con ECV:  ", n_ecv, " (", round(n_ecv/n_total*100, 2), "%)\n")
cat("Posterior:  Beta(", alpha_post, ",", beta_post, ")\n")
cat("Media posterior:  ", round(prev_media, 6), "\n")
cat("Moda posterior:   ", round(prev_moda, 6), "\n")
cat("IC 95% credibilidad: [", round(prev_ic95[1], 5), ",", round(prev_ic95[2], 5), "]\n")

# Gráfico de la posterior Beta
margen <- 0.004  # Espacio intervalo del IC

theta_seq <- seq(prev_ic95[1] - margen, 
                 prev_ic95[2] + margen, 
                 length.out = 1000)

dens_post <- dbeta(theta_seq, alpha_post, beta_post)

grafico_beta_bernoulli <- ggplot(
  data.frame(theta = theta_seq, densidad = dens_post),
  aes(x = theta, y = densidad)
) +
  geom_line(color = "#E63946", linewidth = 1.2) +
  geom_vline(xintercept = prev_media,    linetype = "dashed", color = "black") +
  geom_vline(xintercept = prev_ic95[1],  linetype = "dotted",
             color = "#2471A3", linewidth = 1) +
  geom_vline(xintercept = prev_ic95[2],  linetype = "dotted",
             color = "#2471A3", linewidth = 1) +
  coord_cartesian(xlim = c(prev_ic95[1] - margen, prev_ic95[2] + margen)) +
  labs(
    title    = "Posterior Beta-Bernoulli: prevalencia de ECV",
    x = "\u03b8 (prevalencia)",
    y = "Densidad posterior"
  ) +
  set_tipografia()

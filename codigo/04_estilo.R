# 03_estilo.R
# Estilo para los graficos
# Autor: gabriel robles
# Fecha: 27 04 2026

# Codigo para estilo y paletas de color para graficos
# Para utilizar usar source("codigo/04_estilo.R")

#si no tiene los paquetes quitar el comentario linea 10
#install.packages(c("cowplot", "ggplot2", "showtext"))

library(ggplot2)
library(cowplot)
library(showtext)


 
colores_sexo <- c(
  "1" = "#01A6EA",
  "2" = "#FFB1CB"
)

# colores factores boleanos
# fumar, alcohol, cardio
colores_factorriesgo <- c(
    "0" = "#D9D9D9",  
    "1" = "#E63946"  
)

# Lo mismo pero al reves 
# active
# nivel de actividad es bueno entonces se invierten
colores_actividad <- c(
  "1" = "#D9D9D9",  
  "0" = "#E63946"  
)

# Para glucosa y colesterol
colores_gluccol = c(
  "1" = "#2A9D8F",  # Normal
  "2" = "#E9C46A",  # Alto
  "3" = "#c75438"   # Muy Alto, cambiado a uno que restalta mas la diferencia
)

# Para variables continuas como edad, ap y peso

colores_default = c("#457B9D", "#1D3557") # Cool blues for density/histograms


# colores finales para riesgo cardiovascular
colores_riesgocardio <- c(
  "Saludable" = "#2E86AB",  
 "En Peligro" = "#F6AF65",
  "Riesgo Alto" = "#F24236"   
)




# Funcion que cambia la tipografia usando cowplot. 
# Se pone despues de un + a los graficos
set_tipografia <- function(base_size = 12, base_family = "serif") {
  theme_cowplot(font_size = base_size, font_family = base_family) %+replace%
    theme(
      plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5),
      axis.title = element_text(face = "italic"),
      legend.position = "bottom",
      panel.grid.major.y = element_line(color = "grey90", size = 0.5),
      strip.background = element_rect(fill = "white", color = "black")
    )
}




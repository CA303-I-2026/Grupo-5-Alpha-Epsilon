# Codigo para estilo y paletas de color para graficos
# Para utilizar usar source("codigo/04_estilo.R")

#si no tiene los paquetes quitar el comentario linea 5
install.packages(c("cowplot", "ggplot2", "showtexts"))

library(ggplot2)
library(cowplot)
library(showtexts)

colores_riesgocardio <- c(
  "Saludable" = "#2E86AB",  
  "Riesgo Alto" = "#F24236",  
  "En Peligro" = "#F6AF65" 
)


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




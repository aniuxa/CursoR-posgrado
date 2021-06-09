
if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse, # conjunto de paquetes tidy
               broom, # paquete para adecuar resultados estadísticos
               DescTools, #caja de herramientas estadísticas
               esquisse, # para hacer ggplot con drag and drop
               haven, # importa archivos desde formatos .dta y .sav
               janitor,# para tabulado y limpieza de nombres
               RColorBrewer, #paletas de color
               wesanderson, #paletas de color películas Wes Anderson
               sjlabelled, #manejo de etiquetas y edición
               readxl, writexl #excel
               )


if (!require("remotes")) {
  install.packages("remotes")
 }

remotes::install_github("diegovalle/mxmaps") # mapas
#https://www.diegovalle.net/mxmaps/

# Cuando instalamos de esta forma es posible que se pregunte por actualizar algún paquetes. 
# Así como si deseas instalar algún paquete que se compile. También hay que poner que sí.


# An�lisis de texto


pacman::p_load(tidyverse, tm, NLP, SnowballC, wordcloud, ggwordcloud,
               quanteda, udpipe, ggraph, igraph,
               sjlabelled, RColorBrewer, readxl, janitor, textrank,
               broom, wesanderson, epubr, pdftools)
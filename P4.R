## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------------------------------------------------------------------
rm(list=ls()) ### borra objetos en el ambiente

# Para los mapas
if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse, readxl,janitor,
               haven, 
               remotes) #carga los paquetes necesarios


## -----------------------------------------------------------------------------------------------------------------------------

#if (!require("remotes")) {
#  install.packages("remotes")
# }

#remotes::install_github("diegovalle/mxmaps") # mapas
#https://www.diegovalle.net/mxmaps/

# Hay que cargar los instalados

library(mxmaps)



## -----------------------------------------------------------------------------------------------------------------------------

ICE_2021 <- read_excel("datos/ICE_2021.xlsx", sheet = "para_importar")
ICE_2021 <- clean_names(ICE_2021) # limpia los nombres



## -----------------------------------------------------------------------------------------------------------------------------
data("df_mxstate_2020") # carga la base estatal del paquete
glimpse(df_mxstate_2020)



## -----------------------------------------------------------------------------------------------------------------------------

df_mxstate_2020$value <- df_mxstate_2020$pop # paso esencial

glimpse(df_mxstate_2020$region)


mxstate_choropleth(df_mxstate_2020,
                   title = "Total población, por Estado") 




## -----------------------------------------------------------------------------------------------------------------------------

ICE_2021$value<- ICE_2021$homicidios

mxstate_choropleth(ICE_2021,
                   title = "Tasa de homicidios") 

mapa<-mxstate_choropleth(ICE_2021,
                         title = "Tasa de homicidios") 
mapa #imprime




## -----------------------------------------------------------------------------------------------------------------------------
data("df_mxmunicipio_2020")

df_mxmunicipio_2020$value <-df_mxmunicipio_2020$indigenous_language/df_mxmunicipio_2020$pop * 100

mxmunicipio_choropleth(df_mxmunicipio_2020, 
                       num_colors = 1,
                       title = "Porcentaje de la población que habla\nuna lengua indígena",
                       legend = "%")


## -----------------------------------------------------------------------------------------------------------------------------
mxmunicipio_choropleth(df_mxmunicipio_2020, 
                       num_colors = 1,
                       zoom = subset(df_mxmunicipio_2020, state_name %in% 
                                       c("Yucatán", "Veracruz"))$region,
                       title = "Porcentaje de la población que habla\nuna lengua indígena en Yucatány Veracruz",
                       show_states = FALSE,
                       legend = "%")

rm(df_mxmunicipio_2020, mapa,df_mxstate_2020) # nos roba mucha memoria



## -----------------------------------------------------------------------------------------------------------------------------
detach("package:mxmaps", unload = TRUE)



## -----------------------------------------------------------------------------------------------------------------------------
rm(list=ls()) ### borra objetos en el ambiente

# Para texto
pacman::p_load(tidyverse, tm, NLP, SnowballC, wordcloud, ggwordcloud,
               quanteda, udpipe, ggraph, igraph,
               sjlabelled, RColorBrewer, readxl, janitor, textrank,
               broom, wesanderson, epubr, pdftools)


## -----------------------------------------------------------------------------------------------------------------------------

## Insumos
path1<-"./datos/kawabata.epub" # formato epub
path2<-"./datos/lobo_antunes.pdf" #formato pdf
path3<-"./datos/amlo_mar2021.txt" #formato txt



## -----------------------------------------------------------------------------------------------------------------------------
udmodel <- udpipe_download_model(language = "spanish")  # esto trabaja con la estructura del español


## -----------------------------------------------------------------------------------------------------------------------------

epub_head(path1) # muestra lo primero
epub_meta(path1) # muestra el meta-data del libro
x <- epub(path1) # Importa todo el libro en el objeto x, pero no queremos todo
x
class(x)


## -----------------------------------------------------------------------------------------------------------------------------
#x<-epub(file.choose())


## -----------------------------------------------------------------------------------------------------------------------------
x$data # esto muestra que aquí adentro está lo que queremos
epub_kaw<-x$data[[1]] # Nos quedamos con esa base de datos
epub_kaw<-epub_kaw[7, ]  # Nos quedamos con el cuento Campanas del templo



## -----------------------------------------------------------------------------------------------------------------------------
kawabata<-udpipe(x = paste(epub_kaw$text), #paste nos asegura que se pegue como caracter
                      object=udmodel) #"tokeniza" el texto



## -----------------------------------------------------------------------------------------------------------------------------
glimpse(kawabata)


## -----------------------------------------------------------------------------------------------------------------------------
kawabata %>% 
  tabyl(upos) # esto nos da un tabulado


## -----------------------------------------------------------------------------------------------------------------------------
stats<- kawabata %>% 
  filter(upos=="NOUN")

stats.freq <- txt_freq(stats$token)
stats.freq$key <- factor(stats.freq$key, levels = rev(stats.freq$key)) # ordena

stats.freq[1:25,] %>% 
  ggplot(aes(x=key, y=freq)) +geom_bar(stat="identity") + 
  coord_flip() + labs(y="Frecuencia", x="Sustantivo") + theme_minimal()



## -----------------------------------------------------------------------------------------------------------------------------
set.seed(42) # tiene un elemento aleatorio

stats.freq[1:25,] %>% 
  ggplot(aes(label=key, size=freq)) +  geom_text_wordcloud() +
  theme_minimal()


## -----------------------------------------------------------------------------------------------------------------------------
stats.freq[1:25,] %>% 
    ggplot(
        aes(
        label =key, size = freq,
        color = factor(sample.int(15, nrow(stats.freq[1:25,]), replace = TRUE))
        )
    ) +
      geom_text_wordcloud_area() +
      scale_size_area(max_size = 12) +
      theme_minimal()



## -----------------------------------------------------------------------------------------------------------------------------
set.seed(1234)

paleta_color<-wes_palette(n=5,
                          name="Darjeeling1",
                          type="continuous")

wordcloud(words = stats.freq$key, freq = stats.freq$freq,
          min.freq = 5,
          max.words=200, 
          random.order=FALSE, 
          colors=paleta_color)

    


## -----------------------------------------------------------------------------------------------------------------------------

stats<- kawabata %>% 
  filter(upos=="ADJ") #checa que esto sobreescribe los objetos que teníamos

stats.freq <- txt_freq(stats$token)
stats.freq$key <- factor(stats.freq$key, levels = rev(stats.freq$key)) # ordena

stats.freq[1:25,] %>% 
  ggplot(aes(x=key, y=freq)) +geom_bar(stat="identity") + 
  coord_flip() + labs(y="Frecuencia", x="Adjetivo") + theme_minimal()


## -----------------------------------------------------------------------------------------------------------------------------
#Elegir palabras comunes
stopwords("spanish") # de la paquetería quanteda
stop<-stopwords("spanish") # guarda todas esas palabras en un objeto
stop


## -----------------------------------------------------------------------------------------------------------------------------
no_upos<-c("X","SYM","PUNCT")


## -----------------------------------------------------------------------------------------------------------------------------
stats<- kawabata %>% 
  filter(!upos%in%no_upos) %>%  #selecciona todos menos estos tipos
  mutate(token=tolower(token)) %>% # cambia a que token no tenga mayúsculas
  filter(!token%in%stop)  # selecciona todas las palabras menos las que están en nuestro vector "stop"

stats.freq <- txt_freq(stats$token)
stats.freq$key <- factor(stats.freq$key, levels = rev(stats.freq$key)) # ordena



## -----------------------------------------------------------------------------------------------------------------------------
stats.freq[1:25,] %>% 
  ggplot(aes(x=key, y=freq)) +geom_bar(stat="identity") + 
  coord_flip() + labs(y="Frecuencia", x="Palabra") + theme_minimal()


## -----------------------------------------------------------------------------------------------------------------------------
paleta_color<-wes_palette(n=5,
                          name="Darjeeling1",
                          type="continuous")

wordcloud(words = stats.freq$key, freq = stats.freq$freq,
          min.freq = 5,
          max.words=120, # aquí le cambié
          random.order=FALSE, 
          colors=paleta_color)



## -----------------------------------------------------------------------------------------------------------------------------
kawabata$id <- unique_identifier(kawabata, fields = c("sentence_id", "doc_id"))

dtm <- kawabata %>% 
  filter(upos %in% c("NOUN", "ADJ")) # objeto de solo nombres y adjetivos

dtm <- dtm %>% 
  document_term_frequencies( document = "id", term = "lemma")
dtm <- document_term_matrix(dtm)
dtm <- dtm_remove_lowfreq(dtm, minfreq = 5)

termcorrelations <- dtm_cor(dtm)
y <- as_cooccurrence(termcorrelations)
y <- subset(y, term1 < term2 & abs(cooc) > 0.2)
y <- y[order(abs(y$cooc), decreasing = TRUE), ]
head(y,24)


## -----------------------------------------------------------------------------------------------------------------------------
write_csv(y, "kawabata_corr.csv") # lo guarda en nuestro directorio


## -----------------------------------------------------------------------------------------------------------------------------
lobo_pdf <- pdf_text(path2)


## -----------------------------------------------------------------------------------------------------------------------------
#Es un vector de caracteres, no una matrix
lobo_pdf[19]


## -----------------------------------------------------------------------------------------------------------------------------
lobo_p18<-lobo_pdf[18]
lobo<-udpipe(x = lobo_p18, object=udmodel) #"tokeniza" el texto



## -----------------------------------------------------------------------------------------------------------------------------
text <- readLines(path3)
amlo<-udpipe(x = text, object=udmodel) #"tokeniza" el texto



## -----------------------------------------------------------------------------------------------------------------------------
stats<- amlo%>% 
  filter(!upos%in%no_upos) %>%  #selecciona todos menos estos tipos
  mutate(token=tolower(token)) %>% # cambia a que token no tenga mayúsculas
  filter(!token%in%stop)  # selecciona todas las palabras menos las que están en nuestro vector "stop"

stats.freq <- txt_freq(stats$token)
stats.freq$key <- factor(stats.freq$key, levels = rev(stats.freq$key)) # ordena



## -----------------------------------------------------------------------------------------------------------------------------
paleta_color<-wes_palette(n=7,
                          name="GrandBudapest2",
                          type="continuous")

wordcloud(words = stats.freq$key, freq = stats.freq$freq,
          min.freq = 5,
          max.words=120, # aquí le cambié
          random.order=FALSE, 
          colors=paleta_color)


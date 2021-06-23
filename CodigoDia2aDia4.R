
## paquetes ----

if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse, readxl, writexl, haven, sjlabelled, foreign,
               janitor, esquisse) #carga los paquetes necesarios para esta prÃ¡ctica


# Más importación ----

## Excel ----

ICI_2018 <- readxl::read_excel("./datos/ICI_2018.xlsx", sheet = "para_importar")
View(ICI_2018)

writexl::write_xlsx(ICI_2018, path = "Mi_Exportacion.xlsx")

## Stata -----

lapop2019 <- haven::read_dta("./datos/lapop2019.dta")
haven::write_dta(lapop2019, "./datos/mi_exportacion.dta", version = 12)

## SPSS ----
encevi_hogar<- haven::read_sav("./datos/encevi_hogar.sav")
haven::write_sav(encevi_hogar, "mi_exportacion.sav")


## Importación archivos separados por comas -----

mig_inter_quin_proyecciones <- read.csv("http://www.conapo.gob.mx/work/models/CONAPO/Datos_Abiertos/Proyecciones2018/mig_inter_quin_proyecciones.csv", encoding="latin1")
#View(mig_inter_quin_proyecciones)
names(mig_inter_quin_proyecciones)
remove(mig_inter_quin_proyecciones)

# Revisión básica ----
class(lapop2019) # tipo de objeto
names(lapop2019) # lista las variables
head(lapop2019) # muestra las primeras 6 líneas
table(lapop2019$soct2, useNA="ifany") # un tabulado simple

 
lapop2019 %>% 
  dplyr::select(q1, q2) %>% 
  head 


lapop2019 %>% 
  dplyr::select(q1, q2) %>% 
  glimpse()

class(lapop2019$q1)

table(lapop2019$q1)
table(sjlabelled::as_label(lapop2019$q1))


# revisión con dplyr ----

dplyr::glimpse(lapop2019)
dplyr::glimpse(lapop2019[,1:10]) # en corchete del lado derecho podemos ojear columnas 

# Etiquetas ----

print(get_labels(lapop2019)) #todas
print(get_labels(lapop2019[, 1:10])) #de las primeras 10 variables
print(get_labels(lapop2019$soct2)) #


#print(get_label(lapop2019)) #todas
print(get_label(lapop2019[, 1:10])) #de las primeras 10 variables
print(get_label(lapop2019$soct2)) #

glimpse(lapop2019$soct2)

# Selección 

x<-lapop2019$ed
x<-lapop2019[["ed"]]  # Â¡Ojo con las comillas! 
x<-lapop2019[,126]
x<-lapop2019[,"ed"]


x<-lapop2019 %>% 
  select(ed)

# Limpieza de nombres ----
names(ICI_2018)
ICI_2018<-janitor::clean_names(ICI_2018) # limpia los nombres tipo "snakecase"
names(ICI_2018)


## Seleccción inversa ----

x<-lapop2019 %>% 
  select(-c(ed, q1, q2)) # punto para "select"

x<-lapop2019[,-c(126, 127)]

rm(x) #rm sólo bota objetos

lapop2019$ed_2<-lapop2019$ed
lapop2019$ed_2<-NULL

library(magrittr)
lapop2019<-lapop2019 %>% 
  dplyr::mutate(ed_2=ed)

# "Subsetting" ----

# Base
subset1<-lapop2019[lapop2019$ed>4,]
subset2<- lapop2019[, c("q1", "q2", "ed")]
subset3<- lapop2019[(lapop2019$ed>4 & lapop2019$q1==1 ), c("q1", "q2", "ed")]

# dplyr
subset4<-lapop2019 %>% 
  dplyr::filter(ed>4 & q1==1) %>% # filtro me selecciona casos
  dplyr::select(q1, q2, ed) # selecciona variables

subset4<- subset4 %>% 
  mutate(var=if_else(q2<30, "Joven", "No joven"))

# Tabulados ----

lapop2019 %>% 
  dplyr::count(q1==2) # cuentan los casos que cumplen con la condición "q1==2"

lapop2019 %>%
  with(table(q1))

lapop2019 %>%
  dplyr::mutate(q1=as_label(q1))  %>%
  janitor::tabyl(q1)

lapop2019 %>% 
  dplyr::mutate(q1=as_label(q1))  %>%
  janitor::tabyl(q1) %>% 
  janitor::adorn_totals()

class(lapop2019$q1) # variable sin etiqueta
class(as_label(lapop2019$q1)) # variable con etiqueta

class(as_label(lapop2019$soct2)) # variable ordinal

# Ojeando 
dplyr::glimpse(lapop2019$q1)

# Tabulado univariado:

lapop2019 %>% mutate(q1=as_label(q1)) %>% # cambia los valores de la variable a sus etiquetas
                tabyl(q1) %>% # para hacer la tabla
                adorn_totals() %>% # añade totales
                adorn_pct_formatting()  # nos da porcentaje en lugar de proporciÃ³n
                
glimpse(lapop2019$soct2
        )

lapop2019 %>%
  mutate(soct2=as_label(soct2)) %>% 
  tabyl(soct2)

lapop2019 %>% 
 # filter(soct2>1) %>% 
  mutate(soct2=as_label(soct2)) %>% 
  tabyl(soct2, show_missing_levels=F ) %>% # esta opción elimina los valores con 0
  adorn_totals()  

# Tabulados bivariados ----
glimpse(lapop2019$soct2)

lapop2019 %>% 
  mutate(soct2=as_label(soct2), 
         q1=as_label(q1)) %>% # para que las lea como factor
  tabyl(soct2, q1, show_missing_levels=F ) %>% # incluimos aquí 
  adorn_totals()  

lapop2019 %>%   
    count(soct2==1 & q1==1) # nos da la segunda celda de la izquierda

lapop2019 %>% 
  mutate(soct2=as_label(soct2)) %>% 
  mutate(q1=as_label(q1)) %>% # para que las lea como factor
  tabyl(soct2, q1, show_missing_levels=F ) %>% # incluimos aquí dos variables
  adorn_totals("row")  


tab1<-
  
  lapop2019 %>% 
  mutate(soct2=as_label(soct2)) %>% 
  mutate(q1=as_label(q1)) %>% # para que las lea como factor
  tabyl(soct2, q1, show_missing_levels=F ) %>% # incluimos aquÃ­ dos variable
  adorn_totals(c("col", "row")) 

write_xlsx(tab1, path="tabulado.xlsx")

lapop2019 %>% 
  mutate(soct2=as_label(soct2)) %>% 
  mutate(q1=as_label(q1)) %>% # para que las lea como factor
  tabyl(soct2, q1, show_missing_levels=F ) %>% # incluimos aquÃ­ dos variable
  adorn_totals(c("col", "row")) %>% 
  adorn_percentages("col") %>% # Divide los valores entre el total de la columna
  adorn_pct_formatting() # lo vuelve porcentaje


lapop2019 %>% 
  mutate(soct2=as_label(soct2)) %>% 
  mutate(q1=as_label(q1)) %>% # para que las lea como factor
  tabyl(soct2, q1, show_missing_levels=F ) %>% # incluimos aquÃ­ dos variable
  adorn_totals(c("col", "row")) %>% 
  adorn_percentages("row") %>% # Divide los valores entre el total de la fila
  adorn_pct_formatting() # lo vuelve porcentaje

lapop2019 %>% 
  mutate(soct2=as_label(soct2)) %>% 
  mutate(q1=as_label(q1)) %>% # para que las lea como factor
  tabyl(soct2, q1, show_missing_levels=F ) %>% # incluimos aquÃ­ dos variable
  adorn_totals(c("col", "row")) %>% 
  adorn_percentages("all") %>% # Divide los valores entre el total de la poblaciÃ³n
  adorn_pct_formatting() # lo vuelve porcentaje

# Medidas numéricas:

summary(lapop2019$ed) ## educaciÃ³n

lapop2019 %>% 
  dplyr::group_by(as_label(q1), as_label(soct2)) %>% 
  dplyr::summarize(media_ed=mean(ed, na.rm=T),
                   mediana_ed=median(ed, na.rm=T),
                   media_edad=mean(q2, na.rm=T))

# Histogramas

hist(lapop2019$ed)

hist(lapop2019$ed, 
     main="Histograma de escolaridad", 
     xlab="Años aprobados", ylab="Frecuencia") 


hist(lapop2019$ed, 
     main="Histograma de escolaridad",
     xlab="Años aprobados", 
     ylab="Frecuencia", col="deeppink1") 




lapop2019 %>% 
    with(
      hist(ed)
         ) # con with, para que entienda

lapop2019 %>% 
  filter(!is.na(ed)) %>% # la ventaja de esta forma es que podemos hacer más operaciones
    with(hist(ed, main= "histograma"))

!is.na(c(NA, 1))


# Gráficos de base ----

plot(lapop2019$soct2)

plot(as_label(lapop2019$soct2))

barplot(table(as_label(lapop2019$soct2)))

# Gráficos de ggplot ----
library(esquisse)

ggplot(ICI_2018) +
  aes(x = homicidios_dolosos, y = indice_de_estados_fragiles) +
  geom_point(
    shape = "diamond open",
    size = 1.5,
    colour = "#440154"
  ) +
  labs(
    x = "Tasa de homicidios",
    y = "Indice de estados frágiles",
    title = "Índice de estados frágiles vs Tasa de homicidos",
    subtitle = "41 países según IMCO",
    caption = "Fuente: IMCO(2018)"
  ) +
  theme_minimal()


g1<-lapop2019 %>%
   ggplot(aes(as_label(soct2)))

g1 # imprime el lienzo


g1  + geom_bar(aes(fill=as_label(q1))) 


# Para variables cuantitativas

g2<-lapop2019 %>% 
  ggplot(aes(x=ed))


g2 + geom_histogram(bins=9)


g2 + geom_density()

# Gráficos bivariados
g1<-lapop2019 %>%
  ggplot(aes(as_label(soct2)))

g1 # imprime el lienzo

g1 +  geom_bar(aes(fill = as_label(q1)),
               position="dodge") #pone las categorías lado a lado y no apiladas


g_bivariado<-g1 +  geom_bar(aes(fill = as_label(q1)),
               position="fill") 

library(RColorBrewer)

paleta<-c("#176777", "#231777")

g_bivariado + scale_fill_brewer(palette="Dark2")

g_bivariado + scale_fill_manual(values=paleta) + theme_minimal()

# Cuanti cuanti ----

ICI_2018 %>% 
  ggplot(aes(x=indice_de_gini,
             y=indice_de_vulnerabilidad_a_efectos_del_cambio_climatico))+
  geom_point()


ICI_2018 %>% 
  ggplot(aes(x=indice_de_gini,
             y=indice_de_vulnerabilidad_a_efectos_del_cambio_climatico))+
  geom_jitter()

# geometría "text"

ICI_2018 %>% 
  ggplot(aes(indice_de_gini,indice_de_vulnerabilidad_a_efectos_del_cambio_climatico)) +
  geom_text(aes(label=indicador), check_overlap = TRUE)


ICI_2018 %>% 
  ggplot(aes(indice_de_gini,indice_de_vulnerabilidad_a_efectos_del_cambio_climatico)) +
  geom_label(aes(label=indicador))

# Etiquetas ----
ICI_2018 %>% 
  janitor::tabyl(miembro_de_la_alianza_para_el_gobierno_abierto)

etiqueta<-c("No miembro", "miembro")

ICI_2018<-ICI_2018 %>% 
  sjlabelled::set_labels(miembro_de_la_alianza_para_el_gobierno_abierto, labels=etiqueta)

glimpse(ICI_2018$miembro_de_la_alianza_para_el_gobierno_abierto)

# renombrar una variable
ICI_2018<-ICI_2018 %>% 
  dplyr::rename(miembros_al=miembro_de_la_alianza_para_el_gobierno_abierto)

ICI_2018$miembros_al

# Introducción de una tercera variable con color
ICI_2018<- ICI_2018 %>% 
  mutate(miembros_al_etiq=as_label(miembros_al))
  
ICI_2018 %>% 
  ggplot() +
  aes(x=indice_de_gini,
             y=indice_de_vulnerabilidad_a_efectos_del_cambio_climatico,
             color=miembros_al_etiq) +
  geom_point() + scale_color_manual(values=paleta)


# Introducción de una tercera variable con "shape"

ICI_2018 %>% 
  ggplot(aes(x=indice_de_gini,
             y=indice_de_vulnerabilidad_a_efectos_del_cambio_climatico,
             shape=as_label(miembros_al), color=as_label(miembros_al))
  ) +
  geom_point() # ojo, hay un límite para las formas


ICI_2018 %>% 
  ggplot(aes(x=indice_de_gini,
             y=indice_de_vulnerabilidad_a_efectos_del_cambio_climatico)) +
  geom_point() + facet_wrap(~as_label(miembros_al), scales="free_x")

# columnas 
ICI_2018 %>% 
  ggplot(aes(x=indice_de_gini,
             y=indice_de_vulnerabilidad_a_efectos_del_cambio_climatico)) +
  geom_point() + facet_grid(.~as_label(miembros_al))


#filas 
ICI_2018 %>% 
  ggplot(aes(x=indice_de_gini,
             y=indice_de_vulnerabilidad_a_efectos_del_cambio_climatico)) +
  geom_point() +
  facet_grid(as_label(miembros_al)~.)


# Smooth 

ICI_2018 %>% 
  ggplot(aes(x=indice_de_gini,
             y=indice_de_vulnerabilidad_a_efectos_del_cambio_climatico)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_grid(as_label(miembros_al)~.)


ICI_2018 %>% 
  ggplot(aes(x=indice_de_gini,
             y=indice_de_vulnerabilidad_a_efectos_del_cambio_climatico,
             color=as_label(miembros_al))) +
  geom_text(aes(label=indicador), check_overlap = T) +
  geom_smooth(method="lm") + scale_color_brewer(palette = "Dark2") +
  theme_minimal()


# Introducción de una cuarta variable cuanti

ICI_2018 %>% 
  ggplot(aes(x=indice_de_gini,
             y=indice_de_vulnerabilidad_a_efectos_del_cambio_climatico,
             color=as_label(miembros_al))) +
  geom_point(aes(size=crecimiento_del_pib))+ # ojo
  theme_minimal()

# Equivalente  

ICI_2018 %>% 
  ggplot(aes(x=indice_de_gini,
             y=indice_de_vulnerabilidad_a_efectos_del_cambio_climatico,
             color=as_label(miembros_al))) +
  geom_point(aes(size=crecimiento_del_pib))+ 
  geom_text(aes(label=indicador))
  theme_minimal()
  
  
  # Agregaremos etiquetas
  
  ICI_2018 %>% 
    ggplot(aes(x=indice_de_gini,
               y=indice_de_vulnerabilidad_a_efectos_del_cambio_climatico,
               color=as_label(miembros_al),
               size=crecimiento_del_pib)) +
    geom_text(aes(label=indicador),
              check_overlap = TRUE)+
    theme_minimal()


# Cuali y cuanti ----
  
  lapop2019 %>%
    ggplot(aes(ed)) # años de escolaridad
  
  
  lapop2019 %>%
    ggplot(aes(ed,
           fill=as_label(q1))) +
    geom_density()
  
  
  # Modificamos la opacidad
  lapop2019 %>%
    ggplot(aes(x=ed, 
               fill=as_label(q1),
               alpha=I(0.5))) + 
    geom_density() + theme_minimal()
  
  
# Indicadores ----

  lapop2019 %>% 
    names()
  
  
  lapop2019 %>% 
    select(starts_with("b"))
  
  lapop2019<-lapop2019 %>%
    mutate(index= rowMeans(across(c(b1:b47a)), na.rm = T))  
  
  summary(lapop2019$index)
  
  lapop2019<-lapop2019 %>% 
    mutate(confianza=if_else(
      b1>4, # condición
      1, # cuando es verdadero
      0 # cuando es falso
    ))
  
  lapop2019 %>% 
    filter(!is.na(confianza)) %>% 
    tabyl(confianza)
  
  
  lapop2019 %>% 
    ggplot(aes(y=index, x=as_label(idio2)))+ geom_boxplot()+ theme_minimal()+
    coord_flip()
  
  
  lapop2019 %>% 
    ggplot(aes(x=index, y=as_label(idio2)))+ geom_boxplot()+ theme_minimal()+
    labs(y="Esta es una etiqueta")
  
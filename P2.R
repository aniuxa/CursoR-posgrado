
## pacman ----

if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse, readxl, writexl, haven, sjlabelled, foreign) #carga los paquetes necesarios para esta prÃ¡ctica


# Más importación ----

## Excel ----

ICI_2018 <- readxl::read_excel("./datos/ICI_2018.xlsx", sheet = "para_importar")
#View(ICI_2018)
writexl::write_xlsx(ICI_2018, path = "Mi_Exportacion.xlsx")

## Stata -----

lapop2019 <- haven::read_dta("./datos/lapop2019.dta")
haven::write_dta(lapop2019, "./datos/mi_exportacion.dta", version = 12)

## SPSS ----
encevi_hogar<- haven::read_sav("./datos/encevi_hogar.sav")
haven::write_sav(lapop2019 , "mi_exportacion.sav")


## Importación archivos separados por comas -----

mig_inter_quin_proyecciones <- read.csv("http://www.conapo.gob.mx/work/models/CONAPO/Datos_Abiertos/Proyecciones2018/mig_inter_quin_proyecciones.csv", encoding="latin1")
#View(mig_inter_quin_proyecciones)
names(mig_inter_quin_proyecciones)


# Revisión básica ----
class(lapop2019) # tipo de objeto
names(lapop2019) # lista las variables
head(lapop2019) # muestra las primeras 6 lÃ­neas
table(lapop2019$soct2) # un tabulado simple


lapop2019 %>% 
  dplyr::select(q1, q2) %>% 
  head
lapop2019 %>% 
  dplyr::select(q1, q2) %>% 
  glimpse

class(lapop2019$q1)

table(lapop2019$q1)
table(sjlabelled::as_label(lapop2019$q1))


# revisión con dplyr ----

dplyr::glimpse(lapop2019)
dplyr::glimpse(lapop2019[,1:10]) # en corchete del lado derecho podemos ojear columnas 

# Etiquetas ----
#print(get_labels(lapop2019)) #todas
print(get_labels(lapop2019[, 1:10])) #de las primeras 10 variables
print(get_labels(lapop2019$soct2)) #


#print(get_label(lapop2019)) #todas
print(get_label(lapop2019[, 1:10])) #de las primeras 10 variables
print(get_label(lapop2019$soct2)) #

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
  select(-ed)
x<-lapop2019[,-126]

rm(x) #rm sólo bota objetos

lapop2019$ed_2<-lapop2019$ed
lapop2019$ed_2<-NULL


# "Subsetting" ----

# Base
subset1<-lapop2019[lapop2019$ed>4,]
subset2<- lapop2019[, c("q1", "q2", "ed")]
subset3<- lapop2019[(lapop2019$ed>4 & lapop2019$q1==1 ), c("q1", "q2", "ed")]

# dplyr
subset4<-lapop2019 %>% 
  dplyr::filter(ed>4 & q1==1) %>%
  dplyr::select(q1, q2, ed)

# Tabulados ----

lapop2019 %>% 
  dplyr::count(q1==2) # cuentan los casos que cumplen con la condición "q1==2"

lapop2019 %>%
  with(
    table(q1)
    )

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
                adorn_totals() %>% # aÃ±ade totales
                adorn_pct_formatting()  # nos da porcentaje en lugar de proporciÃ³n
                
glimpse(lapop2019$soct2
        )

lapop2019 %>%
  mutate(soct2=as_label(soct2)) %>% 
  tabyl(soct2)

lapop2019 %>% 
  mutate(soct2=as_label(soct2)) %>% 
  tabyl(soct2, show_missing_levels=F ) %>% # esta opción elimina los valores con 0
  adorn_totals()  

# Tabulados bivariados ----

lapop2019 %>% 
  mutate(soct2=as_label(soct2)) %>% 
  mutate(q1=as_label(q1)) %>% # para que las lea como factor
                tabyl(soct2, q1, show_missing_levels=F ) %>% # incluimos aquÃ­ 
                adorn_totals()  

lapop2019 %>%   
    count(soct2==1 & q1==1) # nos da la segunda celda de la izquierda

lapop2019 %>% 
  mutate(soct2=as_label(soct2)) %>% 
  mutate(q1=as_label(q1)) %>% # para que las lea como factor
  tabyl(soct2, q1, show_missing_levels=F ) %>% # incluimos aquí dos variables
  adorn_totals("col")  


lapop2019 %>% 
  mutate(soct2=as_label(soct2)) %>% 
  mutate(q1=as_label(q1)) %>% # para que las lea como factor
  tabyl(soct2, q1, show_missing_levels=F ) %>% # incluimos aquÃ­ dos variable
  adorn_totals(c("col", "row")) 

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
  summarise(nombre_indicador=mean(ed, na.rm=T))

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
    with(hist(ed)) # con with, para que entienda

lapop2019 %>% 
  filter(!is.na(ed)) %>% # la ventaja de esta forma es que podemos hacer más operaciones
    with(hist(ed, main= "histograma"))



# Introducci�n al paquete ----

2+5
5*3

#Para escribir comentarios y que no los lea como operaciones ponemos el símbolo de gato
# Lo podemos hacer para un comentario en una línea o la par de una instrucción
1:5               # Secuencia 1-5

seq(1, 10, 0.5)   # Secuencia con incrementos diferentes a 1

c('a','b','c')  # Vector con caracteres
1:7             # Entero
40<80           # Valor logico
2+2 == 5        # Valor logico
T == TRUE       # T expresion corta de verdadero


# Objetos ---
x <- 24         # Asignacion de valor 24 a la variable x para su uso posterior (OBJETO)
x/2             # Uso posterior de variable u objeto x
x               # Imprime en pantalla el valor de la variable u objeto
x <- TRUE       # Asigna el valor logico TRUE a la variable x OJO: x toma el ultimo valor que se le asigna
x


y <- c(2,4,6)     # Vector numerico
y <- c('Primaria', 'Secundaria') # Vector caracteres


y[2]              # Acceder al segundo valor del vector y
y[3] <- 'Preparatoria y más' # Asigna valor a la tercera componente del vector
sex <-1:2         # Asigna a la variable sex los valores 1 y 2
names(sex) <- c("Femenino", "Masculino") # Asigna nombres al vector de elementos sexo
sex[2]            # Segundo elemento del vector sex


# Matrices ----
m <- matrix (nrow=2, ncol=3, 1:6, byrow = TRUE) # Matrices Ejemplo 1
m

m <- matrix (nrow=2, ncol=3, 1:6, byrow = FALSE) # Matrices Ejemplo 1
m
dim(m)
attributes(m)
n <- 1:6     # Matrices Ejemplo 2
dim(n) <- c(2,3)
n
xx <-10:12   # Matrices Ejemplo 3
yy<-14:16
cbind(xx,yy) # Une vectores por Columnas
rbind(xx,yy) # Une vectores por Renglones

mi_matrix<-cbind(xx,yy) # este resultado lo puedo asignar a un objeto

# Funciones ----

sum (10,20,30)    # Funci�n suma
rep('R', times=3) # Repite la letra R el numero de veces que se indica
sqrt(9)           # Raiz cuadrada de 9



help(sum)         # Ayuda sobre función sum
example(sum)      # Ejemplo de función sum

# Ambiente ----
ls()
gc()           # Garbage collection, reporta memoria en uso


rm(list=ls())  # Borrar objetos actuales

# Directorio de trabajo ----
getwd()           # Directorio actual
setwd("C:/Users/anaes/Dropbox/2021/CursoR-posgrado")# Cambio de directorio

list.files()      # Lista de archivos en ese directorio

# Paquetes ----

#install.packages("foreign", dependencies = TRUE)
#install.packages("haven", dependencies = TRUE)



library(foreign)
library(haven)


ecovid0420<-read.dbf("./datos/ECOVID0420.DBF") #checa cómo nos vamos adentro de nuestro directorio

## pacman ----

if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse, readxl, writexl, haven, sjlabelled, foreign) #carga los paquetes necesarios para esta práctica


# M�s importaci�n ----
ICI_2018 <- readxl::read_excel("./datos/ICI_2018.xlsx", sheet = "para_importar")
#View(ICI_2018)


writexl::write_xlsx(ICI_2018, path = "Mi_Exportacion.xlsx")


lapop2019 <- haven::read_dta("./datos/lapop2019.dta")


haven::write_dta(lapop2019, "./datos/mi_exportacion.dta", version = 12)


encevi_hogar<- haven::read_sav("./datos/encevi_hogar.sav")

haven::write_sav(lapop2019 , "mi_exportacion.sav")

# Revisi�n ----
class(lapop2019) # tipo de objeto
names(lapop2019) # lista las variables
head(lapop2019) # muestra las primeras 6 líneas
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


# Importaci�n archivos separados por comas

mig_inter_quin_proyecciones <- read.csv("http://www.conapo.gob.mx/work/models/CONAPO/Datos_Abiertos/Proyecciones2018/mig_inter_quin_proyecciones.csv", encoding="latin1")
#View(mig_inter_quin_proyecciones)
names(mig_inter_quin_proyecciones)


# revisi�n con dplyr

dplyr::glimpse(lapop2019)
dplyr::glimpse(lapop2019[,1:10]) # en corchete del lado derecho podemos ojear columnas 


#print(get_labels(lapop2019)) #todas
print(get_labels(lapop2019[, 1:10])) #de las primeras 10 variables
print(get_labels(lapop2019$soct2)) #


#print(get_label(lapop2019)) #todas
print(get_label(lapop2019[, 1:10])) #de las primeras 10 variables
print(get_label(lapop2019$soct2)) #


x<-lapop2019$ed
x<-lapop2019[["ed"]]  # ¡Ojo con las comillas! 
x<-lapop2019[,126]
x<-lapop2019[,"ed"]


x<-lapop2019 %>% 
  select(ed)


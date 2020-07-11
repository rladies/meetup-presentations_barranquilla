--------------------------------------
#PRIMER ENCUENTRO R-LADIES BARRANQUILLA 
#SÁBADO, JULIO 11 2020
#HAGAMOS UN POCO DE ARTE EN RSTUDIO
--------------------------------------
#Instalamos los paquetes necesarios:
install.packages(c("dplyr","Hmisc","stringr","readxl","forcats","data.table"))
#Lectura de datos
#Cargamos el paquete que utilizaremos.
library(readxl)
#Cargamos la base de datos (Para asignar podemos utilizar "=")
CEP_excel = readxl::read_excel("CEP_sep-oct_2017.xlsx")
#Solo nos muestra la primera hoja y la que nos interesa es la segunda
#Le indicamos que necesitamos leer la hoja en la POSICIÓN 2
#(Para asignar podemos utilizar "<-")
CEP_excel  <- read_excel("CEP_sep-oct_2017.xlsx",sheet = 2 )
#Le indicamos que necesitamos leer la hoja que tiene NOMBRE "DATOS"
read_excel("CEP_sep-oct_2017.xlsx", sheet = "DATOS") -> CEP_excel1

###Otra forma es usar la función read.csv
CEP_csv  <- read.csv("CEP_sep-oct_2017 .csv")
#Se observa que solo tenemos una columna
#Para corregirlo podemos agregar la función "sep="
CEP_csv  <- read.csv("CEP_sep-oct_2017 .csv", sep = ";")
#Para visualizar los datos
View(CEP_csv)
#Con la función read.csv2 no es necesario usar sep=";"
CEP_csv2 <- read.csv2("CEP_sep-oct_2017 .csv")

#Importamos los datos desde excel en formato xlx en Import Dataset.
#Cargamos los paquetes
library(dplyr)
library(Hmisc)
library(stringr)
library(forcats)
library(data.table)
#Importamos la base de datos.
library(readxl)
violencia_intrafamiliar_2018 <- read_excel("C:/Users/usuario/Desktop/RLADIES/Capacitación 1/Base de datos/violencia-intrafamiliar-2018.xlsx", 
                                           skip = 8)
View(violencia_intrafamiliar_2018)

#Creamos un nuevo objeto llamado datos para facilitar el manejo de la base de datos.
datos <- violencia_intrafamiliar_2018
#Para conocer la dimensión de la base de datos
dim(datos)
# Para explorar las variables y conocer su estructura
dplyr::glimpse(datos)
#Otra forma de observar la estructura de los datos
str(datos)
#Debido a que la edad no es un caracter, se convierte en numérico
datos$Edad<- as.numeric(datos$Edad)
str(datos$Edad)
#Análisis de la edad
summary(datos$Edad)
# Para obtener las 6 primeras observaciones de la base de datos
head(datos)
# Para obtener las últimas 6 observaciones de la base de datos
tail(datos)
#colnames para observar los nombres de las columnas o variables
colnames(datos)
#Para colocar los nombres de las variables en mayuscula (opcional)
toupper(colnames(datos))
# Resumen o descripción de cada una de las variables
Hmisc::describe(datos)
#Lista de municipios de Colombia que aparecen en la base de datos
unique(datos$Municipio)
# Nombres de municipios  en mayuscula
datos$Municipio<- toupper(datos$Municipio)
#Fusionar niveles
datos$Municipio=forcats::fct_collapse(datos$Municipio,"BARRANQUILLA (CT)"=c("QUILLA (CT)", "KILLA (CT)","BARRANUQUILLA (CT)"))
datos$Municipio=forcats::fct_collapse(datos$Municipio,"MEDELLÍN (CT)"=("MEDELLÍN(CT)"))
#Conteo de los municipios
table(datos$Municipio)
#Lista de departamentos de Colombia que aparecen en la base de datos
unique(datos$Departamento)
#Eliminar la tildes
datos$Departamento <- chartr(old = "ÁÉÍÓÚ",new = "AEIOU",x = datos$Departamento)
#Primera letra en mayúscula y las otras en minuscula
stringr::str_sub(datos$Departamento,start = 1,end = 1) <- stringr::str_to_upper(str_sub(datos$Departamento,1,1))
stringr::str_sub(datos$Departamento, start = 2) <- stringr::str_to_lower(str_sub(datos$Departamento, 2))
# Conteo de los departamentos.
table(datos$Departamento)
#Reemplazamos los "-" de la base de datos por NA
datos[datos== "-" ] <- NA
#
is.na(datos)
#Eliminamos las filas que tengan NA
datos <- datos[stats::complete.cases(datos),]
# Ver datos de Barranquilla
# Seleccionar solo las filas donde ciudad es igual a Barranquilla
Barranquilla <- datos[datos$Municipio == "BARRANQUILLA (CT)", ]
#Número máximo de casos denunciados en un día
max(Barranquilla$Cantidad)
#Número mínimo de casos denunciados en un día
min(Barranquilla$Cantidad)
str(Barranquilla$Cantidad)
#Media de los datos
mean(Barranquilla$Cantidad)

#Para cambiar los nombres de las columnas
#FORMA 1
colnames(datos) <-  c("x1", "x2","x3","x4",
                      "x5","x6", "x7", "x8",
                      "x9", "x10", "x11",
                      "x12", "x13", "x14", "x15",
                      "x16", "x17", "x18")
#FORMA 2
colnames(datos)[colnames(datos)=="x1"] <- "Departamentos"

#FORMA 3
# Renombrar una columna en R
names(datos)[3]<-"Día"

#FORMA 4
data.table::setnames(datos, old=c("x4", "x5"), new=c("Barrio", "Zona"))


#Deja tu corazón en todo lo que haces, si lo haces con pasión...
                         #LO TIENES TODO.
#PRIMER ENCUENTRO R-LADIES BARRANQUILLA 

#                                Club de lectura
#                       Sesion 5 - Capitulo:6 Efficient data carpentry: 6.1-6.3
#            "Efficient R programming - Colin Gillespie & Robin Lovelace"
#Organiza: R-Ladies Medellín, R-Ladies Galápagos, R-Ladies Barranquilla.
#Conferencistas: Viviana Flórez Camacho y  Mary Jane Rivero Morales. (R-Ladies Barranquilla).

#----Instalación de paquetes ----
# install.packages("tibble")
# install.packages("tidyr")
# install.packages("stringr")
# install.packages("readr")
# install.packages("dplyr")
# install.packages("broom")
# install.packages("tidyverse") #en lugar de instalar: "tidyr","stringr","readr","dplyr"

#---- Cargando las librerias ----
library(tibble)
library(tidyverse)
library(broom)

#---- 6.2 Marcos de datos eficientes con tibble ----
#Creación de vectores con la función concatenar
#personaje: almacena los nombres de los personajes principales de FRIENDS.
Personaje <- c("Rachel Green","Ross Geller","Monica Geller","Joey Tribbiani",
               "Chandler Bing","Phoebe Buffay")
#profesion: almacena las ocupaciones de los personajes principales de FRIENDS.
Profesion <- c("Ejecutiva","Paleontologo","Chef","Actor","Analista de datos",
               "Masajista")
#nacimiento : almacena la fecha de nacimiento de los personajes principales de FRIENDS.
Nacimiento <- c("05-05-1971","18-10-1967","22-04-1969","25-05-1968",
                "08-04-1969","10-10-1968")
#sexo: almacena el sexo de cada uno de los personajes
Sexo <- c("F","M","F","M","M","F")

#Creando una tibble
my_tibble <- tibble(Personaje,Profesion,Nacimiento,Sexo)
my_tibble #Imprimimos el objeto para observarlo en la consola.

#Creando un dataframe para determinar las diferencias
#Usando la función data.frame de R base
df <- data.frame(Personaje,Profesion,Nacimiento,Sexo)
df #Imprimimos el objeto para observarlo en la consola.

#Principales diferencias:
#PRIMERA DIFERENCIA: ver la clase de cada variable
my_tibble #Con tibble
df        #Con dataframe

#SEGUNDA DIFERENCIA: método de print
print(my_tibble) #con tibble
#1. Muestra los datos relevantes.
#2. Los datos son una tibble.
#3. Podemos ver 10 observaciones por defecto.
#4. Podemos ver el número de columnas que el tamaño de la consola permite visualizar.

print(df) #con data.frame
#1. Muestra todos los datos.
#2. No dice de qué tipo es cada variable.

#TERCERA DIFERENCIA: subconjuntos
#Con tibble
my_tibble[,1] #Nos extrae lo que queremos ver la primera columna en forma de columna

#Para extraer una columna en forma de vector:
my_tibble[[1]]           #Usanado la posicion
my_tibble[["Personaje"]] #Usando el nombre de la columna
my_tibble$Personaje      #Con el signo peso

#Con dataframe
df[,1:2] #Obtenemos un marco de datos de dos columnas
df[,1]   #ahora tenemos un vector,con los valores de la primera columna
df$Personaje


#---- 6.3.1 De datos anchos a largos con pivot_longer() ----
# Pivotar más largo significa hacer que las tablas "anchas" sean "largas", convirtiendo
# los nombres de las columnas en una nueva variable. Esto se hace con la función pivot_longer()
# (cuya inversa es pivot_wider()).

#Usando la función: pivot_longer
# data: marco de datos en el que los nombres de las columnas se convertirán en valores de fila.
# names_to: nombre de la variable categórica en la que se convierten los nombres de las columnas de los conjuntos de datos originales.
# values_to: el nombre de las columnas de valor de celda.
#cols: nombre de las columnas que se convertiran en observaciones.

dim(my_tibble) #obteniendo las dimensiones de la tibble
data <-  pivot_longer(data = my_tibble, cols = c("Profesion","Nacimiento","Sexo"),names_to = "Variables",
                      values_to = "Valor")
data
dim(data) #obteniendo las dimensiones de la tibble posterior a la funcion

#De datos largos a anchos
#id_cols: conjunto de columnas que identifican de forma única cada observación.
data <- pivot_wider(data = data, id_cols = 'Personaje',
                    names_from = 'Variables',
                    values_from = 'Valor')
data

#---- 6.3.2 Dividir variables de unión con separate() ----
data <-  data %>% separate(Personaje, into=c("Nombre","Apellido"),sep=" ")
View(data)

#---- 6.3.3 Otras funciones de tidyr -----
glmfit <- glm(am ~ wt, mtcars, family = "binomial") #Creación de un modelo logístico
tidy(glmfit).    #construye un tibble que resume los hallazgos estadísticos del modelo.
augment(glmfit)  #agrega columnas a los datos originales que se modelaron. Esto incluye predicciones, residuales y asignaciones de conglomerados.
glance(glmfit)   #se pueden calcular varios estadísticos de resumen para toda la regresión, como R^2 y el estadístico F:

#---- 6.3.4 Expresiones regulares ----
#grepl y str_detect devuelven valores logicos.
#Usando grepl
grepl(pattern ="Monica" ,x =data$Nombre)

#Usando str_detect
str_detect(string = data$Nombre,pattern = "Monica")

#Filtrar los personajes que empiezan por R
#^ palabras comunes que empiezan por
minidatos <- data%>%
     filter(str_detect(data$Nombre,"^R"))
minidatos

#$ palabras comunes que terminan en r
apellidos <- data%>%
     filter(str_detect(data$Apellido,"r$"))
apellidos

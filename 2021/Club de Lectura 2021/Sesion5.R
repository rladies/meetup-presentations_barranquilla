#                                Club de lectura
#                       Sesion 5 - Capitulos 10 y 11
#            "R for Data Science" de Hadley Wickham y Garrett Grolemund
#Organiza: R-Ladies Galapagos, R-Ladies Barranquilla, R-Ladies Milagro, R-Ladies Guayaquil
#Expositoras: Mary Jane Rivero Morales y Danisse Maria Carrascal Polo (R-Ladies Barranquilla)

#---- Capitulo 10: Tibbles ----
#Instalar el paquete tidyverse o tibbles
#install.packages("tidyverse")
#install.packages("tibble")

#Cargar el paquete
library(tidyverse)

#Creando tibbles.
MyTibble <- tibble(x=1:4,
                y=c("Barranquilla","Galapagos","Milagro","Guayaquil"))
#Imprimimos el objeto para observarlo en la consola.
MyTibble

#Creando un data.frame
MyDF <- data.frame(x=1:4,
                   y=c("Barranquilla","Galapagos","Milagro","Guayaquil"))
#Imprimimos el objeto para observarlo en la consola.
MyDF 

#También podemos convertir data.frame en tibble.
DFTbl <- as_tibble(MyDF)
DFTbl 

#----Diferencia entre Tibbles y data.frames----
#PRIMERA DIFERENCIA: cadenas como factores

#Con dataframe:
#En este caso la varible y es character
class(MyDF$y)
#En caso de que el resultado dé factor
# MyDFALSE <- data.frame(x=1:4,
#                    y=c("Barranquilla","Galapagos","Milagro","Guayaquil"),
#                    stringsAsFactors = FALSE)
#class(MyDFALSE$y)

#Con tibbles:
class(MyTibble$y)
#Convirtiendo a factores
MyTibbleF <- tibble(x=1:4,
                   y=factor(c("Barranquilla","Galapagos","Milagro","Guayaquil")))
MyTibbleF

#SEGUNDA DIFERENCIA: metodo de print
# library(dplyr) o library(tidyverse)

#Impresión del tibble:
data("starwars")
starwars
#1. Muestra los datos relevantes.
#2. Los datos son una tibble.
#3. Podemos ver 10 observaciones por defecto.
#4. Poswmoa ver el número de columnas que el tamaño 
#de la consola permite visualizar.

#Convirtiendo la base de datos a data.frame para observar la diferencia
as.data.frame(starwars)
#1. Muestra todos los datos.
#2. No dice de qué tipo es cada variable.

#TERCERA DIFERENCIA: subconjuntos
#Con dataframe
MyDF[,1:2]
#Obtenemos un marco de datos de dos columnas
MyDF[,1]
#ahora tenemos un vector,con los valores de la primera columna

#Con tibble
MyTibble[,1]
#Nos extrae lo que queremos ver la primera columna en forma de columna

#Si queremos extraer una columna
#en forma de vector cuando es tibble usamos corchetes dobles
MyTibble[[1]]
#Con el nombre de la columna
MyTibble[["y"]]
#Con el signo peso
MyTibble$x

#Creacion de varibale con data.frame
df <- data.frame(narticulo= c(4,3,5),
                 precio=c(20000,45000,50000),
                 TotalPagar= narticulo*precio)
#No es capaz de crear variables en una secuencia. 

#Creación de variables con tibble:
tibbles <-tibble(Narticulo= c(4,3,5),
precio=c(20000,45000,50000),
TotalPagar= Narticulo*precio)

#Permite usar nombre de variables no sintaticos.
tb <- tibble(
  `:D` = "felicidad",
  ` ` = "espacio",
  `1998` = "número")
tb

#---- Capítulo 11: Importación de datos----
#install.packages("tidyverse")
library(tidyverse)
mercado<-read_csv("Groceries_dataset.csv")
com_vac<-read_csv("reddit_vm.csv")
violencia<-read_csv("violence_data.csv")

#---- Introduccion ----
#        readr

#read_csv()

#Da feedback de cada tipo de columna en el dataset, producen tibbles.
netflix<-read_csv("netflix_titles.csv")
netflix<-read_delim("netflix_titles.csv", delim = ",")

#Hacer un csv "inline". Toma la primera linea como encabezado.
k<-read_csv("a,b,c
d1,d2,e3
a4,s5,f6")

read_csv2("a;b\n1;3")

#Para que no la tome como encabezado: skip = n, comment = "#", col_names = FALSE o valor

#skip=n
read_csv("No quiero esta linea 
  Esta tampoco
  Y esta menos
  x,y,z
  1,2,3", skip = 3)

#podemos quitar los nombres de las columnas 
datos1 <- read_csv("netflix_titles.csv",col_names = FALSE)
#O podemos cambiarlos 
datos1 <- read_csv("netflix_titles.csv",skip=1, col_names = c("1","2","3","4","5","6",
                                                              "7","8","9","10","11","12"))

#comment = "#"
read_csv("# un cafesito por favor
  x,y,z
  # con dos de azucar
  1,2,3", comment = "#")

#col_names = FALSE
read_csv("1,2,3\n4,5,6", col_names = FALSE) 
#/n un atajo para agregar una nueva linea debajo

#col_names = c(...)
read_csv("1,2,3\n4,5,6", col_names = c("r", "l", "s"))

#tips
#  na
read_csv("c, o, l\n1,.,.\nx,2,x\nNA,NA,3", na = c(".","x", "NA"))

#  quote
read_csv("x,y\n1,'a,b'")
read_csv("x,y\n1,'a,b'", quote = "''") 
#otras formas de especificar este quote: '\'' o "\'"


#---- Segmentar un vector----
str(parse_logical(c("TRUE", "FALSE", "NA")))
str(parse_integer(c("1", "2", "3")))
str(parse_date(c("2010-01-01", "1979-10-14")))

#Analicemos un vector de enteros
parse_integer(c("1", "231", ".", "456"))
#Para arreglar el error le debemos aclarar cuales son 
parse_integer(c("1", "231", ".", "456"), na = ".")


x <- parse_integer(c("123", "345", "abc", "123.45"))
#problems() para una mejor visualización del error o errores
problems(x)

#Numeros
parse_double("1.23")

#para diferentes marcas decimales
parse_double("1,23", locale = locale(decimal_mark = ","))

#Para analizar números
z<-c("Hoy comi 15 hamburguesas", "2 gaseosas", "3 deditos", "Un Joey Special (2 pizzas)",
     "1 manzana", "3 melocotones", "1 banano", "1 y media de arroz", "2 vasos de jugo")
parse_number(z)


d<-parse_number(k$a)
d

#Cadenas de caracteres
charToRaw("Danisse") 
ASCII
UTF - 8

x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"
x1;x2
guess_encoding(charToRaw(x1));guess_encoding(charToRaw(x2))
parse_character(x1, locale = locale(encoding = "Latin1"))
parse_character(x2, locale = locale(encoding = "Shift-JIS"))

#Factores
fruta <- c("manzana", "banana")
parse_factor(c("manzana", "banana", "bananana"), levels = fruta)


parse_date("1 enero 2015", "%d %B %Y", locale = locale("es"))

d1 <- "Enero 1, 2010"
parse_date(d1,"%B %d %* %Y", locale = locale("es"))

d2 <- "2015-Ene-07"
parse_date(d2, "%Y %b %d", locale = locale("es"))


#---- Segmentación de archivos ----
guess_parser(c("10,572,600")) #deducción para el tipo de dato

str(parse_guess("2010-10-10")) #utiliza esa deducción para analizar el dato

#Leamos un ejemplo
desafio <- read_csv(readr_example("challenge.csv"))

#revisar problemas
problems(desafio)

#Estrategia pra solucionarlo
desafio2 <- read_csv(readr_example("challenge.csv"), guess_max = 1001)

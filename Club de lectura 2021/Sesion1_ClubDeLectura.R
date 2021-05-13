#                                Club de lectura
#                       Sesion 1 - Capitulos 14, 15 y 20
#            "R for Data Science" de Hadley Wickham y Garrett Grolemund
#Organiza: R-Ladies Galapagos, R-Ladies Barranquilla, R-Ladies Milagro, R-Ladies Guayaquil
#Expositoras: Mary Jane Rivero Morales y Danisse Maria Carrascal Polo (R-Ladies Barranquilla)

# install.packages("plotly")
# install.packages("tidyverse")
library(tidyverse)
#---- Vectores ----
#Crea un vector numerico de 5 elementos
elemento <- c(1,12,15,2,4)

#Crea un vector logico de 5 elementos
elemento2 <- c(T,F,T,T,F,F,T)

#Crea un vector de 4 cadenas de caracteres
elemento3 <- c("Barranquilla", "Galapagos","Guayaquil","Milagro")
elemento3[c(1,3)]

#Creando y concatenando 2 vectores
x<-c(11,13,15)
y<-c(17,19,21)
c(x,y)

#Extraccion de elementos de un vector
vector<-c("a","b","c","d","e","f","g")
vector[c(1,3,6)]

#Extraccion de elementos de un vector
vector2<-c(1,2,3,4)
vector2[c(1,3)]

#Un numero negativo precediendo al indice significa
#exclusion. 
vector2[-c(1,3)]

#Especificar una condicion logica. En el caso del vector x creado arriba
x <- c(12,5,13,14,15,3,2,1)
#Condicion
x<=5
#Almacena los valores menores e iguales a 5
x[x<=5]

# En el caso de un vector de variables, podemos utilizar los nombres de 
#las variables para extraer los elementos
M<-1
A<-2
R<-4
y<-c(M,A,R)
y

#Ordenes para crear vectores de forma automatica
1:5
#Genera numeros enteros entre 1 y 6.
seq(1,6)
#se va sumando al numero anterior 0.5 hasta que se llega a 6
seq(1,6,by=0.5)
#Genera una secuencia de 10 numeros entre 1 y 6.
seq(1,6,length=10)
#Repetir el 1, 5 veces
rep(1,5)
#se repite el patron (1,2) cinco veces.
rep(c(1,2),5)
#Se repite el patron 123, 2 veces
rep(1:4,2)
#Repetir el 1, una vez, el 2 cuatro veces y el 3 cinco veces
rep(1:3,c(1,4,5))
#valores faltantes
#El simbolo de valor faltante es NA (significa Not Available). 
#Cualquier operacion aritmetica que involucre a un NA da por resultado un NA. 
x<-c(1,2,3,NA,4,5)
x
#Observar los valores faltantes 
is.na(x)
#Valores mayores a 2
x[x>2]
#Multiplicamos por 2
x*2
#De esta manera eliminamos los valores faltantes del vector x.
x<-x[!is.na(x)]
x

#---- Listas ----
#Vamos a crear los elementos que incluiremos en nuestra lista
#Lectura de datos
datos <- read.csv("netflix_titles.csv")
#Filtrar las peliculas 
minidatos <- datos%>% 
  filter(str_detect(datos$country,"Mexico"))
# View(minidatos)
titulos <- datos$title
matriz <- matrix(c("x", "y", "z"),
                 nrow = 3,
                 ncol=3)
matriz
muestra<-as.data.frame(datos[1:3,1:5])

#Creemos nuestra lista
lista<-list(titulos, matriz, minidatos)
lista; View(lista)

#Cambiamos los nombres de nuestros elementos. De [[1]] a "nombre"
names(lista)<- c("titulos", "matriz", "datos mini")
View(lista)

#Veamos como seria si utilizamos un vector
ejem_vector<-c(titulos, matriz, minidatos)
ejem_vector; View(ejem_vector)
#Unifica todas las observaciones en un solo conjunto. 

#Seleccionar elemento de una lista
lista[["titulos"]]
lista$titulos
#Se puede hacer asi, pero puede presentar errores

#Para seleccionar algo mas especifico:
lista[["titulos"]][3]
lista[["matriz"]][3,2]
lista[["datos mini"]][5]
lista[["datos mini"]][5,3]

#Agreguemos otro item a la lista
lista[["dataframe"]]<-muestra
View(lista)

#Quitemos algo de la lista
lista[["datos mini"]]<-NULL
View(lista)

#---- Cadenas de caracteres ----
#Creamos un vector llamado frutas
frutas <- c("manzana","pera","fresa","banano")
#Uso de la funcion str_length(): # de caracteres de una cadena
str_length(frutas)
#Uso de la funcion str_to_upper()
str_to_upper(frutas)
#Uso de la funcion str_to_title()
str_to_title(frutas)
#Uso de la funcion str_to_lower()
str_to_lower(frutas)
#Uso de la funcion str_sub(): Para extraer partes de una cadena.
str_sub(frutas,1,3)

#Ejemplo base de datos
#Lectura de datos
datos <- read.csv("netflix_titles.csv")
#Separar la duracion de sus unidades
#*Extra 
datos <-  datos %>% separate(duration, into=c("Tiempo","unidades"),sep=" ")
#Numero de caracteres de la observacion director
datos$caracteres<- str_length(datos$director)
#Escribir en mayuscula
datos$type <- str_to_upper(datos$type)
#Escribir como titulo 
datos$type <- str_to_title(datos$type)
#Combinar las funciones str_sub y str_to_upper
datos$Inicial <- str_to_upper(str_sub(datos$type, 1, 1))
#ver la base de datos
View(datos)


#Uso de la funcion str_detect(): sirve Para determinar si un vector
#de caracteres coincide con un patron de busqueda
str_detect(frutas ,"a")
str_detect(frutas ,"r")

#Ejemplo base de datos
#Pipe ctrl+shif+m
#Filtrar las peliculas 
minidatos <- datos%>% 
                filter(str_detect(datos$country,"Mexico"))
#View(minidatos)

#Peliculas que se grabaron unicamente en Mexico
#^ palabras comunes que empiezan por m
#$ palabras comunes que terminan en o 
minidatos <- datos%>% 
  filter(str_detect(datos$country,"^Mexico$"))
View(minidatos)

#Filtrado mas amplio
minidatos_paises <- datos%>% 
  filter(str_detect(datos$country,c("^Mexico$","^Brazil$","^United States$",
                                    "^India$", "^Italy$", "^Germany$","^Russia$","^Japon$",
                                    "^Canada$","^Turkey$","^Egypt$", "^France$","^South Korea$",
                                    "^United Kingdom$", "^Spain$")) &  unidades == "min")

#Uso de la funcion str_c()
descripcion<-minidatos_paises$description
View(minidatos_paises)
intro<- "The movie is about"

hola<-str_c(
  intro, descripcion
)
hola

#---- Factores ----
#Observando la estructura de los datos
str(minidatos_paises)
#Convertir la variable de la base de datos a factor
minidatos_paises$country <- factor(minidatos_paises$country)
#Observar los niveles de la variable pais
levels(minidatos_paises$country)

#Contar observaciones por pais
minidatos_paises %>% count(country) 

#Graficando los datos
ggplot(minidatos_paises, aes(country,fill=country))+
  geom_bar()+
  labs(title="Peliculas por pais", x= "Paises",y="Numero de peliculas")+
  theme_minimal()

#Plotly  
library(plotly)
ggplotly(ggplot(minidatos_paises, aes(country,fill=country))+
           geom_bar()+
           labs(title="Peliculas por pais", x= "Paises",y="Numero de peliculas")+
           theme_minimal())


#Graficando los datos
str(minidatos_paises$Tiempo)
minidatos_paises$Tiempo <- as.integer(minidatos_paises$Tiempo)

#Calculo del tiempo promedio de duracion
resumen_pais <- minidatos_paises %>%
  group_by(country) %>%
  summarise(tiempoMin = mean(Tiempo),
    n = n())

#Graficando el Numero de peliculas Vs Tiempo en minutos
ggplot(resumen_pais, aes(tiempoMin, country, color=country, fill=country)) + 
  geom_point(size=4, shape=21)+
  labs(title="Duracion peliculas", x= "Tiempo en minutos",y="Numero de peliculas")+
  theme_minimal()

#Ordenando con ayuda de la funcion fct_reorder
ggplot(resumen_pais, aes(tiempoMin, fct_reorder(country,tiempoMin), color=country,fill=country))+  geom_point(size=4, shape=21)+
  labs(title="Duracion peliculas", x= "Tiempo en minutos",y="Numero de peliculas")+
  theme_minimal()

#Usando .desc
ggplot(resumen_pais, aes(tiempoMin, fct_reorder(country,tiempoMin, .desc=T), color=country,fill=country))+  geom_point(size=4, shape=21)+
  labs(title="Duracion peliculas", x= "Tiempo en minutos",y="Numero de peliculas")+
  theme_minimal()

#Para observar los niveles de la base de datos
levels(resumen_pais$country)

#Uso de fct_recode para renombrar los niveles

resumen_pais$country <- fct_recode(resumen_pais$country,
                          UK= "United Kingdom", SK ="South Korea")

levels(resumen_pais$country)


#---- Referencias ----
#"R for Data Science" - Hadley Wickham y Garrett Grolemund
#https://gonzalezgouveia.com/listas-en-r/
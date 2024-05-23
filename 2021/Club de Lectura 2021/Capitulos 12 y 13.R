####---- Paquetes ----####
library(tidyverse)
library(datos)

####---- 1: Pivot Longer ----####

##Para este ejemplo vamos a usar el dataset relig_income que muestra los ingresos en cada religion 
##el dataset tiene en cada columna los rangos de ingreso y esta no es la forma correcta
##La idea es que sea una variable (columna) que se llame ingreso y ahí colocar los ingresos

datos <- relig_income

#Para ello, vamos a usar la función pivot_longer que nos permite pivotar las columnas o
#convertirlas en variables para organizar el dataset. Los argumentos de esta funcioón son: 

#1: El conjunto de columnas cuyos nombres son valores y no variables. 
#2: El nombre de la variable cuyos valores forman los nombres de las columnas. 
#3: El nombre de la variable cuyos valores están repartidos actualmente por las celdas.

datos %>%
        pivot_longer(!religion, names_to = "ingreso", values_to = "Cantidad")

#Ejemplo 2: usamos el dataset billboard

datos2 <- billboard
#En este dataset tenemos los artistas, nombre de la canción y la fecha en la que fue ingresada 
#pero en semanas y cada semana es una columna, por lo que se necesita convertir esas columnas
#en observaciones.

#Este ejemplo es particular porque podemos usar la función starts_with porque todas las
#columnas inician con el mismo nombre de wk
#usamos también la funcion names_prefix para eliminar el texto coincidente del principio
#de cada nombre, esto es para dejar unicamente los números de la semana en la columna
datos2 <- datos2 %>%
        pivot_longer(cols = starts_with("wk"), names_to ="Week", values_to ="rank", names_prefix = "wk")


family <- tribble(
        ~family,  ~dob_child1,  ~dob_child2, ~gender_child1, ~gender_child2,
        1, "1998-11-26", "2000-01-29",             "F",             "M",
        2, "1996-06-22",           NA,             "F",             "F",
        3, "2002-07-11", "2004-04-05",             "M",             "M",
        4, "2004-10-10", "2009-08-27",             "F",             "M",
        5, "2000-12-05", "2005-02-28",             "M",             "F",
)


#En este ejemplo observamos que se usa el names_sep y la instrucción .value que le indica 
#a la función pivot_longer que esa parte del nombre de la columna especifica el "valor" 
#que se está midiendo (que se convertirá en una variable en la salida).

family <- family %>% 
        pivot_longer(
                !family, 
                names_to = c(".value", "child"), 
                names_sep = "_", 
                values_drop_na = TRUE
        )

####---- 2: Pivot wider ----####
#Esta función es lo opuesto a la anterior, en este caso la idea es convertir valores que 
#se encuentran como observaciones (en las filas) en variables (columnas)
#Para esta función solo se necesitan dos parametros: 
#1: La columna desde la que obtener los nombres de las variables. 
#2: La columna desde la que obtener los valores.
#Usamos el dataset creado twins que contiene los nombres de los gemelos y el orden de ellos
#según el momento en que nacieron

twins <- tribble(
        ~family,  ~name,  ~n, 
        "Lopez",  "Mark",      1,
        "Lopez",  "Scott",    2,
        "Quin",   "Tegan",     1,
        "Quin",   "Sara",      2,
)

#podemos usar la función pivot_wider para organizar y que muestre para cada familia 
#en una unica fila la información de sus gemelos y usamos la función names prefix para que 
#agregue el nombre twin_n
twins <- pivot_wider(twins,
                     names_from = "n", 
                     names_prefix = "twin_", 
                     values_from = "name"
)

#Para multiples valores: 
#Construimos un nuevo dataset que viene de unas encuestas aplicadas, en el que la 
#variable pop_renter indica la totalidad de quienes viven rentados, 

datos4 <- tribble(
        ~name,  ~variable,  ~measure, ~value,
        "Alabama", "pop_renter",  "estimate", 1434765,
        "Alabama", "pop_renter",  "error",      16736,
        "Alabama", "median_rent", "estimate",     747,
        "Alabama", "median_rent", "error",          3,
        "Georgia", "pop_renter",  "estimate", 3592422,
        "Georgia", "pop_renter",  "error",      33385,
)

##Para este ejemplo podríamos organizar como nueva variable a partir de las columnas
#"measure" que indica si el valor es el error o es el estimado y la columna "variable"
datos4 <- pivot_wider(datos4,
                      names_from = c(variable, measure), 
                      values_from = value
)

####---- 4: unir ----####
#Esta función es opuesta a la anterior, tal como su nombre lo indica nos permite unir 
#dos o mas columnas en una sola. Los argumentos que necesita son: 
#1: la base de datos donde están las columnas que se van a unir 
#2: el nombre de la nueva columna 
#3: el nombre de las columnas actuales 
#4: se puede usar también el argumento sep 

#Para este ejemplo podemos usar los datos 6 y 7 que fueron separados anteriormente: 
#Uniendo los datos 6: Si no le especificamos un separador, colocará un _ por defecto
#En este caso quiero un - 

datos6 <- unite(data = datos6, col = 'Stats', c('points','assists'),sep = '-')

#Lo mismo ocurre si unimos los datos 7, esta vez son tres columnas que se van a unir: 
datos7 <- unite(data = datos7, col = 'Stats', c('points','assists','passes'),sep = '-')

----#### 3: Separate ----####
#desarma una columna en varias columnas, 
#dividiendo de acuerdo a la posición de un carácter separador.
#Los argumentos son: 
#1: la base de datos que se va a separar 
#2: nombre de la columna que se va a separar 
#3: el nombre de la nueva columna 
#4: el elemento separador 

#ejemplo: vamos a crear un dataframe en el que una misma columna (stats) contenga los puntos 
#y las asistencias:

datos6 <-  data.frame(player=c('A', 'A', 'B', 'B', 'C', 'C'),
                      year=c(1, 2, 1, 2, 1, 2),
                      stats=c('22-2', '29-3', '18-6', '11-8', '12-5', '19-2'))

#ahora vamos a separar la columna stats en dos nuevas columnas que contengan
#los puntos y las asistencias 

datos6 <- separate(datos6, col=stats, into=c('points', 'assists'), sep='-')

#También se pueden separar en más de dos columnas: tomemos el siguiente ejemplo 
#En este se guarda en una misma columna llamada stats los puntos, asistencias y pases

datos7 <- data.frame(player=c('A', 'A', 'B', 'B', 'C', 'C'),
                     year=c(1, 2, 1, 2, 1, 2),
                     stats=c('22/2/3', '29/3/4', '18/6/7', '11/1/2', '12/1/1', '19/2/4'))

#Para separarlos: 
datos7 <- separate(datos7, col=stats, into=c('points', 'assists', 'passes'), sep='/')



####---- Capitulo 13: datos relacionales ----####
#Para abordar este capitulo usaremos las tablas vuelos, aviones, aeropuertos, aerolineas y clima
#que representan un dataset real de vuelos y cuya informacion se encuentra repartida entre esas
#5 tablas 

####---- Datos ----####
a <- vuelos
b <- aviones
c <- aeropuertos
d <- aerolineas
e <- clima

#Esta es una manera de verificar las llaves primarias, ya que al ser primarias deben ser unicas y 
#podemos evaluar eso al contar las que sean mayores a 1, para las pk no deben haber más de 1

####---- Primary keys ----####
aviones %>%
     count(codigo_cola) %>%
     filter(n > 1)

aeropuertos %>%
     count(codigo_aeropuerto) %>%
     filter(n > 1)

aerolineas %>%
     count(aerolinea) %>%
     filter(n > 1)

####---- Uniones ----####
#Si por ejemplo ahora queremos unir variables a partir de dos tablas, lo primero que 
#debemos hacer es buscar las coincidencias según sus claves. 


####---- Lleft_join ----####
datos8 <- vuelos %>%
     left_join(aerolineas, by = "aerolinea") %>%
     count(aerolinea) 

#La idea de hacer estas uniones es poder analizar información de interés. Por ejemplo, de la unión 
#anterior podemos obtener la cantidad de vuelos de cada aerolinea ahora integrando funciones de tidyverse
#count() para contar cuantos vuelos de cada aeroliea y arrange para ordenarlos
datos8 <- vuelos %>%
     left_join(aerolineas, by = "aerolinea") %>%
     count(nombre) %>%
     arrange(desc(n))

####---- inner_join ----####
#en una unión de igualdad interior, el output es un nuevo data frame que contiene la clave, 
#los valores de x y los valores de y.

#Por ejm creemos dos nuevos DF con algunas variables de vuelos y de aviones: 
df2 <- vuelos %>%
     select(dia, mes, anio, codigo_cola, origen,destino)

df3 <- aviones %>%
     select(codigo_cola, tipo, fabricante)

#Si ahora queremos ver cual fue el principal tipo de avión salió ese día para cada vuelo, necesitamos 
#unir con la tabla aviones 

datos9 <- df2 %>%
     inner_join(df3, by = "codigo_cola") %>%
     count(tipo) 

#Como podemos observar, se redujo el número de observaciones y esto es porque en una unión 
#interior, las filas no coincidentes, no se agregan al resultado, esto significa que 
#generalmente las uniones interiores no son apropiadas para su uso en el análisis 
#de datos dado que es muy fácil perder observaciones.

####---- rihgt_join ----####
#conectar vuelos con aeropuertos a traves de codigo aeropuerto en c y origen y destino en a. 
#Para ello, vamos a crear dos data frames que contengan lo que queremos unir: la idea es ver
#los nombres de los aeropuertos origen 

df5 <- vuelos %>%
     select(dia, mes, anio, codigo_cola, origen)

df6 <- aeropuertos %>%
     select(codigo_aeropuerto, nombre)

#Cambiamos el nombre de la ultima columna para que quede con el mismo nombre y unir
colnames(df5)[5] <- "codigo_aeropuerto"

datos10 <- right_join(df5, df6, by =  "codigo_aeropuerto")

#en este caso, se usa la funcion right_join que le da prioridad a las observaciones
#que se encuentran en y (df6)

###Una forma de hacerlo sin cambiar el nombre de la columna: 
df7 <- df5 %>%
     right_join(df6, c("origen" = "codigo_aeropuerto")) 
#Esto va a unir la variable origen en la tabla df5 con la variabla codigo_aeropuerto en la tabla df6.
#Las variables de df6 se usarán en el output.

#De aqui podemos obtener por ejemplo los nombres de los 3 principales aeropuertos de origen: 
origen_populares <- df7 %>%
     count(nombre, sort = TRUE) %>%
     head(3)
     

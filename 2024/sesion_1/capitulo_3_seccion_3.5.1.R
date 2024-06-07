############## CLUB DE LECTURA ##############
### FUNDAMENTOS DE CIENCIA DE DATOS CON R ###
############## CAPÍTULO 1,2,3 ###############


### 3.5.1 Estructuras y tipos de datos

## Tablas de datos

library(datasets)
head(CO2,3)



## Vectores 

#Largo. Es el número de elementos que contiene un vector. 
#El largo es la única dimensión que tiene esta estructura de datos.

plantas <- CO2$Plant

mi_vector <- c(1, 2, 3)

length(mi_vector)
which.max(mi_vector)


class(mi_vector)

nombres <- c("concentración","consumo")


#Como las cadenas de texto son el tipo de dato más flexible, 
#siempre que creamos un vector que incluye un dato de este tipo, 
#el resultado será un vector de texto.

mi_vector_mezcla <- c(FALSE, 2, "tercero", 4.00)



# Matrices

#Las matrices son, por lo tanto, una estructura con forma rectangular, con renglones y columnas.

# Tres renglones y cuatro columnas
matrix(1:12, nrow = 3, ncol = 4)



vector_1 <- 1:4
vector_2 <- 5:8
vector_3 <- 9:12
vector_4 <- 13:16
#Usamos rbind() para crear un matriz, en la que cada vector 
#será un renglón.

matriz <- rbind(vector_1, vector_2, vector_3, vector_4)

#Si utilizamos cbind(), entonces cada vector será una columna.

matriz <- cbind(vector_1, vector_2, vector_3, vector_4)


cantidades <- as.matrix(CO2[,4:5])

cantidades <- as.matrix(CO2[2,4:5])

cantidades <- as.matrix(CO2[1:5,4:5])

## Factor

levels(CO2$Type)
levels(CO2$Plant)

mi_factor <-factor(levels(CO2$Plant),levels = sort(levels(CO2$Plant)))
levels(mi_factor)

## Listas

#Las listas, al igual que los vectores, son estructuras de datos unidimensionales, 
#sólo tienen largo, pero a diferencia de los vectores cada uno de sus elementos puede 
#ser de diferente tipo o incluso de diferente clase, por lo que son estructuras heterogéneas.

nombres <- list(names(CO2))

nombres_2 <-list(names(CO2),paste0(names(CO2),"_1"))

nombres <- list(names_co2=names(CO2))

unlist(nombres_2[[2]])

nombres_2 <-list(names(CO2),paste0(names(CO2),"_1"),
                 elemento_3=c(1,2,3,4,5))

nombres_2 <-list(names(CO2),paste0(names(CO2),"_1"),
                 elemento_3=c(1,2,3,4,5))


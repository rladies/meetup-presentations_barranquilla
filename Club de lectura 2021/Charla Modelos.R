install.packages("modelr"); install.packages("tidyverse"); install.packages("carData");
install.packages("plotly")

library(modelr);library(tidyverse); library(carData);library(plotly)

#### ---- Cargamos los datos ---- ####
datos <- read_delim(file="ResistenciaPapel.csv", delim = ";", 
                    locale = locale(decimal_mark = "."))

datos2<-read.csv("ResistenciaPapel.csv", sep = ";")
#Ambas formas son validas.


#### ---- Hacemos un analisis exploratorio de los datos ---- ####

ggplot(datos, aes(Resistencia, ConcentrMadera)) +
  geom_point()


#En este grafico podemos observar que es posible identificar que a mayor 
#Concentracion de la madera, pareciera que la resistencia tambien es mayor. Sin 
#embargo, hasta este punto aun no podemos a afirmar nada, es necesario realizar 
#las validaciones estadisticas necesarias. 

#### ---- Modelo ---- #### 
#Para este caso vamos a usar un modelo de regresion lineal simple, ya que, 
#como vimos en el grafico anterior, es posible que una relacion lineal se ajuste
#a nuestros datos. De este modo, se podra probar si existe o no una relacion entre
#las variables de interes.
#Esta formula indica que la resistencia es la variable de respuesta y que la
#concentracion de la madera es la variable explicativa 
modelo <- lm(Resistencia~ConcentrMadera,data = datos)

#Visualizamos el modelo
ggplot(datos, aes(x=ConcentrMadera, y=Resistencia)) + 
  geom_point() +
  geom_smooth(method='lm', formula=y~x, se=FALSE, col='dodgerblue1') +
  theme_minimal()

#Visualizamos los coeficientes del modelo: 
coef(modelo)

#La formula seria: y = 15.64*x + 93.34
#Esto nos indica que cuando la concentracion de la madera es 0, la resistencia del
#papel es de 93.34. Asi mismo, observamos que para cada aumento unitario en la
#concentracion de la madera, la resistencia del papel aumenta 15.64 unidades

#Con la funcion summary podemos visualizar la informacion del modelo
summary(modelo) 
#Observamos que es un modelo significativo por lo valores P obtenidos, los cuales
#se interpretan de la siguiente forma: tenemos dos hipotesis: 
#Ho: los coeficientes del modelo son 0
#H1: al menos uno de los coeficientes es diferente de cero
#Observamos que para ambos casos el valor P es menor a 0.05 y se rechaza la 
#hipotesis nula. Por lo que podemos decir que los coeficientes son significativos y
#el modelo tambien es significativo.

#Ahora vamos a visualizar el modelo con los datos originales, las predicciones y
#la distancia (diferencia entre la prediccion y el dato real)
#Lo primero es incluir en la base de datos original, las predicciones
datos$predicciones <- predict(modelo)

#Usamos ggplot
gra_mod<- ggplot(datos, aes(x=ConcentrMadera, y=Resistencia)) +
  geom_smooth(method="lm", se = FALSE, color="lightgrey") +
  geom_segment(aes(xend=ConcentrMadera, yend=predicciones), col='red', lty='dashed') +
  geom_point() +
  geom_point(aes(y=predicciones), col='red') +
  theme_light()

#Para una mejor interaccion con el grafico, podemos usar plotly.
ggplotly(gra_mod)

#Tambien es util observar lo que el modelo no captura, los llamados residuos 
#que se obtienen restando las predicciones a los datos. 
datos <- datos %>% 
  add_residuals(modelo)
datos

ggplot(datos, aes(resid))+
  geom_freqpoly()

residuos <- rstandard(modelo)
#obtenemos las predicciones
valores.ajustados <- fitted(modelo)
#graficamos
plot(valores.ajustados, residuos)
#analizamos la normalidad
qqnorm(residuos)
qqline(residuos)
#Aqui observamos que los residuos tienen un comportamiento aleatorio, lo cual es 
#deseable para evidenciar que no existe dependencia entre ellos.

#---- Variables categoricas ----
#otro ejemplo
#Para este caso usaremos también la función lm()
#usaremos la base de datos sim2 del paquete modelr que contiene una variable dependiente (y)
#y una variable predictora de tipo categorica con 4 niveles
sim2

#EDA: vamos a hacer un grafico inicial para revisar el comportamiento de las variables
ggplot(sim2) +
  geom_point(aes(x, y))

#Modelo 2: ahora procedemos a realizar el modelo de regresion lineal con la función lm(),
#le especificamos la variable dependiente y la independiente, así como también la base de datos
mod2 <- lm(y ~ x, data = sim2)

#agregamos las predicciones con la función add_predictions 
grid <- sim2 %>%
  data_grid(x) %>%
  add_predictions(mod2)
grid

#Graficando las predicciones
ggplot(sim2, aes(x)) +
  geom_point(aes(y = y)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 4)

#en este ejemplo vamos a usar el dataset de iris 
#cargamos los datos en un objeto llamado flores, que contiene la longitud del sepalo de varias flores,
#la cual usaremos como variable dependiente, el ancho del sepalo, longitud y ancho del petalo
#y la especie de flor
flores <- iris
#Hacemos una grafica para analizar la correlación de las variables
pairs(flores)
#observamos que se muestra correlación entre la variable dependiente y las variables predictoras, lo
#que es deseable. Sin embargo, también se aprecia cierta correlacion entre algunas variables
#predictoras como el largo y ancho del petalo.

#Hacemos el modelo usando la función lm() y le indicamos que la variable dependiente es la longitud
#del sepalo y al colocar un . le indicamos que toma todas las demás variable como preditoras
#del dataser flores
modelo <- lm(Sepal.Length~.,data=flores)

#miramos el modelo obtenido 
summary(modelo)
#Observamos que todas son significativas, se rechaza la hipotesis nula. Ademas, tenemos un rcuadrado
#ajustado de 86%, lo que indica que el 86% de las variaciones son explicadas por el modelo y no por 
#los residuos, lo cual es bueno

#Agregamos las predicciones
grid <- flores %>%
  data_grid(Sepal.Width,Petal.Length,Petal.Width,Species) %>%
  add_predictions(modelo)
grid

#obtenemos los residuos del modelo con la función rstandard
residuos <- rstandard(modelo)
#obtenemos los valores ajustados del modelo
valores.ajustados <- fitted(modelo)
#Graficamos los residuos y los valores ajustados 
plot(valores.ajustados, residuos)
#observamos si los residuos se ajustan a una distribución normal
qqnorm(residuos)
qqline(residuos)

#---- Continuas ----#
####otro ejemplo 
#en este ejemplo los datos de 25 personas y queremos ver si la cantidad de grasa 
#en una persona depende del peso y la edad 
grasas <- read.table('http://verso.mat.uam.es/~joser.berrendero/datos/EdadPesoGrasas.txt', header = TRUE)
#hacemos un grafico para analizar las variables
pairs(grasas)
#observamos que hay datos atipicos, así como también hay una clara corelación entre
#las variables de edad y grasa, lo cual es deseable, pero no hay correlacion entre peso
#y grasas y tampoco entre las predictoras, lo que es deseable
#hacemos la matriz de correlacion 
cor(grasas)
#teniendo en cuenta la baja correlación entre la variable de respuesta y la variable
#predictora de peso, no se tendrá en cuenta en el modelo 

#hacemos el modelo
regresion <- lm(grasas ~ edad+peso, data = grasas)
summary(regresion)
#nuestra ecuación es y=102.575+5.321x1+0.417x2, lo que indica que por cada aumento unitario en años 
#de la persona, la grasa en su cuerpo aumenta 5.32 y por cada año adicional en la edad, la grasa
#aumenta 0.417. 

#observamos que Los correspondientes p-valores aparecen en la columna Pr(>|t|). 
#En este caso son muy pequeños por lo que se rechazan ambas hipótesis para los niveles 
#de significación habituales, por lo que decimos que la variable edad es significativa
#para determinar la grasa de una person. Sin embargo, la edad no es significativa, por lo que no debe ser tenida en cuenta
#en el modelo ni usarse para predecir.

#Tambien tenemos el coeficiente de rcuadrado ajustado, que nos indica que 
#el 68% de la variabilidad es explicada por el modelo y no por los errores, lo cual
#puede ser bueno 

#graficamos los datos y la recta del modelo
plot(grasas$edad, grasas$grasas, xlab='Edad', ylab='Grasas')
abline(regresion)

#predicciones
#Si ahora queremos predecir las grasas para individuos que se encuentran en el rango de
#edades de 30 hasta 50 años con el modelo
#creamos el data frame de las nuevas edades
nuevas.edades <- data.frame(edad = seq(30, 50))
#hacemos las predicciones usando el modelo creado
predict(regresion, nuevas.edades)
#de esta manera podemos saber, por ejemplo, que una persona de 30 años predecimos una 
#cantidad de grasa de 262,2 

#Diagnostico del modelo
#con fitted y residuals podemos obtener los errores del modelo y los valores de las predicciones
#algo que nos ayudará a determinar si es un buen modelo 
#obtenemos los residuos
residuos <- rstandard(regresion)
#obtenemos las predicciones
valores.ajustados <- fitted(regresion)
#graficamos
plot(valores.ajustados, residuos)
#en la grafica observamos que hay aleatoriedad, lo cual es deseable porque no queremos encontrar
#sesgos en los reiduos, tampoco tendencias ni varianza no constante. En caso de encontrar algo
#asi, quiere decir que el modelo no se ajusta a nuestros datos y es posible que necesitemos otro
#modelo mucho mas robusto 

#analizamos la normalidad
qqnorm(residuos)
qqline(residuos)
#observamos que los residuos se ajustan a una linea recta normal, lo cual es deseable, en caso
#de que no, es posible que nuestro modelo no se ajuste a los datos y no los logre explicar, por
#lo que deberiamos cambiar de modelo. 


#----Continuas y categoricas ----#
#En este ejemplo queremos descubrir si el salario de ciertos profesores
#depende de su sexo, rango, disciplina, años de servicio, años de que terminaron
#el phd
#Usamos la base de datos salaries del paquete cardata y la agregamos al objeto salarios
salarios <- Salaries
#Miramos la correlacion entre las variables 
pairs(salarios)
#puedo hacer el modelo de regresion para verificar si el salario depende
#del sexo y de los años de servicio 
#observamos que tenemos variables continuas y categoricas
modelo <- lm(salary~.,data=salarios)
summary(modelo)
#observamos que de todas las variables, el sexo no es significativo para predecir
#el salario de una persona. 
#Lo que hace R es tomar los niveles de las variables categoricas y transformarlos en 0 y 1
#para hacer el modelo y de esta manera hacer las predicciones. La ecuacion del modelo se sigue
#escribiendo igual y se tiene en cuenta los 0 y 1 al momento de hacer las predicciones

#Agregamos las predicciones
grid <- salarios %>%
  data_grid(rank,yrs.service, discipline, yrs.since.phd, sex) %>%
  add_predictions(modelo)
grid

#Graficamos las predicciones para cada variable independiente
ggplot(salarios, aes(sex)) +
  geom_point(aes(y = salary)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 4)

ggplot(salarios, aes(discipline)) +
  geom_point(aes(y = salary)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 4)

#obtenemos los residuos del modelo con la función rstandard
residuos <- rstandard(modelo)
#obtenemos los valores ajustados del modelo
valores.ajustados <- fitted(modelo)
#Graficamos los residuos y los valores ajustados 
plot(valores.ajustados, residuos)
#observamos si los residuos se ajustan a una distribución normal
qqnorm(residuos)
qqline(residuos)




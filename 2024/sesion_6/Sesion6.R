#Paquetes necesarios ----
install.packages("caret")
install.packages("rsample")
install.packages("ggstatsplot")
install.packages("tidyverse")
install.packages("CDR")

library(caret) 
library(rsample)
library(ggstatsplot)
library(tidyverse)
library(CDR)

#Capítulo 10 - Herramientas para el análisis en ciencia de datos----
#Leemos los datos ----
Madrid_Sale_num <- read.csv("Madrid_Sale_num.csv")
View(Madrid_Sale_num)


#Partición del conjunto de datos ----
#Muestreo aleatorio simple ----
set.seed(123) # para permitir reproducirlo
index <- createDataPartition(Madrid_Sale_num$PRICE, p = 0.7, list = FALSE) #p es el porcentaje de datos que va a training
train <- Madrid_Sale_num[index, ]
test <- Madrid_Sale_num[-index, ]
#Muestreo aleatorio estratificado ----
#Debemos dividir nuestra variable objetivo en secciones para poder comprobar la proporción
#de la distribución de nuestros datos. 

#Tomamos una muestra
set.seed(7)
Madrid_Sale_num_sample <- sample(1:nrow(Madrid_Sale_num), size = 5000, replace = FALSE)
Madrid_Sale_num_sample <- Madrid_Sale_num[Madrid_Sale_num_sample, ]
# Se realiza binning con cuatro bins
Madrid_Sale_num_sample_bin <- Madrid_Sale_num_sample |>
  mutate(price_bin = cut(PRICE, breaks = c(0, 250000, 500000, 750000, 10000000), labels = c("primerQ", "segundoQ", "tercerQ", "c"), include.lowest = TRUE)) |>
  select(price_bin, CONSTRUCTEDAREA, ROOMNUMBER, BATHNUMBER, HASTERRACE, HASLIFT)
# Se eliminan los registros con valores missing
Madrid_Sale_sample_na <- drop_na(Madrid_Sale_num_sample_bin)

#Se estratifica el cojunto de datos
set.seed(123) # para permitir reproducirlo
table(Madrid_Sale_num_sample_bin$price_bin) |> prop.table()
split_estrat <- initial_split(Madrid_Sale_num_sample_bin, prop = 0.7, strata = "price_bin")
train_estrat <- training(split_estrat)
test_estrat <- testing(split_estrat)

#Al comparar las proporciones de las muestras aleatorias estratificadas por la variable 
#objetivo, la distribución de estas en los subconjuntos de entrenamiento y de prueba es 
#aproximadamente igual:

table(train_estrat$price_bin) |> prop.table()
table(test_estrat$price_bin) |> prop.table()


#Tecnicas para manejar datos no equilibrados ----
# Se especifica que el modelo se entrene con downsampling
ctrl <- trainControl(
  method = "repeatedcv", repeats = 5,
  classProbs = TRUE,
  sampling = "down"
)
Madrid_Sale_num_sample_bin_downsample <- train(price_bin ~ .,
                                               data = Madrid_Sale_num_sample_bin,
                                               method = "gbm",
                                               preProcess = c("range"),
                                               verbose = FALSE,
                                               trControl = ctrl
)



#Enfoque de validacion----
#Validación cruzada VC
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)


Madrid_cv <- train(price_bin ~ .,
                       data = Madrid_Sale_num_sample_bin,
                       method = "gbm",
                       preProcess = c("range"),
                       verbose = FALSE,
                       trControl = control
)



#Ajuste de hiperparametros----
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
seed <- 7
metrica <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(Madrid_Sale_num_sample_bin))
tunegrid <- expand.grid(.mtry = mtry)

rf_default <- train(price_bin ~ ., data = Madrid_Sale_num_sample_bin, method = "rf", metric = metrica, tuneGrid = tunegrid, trControl = control)
print(rf_default)
  

#Capítulo 11 - Análisis exploratorio de datos----
#El cuarteto de Anscombe
anscombe<- as.data.frame(anscombe)
anscombe |> summarise(across(x1:y4, .fns = mean))
anscombe |> summarise(across(x1:y4, .fns = sd))

example("anscombe")

#Variables cualitativas----
accidentes2020_data |>
  count(tipo_accidente) |>
  mutate(porc = 100 * n / sum(n))

accidentes2020_data |>
  ggplot() +
  geom_bar(aes(y=tipo_accidente), fill = "darkmagenta")

#Tip: Más colores en R
#?colours o en https://r-charts.com/es/colores/

#El paquete ggstatsplot realiza gráficos que incluyen análisis estadísticos. 
#Por ejemplo, la función ggpiestats() proporciona un gráfico de sectores con algunos tests 
#estadísticos

ayuntam |>
  ggpiestats(x = serv)
#No sé si incluirlo porque no sé cómo interpretar la gráfica


#Otra opción para variables discretas es el graficos de waffles, paquete adicional
install.packages("waffle")
library(waffle)

datos<-data.frame(
  nombre = c("Maria", "Alejandra", "Marta", "Oscar", "Omar"),
  valor = c(20, 40, 15, 8, 30)
)

waffle(datos,
       rows = 5,
       colors = c("Yellow", "Blue", "Red", "deeppink", "Green"),
       title = "Reporte")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Variables cuantitativas----

View(renta_municipio_data)
summary(renta_municipio_data$`2019`)

#Histograma, representación gráfica tabla de frecuencias
#Calcular número de clases 
renta_municipio_data |>
  mutate(clases_sturges_renta = cut(`2019`,
                                    breaks = nclass.Sturges(`2019`)
  )) |>
  count(clases_sturges_renta)


p <- renta_municipio_data |>
  drop_na() |>
  ggplot(aes(`2019`))

h1 <- p + geom_histogram(color = "deeppink", fill = "pink")

h2 <- p + geom_histogram(
  color = "pink", fill = "deeppink",
  bins = nclass.Sturges(renta_municipio_data$`2019`)
)
h3 <- p + geom_histogram(color = "steelblue", fill = "pink", bins = 20)

library(patchwork)
h1 + h2 + h3

#Histograma + línea de densidad
p2 <- renta_municipio_data |>
  drop_na() |>
  ggplot(aes(`2019`))

p + geom_histogram(aes(y = after_stat(density)), 
                   position = "identity", 
                   color = "deeppink", fill = "pink") +
  geom_density(lwd = 1, colour = 4)

p <- renta_municipio_data |>
  drop_na() |>
  ggplot(aes(x=0, y= `2019`)) 
boxplot <- p + geom_boxplot(color = "yellow", fill = "pink")
violin <- p + geom_violin(aes(), color = "yellow", fill = "pink")
boxplot + violin

#Análisis exploratorio de varias variables----
table(ayuntam$signo_gob , ayuntam$serv)
p <- ayuntam |>
  ggplot(aes(signo_gob, fill = serv))

frecuencias <- p + geom_bar() 
proporciones <- p + geom_bar(position = position_fill())

frecuencias + proporciones

#Variables cualitativas y cuantitativas

contam_mad |>  
  na.omit() |>  
  filter(nom_abv == "PM10") |> 
  filter(between(fecha, left = as.Date("2022-03-10"), right = as.Date("2022-03-20"))) |>
  ggplot(aes(zona, daily_mean)) +
  geom_violin() +
  geom_jitter(height = 0, width = 0.01) +
  aes(x = zona, y = daily_mean, fill =zona)


pm10_nox_mad <- contam_mad |>
  na.omit() |>
  filter(nom_abv %in% c("PM10", "NOx")) |>
  # período del estado de alarma
  filter(between(fecha,left=as.Date("2020-03-14"), right=as.Date("2020-06-30"))) |> 
  select(estaciones, zona, tipo, nom_abv, daily_mean, fecha) |>
  pivot_wider(names_from = "nom_abv", values_from = "daily_mean", values_fn = mean)

pm10_nox_mad |>
  ggplot(
    aes(x = PM10, y = NOx, colour = tipo, size = zona )) +
  geom_point()

pm10_nox_mad |>
  drop_na() |>
  ggplot(aes(y=NOx, x= PM10, colour = tipo, shape = zona)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(vars(estaciones)) 

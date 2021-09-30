#Visualizacion de datos en RStudio
#Evento: R Weekend
#Ponentes: Mary Jane Rivero e Isabel Vasquez
#Fecha: Sabado, 02 de octubre de 2021

#---- Instalar y cargar paquetes ----
# install.packages(c("plotly","tidyverse","png","ggpubr","ggpattern"))
library(tidyverse)
library(ggplot2)
library(plotly)
library(png)#contiene la funcion readPNG()
library(ggpubr)# contiene la funcion background_image()
library(plyr)
library(quantmod)
#library(ggpattern) #patrones en ggplot
#install.packages("remotes")
#remotes::install_github("coolbutuseless/ggpattern")# otra alternativa


#---- Ejemplo 1: Diagrama de cajas ----
#ToothGrowth: base de datos
#dose: (dosis) variable categorica
#len: (longitud) Variable numerica
#data: conjunto de datos o data.frame.
#aes() es la configuracion de los ejes.

#Visualizando la base de datos
View(ToothGrowth)
#Observando la estructura de los datos
str(ToothGrowth)
#Convirtiendo la variable dose a factor
ToothGrowth$dose <- factor(ToothGrowth$dose)
#Graficando
a <- ggplot(ToothGrowth, aes(x=dose, y=len,
                        fill=dose)) + 
  geom_boxplot() + #genera diagrama de caja
  labs(title="Efecto de la vitamina C", 
       subtitle="en el crecimiento de los dientes de los cerdos de Guinea", 
       caption="fuente: C. I. Bliss (1952) The Statistics of Bioassay. Academic Press.", 
       y="Longitud %", 
       x="Dosis") +  # Titulo, subtitulo y leyenda
  theme(legend.position = "none",plot.caption = element_text(hjust=0.5))#posicion de la leyenda
a

#Ejemplo 2: otro estilo de diagrama de caja
#mpg: base de datos
#class: variable categorica
#hwy: variable numerica 
ggplot(mpg, aes(x=class, y=hwy)) + 
  geom_boxplot(
    # Cajas personalizadas
    color="blue",# borde del diagrama de caja
    fill="blue", #relleno del diagrama de caja
    alpha=0.2, #transparencia de la caja
    # Notch
    notch=T, #permite evaluar si las medianas son diferentes.
    notchwidth = 0.8,
    #Datos atipicos personalizados
    outlier.colour="red", #cambio de color
    outlier.size=3 #tamano del circulo
  ) +
  labs(title="Fuel economy data", 
       subtitle="Distribucion de las millas por galon segun la clase", 
       caption="fuente: http://fueleconomy.gov", 
       y="Millas por galon (autopista)", 
       x="Clase") +  # Titulo, subtitulo y leyenda
  theme(plot.title = element_text(hjust = 0.5), #posicion del titulo
        plot.subtitle = element_text(hjust = 0.5))#posicion del subtitulo

# ---- Ejemplo 3: Grafico de burbujas ---- 
#diamonds: base de datos
datos = diamonds %>% sample_n(100) #extrayendo una submuestra
# View(datos)
c <- ggplot(datos, aes(x=carat, y=price, size=depth, color=carat)) + 
  geom_point(alpha=0.4) +
  #escala para el area o radio
  scale_size_continuous( trans="exp",#tipo de transformacion
                         range =  c(1, 25)) + #indica el valor minimo y max para el tamaÃ±o del radio
  labs(title="Relacion precio / quilates", 
       subtitle="Tamano de las burbujas por profundidad", 
       caption="Fuente: pmoracho.github.io", 
       y="precio", 
       x="Quilates") # Titulo, subtitulo y leyenda
c


#----Ejemplo 4: Grafico de lineas y puntos -----
#Lectura de imagen en png
img=readPNG("Homer.png")
#Lectura de datos
data <- read.csv("TheSimpsons.csv", sep=";")
colnames(data)[1] <- "Temporada"
#data: conjunto de datos o data.frame.
#aes() es la configuracion de los ejes.
#geom_line: genera un grafico de linea.
#geom_point: genera un grafico de dispersion 

ggplot(data=data, aes(x=Temporada, y=Audiencia )) + 
  background_image(img)+#anade la imagen de fondo
  geom_line(colour="red", size= 1)  + 
  #size: grosor de la linea o tamano del punto.
  #colour: color del contorno.
  geom_point(size=2, shape=21, fill="red", colour="red") + 
  #colour: color del contorno.
  #fill: relleno de la forma.
  #shape: forma.
  labs(title = "Audiencia por temporadas",#titulo
       subtitle = "Serie: Los Simpsons",#Subtitulo
       caption = "Fuente: La huella digital")+#leyenda
  theme_minimal() #tema

#---- #Ejemplo 5 -----
df <- data.frame(perros = c("Beagle", "Cocker", "Dalmata", "Beagle", "Cocker", "Dalmata", "Beagle"),             
                 cantidad = c(1, 2, 7, 4, 5, 3, 1),
                 grupo = c(1, 1, 1, 1, 2, 2, 2))

ggplot (df, aes (perros,cantidad, fill = perros) )  +  
  # Patron diferente para cada grupo 
  geom_bar_pattern ( stat =  "identity" ,#diagrama de barras con un patron
                     pattern_color =  "white" , #borde del patron
                     pattern_fill =  "white" , #relleno del patron
                     alpha= 0.5,#transparencia
                     aes (pattern= perros))+ #patron diferente a cada grupo
  facet_wrap(~grupo,nrow = 1)+ #fasea las variables una al lado de otra
  labs(y="Cantidad de perros",x="Perros")+ #cambiar nombre a los ejes
  coord_fixed(ratio = 1/2) #ajusta el grafico



## GGplotly

ggplotly(a)
ggplotly(c)

#---- Ejemplos de plotly----
# Facet grid pero en plotly

df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/1962_2006_walmart_store_openings.csv")
total <- plyr::count(df$YEAR)

fig2 <- plot_ly(data = total, x = ~x, y = ~freq, type = "bar", showlegend=FALSE,
                marker=list(color=~x, showscale=FALSE)) %>% 
  layout(showlegend=FALSE, xaxis = list(side="right", showgrid=FALSE),
         yaxis=list(showgrid=FALSE))
fig2

# second plot - scattergeo map

fig3 <- plot_geo(df, lat = ~LAT, lon = ~LON) %>% 
  add_markers(
    text = ~OPENDATE, showlegend=FALSE,
    marker=list(color = ~YEAR, showscale=FALSE),
    hoverinfo = "text")%>% layout(geo = list(scope = 'usa'),showlegend=FALSE)
fig3

fig <- subplot(fig2, fig3, nrows = 2)%>% 
  layout(title = "Tiendas de Walmart abiertas por año")

fig



# controlador del rango de visualización
getSymbols(Symbols = c("AAPL", "MSFT"), from = '2018-01-01', to = '2019-01-01')
ds <- data.frame(Date = index(AAPL), AAPL[,6], MSFT[,6])
fig <- plot_ly(ds, x = ~Date) %>% 
  add_lines(y = ~AAPL.Adjusted, name = "Apple")%>% 
  add_lines(y = ~MSFT.Adjusted, name = "Microsoft")%>%
  layout(title = "Stock Prices", 
         xaxis = list(
           rangeselector = list(
             buttons = list(
               list(
                 count = 3,
                 label = "3 mo",
                 step = "month",
                 stepmode = "backward"),
               list(
                 count = 1,
                 label = "YTD",
                 step = "year",
                 stepmode = "todate"),
               list(step = "all")))
           ,rangeslider = list(type = "date")),
         yaxis = list(title = "Price"))
fig


# Grafica dentro de una grafica
fig <- plotly::plot_ly()
fig <- plotly::add_trace(fig, x = c(1, 2, 3), y = c(4, 3, 2), mode='lines')
fig <- plotly::add_trace(fig, x = c(20, 30, 40), y = c(30, 40, 50), xaxis='x2', yaxis='y2', mode='lines')
fig <- plotly::layout(fig, xaxis2 = list(domain = c(0.6, 0.95), anchor='y2'),
                      yaxis2 = list(domain = c(0.6, 0.95), anchor='x2'))

fig









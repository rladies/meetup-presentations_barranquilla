###########################################################
#Club de lectura: Fundamentos de ciencia de datos con R
#Sabado, 29 de junio
#R-Ladies Barranquilla
###########################################################

#------------- Librerias ---------------
library(tidyverse)
library(readxl)
library(janitor)
library(friends)
library(openxlsx)

#-------- Importacion de datos ---------
#########download.file#########
#permite descargar cualquier archivo disponible en una url
download.file(url = "http://emilio.lcano.com/b/adr/p/datos/RRHH.xlsx",
              destfile = "data/RRHH.xlsx",
              mode = "wb")

#########read_excel#########
#permite la lectura de datos con formato xlsx
data_rrhh <- readxl::read_excel("data/RRHH.xlsx")

#recomendacion usar el paquete janitor::clean_names() para realizar la
#limpieza de los nombres de las columnas.
data_rrhh <- readxl::read_excel("data/RRHH_v2.xlsx") |> clean_names()

#########read.csv2#########
#csv (comma separated values) desde la web
download.file(url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv",
              destfile = "data/datos")

datos_csv <- read.csv2("data/datos",sep = ",")

datos2_csv <-read.csv2("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv", 
                   sep = ",")

#-------- Exportacion de datos ---------
#########write.xlsx#########
write.xlsx(x = datos2_csv, file = "data/mundiales.xlsx")

#########write.csv#########
write.csv(x = datos2_csv, file = "data/mundiales.csv")

#########save#########
#Guardar en formato .RData () almacenan un espacio de trabajo entero
save(datos2_csv,file = "data/mundiales30-18.RData")
rm(datos2_csv) #elimina el objeto del enviroment
load("data/mundiales30-18.RData") #carga de nuevo el objeto

#########saveRDS#########
#Guardar en formato .rds almacenan un único objeto en un archivo
saveRDS(object = datos2_csv, 
        file = "data/mundo.rds")

nuevo_objeto <- readRDS(file = "data/mundo.rds")

# El paquete foreign de R base y otros paquetes especializados pueden exportar 
# datos a otros formatos de archivo, que no se tratan en detalle en este capítulo. 
#(SPS, ARFF, SAS XPORT, etc.)

#-------- Organización de datos con el tidyverse ---------
#########pipe#########
length(colnames(friends_emotions))

friends_emotions |> colnames() |> length()

# -------- Transformación de datos con dplyr -----
#########filter#########
friends |> 
  filter(speaker == "Monica Geller", #se filtra por speaker (chr)
         season == 1,  #se filtra por season  (int)
         episode == 1)  #se filtra por episode (int)

friends |> 
  filter(str_detect(tolower(text),"carol"), #se filtra por Monica (chr)
         season != 10,  #se filtra por season  (int)
         episode == 1) 

#########arrange#########
friends_info |> 
  arrange(air_date)

friends_info |> 
  arrange(desc(air_date)) 

#########slice#########
friends |> 
  slice(10:15) # extrae filas desde la 10 a la 15

#########slice_tail#########
friends |> slice_tail(n = 3) # extrae las tres últimas filas

#########slice_max#########
friends_info |> 
  slice_max(order_by = imdb_rating ) # episodios con mayor rating en imdb

#########slice_sample#########
set.seed(1) # Para que la muestra aleatoria sea reproducible
friends_info |> slice_sample(n = 4) # muestra 4 registros


#------------- Operaciones con columnas----------------
#########select#########
friends |> 
  select(speaker, season, episode)

friends |> 
  select(where(is.numeric))

friends |> 
  select(-c(text:season))

#########rename#########
friends_emotions |> 
  rename(temporada = season,
         episodio  = episode,
         escena    = scene)

#########relocate#########
friends_info |> 
  relocate(imdb_rating, .before = title)

friends_info |> 
  relocate(imdb_rating, .after = title)

#########mutate#########
friends_info |> 
  select(season, episode, title, imdb_rating) |> 
  mutate(status = case_when(
    imdb_rating>=9 ~ "Excellent",
    between(imdb_rating, 6,9)~ "Good",
    between(imdb_rating,3,6)~ "Regular",
    between(imdb_rating,1,3)~ "Bad"))

#########transmute#########
friends_info |> 
  transmute(temporada = season,
            episodio = episode,
            puntuacion = imdb_rating,
            status = case_when(
              imdb_rating>=9 ~ "Excellent",
              between(imdb_rating, 6,9)~ "Good",
              between(imdb_rating,3,6)~ "Regular",
              between(imdb_rating,1,3)~ "Bad"))

# -----------Operaciones de resumen y agrupación---------
#########tally#########
friends_info |> tally()

#########count#########
friends_info |> count()

friends_info |> count(season)

friends_info |> count(season,sort = T)

#########summarize#########
data_friends <- friends |>  
  filter(season  ==5,
         str_detect(tolower(speaker),"monica|chandler")) |> # se filtra por N0x
  group_by(speaker,text) |>
  summarize(
    primer_episodio = min(episode, na.rm = TRUE),
    ultimo_episodio = max(episode, na.rm = TRUE),
    cantidad = n()
  ) |> 
  ungroup() |>  #parte 2.
  group_by(speaker) |> 
  filter(cantidad == max(cantidad,na.rm = T)) |> 
  ungroup() |> 
  mutate(recaudo = 1233980287403)


#######Exportando data en xlsx ##############
#Crea un nuevo libro de trabajo (workbook) en R utilizando el paquete openxlsx
wb <- createWorkbook()

#Añade una nueva hoja de trabajo llamada "Friends" al libro de trabajo wb, 
#deshabilitando las líneas de cuadrícula (gridLines).

addWorksheet(wb, sheetName = "Friends", gridLines = FALSE)

#Escribe los datos del objeto friends (convertido en un dataframe) en la hoja de trabajo 
#"Friends" del libro de trabajo wb, incluyendo los nombres de las columnas.

writeDataTable(wb, sheet = "Friends",
               x = friends |> as.data.frame(),
               colNames = TRUE)

#Añade una nueva hoja de trabajo llamada "data_friends" al libro de 
# trabajo wb, deshabilitando las líneas de cuadrícula.
addWorksheet(wb, sheetName = "data_friends", gridLines = FALSE)

#Numero de filas de dataframe + 1
filas_hoja0 <- data_friends |> nrow()+1

# Crea un estilo de celda con el formato de número "currency" (moneda) y lo asigna a s_hoja0.
s_hoja0     <- createStyle(numFmt = "currency")

#Aplica el estilo s_hoja0 a las celdas en las filas de 2 a filas_hoja0 y en la
# columna 7 de la segunda hoja de trabajo en el libro de trabajo wb.
addStyle(wb, sheet = 2, style = s_hoja0,
         rows = 2:filas_hoja0, 
         cols = 7)

# Escribe los datos del objeto data_friends (convertido en un dataframe) en la hoja 
#de trabajo "data_friends" del libro de trabajo wb, incluyendo los nombres de las columnas.
writeDataTable(wb, sheet = "data_friends",
               x = data_friends |> as.data.frame(),
               colNames = TRUE)

#Guarda el libro de trabajo wb en un archivo llamado "data_final_friends.xlsx", 
# sobrescribiendo cualquier archivo existente con el mismo nombre.
openxlsx::saveWorkbook(wb,"data/data_final_friends.xlsx" , overwrite = TRUE)


######### R-Ladies Barranquilla ########
### Introducción Tablas con Rstudio ####
############## 13/12/2022 ##############


#---- Librerias ----
# install.packages("tidyverse")
# install.packages("reactable")
# install.packages("gt")
# install.packages("DT")

library(tidyverse)
library(reactable)
library(gt)
library(DT)

#---- Datos ----

worldcups <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv')


#---- Tablas Basicas----

reactable(data=worldcups)

DT::datatable(data=worldcups)

gt(data=worldcups)

attach(worldcups)   ## acceder rapidamente al nombre de variables

#---- Ejercicio 1----
# Renombrar los titulos de columnas

worldcups %>%
  head() %>% 
  select(year,host,attendance)


#---- Ejercicio 2----
# Alineación de columnas

worldcups %>%
  head() %>% 
  select(year,host,attendance)


#---- Ejercicio 3----
# Eliinar duplicados en filas


worldcups %>% filter(duplicated(winner)) %>% 
  group_by(winner) %>% 
  arrange(.by_group = TRUE) %>%
  select(winner,year,host)


worldcups %>% 
  filter(winner  %in%  c("Uruguay","Italy","France","West Germany","Brazil","Argentina")) %>% 
  group_by(winner) %>% 
  arrange(.by_group = TRUE)


#---- Ejercicio 4----
# Agrupar columnas


worldcups %>%
  head() %>% 
  select(year,host,winner,second,third,fourth)


#---- Ejercicio 5----
# Estilo de celdas

worldcups %>%
  head() %>% 
  select(year,host,goals_scored,attendance)


#---- Ejercicio 6----
# Dt especial 

## Botones

worldcups %>% 
  datatable(extensions = 'Buttons', options = list(
    dom='Bt',
    buttons=c('copy', 'csv', 'excel', 'pdf', 'print')
  ))

## Editable 

worldcups %>% 
  mutate(Goleador= as.character(NA)) %>% 
  datatable(rownames = FALSE,editable = list(target = 'cell', disable = list(columns = c(0)))
  )


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
  select(year,host,attendance) %>% 
  gt() %>% 
  cols_label(
    year="Año",
    host="Organizador",
    attendance="Asistentes"
  )


worldcups %>%
  head() %>% 
  select(year,host,attendance) %>% 
  reactable(columns = list(
    year= colDef(name = "Año"),
    host=colDef(name="Organizador"),
    attendance=colDef(name = "Asistentes")
  ))
  

worldcups %>%
  head() %>% 
  select(year,host,attendance) %>% 
  datatable(colnames = c("Año","Organizador","Asistentes"))


#---- Ejercicio 2----
# Alineación de columnas

worldcups %>%
  head() %>% 
  select(year,host,attendance) %>% 
  gt() %>% 
  cols_align(align = "center",columns = "host") %>% 
  cols_align(align = "left",columns = c(1,3))
  

worldcups %>%
  head() %>% 
  select(year,host,attendance) %>%
  reactable(columns = list(
    year= colDef(name = "Año",align = "center"),
    host=colDef(name="Organizador",align = "center"),
    attendance=colDef(name = "Asistentes",align = "left")
  ))

worldcups %>%
  head() %>% 
  select(year,host,attendance) %>%
  reactable(defaultColDef = colDef(align = "center"),
    columns = list(
    year= colDef(name = "Año"),
    host=colDef(name="Organizador"),
    attendance=colDef(name = "Asistentes")
  ))




#---- Ejercicio 3----
# Eliminar duplicados en filas


worldcups %>% filter(duplicated(winner)) %>% 
  group_by(winner) %>% 
  arrange(.by_group = TRUE) %>%
  select(winner,year,host)


worldcups %>% 
  filter(winner  %in% c("Uruguay","Italy","France","West Germany","Brazil","Argentina")) %>% 
  group_by(winner) %>% 
  arrange(.by_group = TRUE) %>% 
  select(winner,year,goals_scored) %>% 
  reactable(groupBy = "winner")


worldcups %>% 
  filter(winner  %in% c("Uruguay","Italy","France","West Germany","Brazil","Argentina")) %>% 
  group_by(winner) %>% 
  arrange(.by_group = TRUE) %>% 
  select(winner,year,goals_scored) %>% 
  gt(groupname_col = "winner")


#---- Ejercicio 4----
# Agrupar columnas


worldcups %>%
  head() %>% 
  select(year,host,winner,second,third,fourth) %>% 
  reactable(columnGroups = list(
    colGroup(name = "Finalistas",columns = c("winner","second"),
             align = "left")
  ))


worldcups %>%
  head() %>% 
  select(year,host,winner,second,third,fourth) %>% 
  gt() %>% 
  tab_spanner(label = "Finalistas",columns = c(3:4))


#---- Ejercicio 5----
# Estilo de celdas

worldcups %>%
  head() %>% 
  select(year,host,goals_scored,attendance) %>% 
  gt() %>% 
  tab_style(style=list(cell_text(color = "red")),
            locations = cells_body(columns = "goals_scored",
                                   row= goals_scored <= 100)
            ) %>% 
  tab_style(style = list(cell_fill(color = "red"),cell_text(color = "white")),
            locations = cells_body(columns = "attendance",
                                   rows = attendance<=1000000))


worldcups %>%
  head() %>% 
  select(year,host,goals_scored,attendance) %>% 
  reactable(columns = list(
    goals_scored= colDef(style = function(x){
      if(x<=100){
        color<- "red"
      } else{
        color<- "black"
      }
      list(color=color,fontWeight="bold")
    }),
    attendance= colDef(style = function(x){
      if(x<=1000000){
        color<- "red"
        color_text= "white"
      } else{
        color<- "white"
        color_text= "black"
      }
      list(background=color,color=color_text)
    })
  ))



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


# Fijar titulos de columnas 

gt(data=worldcups, id="name_css") %>% 
  opt_css(css="
        #name_css th{position: sticky; top: 0; z-index: 1;}
        #name_css thead {position: sticky; top:0; z-index:1;}
        #name_css {max-height:500px;overflow:auto;}")



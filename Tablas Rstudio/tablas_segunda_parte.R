

## Datos

plataformas<- tibble(
         plataforma= c("netflix","hbo max","disney +",
                        "peacock", "paramount +","hulu"),
         enero=c(221844000,73800000,129800000,
                    9000000,32800000,45200000),
         marzo=c(221641000,76800000,137700000,
                    13000000,39600000,45600000),
         junio=c(220672000,77100000,152500000,
                    13000000,43300000,46200000),
         septiembre=c(223085000,77175000,164200000,
                     15000000,46000000,47200000),
         diciembre=c(230930000,81000000,161800000,
                    20000000,55900000,48000000)
         )


plataformas<-plataformas |>
  rowwise() %>% 
  mutate(img = paste0(plataforma, ".png"),
         promedio=mean(enero:diciembre),
         plataforma=str_to_title(plataforma)) |> 
  arrange(desc(promedio)) |> 
  select(img,plataforma,everything())


plataformas%>% 
  gt() %>% 
  text_transform(
    locations = cells_body(c(img)),
    fn = function(x){
      local_image(x,height = 40)
    }) |> 
  opt_table_font(font = google_font(name = 'Open Sans')) |> 
  cols_width(
    c(img) ~ px(60),
    c(plataforma) ~ px(120),
    c(enero, marzo, junio, septiembre,diciembre,promedio) ~ px(110)
  ) %>% 
  cols_align(
    columns = c(img),
    align = "center") %>% 
  cols_align(
    columns = c('plataforma'),
    align = "left") %>% 
  data_color(
    columns=c("enero", "marzo", "junio", "septiembre","diciembre",promedio),
    colors = scales::col_numeric(
      palette = as.character(c("#DBC3D6FF" ,"#CAA5C2FF", "#B887ADFF", "#7D4F73FF", "#53354DFF")),#paletteer::paletteer_d("Redmonder::dPBIYlPu", n = 7:11)),
      domain = NULL)) %>% 
  fmt_number(columns = c("enero", "marzo", "junio", "septiembre","diciembre",promedio),
             decimals = 0) %>% 
  tab_options(table.background.color = '#f9f9f9',
              table.border.top.color = "#36454f",
              table.border.bottom.color = "#36454f") %>% 
  tab_style(style=list(cell_borders(
    sides = "left",
    color = "black",
    weight = px(3))),
    locations=list(
      cells_body(
        columns = vars(promedio)))) %>% 
  tab_style(
    style = list(cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(3))),
    locations = list(
      cells_column_labels(
        columns = gt::everything()))) %>% 
  tab_style(
    style = cell_text(
      #font = google_font(name = 'Rye'),
      weight = 'bold',
      size = px(35),
      align = 'center'),
    locations = cells_title(groups = 'title')) %>%
  tab_style(
    style = cell_text(
      size = px(15),
      style = 'italic',
      align = 'center'),
    locations = cells_title(groups = 'subtitle')) %>%  
  tab_style(
    style=cell_text(
      weight = 'bold',
      size = px(15),
      align = 'center'),
    locations = cells_column_labels(gt::everything())) %>% 
  cols_label(
    img = "",
    plataforma = "Plataforma",
    promedio = "Promedio",
    enero = "Enero",
    marzo = "Marzo",
    junio = "Junio",
    septiembre = "Septiembre",
    diciembre = "Diciembre") %>% 
  tab_source_note("TABLE: @Rladiesbquilla | Inspirada en: Bill Schmid | DATA: FlixPatrol.com") %>% 
  tab_header(title = "Subscriptores Servicios Streaming",
             subtitle = html("De enero a diciembre de 2022, 
             <b style = 'color:#FF8200'>Netflix</b> 
             se mantuvo como la principal plataforma de streaming del mundo.
             En segundo lugar se encuentra
             <b style = 'color:#000E2F'>Disney + </b> 
             que obtuvo un cierre de año magnifico en su numero de 
             subscriptores <br> ")) %>% 
  #tab_spanner(label = "Trimestres 2022", columns = 3:7) %>% 
  tab_footnote(footnote = "Promedio de subcriptores por trimestre",
               locations = cells_column_labels(columns = 8))



## Reactable

library(reactable)
library(reactablefmtr)
library(friends)

friends
friends_emotions
friends_info



lineas<- friends |>
  filter(speaker %in% c("Rachel Green","Ross Geller",
                        "Monica Geller","Chandler Bing",
                        "Phoebe Buffay","Joey Tribbiani"),
         season<=4) |> 
  group_by(season,speaker) |> 
  summarise(lineas=n())


emociones<-friends |> 
  left_join(friends_emotions,by=c("season","episode","scene","utterance")) |> 
  select(speaker,season,emotion) |>
  filter(!is.na(emotion)) |> 
  group_by(season,speaker,emotion) |>
  summarise(n_veces=n()) |> 
  filter(speaker %in% c("Rachel Green","Ross Geller",
                        "Monica Geller","Chandler Bing",
                        "Phoebe Buffay","Joey Tribbiani"),
         emotion %in% c("Joyful","Mad","Scared")) |> 
  pivot_wider(names_from = "emotion",values_from ="n_veces")


friends_data<- lineas |>
  left_join(emociones, by=c("season","speaker")) |> 
  rename(temporada=season,personaje=speaker,alegre=Joyful,
         enojado=Mad,asustado=Scared) |> 
  mutate(img=paste0("season",temporada,".jpg"),
         temporada=paste0("Temporada",temporada)) |> 
  relocate(img,.before = temporada)


friends_data |> 
  reactable(
      defaultColDef = colDef(header = function(value) str_to_sentence(value),
                             minWidth = 90),
      columns= list(
        img= colDef(
          name="",
          cell = function(value) {
            image <- img(src =paste0(value), style = "height: 24px;", alt = value)
            tagList(
              div(style = "display: inline-block; width: 45px;", image)
            )
          }),
        temporada= 
      )
  )

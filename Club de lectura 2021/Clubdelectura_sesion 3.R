#                          Club de lectura
#                       Sesion 1 - Capitulo 19
#            "R for Data Science" de Hadley Wickham y Garrett Grolemund
#Organiza: R-Ladies Galapagos, R-Ladies Barranquilla, R-Ladies Milagro, R-Ladies Guayaquil
#            Expositora: Isabel Vasquez Alvarez (R-Ladies Barranquilla)


#----Ejemplo función----

#Construya una función llamada salario que le ingrese el salario 
#por hora y el número de horas trabajadas durante una semana por 
#un trabajador. La función debe calcular el salario neto semanal.

salario<-function(sporhoras,horas){
  
  sal<- sporhoras*horas
  return(paste0("El salario neto es: $",sal))
}

tabla_salarios<- data.frame(Id=1:10,horas= trunc(runif(10,10,48)))

salario2<-function(datos){
  datos[,NCOL(datos)+1]<- 120*datos$horas
  return(datos)
}

salario2(tabla_salarios)

#----Ejemplo condicional----

#Construya una función llamada salario que le ingrese el salario 
#por hora y el número de horas trabajadas durante una semana por 
#un trabajador. La función debe calcular el salario neto semanal, 
#teniendo en cuenta que si el número de horas trabajadas durante 
#la semana es mayor de 48, esas horas de demás se consideran horas 
#extras y tienen un 35% de recargo. Imprima el salario neto.

tabla_salarios<- data.frame(Id=1:10,horas= trunc(runif(10,10,55)))

salario2<-function(datos){
  for (i in 1:NROW(datos)) {
    if (datos[i,2]<48){datos[i,3]<- 120*datos[i,2]
    }
    else{datos[i,3]<- 120*48+ 120*1.35*(datos[i,2]-48)
    } 
   
    }
  
  return(datos)
}
salario2(tabla_salarios)

###Condicional consola

encuesta <- function() {
  r <- readline("¿Te gusta R? (s/n) : ")
  if ( r == "s" || r == "S") {
    cat("¡Estaba seguro de eso!\n")
    return(invisible(0))
  } else {
    cat("¿Estás seguro/a? Creo que te has equivocado al responder.\nVuelve a intentarlo.\n\n")
    encuesta()
  }
}

#Construya una función llamada nota que calcule la nota obtenida 
#por un alumno en una evaluación de tres puntos cuya ponderación o 
#importancia son 20%, 30% y 50% para los puntos I, II y III 
#respectivamente. Adicionalmente la función debe generar un mensaje 
#sobre si el estudiante aprobó la evaluación o no. El usuario debe 
#ingresar las notas individuales de los tres puntos y la función debe 
#entregar la nota final de la evaluación.

nota<- function(p1,p2,p3){
  nota<- p1*0.2+p2*0.3+p3*0.5
  msj<-readline("El estudiante ha obtenidos bonos para la calificación (s/n): ")
if(msj == "s" || msj== "S"){
  msj2<- readline("digite el valor del bono: ")
  bono <- as.numeric(msj2)
  nota<- (p1*0.2+p2*0.3+p3*0.5)+bono
  if(nota< 3){
    return(paste0("El estudiante reprobó con ",nota))
  }
  else{
    return(paste0("El estudiante aprobó con ",nota))
  }
 
}
  else{
    if(nota< 3){
      return(paste0("El estudiante reprobó con ",nota))
    }
    else{
      return(paste0("El estudiante aprobó con ",nota))
    }
  }
  
}

nota(4.3,2,2)


#### argumentos adicionales ...

coseno <- function(w, ...) {
  x <- seq(-2 * pi, 2 * pi, length = 200)
  plot(x, cos(w * x), ...)
}

coseno(1)
coseno(w = 2, col = "red", type = "l", lwd = 2)

coseno(w = 2, ylab = "", xlab="")


datos<- read.csv("encuestas.csv", sep = ";")

graf_frecuencias<- function(dataset,columna,...){
library(ggplot2)
variable<-  dataset[,columna]
dat<-data.frame(table(variable))
ggplot(dat,aes( x = variable, y= Freq, fill= variable))+
  geom_bar(position="dodge", stat="identity")+
  theme_classic() +
  labs(x = "Valoración",
       y = "Frecuencia")+
  geom_text(aes(label = paste0(dat[,2]) , y = dat[,2]),
            vjust = 1.2, size = 5, color = "white" )+
  theme(...)
}

graf_frecuencias(datos,1,legend.position="none")
graf1<-graf_frecuencias(datos,1,legend.position="none")

#### Valores de entorno

sporhora<- 120

salario2<-function(datos){
  
  datos[,NCOL(datos)+1]<- sporhora*datos$horas
  return(datos)
}

salario2(tabla_salarios)









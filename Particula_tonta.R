#install.packages("imager")
library(imager)
library(dplyr)
library(ggplot2)
library(sqldf)
library(plotly)

#De Will Pittenger - Trabajo propio, Dominio público, https://commons.wikimedia.org/w/index.php?curid=4399317
ubicacion_figura = "C:/temp/personales/wordpress/Aprendizaje/"
jarama <- load.image(paste0(ubicacion_figura,"Circuito_Permanente_del_Jarama.svg"))
plot(jarama)

df <- as.data.frame(jarama)
summary(df$x)
summary(df$y)


df <- filter(df, value==0)
g <- ggplot(df,aes(x,y)) + geom_point() + labs(title="ANTES EL JARAMA QUE LOS CHALETS")

#Es necesario dar la vuelta
df$y <- -df$y + max(df$y)

#Trabajamos con muestra
indices <- sample( 1:nrow( df ), 10000 )
circuito <- df[ indices, ]
circuito <- circuito %>% arrange(x,y) %>% mutate(id=row_number())
row.names(circuito) <- NULL

# Inicio del circuito
inicio <- circuito %>% filter(x>=750 & x<=760 & y <= 200) 
inicio <- inicio[1,]

#Buscamos observaciones cercanas con una dirección
direccion = "izquierda"
otra= "derecha"
recorrido <- select(inicio,id,x,y)
recorrido$direccion=direccion
referencia <- inicio

for (i in (1:1650)){
  
  cambio_direccion = 0
  if (direccion=="izquierda") {operador="<"} else {operador=">"}
  consulta <- paste0("select a.id, a.x, a.y ,sqrt(power(a.x-b.x,2)+power(a.y-b.y,2)) as dist
                     from circuito a, referencia b where a.x", operador, "b.x  order by dist")
  busca <- sqldf(consulta)
  busca = busca[1,-4]
  busca$direccion=direccion
  
  cambio_direccion <- ifelse(is.na(busca$x), 1, 0)
  direccion <- ifelse(cambio_direccion==1, otra, direccion)
  otra = ifelse(direccion=="izquierda","derecha","izquierda")
  
  if (cambio_direccion==0) 
  {referencia <- busca 
  recorrido <- rbind.data.frame(recorrido,busca)} 
  
  else {referencia <- recorrido[i-1,]}
}

library(animation)
ani.options("C:/Program Files/ImageMagick-7.0.4-Q16/convert.exe")

saveGIF(
  for (i in seq(from= 1, to =nrow(recorrido), by=5)){
    plot(recorrido[1:i,]$x, recorrido[1:i,]$y,
         xlim=c(0,max(recorrido$x)),ylim=c(0,max(recorrido$y)),
         cex=0.1)
  }
  ,interval=0.1
  ,movie.name="c:/temp/animaciones/prueba.gif")



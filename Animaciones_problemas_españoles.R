library(XML)
library(dplyr)
library(lubridate)

#Lectura de los datos del CIS
url <- "http://www.cis.es/cis/export/sites/default/-Archivos/Indicadores/documentos_html/TresProblemas.html"
doc = htmlParse(url,  encoding = "UTF-8")
tableNodes = getNodeSet(doc, "//table")

#Leemos la tabla que tiene un formato un tanto peculiar
problemas <- readHTMLTable(tableNodes[[2]], skip.rows=1)
problemas <- problemas %>% filter(!is.na(V1)) 

#Transformamos los puntos en 0, parece que estuviera hecho con SAS
haz.cero.na=function(x){ifelse(x==".",0,as.numeric(as.character(x)))}

problemas <- cbind.data.frame(as.character(problemas$V1),sapply(problemas,haz.cero.na),stringsAsFactors=FALSE)
problemas <- problemas %>% select(-V1)

#El primer elemento de la tabla contiene los nombres que vamos a emplear
nombres <- readHTMLTable(tableNodes[[2]])[1,]
nombres$V1="Problema" 
nombres <- as.vector(t(nombres))

names(problemas) <- nombres

#Hay un registro en la tabla que tiene el número de encuestas, no es necesario
problemas <- filter(problemas,Problema != "(N)")

#Leemos mes a mes y creamos un data frame por mes que vamos uniendo horizontalmente
for (i in 2:length(nombres)){
  mes = nombres[i]
  instruccion <- paste0('df <- problemas %>% arrange(desc(', mes, 
                        ')) %>%  mutate(Problema = ifelse(row_number()>=11,"RESTO",Problema)) %>% 
                          select(Problema,', mes, ') %>% group_by(Problema) %>% summarise(porcentaje=sum(',mes,')) %>%
                        mutate(mes = "',mes,'")%>% arrange(desc(porcentaje)) %>% mutate(rank=row_number())')
  eval(parse(text=instruccion))
  if (i == 2) {resultado <- df}
  else {resultado <- rbind.data.frame(df,resultado)}
}

#A la hora de realizar la animación es mejor disponer de fechas por lo que tenemos que transformar
resultado$mes <- parse_date_time2(resultado$mes, orders = "my")

#Ya estamos en disposición de realizar el gráfico  
library(ggplot2)
library(gganimate)

#El primer y más importante paso es realizar el gráfico estático
estatico <- ggplot(resultado, aes(x=-rank, group=Problema, y=porcentaje,fill=Problema, color=Problema)) +
  geom_text(aes(y = 0, label = paste(Problema, " ")), vjust = 0.2, hjust = 1, size=4) +
  geom_bar(stat='identity')  + theme_minimal() + theme(legend.position = "none") + 
  coord_flip(clip = "off", expand = TRUE) +
  scale_y_continuous(limits = c(0, 100)) +
  theme(plot.title = element_text(size = 14),plot.margin = margin(1,1,1,9, "cm"), 
        axis.title.y=element_blank(), axis.text.y=element_blank(),  axis.ticks.y=element_blank())

# Especificamos la transición
p <- estatico + transition_manual(mes) + labs(title = "Mes de encuesta: {month(current_frame)}/{year(current_frame)}") 

#Realizamos la animación
animate(p, fps = 0.7, width = 800, height = 600)

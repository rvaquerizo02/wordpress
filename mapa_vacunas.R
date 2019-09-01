library(mapSpain)
library(sf)
library(tidyverse)
library(lubridate)
library(gganimate)

#Obtenemos los datos de datadista
datadista = "https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_vacunas.csv"

tabla_ccaa <- read.csv2(datadista, sep=',',encoding = 'UTF-8', check.names=FALSE)
tabla_ccaa <- tabla_ccaa %>% mutate(fecha = as.Date(`Fecha publicación`))

CCAA.sf <- esp_get_ccaa()

CCAA.sf = CCAA.sf %>% mutate(CCAA = ine.ccaa.name) %>% mutate(CCAA=case_when(
  grepl('Madrid', CCAA, fixed = T)>0 ~ 'Madrid',
  grepl('León', CCAA, fixed = T)>0 ~ 'Castilla y Leon',
  grepl('Asturias', CCAA, fixed = T)>0 ~ 'Asturias',
  grepl('Balea', CCAA, fixed = T)>0 ~ 'Baleares',
  grepl('Mancha', CCAA, fixed = T)>0 ~ 'Castilla La Mancha',
  grepl('Rioj', CCAA, fixed = T)>0 ~ 'La Rioja',
  grepl('Navarra', CCAA, fixed = T)>0 ~ 'Navarra',
  grepl('Valencia', CCAA, fixed = T)>0 ~ 'C. Valenciana',
  grepl('Murcia', CCAA, fixed = T)>0 ~ 'Murcia',
  TRUE ~ CCAA))

CCAA.sf <- CCAA.sf %>% left_join(select(tabla_ccaa, CCAA,`Porcentaje sobre entregadas`, fecha)) %>%
  rename(`% sobre entrega` = `Porcentaje sobre entregadas`)

p <- ggplot() + geom_sf(data=CCAA.sf, aes(fill=`% sobre entrega`)) +
  scale_fill_continuous(low="white",high="darkred") + geom_sf(data = esp_get_can_box(), colour = "grey50") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  transition_time(fecha) + labs(title = 'Fecha de datos: {frame_time}')

mapa_vacunas <- animate(p, duration = 10, end_pause = 45)
magick::image_write(mapa_vacunas, path="/home/rvaquerizo/Documentos/wordpress/mapa_vacunas.gif")

tabla_ccaa <- tabla_ccaa %>% filter(CCAA=="España") %>% rowwise() %>% 
  mutate(Dosis=sum(as.numeric(gsub('.','',`Dosis entregadas Pfizer`, fixed=TRUE)),
                   as.numeric(gsub('.','',`Dosis entregadas Moderna`, fixed=TRUE)),
                   na.rm=TRUE))
  
dosis <- tabla_ccaa %>% filter(CCAA=="España") 

ggplot(dosis, aes(x=fecha, y=`Porcentaje sobre entregadas`)) + geom_line()




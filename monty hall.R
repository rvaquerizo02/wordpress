
library(tidyverse)

# Un solo juego
puertas <- data.frame(puerta=seq(1:3))
puertas$aleatorio1=runif(1)
puertas <- puertas %>% mutate(premio = case_when(
  aleatorio1<=1/3 ~ 1,
  aleatorio1<=2/3 ~ 2,
  T ~ 3),
  premio = ifelse(premio==puerta, 'coche','cabra'))

puertas$aleatorio2 = runif(1)
puertas <- puertas %>% mutate(eleccion1 = case_when(
  aleatorio2<=1/3 ~ 1,
  aleatorio2<=2/3 ~ 2,
  T ~ 3))

rm1 <- puertas %>% filter(premio != 'coche' & eleccion1 != puerta) %>% 
  sample_n(1)
puertas$presentador = rm1$puerta

puertas$eleccion_final = ifelse(puertas$puerta != puertas$eleccion1 & puertas$puerta != puertas$presentador, puertas$puerta, 0)
gana <- puertas %>% filter(premio=='coche') %>% mutate(gana = eleccion_final==puerta)
partidas = sum(gana$gana)


#  Mantenemos nuestra elección
n_juegos = 100
for (i in (seq(1:n_juegos))){
puertas <- data.frame(puerta=seq(1:3))
puertas$aleatorio1=runif(1)
puertas <- puertas %>% mutate(premio = case_when(
  aleatorio1<=1/3 ~ 1,
  aleatorio1<=2/3 ~ 2,
  T ~ 3),
  premio = ifelse(premio==puerta, 'coche','cabra'))

puertas$aleatorio2 = runif(1)
puertas <- puertas %>% mutate(eleccion1 = case_when(
  aleatorio2<=1/3 ~ 1,
  aleatorio2<=2/3 ~ 2,
  T ~ 3))

rm1 <- puertas %>% filter(premio != 'coche' & eleccion1 != puerta) %>% 
  sample_n(1)
puertas$presentador = rm1$puerta

puertas$eleccion_final = puertas$eleccion1
gana <- puertas %>% filter(premio=='coche') %>% mutate(gana = eleccion_final==puerta)
if (i == 1) {partidas <- gana}

partidas <- rbind(partidas, gana)}

sum(partidas$gana)/n_juegos

# Cambio la elección
n_juegos = 100
for (juegos in (seq(1:n_juegos))){
  puertas <- data.frame(puerta=seq(1:3))
  puertas$aleatorio1=runif(1)
  puertas <- puertas %>% mutate(premio = case_when(
    aleatorio1<=1/3 ~ 1,
    aleatorio1<=2/3 ~ 2,
    T ~ 3),
    premio = ifelse(premio==puerta, 'coche','cabra'))
  
  puertas$aleatorio2 = runif(1)
  puertas <- puertas %>% mutate(eleccion1 = case_when(
    aleatorio2<=1/3 ~ 1,
    aleatorio2<=2/3 ~ 2,
    T ~ 3))
  
  rm1 <- puertas %>% filter(premio != 'coche' & eleccion1 != puerta) %>% 
    sample_n(1)
  puertas$presentador = rm1$puerta
  
  # La elección será la que no hemos elegido al principio ni la que ha hecho el presentador
  # Da lo mismo el resultado del resto de puertas, siempre nos quedemos con el registro del coche
  puertas$eleccion_final = ifelse(puertas$puerta != puertas$eleccion1 & puertas$puerta != puertas$presentador, puertas$puerta, 0)
  gana <- puertas %>% filter(premio=='coche') %>% mutate(gana = eleccion_final==puerta)
  if (juegos == 1) {partidas <- gana}
  
  partidas <- rbind(partidas, gana) }

sum(partidas$gana)/n_juegos
    
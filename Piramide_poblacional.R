library(readxl)
library(readxl)
library(dplyr)

df <- read_excel("C:\\temp\\personales\\wordpress\\10256.xlsx")

library(plotrix)

Edad <- c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21",
          "22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43",
          "44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65",
          "66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85 y más")

rango <- c("0-5","0-5","0-5","0-5","0-5","0-5","6-10","6-10","6-10","6-10","6-10","11-15","11-15","11-15","11-15",
           "11-15","16-20","16-20","16-20","16-20","16-20","21-25","21-25","21-25","21-25","21-25","26-30","26-30",
           "26-30","26-30","26-30","31-35","31-35","31-35","31-35","31-35","36-40","36-40","36-40","36-40","36-40",
           "41-45","41-45","41-45","41-45","41-45","46-50","46-50","46-50","46-50","46-50","51-55","51-55","51-55",
           "51-55","51-55","56-60","56-60","56-60","56-60","56-60","61-65","61-65","61-65","61-65","61-65","66-70",
           "66-70","66-70","66-70","66-70","71-75","71-75","71-75","71-75","71-75","76-80","76-80","76-80","76-80",
           "76-80","+80","+80","+80","+80","+80")

tramos_edad <- cbind.data.frame(Edad,rango)

df <- df %>% left_join(tramos_edad)

library(animation)

agelabels<-c("0-5","5-10","10-15","15-20","20-25","25-30","30-35","35-40","40-45","45-50","50-55","55-60",
             "60-65","65-70","70-75","75-80","+80")

fcol<-color.gradient(c(0,0,0.5,1),c(0,0,0.5,1),c(1,1,0.5,1),17)
mcol<-color.gradient(c(1,1,0.5,1),c(0.5,0.5,0.5,1),c(0.5,0.5,0.5,1),17)

ani.options("C:/Program Files/ImageMagick-7.0.4-Q16/convert.exe")

saveGIF(
  for (year in c("1975","1980","1985","1990","1995","2000",
                 "2005","2010","2015","2018")){
    
    hombres <- df %>% filter(Anio==year) %>%
      group_by(Anio,rango) %>% 
      summarise(Hombres=sum(Hombres), orden=min(Edad)) %>%
      arrange(orden)
    
    mujeres <- df %>% filter(Anio==year) %>%
      group_by(Anio,rango) %>% 
      summarise(Mujeres=sum(Mujeres), orden=min(Edad)) %>%
      arrange(orden)
    
    hombres <- as.vector(unlist(hombres$Hombres))
    hombres <- (hombres/sum(hombres)) * 100 
    mujeres <- as.vector(unlist(mujeres$Mujeres))
    mujeres <- (mujeres/sum(mujeres)) *100
    
    par(mar=pyramid.plot( hombres, mujeres, labels=agelabels,lxcol=mcol,rxcol=fcol,show.values=TRUE))
    title(paste0("Pirámide poblacional de España año ",year),cex.sub=1.7)
  },
  interval=1,
  movie.name="c:/temp/animaciones/prueba.gif"
)
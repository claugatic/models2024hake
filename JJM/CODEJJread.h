
rm(list=ls()) # LIMPIA DATOS
WD<- getwd()  # UBICA DIRECTOR
setwd(WD)     # RECONOCE DIRECTORIO


# Merluza comun en JJM de jim ianelli

#install.packages("devtools")
#devtools::install_github("SPRFMO/jjmR")
library(jjmR)
library(here)
library(dplyr)
library(foreach)
library(doParallel)
registerDoParallel(6)



path.1<- file.path("c:/CEGM/MODEL/merluza/jjmhake/merlu2022/jjm/")
path.2<- file.path("c:/CEGM/MODEL/merluza/jjmhake/merlu2022/jjm/config") # config
path.3<- file.path("c:/CEGM/MODEL/merluza/jjmhake/merlu2022/jjm/input") # input
path.4<- file.path("c:/CEGM/MODEL/merluza/jjmhake/merlu2022/jjm/results") #results


# Lectura Modelos
#hake1 <- jjmR::readJJM("mtwojim", path = "config", input = "input")
hake1 <- jjmR::readJJM("octubre23", path = "config", input = "input")

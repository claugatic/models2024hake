
rm(list=ls()) # LIMPIA DATOS
WD<- getwd()  # UBICA DIRECTOR
setwd(WD)     # RECONOCE DIRECTORIO


#install.packages("devtools")
#devtools::install_github("SPRFMO/jjmR")
library(jjmR)
library(here)
library(dplyr)
library(foreach)
library(doParallel)
registerDoParallel(6)


file.path()
path.1<- file.path("C:/CEGM/MODEL/merluza/M23/DEV24/JJM/")
path.2<- file.path("C:/CEGM/MODEL/merluza/M23/DEV24/JJM/config") # config
path.3<- file.path("C:/CEGM/MODEL/merluza/M23/DEV24/JJM/input") # input
path.4<- file.path("C:/CEGM/MODEL/merluza/M23/DEV24/JJM/results") #results


# Correr casos
#modh1_0.00rat <- jjmR::runit(mod="mtwojimw", est=T, pdf=T,
#exec=file.path(path.1,"jjms"),
#path=path.2,
#input=path.3,
#output=path.4)



# Correr casos
modh1_0.00rat <- jjmR::runit(mod="oct1", est=T, pdf=T,
exec=file.path(path.1,"jjms"),
path=path.2,
input=path.3,
output=path.4)

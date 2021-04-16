install.packages("ploty")
install.packages("reshape2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)
library(plotly)
library(reshape2)

#1. Descargar el archivo
MICRO<-readxl::read_excel("C:/Users/Sebastian Marin/Dropbox/Data/BD PROYECTO BIG DATA2.xlsx",sheet = 7)
na.omit(MICRO)
View(MICRO)
MACRO<-readxl::read_excel("C:/Users/Sebastian Marin/Dropbox/Data/BD PROYECTO BIG DATA2.xlsx",sheet = 6)
na.omit(MACRO)
View(MACRO)
COMPANY<-readxl::read_excel("C:/Users/Sebastian Marin/Dropbox/Data/BD PROYECTO BIG DATA2.xlsx",sheet = 8)
na.omit(COMPANY)
View(COMPANY)

#2. Agregar columnas con calculos 
COMPANY<-mutate(COMPANY,Bruta=Ventas-Costo)
COMPANY<-mutate(COMPANY,Operativa=Bruta-Gastos)
COMPANY<-mutate(COMPANY,MargenBr=Bruta/Ventas*100)
COMPANY<-mutate(COMPANY,MargenOp=Operativa/Ventas*100)
MACRO<-mutate(MACRO,PIB=PIB_Primario+PIB_Secundario+PIB_Terciario)

#3. Filtrar Data Frames
names(MICRO)
Consumo<-filter(MICRO,Giro %in% c("Productos de Consumo"))
MACROMOD<-filter(MACRO,Anno>=2015)#Filtramos desde 2015 para usar en el modelo

Bimbo<-filter(Consumo,Empresa %in% c("Bimbo"))#Únicamente extrae la empresa Bimbo
Walmex<-filter(Consumo,Empresa %in% c("Walmex"))
Inf2018<-filter(Bimbo, Anno == 2018)
Inf2018consumo<-filter(Consumo, Anno == 2018)

#4. Histograma
hist(Bimbo$Ventas)
hist(x = Bimbo$Ventas, main = "Histograma de Ventas", 
     xlab = "Ventas", ylab = "Frecuencia")
hist(Bimbo$Costo)
hist(x = Bimbo$Costo, main = "Histograma de Costo", 
     xlab = "Costo", ylab = "Frecuencia")
hist(Bimbo$Gastos)
hist(x = Bimbo$Gastos, main = "Histograma de Gastos", 
     xlab = "Gastos", ylab = "Frecuencia")
hist(Walmex$Ventas)
hist(MACRO$TC)
hist(MACRO$PIB_Primario)
hist(MACRO$IPC)


plot(x = Bimbo$Anno, y = Bimbo$Ventas, main = "Ventas Año", 
     xlab = "Anno", ylab = "Ventas", 
     col= c(blues9))#visualizar el mov de ventas Bimbo por año

ggplot(Inf2018,aes(x=Trimestre, y=Ventas, colours(distinct = FALSE)))+
  geom_point(shape=25, fill="blue", color="darkred", size=3)+
  geom_smooth()#Visualizar ventas por trimestre tomando de ref año 2018

ggplot(MACRO,aes(x=Anno, y=TC, colours(distinct = FALSE)))+
  geom_point(shape=25, fill="blue", color="darkred", size=3)+
  geom_smooth()#visualizar el movimiento del T.C


#5. Modelo
M1<-lm(Bimbo$Ventas~MACROMOD$PIB);summary(M1)
M1.1<-lm(Bimbo$Ventas~MACROMOD$PIB+MACROMOD$TIIE);summary(M1.1)

M2<-lm(Bimbo$Ventas~MACROMOD$Inflacion);summary(M2)
M2.1<-lm(Bimbo$Ventas~MACROMOD$Inflacion+MACROMOD$PIB_Primario);summary(M2.1)

M3<-lm(Bimbo$Ventas~Walmex$Ventas);summary(M3)

length(Bimbo$Ventas)#se usó como prueba porque originalmente me marcaba error
length(MACRO$PIB)#se usó como prueba porque originalmente me marcaba error

plot(M1)









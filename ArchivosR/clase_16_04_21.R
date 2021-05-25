###############################################################
#                UNIVERSIDAD SERGIO ARBOLEDA                  #
#    PROGRAMA DE INGENIERÍA DE SISTEMAS Y TELECOMUNICACIONES  #
#                   JULIETH GOMEZ MONTES                      #
#             JULIETH.GOMEZ@CORREO.USA.EDU.CO                 #
#                       16/04/21                   #
###############################################################

#Limpiar environment
#rm(list=ls())

#Link data
urlEDA = 'https://raw.githubusercontent.com/corredor-john/universidadsergioarboleda/main/charges_insurance.csv'
dfCharges_insurance = read.table(file = urlEDA, header = T, 
                               sep = ',')

#Se necesitan las bibliotecas
library(ggplot2)
library(dplyr)
library(tidyr)
library(GGally)

#Visualizacion de las variables (columnas del DataFrame charges_insurance)
#en todo el entorno.
attach(dfCharges_insurance)

#Inspeccion basica de los datos
head(dfCharges_insurance)

#Ver la estructura de los datos y columnas
str(dfCharges_insurance)

#Se requiere cambiar los valores discretos (categoricas) en factores
children <- as.factor(children)

#Gráfica de caja (BoxPlot) para los niños según el sexo
grafica1 <- ggplot(data = dfCharges_insurance,
                   mapping = aes(sex, expenses))+ geom_boxplot()
grafica1

#Gráfica de fumadores con no fumandores
grafica2 <- ggplot(data = dfCharges_insurance,
                   mapping = aes(bmi, expenses, 
                                 col = smoker))+ geom_point()
grafica2

#De la grafica se puede decir que en el eje x edad y en el 
#y gastos, los que fuman tienne mau¿to tendendia a gastar, una 
#persota de 45 o aprox 54 tiene gastos de casi los 70. mil de solares

#En el nivel rojo parecienra que esa poliza de los seguro no alcanza a llegar a los 20 mil fumadores.
#Los que si fuman no estan en la primera poliza sino que en el segundo
#Una poliza muy cara la pagan los fumadores

#Grafico de todos los datos
grafica3 <- ggplot(data = dfCharges_insurance,
                   mapping = aes(age, expenses, 
                                 col = smoker))+ geom_point()
grafica3

#Gráfico de todos los datos con ggally
grafica4 <- ggpairs(data = dfCharges_insurance)
grafica4

#Padres fumadores vs no fumadores
#madres
dfCharges_insurance %>% filter(children != 0, sex == 'female') %>% group_by(smoker) %>%
  summarise(probmi = mean(bmi), proexpenses = mean(expenses), n = n())

#padres
dfCharges_insurance %>% filter(children != 0, sex == 'male') %>% group_by(smoker) %>%
  summarise(probmi = mean(bmi), proexpenses = mean(expenses), n = n())

#region
dfCharges_insurance %>% group_by(region) %>%
  summarise(probmi = mean(bmi), proexpenses = mean(expenses), n = n())

#TAREA HACER LO MISMO CON PANDAS



##########################################
#       UNIVERSIDAD SERGIO ARBOLEDA      #
#    PROGRAMA DE INGENIERÍA DE SISTEMAS  #
#          JULIETH GOMEZ MONTES          #
#     JULIETH.GOMEZ@CORREO.USA.EDU.CO    #
#                12/03/21                #
#          INTRODUCCIÓN A R              #
##########################################


#Se requiere importar el modulo ISLR el cual contiende un conjunto
#bien documentado de datos para el analisis exploratorio de datos.
#Andes de ser invocado se debe instalar en nuestro entorno. 
#Para su instalacion se hace uso del comando install.packages('ISLR), 
#en la consola.

library(ISLR)


#Se requiere presentar las caracteristicas principales del dataset Carseats
str(Carseats)

##Sales
#Unit sales (in thousands) at each location

##CompPrice
#Price charged by competitor at each location

#Income
#Community income level (in thousands of dollars)

##Advertising
#Local advertising budget for company at each location (in thousands of dollars)

##Population
#Population size in region (in thousands)

##Price
#Price company charges for car seats at each site

##ShelveLoc
#A factor with levels Bad, Good and Medium indicating the quality of the shelving location for the car seats at each site

##Age
#Average age of the local population

##Education
#Education level at each location

##Urban
#A factor with levels No and Yes to indicate whether the store is in an urban or rural location

##US
#A factor with levels No and Yes to indicate whether the store is in the US or no

#Le asignamos una variable a nuestro dataframe
CarS <- ISLR::Carseats

#Se requiere tener una vista del dataframe
View(CarS)

#Se requiere obtener un reporte estadistico de todas las variables del dataframe

library(psych)
describe((CarS))#Si muestra la cantidad de números nulos
summary(CarS) #No me da la cantidad de numeros nulos

#//Se observa que no se tienen para el dataset valores desaparecidos (missing values),
#//por lo que se requiere contaminar el dataset con valores nulos (De forma aleatoria)

#En primer lugar la semilla para la creacion de numeros aleatorios
set.seed(123)

#Insertamos la muestra de valores null aleatorios en la columna Income
#sample por filas, con secuencia// Funcion lambda//genera la distancia entre la aleatoriedad
CarS[sample(seq(NROW(CarS)),30),'Income']<-NA

#Insertamos la muestra de valores null aleatorios en la columna urban
CarS[sample(seq(NROW(CarS)),25),'Urban']<-NA

#Para comprobar que realmente se han insertado los valores null o desaparecidos 
#hacemos uso de la función estadistica. función describe
describe(CarS)

#Los valores corresponden a:
#n: Números de observaciones no desaparecidas
#mean: Promedio aritmetico
#sd: Desviacion estandar
#median: La mediana
#trimmed: M
#mad:
#Skew: Asimetria
#kurtosis: Probabilidad de Kurtosis(medida estadistica, que define el grado de diferencia entre las colas de una distribucion
#y las de una distribucion normal).
#se:

#Se requiere hacer analisis estadistico de 'sales', 'income' y 'population'
#describe(CarS$Sales,CarS$Income,CarS$Population)

#Cálculo de coeficiente de correlación
library("lsr")
correlate(CarS)
correlate(CarS$Sales,CarS$Income)

#Visualizacion de la correlacion 
library("dlookr")
plot_correlate(CarS)
plot_correlate(CarS,Sales,Price)

#Visualizacion por grupo atraves de una función considerando una variable categorica
##Crear una funcion

CarS %>%
  filter(ShelveLoc=="Good") %>%
  group_by(Urban) %>%
  plot_correlate(Sales)

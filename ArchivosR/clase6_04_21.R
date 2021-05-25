###############################################################
#                UNIVERSIDAD SERGIO ARBOLEDA                  #
#    PROGRAMA DE INGENIERÍA DE SISTEMAS Y TELECOMUNICACIONES  #
#                   JULIETH GOMEZ MONTES                      #
#             JULIETH.GOMEZ@CORREO.USA.EDU.CO                 #
#                       06/04/21                              #
#           TALLER:                #
###############################################################

#limpiar environment
#rm(list = ls())

#Se cargan los datos desde el enlace
urlLung <- "https://raw.githubusercontent.com/julySgm/Analitica-de-datos/master/LungCapDataTabla1.csv"
LungCapData = read.table(file = urlLung, header = T, sep= ",")

#Se requiere observar el dataframe
View(LungCapData)

#Visualizacion de la variables (columnas del dataframe 'LungCapData) en todo el entorno
attach(LungCapData)

#Se requiere hacer un estudio sobre las variables 'Age' y 'Height'
# Tipo de clase de 'Age' y 'Height'
class(Age)
class(Height)

#Visualizacion Grafica de la relacion entre 'Age' y 'Height'
#Para ello se hace un gráfico de dispersión
plot(Age, Height, 
     main = 'Dispersion "Age" vs "Height" ',
     col = 'red',
     xlim = c(0,25),
     ylim = c(30,95))

#Segun la gráfica se puede apreciar una relación significativa entre la edad y la altura
#Graficar una línea que se ajuste a la gráfica: regresion lineal
#abline: Añade una o más lineas rectas a la gráfica
#lm: Esta línea me permite a mi presentar la recta de ajuste en un mdelo de regresion lineal
abline(lm(Height~Age),
       col = 'blue')

#Cálculo de la correlación y la co-varianza
#El factor de correlación va entre 1 y -1, es decir que si el valor de correlacion es cercano a 1
#las variables tienen una relación directas proporcionales y si el valor de correlacion es cercano 
#a -1 las variables tienen una relacion inversa proporcional

#Correlación Rango de Spearman: Medida no parametrica de la asociacion monotona entre dos variables
#numerica

#Correlacion Rango de Kendall: Medida no parametrica de la asociacion basada en concordancia y no concordancia 
#(discordancia) del par X~Y

#Correlacion de Pearson: Medidad de la fuerza de una asociacion lineal entre dos 
#variables. Basicamente va tratar de buscar una línea de mejor ajuste entre esas dos
#variables, y el coeficiente de correlacion indica cual lejos estan los puntos de esa 
#linea de mejor ajuste.



#El método de pearson para el cálculo de coeficiente correlacion esta por defecto


#Método de Pearson
cor(Age, Height)
cor(Height, Age)

#Dado que se acerca a 1 se puede decir que hay una relacion significativa directa 
#y proporcional entre las dos variables 'Age' y 'Height'

#El orden de las variables no incide

#Método de Spearman para el cálculo del coeficiente de correlación
cor(Height, Age, method = 'spearman')

#Método de Kendall para el cálculo del coeficiente de correlación
cor(Height, Age, method = 'kendall')

#Calculamos la co-varianza
cov(Height,Age)

#Observemos los graficos para cada variable(Numerica) dataFrame
pairs(LungCapData[1:3])

#Observemos las correlaciones para cada variable numerica en el dataFrame
cor(LungCapData[1:3])


#






###############################################################
#                UNIVERSIDAD SERGIO ARBOLEDA                  #
#    PROGRAMA DE INGENIERÍA DE SISTEMAS Y TELECOMUNICACIONES  #
#                   JULIETH GOMEZ MONTES                      #
#             JULIETH.GOMEZ@CORREO.USA.EDU.CO                 #
#                       06/04/21                              #
#                   Regresion Lineal                          #
###############################################################


# Crear un modelo de regresion lineal simple para las variables 'Age'
# y 'LungCap'
# LungCap es la variable dependiente(y)

#Se cargan los datos desde el enlace
urlLung <- "https://raw.githubusercontent.com/julySgm/Analitica-de-datos/master/LungCapDataTabla1.csv"
LungCapData = read.table(file = urlLung, header = T, sep= ",")

#Se requiere observar el dataframe
View(LungCapData)

#Visualizacion de la variables (columnas del dataframe 'LungCapData) en todo el entorno
attach(LungCapData)

# Se gráfica las variables 'Age' y 'LungCap'
plot(Age, LungCap, main = 'Gráfica LungCap vs Age',
     xlim = c(0,22),
     ylim = c(-2,16),
     col = 'blue',
     cex = 0.5)

# Se calcula la correlacion entre las dos variables
cor(LungCap, Age)

#Se presenta una asociación positiva, directa y proporcional
#Entre las dos variables.

#Se ajusta la regresion lineal entre las dos variables
#Es importante el orden de las variables 
#(y:Primera variable; x: Segunda variable)
modeloRegresion = lm(LungCap~Age)

abline(modeloRegresion,
       lw = 4)


#Se requiere un resumen estadistico del modelo
summary(modeloRegresion)

#Residuals: Resumen de los residuos o errores
#R-Squared: La variacion explicada / la variacion total
#Siempre esta entre 0 y 1 (0-100%)
#0 significa que el modelo no explica ninguna de las variabilidades
#100% significa que el modelo explica toda la variabilidad de los datos
#en torno a su media

#Observar los atributos del modelo
attributes(modeloRegresion)


#Observar los coeficientes del modelo
coef(modeloRegresion)

#1.19 si tengo 0 edad deberia tener una capacidad pulmonar de 1.19
#0.54 por cada unidad de crecimiento de la capacidad pulmonar hay 0.54 
#unidades de crecimiendo de la edad


#Observar los intervalos de confianza para los coeficientes
confint(modeloRegresion)
#Intervalo de confianza, el intercepto de97.5 deberia ser 1.5 para arriba y para abajo un 0.83

#Se quiere un intervalo de confianza de 99%
confint(modeloRegresion, level = 0.99)

#tabla de analisis de varianza (ANOVA)
##La capacidad pulmonar (LungCap) sera 'y' (salida, o variable dependiente)
##La línea de regresion puede ser pensada como el valor predicho (o ajustado)
##El valor predicho se etiqueto como  ŷ
anova(modeloRegresion)

#Errores residuales: Son la diferencia entre el valor real (y) y 
#el valor predicho (ajustado) ( ŷ)
#Supuestos en funcion del grafico:
##1. Los valores de y son independientes
##2. Los valores de 'y' "pueden" ser expresados como una funcion lineal
##3. La variacion de las observaciones (valores de y) alrededor de la línea de 
##   regresion lineal es constante.

#R Studio tiene diferentes graficas para hacer un estudio de diagnostico de 
#regresion (4 en total)

plot(modeloRegresion)
#Primer Grafico: El primer gráfico significa si se cumple el supuesto de linealidad
#se debe observar una linea recta. Se observa una nube de puntos, sin ningun patron.
#No cumple el supuesto lineal

#Segundo gráfico: cuartil - cuartil (QQ)
#El eje x representa errores residuales ordenados teoricos
#El eje y representa errores residuales ordenados estandarizados
#Se observa para el supuesto si se aprecia una linea diagonal recta, por lo 
#Para el supuesto se espera observar o visualizar una linea diagonal

#Tercer y cuarto Gráfico:
#Identifica la no linealidad, no varianza constante. 

#Se desea observar las 4 gráficas al mismo tiempo 
par(frow = c(2, 2))
plot(modeloRegresion)




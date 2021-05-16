###############################################################
#                UNIVERSIDAD SERGIO ARBOLEDA                  #
#    PROGRAMA DE INGENIERÍA DE SISTEMAS Y TELECOMUNICACIONES  #
#                   JULIETH GOMEZ MONTES                      #
#             JULIETH.GOMEZ@CORREO.USA.EDU.CO                 #
#                       13/04/21                              #
#           Regresion Lineal Multivariable                    #
###############################################################

#Se cargan los datos desde el enlace
urlLung <- "https://raw.githubusercontent.com/julySgm/Analitica-de-datos/master/LungCapDataTabla1.csv"
LungCapData = read.table(file = urlLung, header = T, sep= ",")

#Poner en el etorno todas las columnas como variables visibles
attach(LungCapData)

# Resumen estadistico del dataset (Por lo menos las variables numericas)
summary(LungCapData)

# Se requiere el nombre de las columnas 
names(LungCapData)

# Se requieren revidar el tipo de dato para cada variable (columna del dataset)
summary(LungCap)
summary(Age)
summary(Height)
class(LungCap)
class(Age)
class(Height)

# Las variables Age, LungCap y Height son del tipo numérico


##############################################
#Para el modelo de regresion lineal (lm: y-> X1+X2+X3+...+Xn
##############################################


##################################
#        -Modelo01-              #   
##################################
# El primer modelo que se requiere construr es de Age(X1) y Height(X2), con la LungCap(y)
# Modelo 01 <- lm(LungCap ~ Age+Height)

modelo01 <- lm(LungCap ~ Age + Height)

# Se revisa el resumen estadistico del modelo
summary(modelo01)

# El R2 score, conocido como coeficiente de determinacion, es una madida de que tan bueno
# es la prediciion para el modelo de regresion. Su rango es de 0 a 1. El valor de 1 correspponde
# a una prediccion perfecta, y el valor de 0 corresponde a un modelo constante que solo predice
# el promedio de las predicciones del conjunto de dtos.
###########

# Multiple R-Squared: 0.842 => Se puede decir que el modelo se ajusta "moderadamente bueno o aceptable".
#Si hubiera sido mayor a 0.9 seria impencindible
# Aproximadamente el 84% de la variación en la capacidad pulmonar (LungCap), puede ser explicada por el modelo
# asociado a las variables 'Age' y 'Height'.

#############

# RSE (Residual standard error: 1.055 on 722 degress of fredom), presenta una idea de cual lejos
# esta la variable real (LungCap) Observada y la variable predicha o ajustada ( ŷ)
# rse = y -  ŷ
############
# (intercept) -11.710182, es el valor medio "estimado" de y cuando todas las observaciones (Age, Height)
# son iguales a cero (0).
# El promedio estimado de la capacidad pulmonar (-11.71), cuando Age, Height = 0
###########
# Age: 0.122229, [Pendiente]. Efecto de la edad 'Age' sobre LungCap, ajustado o controlado por la 
# altura o 'Height'. En otras palabras, se asocia un aumento de 1 año en la edad con un aumento
# de 0.122229 en la capacidad pulmonar (LungCap), ajustado o controlado por 'Height'.

# Height: 0.278747, [Pendiente]. Efecto de la altura 'Height' sobre LungCap, ajustado o controlado por la 
# 'Age'. En otras palabras, se asocia un aumento de 1 pulgada en la Height con un aumento
# de 0.122229 en la capacidad pulmonar (LungCap), ajustado o controlado por 'Age'.

###############

# Calculo de la correlacion de 'pearson' entre la variable independiente
cor(Age, Height)

# Se observa una alta correlación
# Presenta que debe evitarse interpretar el efecto de Age sobre LungCap,
# debido a que se presenta una fuerte vinculacion (asociacion/relacion)
# entre Age, Height (0.835) (Colinealidad)

##################

#Intervalos de confianza para los parametros del modelo (95%)
confint(modelo01, level = 0.95)
## intervalo de intercepto = -11.710182 (95% que se encuentre en el intervalo)
# ----> (-12.64522954, -10.7751342)
#intervalo de Age = -11.710182 (95% que se encuentre en el intervalo)
# ----> (-12.64522954, -10.7751342)
#intervalo de Height = -11.710182 (95% que se encuentre en el intervalo)
# ----> (-12.64522954, -10.7751342)

##############################################
#Para el modelo de regresion lineal (lm: y-> X1+X2+X3+...+Xn
##############################################


##################################
#        -Modelo02-              #   
##################################


# El segundo modelo que se requiere construr es de Age(X1) y Height(X2), con la LungCap(y)
# Modelo 02 <- lm(LungCap ~ Age+Height+Smoke+Gender+Caesarean)
modelo02 <- lm(LungCap~ Age + Height + Smoke + Gender + Caesarean )

# Se revisa el resumen estadistico del modelo
summary(modelo02)

# (intercept) -12.64522954, es el valor medio "estimado" de y cuando todas las observaciones 
#(Age+Height+Smoke+Gender+Caesarean) son iguales a cero (0).
# El promedio estimado de la capacidad pulmonar (-12.64522954), cuando Age, Height = 0
###########
# Age: 00.15533, [Pendiente]. Efecto de la edad 'Age' sobre LungCap, ajustado o controlado por la 
# altura o 'Height'. En otras palabras, se asocia un aumento de 1 año en la edad con un aumento
# de 0.15533 en la capacidad pulmonar (LungCap), ajustado o controlado por 'Height'.

# Height: 0.26497, [Pendiente]. Efecto de la altura 'Height' sobre LungCap, ajustado o controlado por la 
# 'Age'. En otras palabras, se asocia un aumento de 1 pulgada en la Height con un aumento
# de 0.26497 en la capacidad pulmonar (LungCap), ajustado o controlado por 'Age'.

###############
## Smokeyes: -0.60881, [Pendiente]. Efecto de 'Smoke' 'yes' (Si fuma)sobre LungCap, ajustado o controlado por la 
# las demas variables. 
#En otras palabras, se asocia un decremento de -0.60881 en la capacidad pulmonar (LingCap), ajustado o controlado
# por las demas variables


##############################################
############VISUALIZACION DEL MODELO01########
plot(modelo01)

##################################
#        -Modelo03-              #   
##################################

#Se requiere hacer un modelo en donde se tenga en cuenta variables
#independientes edad y fumadores frente a la capacidad pulmonar (LugCap)
modelo03 = lm(LungCap ~ Age + Smoke)

#Resumen estadistico del modelo03
summary(modelo03)

#Gráfico de la edad y capacidad pulmonar para entender bien el model
plot(Age[Smoke=='no'],LungCap[Smoke=='no'], col= 'blue',
     ylim = c(-2,15),xlim = c(0,20), 
     main = 'Capcidad pulmonar vs. Age , Smoke',
     xlab = 'Edad', ylab = 'Capacidad Pulmonar')

#Se agregan los fumadores
points(Age[Smoke=='no'],LungCap[Smoke=='yes'], 
       col='red', pch=18)

legend(0, 15, legend = c('No Fumadores', 'Fumadores'),
       col = c('blue', 'red'), pch = c(1,18))


#Se agrega al gráfico las lineas de regresion
#´primerp la de no fumadores.
abline(1.13750,b = 0.55168, col = 'blue',lwd = 4)

#para los fumadores
abline(a = (1.137 -0.64543), b = 0.55168, col = 'red',
       lwd = 3)

#Conclusiones
#si la edad incrementa por un año se espera que el promedio de la
#capacidad pulmonar incremente por 0.64543 unidades.
#2. Se observa que los fumadores tienen un efecto sobre la
#capacidad pulmonar, es decir para un fumador el promedio 
#de la capacidad pulmonar decrece 0.64543 para todas las edades
#3. El modelo asume que la edad tienen un efecto sobre el promedio
#de la capacidadpulmonar
#4. Los fumadores tienen un efecto sobre el promedio de la capacidad
#pulmonar, pero el efecto de la edad es independiente de fumar, y viceversa.
#
###############################################################
#                UNIVERSIDAD SERGIO ARBOLEDA                  #
#    PROGRAMA DE INGENIERÍA DE SISTEMAS Y TELECOMUNICACIONES  #
#     JULIETH GOMEZ MONTES (JULIETH.GOMEZ@CORREO.USA.EDU.CO)  #
#      ELKIN RODRIGUEZ (ELKIN.RODRIGUEZ2@CORREO.USA.EDU.CO)   #
#        CAMILO ZARATE (DAVIDC.ZARATE@CORREO.USA.EDU.CO)      #
#                       30/04/21                              #
#                   Regresion Lineal                          #
###############################################################

#Se cargan los datos desde el enlace
urlhousing <- "https://raw.githubusercontent.com/Efkrod/EDA/main/housing.data"
housing = read.table(file = urlhousing, header = T)

#Se requiere observar el dataframe
View(housing)


# Se requiere poner en el etorno todas las columnas como variables visibles
attach(housing)

# Se requiere obtener un resumen estadistico del dataset 
#(Por lo menos las variables numericas)
summary(housing)

# Se requiere el nombre de las columnas 
names(housing)

# Se requieren revidar el tipo de dato para cada variable (columna del dataset)
summary(CRIM)
summary(ZN)
summary(INDUS)
class(CRIM)
class(ZN)
class(INDUS)

# Las variables CRIM, ZN y INDUS son del tipo numérico

#Inspeccion basica de los datos
head(housing)

#Ver la estructura de los datos y columnas
str(housing)

# Se pudo observar que hay un dato de tipo categorico llamado 'CHAS' el cual 
# indica que si es 1 es porque en el terreno residencial se encuentra cerca un 
# rio, y si es 0 es porque no esta cerca a un rio.

# Se quiere observar el porcentil de la variable 'MEDV' la cual indica el precio 
# promedio, como estan distribuidos
quantile(MEDV)

# Se requiere saber el número de filas y columnas del dataset 'housing'
dim(housing)

# Se requiere saber si hay datos nulos en el dataset por columna
sapply(housing, function(x) sum(is.na(x)))

# Se requiere saber cuanto espacio ocupa en memoria el dataset
object.size(housing)


# Se requiere hacer una gráfica 
# Se gráfica las variables MEDV vs CRIM
plot(MEDV, CRIM, main = 'Gráfica MEDV vs CRIM',
     col = 'blue',
     cex = 0.5)

# Conclusiones:
# - El valor del indice de seguridad per capital es mayor en las viviendas 
#   ocupadas por sus dueños con un costo promedio menor a 15.000 dolares



##############################################
################MODELO 1######################
##############################################

#Se crea un modelo de regresión 
modelo1 = lm(MEDV~CRIM+CHAS)

#Se debe tener en cuenta que la variable dependiente (y)= 'MEDV' y la variable 
# independiente (X) es 'CRIM'
# Gráfica de CRIM vs MEDV
plot(CRIM[CHAS==0],MEDV[CHAS==0],
     main = 'Gráfica MEDV vs CRIM',
     col="red",
     xlab="Tasa de criminalidad per-capital(CRIM)",
     ylab = "Valor medio de viviendas ocupadas(MEDV)")


# Se agrega a la grafica anterior las viviendas que si se encuentran 
# en limites con rios.
points(CRIM[CHAS==1],MEDV[CHAS==1],
       col="blue")

#Le agregamos una leyenda al grafico
legend(40,50, legend = c('lim con rio','no lim. con rio'),
       col = c('blue','red'),
       pch=c(1,18))

# Se requiere observar un resumen estadistico del modelo1
summary(modelo1)

# Se requiere observar los coeficientes del modelo1
coef(modelo1)

# Se requiere observar los intervalos de convianza de los coeficientes del
# modelo.
confint(modelo1, level = 0.99)

#Linea para observar como se comportan los datos cuando
# limitan con río
abline(a=23.61403, b=-0.40598, col='blue',
       lwd=3)
#linea con pendiente negativa

#Linea para observar como se comportan los datos estimados cuando
#No limitan con río
abline(a=(23.61403+5.57772), b=-0.40598, col='orange',
       lwd=3)

# Se requiere calcular las tablas de analisis de varianza
anova(modelo1)
par(mfrow=c(2,2))
plot(modelo1)


##############################################
################MODELO 2######################
##############################################

#Se crea un modelo de regresión 
modelo2 = lm(MEDV~NOX+CHAS)


#Se debe tener en cuenta que la variable dependiente (y)= 'MEDV' y la variable 
# independiente (X) es 'CRIM'
# Gráfica de CRIM vs MEDV
plot(NOX[CHAS==0],MEDV[CHAS==0],
     main = 'Gráfica MEDV vs NOX',
     col="red",
     xlab="Consentración de oxidos nitricos(NOX)",
     ylab = "Valor medio de viviendas ocupadas(MEDV)")


# Se agrega a la grafica anterior las viviendas que si se encuentran 
# en limites con rios.
points(NOX[CHAS==1],MEDV[CHAS==1],
       col="blue")

#Le agregamos una leyenda al grafico
legend(0.7,50, legend = c('lim con rio','no lim. con rio'),
       col = c('blue','red'),
       pch=c(1,18))


# Se requiere observar un resumen estadistico del modelo1
summary(modelo2)

# Se requiere observar los coeficientes del modelo1
coef(modelo2)

# Se requiere observar los intervalos de convianza de los coeficientes del
# modelo.
confint(modelo2, level = 0.99)

#Linea para observar como se comportan los datos cuando
# limitan con río
abline(a=41.672, b=-35.480, col='blue',
       lwd=3)
#linea con pendiente negativa

#Linea para observar como se comportan los datos estimados cuando
#No limitan con río
abline(a=(41.672+7.822), b=-35.480, col='orange',
       lwd=3)

# Se requiere calcular las tablas de analisis de varianza
anova(modelo2)
par(mfrow=c(2,2))
plot(modelo2)

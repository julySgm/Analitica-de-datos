###############################################################
#                UNIVERSIDAD SERGIO ARBOLEDA                  #
#    PROGRAMA DE INGENIERÍA DE SISTEMAS Y TELECOMUNICACIONES  #
#                   JULIETH GOMEZ MONTES                      #
#             JULIETH.GOMEZ@CORREO.USA.EDU.CO                 #
#                       19/03/21                              #
#           TALLER: Fundamentos del lenguaje R                #
###############################################################

#Se cargan los datos desde el enlace
urlLung <- "https://raw.githubusercontent.com/julySgm/Analitica-de-datos/master/LungCapDataTabla1.csv"
LungCapData = read.table(file = urlLung, header = T, sep= ",")

#Se requiere observar el dataframe
View(LungCapData)

#Se requiere cargar los datos de las columnas del dataframe
#como variables independientes
attach(LungCapData)

#Si se desea limpiar la consola, se emplea la comanda 'CTR + L'

#Si se requiere observar las variables actuales cargadas en el entorno
ls()

#Se requiere un breve 'resumen' estadistico del dataframe
summary(LungCapData)

#Se requiere un t-tes: me permite comparar dos variables (LungCap ~ Smoke)
t.test(LungCap ~ Smoke)

#La variable 'Smoke': quiero saber que tipo de variable es
class(Smoke)

#La variable Age y la varible Gender: tipo de variables
class(Age)
class(Gender)

#Se refiere al promedio de la edad y el LungCap del dataframe
mean(Age)
mean(LungCap)

################################################
################################################
##################APPLY#########################
#Calcular el promedio de edad 'Age' de los fumadores 'Smoke' y no
#Fumadores por separado
tapply(X=Age, INDEX = Smoke, FUN = mean)

#Para el cálculo del promedio por separado anterior se pueden omitir
#los nombres de los argumentos//Forma eficiente de hacer una agrupación
tapply(Age, Smoke, mean)

#Se puede hacer lo anterior sin 'tapply' sin embargo el 'tapply' es 
#más eficiente para volumenes de datos significativos.
mean(Age[Smoke=='yes'])
mean(Age[Smoke=='no'])

#El 'tapply' tambien se puede aplicar al resumen
tapply(X=Age, INDEX = Smoke, FUN = summary)

#Con el 'tapply': se requiere saber los valores de los "percentiles"
#para el 20% y el 80%
tapply(Age, Smoke, quantile, probs = c(0.2,0.8))

#Con tapply se puede hacer un subconjunto con más variables/matrices/array
#Se requiere calcular el promedio de edad 'Age' por sumador 'Smoke' por genero 'Gender'
tapply(X=Age, INDEX = list(Smoke,Gender), FUN = mean)#en relacion con la edad

#Se requiere hacer lo anterior sin el 'tapply' aunque sepa que es menos eficiente
mean(Age[Smoke=='yes' & Gender=="female"])
mean(Age[Smoke=='yes' & Gender=="male"])
mean(Age[Smoke=='no' & Gender=="female"])
mean(Age[Smoke=='no' & Gender=="male"])

#La función 'by' es similar a la función 'tapply',
#el resultado es un array <matriz>
tapply(Age, Smoke, mean)
by(Age, Smoke, mean)

tapply(X=Age, INDEX = list(Smoke,Gender), FUN = mean)
by(Age, list(Smoke, Gender), mean)

################################################
################################################
##################VISUALIZACIÓN#################

#Si se requiere saber la ayuda de una función barplot
?barplot
#Es una gráfica de barras que presenta una muestra visual 
#de la frecuencia para cada variable: categórica, a la frecuencia
#relativa (%) para cada categoria

#Se requiere crear una gráfica que represente el porcentaje de la 
#variable 'Gender'
auxVar <- table(Gender)
porcentaje <- auxVar/length(Gender)

#Se visualiza los resultados en una grafica de barras
barplot(porcentaje)

#Se requiere agregar información a la gráfica
barplot(porcentaje, main = "Porcentaje por 'Gender' del datraframe LugCapData", 
        xlab = "Gender", ylab = "Porcentaje(x)",las = 2)

#Se requiere al gráfico cambiar las etiquetas
barplot(porcentaje, main = "Porcentaje por 'Gender' del datraframe LugCapData", 
        xlab = "Genero", 
        ylab = "Porcentaje(x)",
        las = 2,
        names.arg = c("Femenino", "Maculino"))

#Se requiere al gráfico presentarse en horizontal
barplot(porcentaje, main = "Porcentaje por 'Gender' del datraframe LugCapData", 
        xlab = "Genero", ylab = "Porcentaje(x)",
        names.arg = c("Femenino", "Maculino"),
        horiz = T)

#Se requiere examinar la relacion entre el 'Gender' y 'Smoke'
relaGen_Sm <- table(Smoke, Gender)
relaGen_Sm

#Gráfica de la relacion
barplot(relaGen_Sm)

#se quiere mejorar la gráfica anterior
barplot(relaGen_Sm, main = "Relación 'Gender y 'Smoke'",
        beside = T, legend.text = c("No fumador", "Fumador"),
        xlab = "Genero",ylab = "Cantidad",
        col = c("blue", "red"),
        names.arg = c("Femenino", "Masculino"))


#1. Se requiere pintar la relacion entre Fumadores/Caesarean
#2. Se requiere pintar una gráfica presentando el porcentaje
#   de cesarea
#3. Se requiere saber la relacion entre el cancer de pulmon y la altura





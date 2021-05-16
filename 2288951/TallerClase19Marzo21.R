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

##Se requiere un t-tes: me permite comparar dos variables (LungCap ~ Smoke)
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
        xlab = "Genero", 
        ylab = "Porcentaje(x)",
        names.arg = c("Femenino", "Maculino"),
        horiz = T)

#Se requiere examinar la relacion entre el 'Gender' y 'Smoke'
relaGen_Sm <- table(Smoke, Gender)
relaGen_Sm

#Gráfica de la relacion
barplot(relaGen_Sm)

#se quiere mejorar la gráfica anterior
barplot(relaGen_Sm, main = "Relación 'Gender y 'Smoke'",
        beside = T, 
        legend.text = c("No fumador", "Fumador"),
        xlab = "Genero",ylab = "Cantidad",
        col = c("blue", "red"),
        names.arg = c("Femenino", "Masculino"))

#########################TAREA############################
#1. Se requiere pintar la relacion entre Fumadores/Caesarean
#2. Se requiere pintar una gráfica presentando el porcentaje
#   de cesarea
#3. Se requiere saber la relacion entre el cancer de pulmon y la altura


#1. Se requiere pintar la relación entre Fumadores/Caesarean
relaFum_Cae <- table(Smoke, Caesarean)
relaFum_Cae

##Gráfica de la relación
barplot(relaFum_Cae,main = "Relación 'Smoke' y 'Caesarean'",
        beside = T,
        xlab = "Caesarean",ylab = "Cantidad",
        col = c("blue", "red"))

#2. Porcentaje de 'Caesarean'
auxVarC <- table(Caesarean)
porcentajeC <- auxVarC/length(Caesarean)

#Se visualiza los resultados en una grafica de barras
barplot(porcentajeC, main = "Porcentaje por 'Caesarean' del datraframe LugCapData", 
        xlab = "Gender", ylab = "Porcentaje(x)",las = 2)

#3. Se requiere saber la relacion entre el cancer de pulmon y la altura
relaLun_Heig <- table(LungCap, Height)
relaLun_Heig

barplot(relaLun_Heig, main= "Relación 'LungCap' y 'Height'",
        beside = T,
        col = c("blue", "red"),
        xlab = "Altura",
        ylab = "Cantidad")

##########CLASE 23/03/21##################

pie(auxVar)

#Se mejora la gráfica de pie
pie(auxVar, main = "Capacidad por genero")

#Se mejora la gráfica de pie
pie(auxVar,col = c("purple", "violetred1"),
    main = "Capacidad por genero")
box()

#Diagrama de caja o boxplot
#tambien conocido como diagrama de bigote o vela japonesa
#metodo entandarizado para representar graficamente
#una serie de datos numericos a traves de sus cuartiles

#visualizar un boxplot de la variable LungCap
boxplot(LungCap)

#Se requiere la visualizacion de boxplot por cuartiles
#de la variable LungCap
quantile(LungCap, probs = c(0,0.25,0.50,0.75,1))

#Se mejora el gráfico de caja
boxplot(LungCap, main="Boxplot de capacidad pulmonar",
        ylab="Capacidad pulmonar",
        ylim=c(0,15),las = 1)

#Boxplot para dos variables
boxplot(LungCap ~ Gender, main="Boxplot de capacidad pulmonar versus Genero",
        ylab="Capacidad pulmonar",
        ylim=c(0,15),las = 1)

#hacer un boxplot de fumadores y capacidad pulmonar
boxplot(LungCap ~ Smoke, main="Boxplot de capacidad pulmonar versus fumadores",
        ylab="Capacidad pulmonar",
        ylim=c(0,15),las = 1)

#Concideraciones del dataframe:
#1. El efecto de fumar se confunde con el efecto de la edad
#2. Son niños entre 3 y 19 años
#3. En promedio los fumadores son mayores respecto a los 
#4. Los niños mayores tendra mayor capacidad pulmonar

#Examinar la relacion entre fumadores y capacidad pulmonar
#Smoke y LugCap, para los mayores de 16 años
boxplot(LungCap[Age>=16 ~ Smoke[Age>=16]], main="Boxplot de capacidad pulmonar versus fumadores",
        ylab="Capacidad pulmonar",
        ylim=c(0,15),las = 1)


#boxplot estratificado: util para examinar la relacion
#entre una variable categorica y una numerica, dentro de 
#estratos o grupos definidos por una tercera variable categorica
#Ejemplo: relacion entre fumadores y capacidad pulmonar.
#dentro de grupos de edad.
grupoEdad <- cut(Age, breaks = c(0,5,10,15,20),
                 labels = c("≤5","6-10","11-15","16+"))

#Se revisa los primeros 5 elementos del grupoEdad
levels(grupoEdad)
grupoEdad[1:5]


#Se hace la gráfica de grupoEdad por fumadores y capacidad
#pulmonar somoke y LungCap
boxplot(LungCap ~ Smoke*grupoEdad,
        main ="Boxplot de capacidad pulmonar versus fumadores, por grupoEdad")
        
        
#Se requiere mejorar el gráfico
boxplot(LungCap ~ Smoke*grupoEdad,
        main="Boxplot de capacidad pulmonar versus fumadores por grupo",
        ylab="Capacidad pulmonar", ylim=c(0,15),las = 2,
        col = c("blue","red"), axes= F,
        xlab = "Estratos de edad")
        
#Mejoramos la presentacion
box()
axis(1,c(1.5 ,3.5 ,5.5,7.5),
     labels = c("≤5","6-10","11-15","16+"))
axis(2,at=seq(0,20,2),seq(0, 20, 2),las=1) 
legend(x = 0.2, y=19,
       legend = c("No fumadores","Fumadores", col = c("blue","red"),
                  pch = 15, cex = 0.8)
               
#De la grafica se puede decir que para los estratos menores que 5 no hay fumadores
####################################################################################
               
#Diagramas de histogramas: son representaciones graficas de una variable en forma de barras
#donde la superficie de cada barra es proporcional a la frecuencia de los valores representados.
#Histograma de capacidad pulmonar 'LungCap'
#->FRECUENCIA
hist(LungCap)
               
#Mejora con densidad
hist(LungCap, freq = F)
               
#Mejora con probabilidad
hist(LungCap, prob = T, ylim = c(0,0.2), breaks = 7)
hist(LungCap, prob = T, ylim = c(0,0.2), breaks = 14)
               
#vector
hist(LungCap, prob = T, ylim = c(0,0.2), breaks = c(0,2,4,6,8,10,12,14,16))
               
hist(LungCap, prob = T,
     ylim = c(0,0.2),
     breaks = seq(0,16,2), 
     main = "Histograma de capacidad pulmonar", 
     xlab = "Capacidad Pulmonar")
               
               
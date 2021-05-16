##########################################
#       UNIVERSIDAD SERGIO ARBOLEDA      #
#    PROGRAMA DE INGENIERÍA DE SISTEMAS  #
#          JULIETH GOMEZ MONTES          #
#     JULIETH.GOMEZ@CORREO.USA.EDU.CO    #
#                09/03/21                #
#          INTRODUCCIÓN A R              #
##########################################

#Cargamos el DataSet en la variable admisión

admision <- read.csv("https://raw.githubusercontent.com/julySgm/Analitica-de-datos/master/Admission_Predict.csv")

#Cuando se quiere desplegar el dataSet 
View(admision)

#Los vectores se crean con la inicial de 'c' y la tupla de elementos que compone
vectorPrueba <- c(1,2,3,4,5)

vectorPrueba2 <- c('Sistemas', 'Electronica', 'Civil','Ambiental')


################################################################

#Estadisticas del DataSet
##Se quiere sacar el promedio de la columna 'SOP'de admission
mean(admision$SOP)

###############################################################

#Se requiere crear un dataset
salarioAnual <- c(72000,76000,81500,120000,76000,92000,79000)
familia.grupo <- c(3,4,5,2,3,6,2)
tipo.auto <- c('lujo', 'compacto', 'mediano','compacto', 'lujo', 'mediano','compacto')

#Se crea el dataFrame
dataFamilia <- data.frame(salarioAnual,familia.grupo,tipo.auto)

#Ver dataFamilia
View(dataFamilia)

#Se quiere imprimir el nombre de las columnas de los dataFrame
names(admision)
names(dataFamilia)

#Se quiere presentar un resumen estadístico del DataFrame
summary(admision)
summary(dataFamilia)

#Se desea sacar la desviacion estandar de la columna 'SOP' del dataFrame admision
sd(admision$SOP)

#Se requieren ver porciones o vistas del DataFrame 
#desde la fila 1 hasta la fila 3 y desde la columna 3 hasta la 6
admision[1:3,3:6]
#Tambien se le puede enviar un correo
admision[1:3,c(3,4,5,6)]

admision[1:3,c('TOEFL.Score', 'University.Rating','SOP','LOR')]

#Se requiere presentar los 'TOEFL.Score' mayores que 112 y 'Chance.of.Admit' superior al 83%
admision[admision['TOEFL.Score'] > 112 & admision['Chance.of.Admit'] > 0.83]

admision[admision$TOEFL.Score > 112 & admision$Chance.of.Admit > 0.95,]


#Se quiere presentatr los que tengan un 'Chance.of.Admit' en 95% 
#o los que tengan el 'TOEFL.Score' superior a 117
admision[admision$TOEFL.Score > 117 | admision$Chance.of.Admit > 0.95,]


#Se quiere presentatr los que tengan un 'Chance.of.Admit' igual 95% 
#o los que tengan el 'TOEFL.Score' igual a 117
admision[admision$TOEFL.Score == 117 | admision$Chance.of.Admit == 0.95,]

#Se quiere presentatr los que tengan un 'Chance.of.Admit' igual 95% 
#y los que tengan el 'TOEFL.Score' igual a 117
admision[admision$TOEFL.Score == 117 & admision$Chance.of.Admit == 0.95,]










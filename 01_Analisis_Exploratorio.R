#Curso de Miner??a de Datos Valsiq 2016

#Nombre el Archivo:                  Regresi??n Log??stica en R
#Fecha de Creaci??n:                  08-02-2016
#Autores:                            Neftali Valdez (nvaldez@valsiq.com) & Daniel Legorreta Anguiano (d.legorreta.anguiano@gmail.com)
#Proposio del C??digo:                Se construyen  modelos de Regresi??n Log??stico, se hace una exploraci??n de las variables y se hace 
#                                    y se revisan los pron??sticos realizados con tablas contingencia y las curvas de ROC.Se dan m??s detalles
#                                    sobre pruebas para modelos de clasificaci??n en la  sexta sesi??n del curso.
#                                    
#Comentario:                         Este c??digo forma parte del material didactico para el curso de "Miner??a de Datos"
#Datos Usados:                       Los datos corresponden a informaci??n de usuarios de Bicicletas, cuenta con datos de las personas y datos del uso de la bicicleta
#                                    Nombre de archivo: Bicicletas.xlsx
#Librer??as Usadas:                   gdata,gmodels,lattice, 

#####################################################################################
#Carga de librer??as y datos
#install.packages("ISLR")
#install.packages("gdata")
#install.packages("gmodels")
#install.packages("lattice")
library(ISLR) # Librer??a ISLR
library(gdata) # Librer??a para manipulci??n o carga de datos.
library(gmodels) # Librer??a con herramientas para estimaci??n de modelos
library(lattice) # Librer??a gr??fica para an??lisis multivaluados

#Se revisa el directorio donde se encuentra ejecutando R
getwd()

#Se cambia de directorio
setwd("/Users/neftalivaldez/Data")
adq = read.xls("Adquisicion.xlsx") 

#Se revisan el tipo de variables y dimensiones de la muestra de datos
head(adq)
dim(adq)
str(adq)

#Se revisan un resumen de estad??sticas generales
summary(adq) 

#Todas las variables categ??ricas dan un resumen de la cantidad de registros por clase 
#Otro modo de ver el resumen de las estad??sticas b??sica de los datos
lapply(adq,summary)

##### Tablas de Contingencia ######
#Ejemplos
CrossTable(adq$Ha_comprado,adq$Region)

CrossTable(adq$Nivel_de_estudios, adq$Ha_comprado)

CrossTable(adq$Ocupacion, adq$Ha_comprado)

CrossTable(adq$Region, adq$Ha_comprado)

########## Exploraci??n Gr??fica ###########################
# Uso de gr??ficas base de R
# Se revisa el comportamiento de los Boxplot de varias variables contra 
# la variable que se modela "Ha_comprado".

par(mfrow=c(2,2))
# Edad
boxplot(Edad~Ha_comprado,data=adq, main="Box Plot", 
        xlab="Ha comprado", ylab="Edad", col="darkgreen")

## Numero_de_Hijos
boxplot(Numero_de_Hijos~Ha_comprado,data=adq, main="Box Plot", 
        xlab="Ha comprado", ylab="Numero_de_Hijos", col="darkred")

## Numero_de_Autos
boxplot(Numero_de_Autos~Ha_comprado,data=adq, main="Box Plot", 
        xlab="Ha comprado", ylab="Numero_de_Autos", col="red")

# Distancia que viaja
boxplot(Distancia~Ha_comprado,data=adq, main="Box Plot", 
        xlab="Ha comprado", ylab="Distancia_que_viaja", col="darkblue")

# Se analia el comportamieto de las variables Edad, Distancia, Region, 
# Ocupaci??n con respecto a la variable objetivo

#Uso de lattice
# Se revisan comportamientos de varias varibles considerando el caso para los 
# que han comprado y los que no.
# Al final se compara como se comportan los ingresos con respecto al genero 
# y el adquirir o no

stripplot(Distancia~Edad|Genero,data=adq,col="darkblue", main="Comportamiento de las distancias por Edad y Genero")

bwplot(Distancia~Ocupacion|Ha_comprado, data=adq, main="Comportamiento de la Distancia con respecto a las Ocupaciones", col="darkred")

bwplot(Distancia~as.factor(Tiene_Casa)|Ha_comprado, data=adq, main="Comportamiento de la Distancia con respecto a al tener casa", col="darkred")

bwplot(Distancia~as.factor(Tiene_Casa)|Ha_comprado+Genero, data=adq, main="Comportamiento de la Distancia con respecto a tener Casa \n separado por Genero y comprar bicicletas", col="darkred")

bwplot(Distancia~Estado_Civil|Ha_comprado, data=adq, main="Comportamiento de la Distancia con respecto a las Ocupaciones", col="darkred")

bwplot(Ingreso_Anual~Numero_de_Hijos|Genero+Ha_comprado,data=adq, main="Box Plot", col="orange")

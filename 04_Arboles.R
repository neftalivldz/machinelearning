
#Curso de Miner??a de Datos Valsiq 2016

#Nombre el Archivo:                  ??rboles de Decisi??n en R
#Fecha de Creaci??n:                  09-02-2016
#Autores:                            Neftali Valdez (nvaldez@valsiq.com) 
#Proposio del C??digo:                Se construyen modelos de ??rboles de Decision, para esto de muestra 4 ejemplos
#                                    se comprar el resultado con respecto a lo que se puede obtener desde los modelos                                    
#                                    de ??rboles de decisi??n cl??sicos y gr??ficas asociadas.
#                                    
#Comentario:                         Este c??digo forma parte del material didactico para el curso de "Miner??a de Datos"
#Datos Usados:                       Son cuatro muestras de datos: 
#                                    1.- Corresponden el peso y tama??o del S??palo y el P??talo de 3 diferentes Iris.
#                                    2.- Corresponden a una muestra de 4601 email clasificados entre spam y no-spam
#                                    3.- Corresponden a mediciones de la calidad del Aire en NY desde Mayo a Septiembre de 1973
#Librer??as Usadas:                   ggplot,lattice,rpart,rattle,rpart.plot,RColorBrewer,ISLR,kernlab, reshape2,tree, randomForest

#################################################################################################################

#Cargamos las librer??as 
#install.packages("ggplot2")
#install.packages("lattice")
#install.packages("rpart")
#install.packages("rattle")
#install.packages("part.plot")
#install.packages("RColorBrewer")
#install.packages("ISLR")
#install.packages("kernlab")
#install.packages("reshape2")
#install.packages("tree")



library(ggplot2)
library(lattice)
library(rpart)
#library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(ISLR)
library(kernlab)
library(reshape2)
library(tree)

#############################################################################3


#Se revisa el directorio donde se encuentra ejecutando R
getwd()


#Se cambia de directorio
setwd("/Users/neftalivaldez/Data")
ds_train = read.csv("ds_train.csv") 
ds_test = read.csv("ds_test.csv")
head(ds_train)
head(ds_test)



fit_arbol <-rpart(Ha_comprado~Numero_de_Autos+Tiene_Casa+ Edad 
          + Ingreso_Anual,data=ds_train,method = "class")
fit_arbol
summary(fit_arbol)

prediction <- predict(fit_arbol,Iris_test,type="class")

#Gr??fica para el ??rbol partiendo de los datos
fancyRpartPlot(fit_arbol, sub="Ha Comprado - ??rbol de Decisi??n")


table(Predicciones=prediction,Iris_test$Species)

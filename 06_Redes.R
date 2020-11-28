#Curso de Miner??a de Datos Valsiq 2016

#Nombre el Archivo:                  Evaluaci??n del Modelo
#Fecha de Creaci??n:                  7-08-2016
#Autores:                            Neftali Valdez (nvaldez@valsiq.com) 
#Proposio del C??digo:               Se utiliza la librer??a ROCR.
#                                    
#Comentario:                         Este c??digo forma parte del material didactico para el curso de "Miner??a de Datos"
#Datos Usados:                       Los datos corresponden a informaci??n de usuarios de Bicicletas, cuenta con datos de las personas y datos del uso de la bicicleta
#                                    Nombre de archivo: Adquisicion.xlsx
#Librer??as Usadas:                  ROCR

#####################################################################################
#Carga de librer??as y datos
#install.packages("gdata")
#install.packages("ROCR")
#install.packages("neuralnet")
#install.packages("nnet")

library(gdata) # Librer??a para manipulci??n o carga de datos.
library(neuralnet)
library(nnet)
#Se revisa el directorio donde se encuentra ejecutando R
getwd()


#Se cambia de directorio
setwd("/Users/neftalivaldez/Data")
ds_train = read.csv("ds_train.csv") 
ds_test = read.csv("ds_test.csv")
head(ds_train)
head(ds_test)

ds_train_nn = ds_train
ds_test_nn = ds_test
summary(ds_train)
ds_train_nn$Numero_de_Autos = as.factor(ds_train_nn$Numero_de_Autos)
ds_test_nn$Numero_de_Autos = as.factor(ds_test_nn$Numero_de_Autos)
ds_train_nn$Tiene_Casa = as.factor(ds_train_nn$Tiene_Casa)
ds_test_nn$Tiene_Casa = as.factor(ds_test_nn$Tiene_Casa)
#ds_train_nn$Edad = as.factor(ds_train_nn$Edad)
#ds_test_nn$Edad = as.factor(ds_test_nn$Edad)
ds_train_nn$Distancia  = as.factor(ds_train_nn$Distancia)
ds_test_nn$Distancia  = as.factor(ds_test_nn$Distancia)
ds_train_nn$Ingreso_Anual = as.factor(ds_train_nn$Ingreso_Anual)
ds_test_nn$Ingreso_Anual = as.factor(ds_test_nn$Ingreso_Anual)

head(ds_train_nn)
summary(ds_train_nn)
is.data.frame(ds_train_nn)

ds_mod_nn = model.matrix( ~ Ha_comprado + Numero_de_Autos + Distancia, data = ds_train_nn)
head(ds_mod_nn)

#Se entrena el modelo con redes neuronales
nn <- neuralnet(Ha_comprado ~ Numero_de_Autos1 + 
  Numero_de_Autos2 + Numero_de_Autos3 + Numero_de_Autos4,
  #+ Distancia1.5  + Distancia3.5 + Distancia7.5 + Distancia12.5,
  data=ds_mod_nn, hidden=1, act.fct = "tanh")

# Matriz de pesos
nn$result.matrix

#Gr??fico de Capas
plot(nn)


## Predicci??n

ds_test_nnm = model.matrix( ~ Numero_de_Autos , data = ds_test_nn)
head(ds_test_nnm)

summary(ds_test_nnm)
DataPred= compute(nn,ds_test_nnm)

DataPred= compute(nn,cbind(ds_test_nnm$Numero_de_Autos1, 
 ds_test_nnm$Numero_de_Autos2, ds_test$Edad, ds_test$Distancia, 
ds_test$Ingreso_Anual))

DataPred= compute(nn,cbind(ds_test_nnm$Numero_de_Autos1, 
                           ds_test$Tiene_Casa, ds_test$Edad, ds_test$Distancia, 
                           ds_test$Ingreso_Anual))

summary(DataPred$net.result)

ds_test$Est_nn = DataPred$net.result
head(ds_test)

write.csv(ds_train,file="ds_train.csv")
write.csv(ds_test,file="ds_test.csv")
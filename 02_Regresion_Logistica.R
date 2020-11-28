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
#                                    Nombre de archivo: Adquisicion.xlsx
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

############# Regresi??n Log??stica ###############################
#Se construyen los conjuntos de datos para evaluar los modelos

nrow(adq)
#18484

#Se toman el 70% para entrenar el modelo y el 30% para probar la calidad
#Para entrenar el modelo se toman 12939 datos

x=1:nrow(adq)

set.seed(10)
train=sample(x,floor(nrow(adq)*.7))
train
length(train)

ds_train = adq[train,]
ds_test = adq[-train,]


logit1 <- glm( Ha_comprado ~ Numero_de_Autos + Edad, 
               data=ds_train, family = "binomial")

logit2 <- glm( Ha_comprado ~ Numero_de_Autos  + Tiene_Casa + Edad + Distancia 
               + Ingreso_Anual + Region,data=ds_train, family = "binomial")

logit3 <- glm( Ha_comprado ~ Distancia_que_viaja + Tiene_Casa + Edad 
               + Ingreso_Anual, data=ds_train,family = "binomial")

#Resumen de la estimaci??n de uno de los modelos
summary(logit1)

#Resumen de los 3 modelos
lapply(list(logit1,logit2,logit3),summary)

#Se puede elegir el modelo logit2 considerando el nivel de estad??stico AIC
lapply(list(logit1,logit2,logit3),AIC)
# AIC(logit2) es de 16633.42, este es el menor valor de AIC entre los 
# 3 modelos propuestos


# Se compran el modelo seleccionado; logit2, se hace una prueba ANOVA para validar 
# si las variables cumplen con el nivel de significancia adecuada
anova(logit2,test="Chisq")

#Se reestima el modelo logit2 pero quitando la varialbe "Tiene_Casa"
logit2.1 <-update(logit2,.~.-Tiene_Casa)
anova(logit2.1,test="Chisq")
#Todas las variables pasan el nivel de significancia adecuada.

#Se realiza la predicci??n del modelo y se integra al data-set de prueba
Est_logit2 = predict(logit2, newdata=ds_test, type="response")

summary(Est_logit2)
head(Est_logit2)
head(ds_test)

ds_test$Est_logit2 = Est_logit2

#Se realiza la predicci??n del segundo modelo y se integra al data-set de prueba
Est_logit1 = predict(logit1, newdata=ds_test, type="response")
ds_test$Est_logit1= Est_logit1

head(ds_test)

write.csv(ds_train,file="ds_train.csv")
write.csv(ds_test,file="ds_test.csv")
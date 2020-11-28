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

library(gdata) # Librer??a para manipulci??n o carga de datos.
library(ROCR)
#Se revisa el directorio donde se encuentra ejecutando R
getwd()

#Se cambia de directorio
setwd("/Users/neftalivaldez/Data")
ds_train = read.csv("ds_train.csv") 
ds_test = read.csv("ds_test.csv")
head(ds_train)
head(ds_test)


#Se crean el objeto de predicci??n
pred_logit2 <- prediction(ds_test$Est_logit2, ds_test$Ha_comprado)
summary(pred_logit2)

#Graficar Simple ROC
perf <- performance(pred_logit2,"tpr","fpr")
plot(perf, colorize = T)
abline(a=0, b= 1)

#Graficar Accuracy en los distintos puntos de corte
perf <- performance(pred_logit2, "acc")
plot(perf, avg= "vertical", 
     spread.estimate="boxplot", 
     show.spread.at= seq(0.1, 0.9, by=0.1))

#Graficar Precisi??n en los distintos puntos de corte
perf <- performance(pred_logit2, "prec")
plot(perf, avg= "vertical", 
     show.spread.at= seq(0.1, 0.9, by=0.1))

# Acuracy vs Lift
perf <- performance(pred_logit2,"acc","lift")
plot(perf, colorize=T)
plot(perf, colorize=T,
     print.cutoffs.at=seq(0,1,by=0.1),
     add=T, text.adj=c(1.2, 1.2),
     avg="threshold", lwd=3)



#Comparar con Segundo Objeto
pred_logit1 <- prediction(ds_test$Est_logit1, ds_test$Ha_comprado)

#Graficar Simple ROC
perf <- performance(pred_logit2,"tpr","fpr")
perf1 <- performance(pred_logit1,"tpr","fpr")

plot(perf, col = "blue")
par(new=TRUE)
plot(perf1, col = "red")
abline(a=0, b= 1)

#Graficar Comparaci??n de lift
perf <- performance(pred_logit2,"lift","rpp")
perf1 <- performance(pred_logit1,"lift","rpp")

plot(perf, col = "blue", xlim = c(0,0.2))
par(new=TRUE)
plot(perf1, col = "red", xlim = c(0,0.2))

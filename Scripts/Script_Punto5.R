#PUNTO 2-------------------------------------------------------------------------
setwd("C:/Users/Camila/OneDrive - Universidad de los Andes/1.Big Data")
install.packages("ggplot2")

require(pacman)
p_load(rio)
library(faraway)#de aqui saco los datos

library(data.table)
library(skimr)
library(DataExplorer)
library(scales)
library(correlate)
library(glmnet)
library(pls)
library(dplyr)
library(caret)
library(ggplot2)
p_load(caret)
install.packages("caret")
p_load(rio)
p_load(dplyr)
p_load(tidyverse)
p_load(stargazer)
library(ggplot)
library(ggplot2)

#Despues del anÃ¡lisis de missings, creo una sin missings
#Miro en que formato estÃ¡ cada variable
geih_des<- subset(geih_fil, select=c(ingtot, age,sex, clase, college, informal, cotPension, cuentaPropia, depto, dsi, ocu, inac, estrato1, maxEducLevel, relab, totalHoursWorked))

#Vuelvo las categoricas categÃ³ricas, ya que todas son int
df_sinmis<-na.omit(geih_des)
#Conviierto variables a sus verdaderas clases

df_sinmis$age<- as.numeric(df_sinmis$age)
df_sinmis$totalHoursWorked<-as.numeric(df_sinmis$totalHoursWorked)
df_sinmis$sex<-as.factor(df_sinmis$sex)
df_sinmis$clase<-as.factor(df_sinmis$clase)
df_sinmis$college<-as.factor(df_sinmis$college)
df_sinmis$cotPension<-as.factor(df_sinmis$cotPension)
df_sinmis$depto<-as.factor(df_sinmis$depto)
df_sinmis$dsi<-as.factor(df_sinmis$dsi)
df_sinmis$ocu<-as.factor(df_sinmis$ocu)
df_sinmis$inac<-as.factor(df_sinmis$inac)
df_sinmis$estrato1<-as.factor(df_sinmis$estrato1)
df_sinmis$maxEducLevel<-as.factor(df_sinmis$maxEducLevel)
df_sinmis$cuentaPropia<-as.factor(df_sinmis$cuentaPropia)
df_sinmis$relab<-as.factor(df_sinmis$relab)
 str(df_sinmis)
#Creo las variables que necesito
df_sinmis$age2<- df_sinmis$age^2
df_sinmis$loging<- log(df_sinmis$ingtot)

"varcat<- c("sex", "clase", "college", "cotPension", "depto", "dsi", "ocu", "inac", "estrato1", "maxEducLevel", "relab")
df_sinmis %>%
  mutate_at(.var=vars(varcat),.fun=factor)"

#MMISSING_______________________________________________________________________
  
missings<-as.data.frame(map(geih_des, ~sum(is.na(.))))

miss<-transpose(missings)
miss$prop <- miss$V1/nrow(geih_des)*100

write.csv2(miss, "miss.csv")
#Relación con las variable dependiente


##Por sexo

sex<-boxplot(loging~sex, data=df_sinmis, main="Log(Ingreso) por sexo",  xlab="Hombre=1, Mujer=0", ylab="Log(Ingreso)")
##Por cuenta propia
cuntap<-boxplot(loging~cuentaPropia, data=df_sinmis, main="Log(Ingreso) si trabajadores cuenta propia",  xlab="Trabajador cuenta propia=1", ylab="Log(Ingreso)")
##Hnivel educ
edu<-boxplot(loging~maxEducLevel, data=df_sinmis, main="Log(Ingresos)-Nivel de educación máximo",  xlab="Nivel educ", ylab="Log(Ingreso)")
#Estrato
est<-boxplot(loging~estrato1, data=df_sinmis , main="Log(Ingresos)- Estrato",  xlab="Estrato", ylab="Log(Ingreso)")

sum(is)
#Punto 5-----------------------------------------------------------------------------------

#Test-train----------------------------------------------------------------------------------------------------


set.seed(10101)

id_train <- sample(1:nrow(df_sinmis), size=0.7*nrow(df_sinmis), replace=F)
train <- df_sinmis[id_train,]
test<-df_sinmis[-id_train,]

#Modelo con constante
m1<-lm(ingtot ~ 1, data=train )
m2<-lm(ingtot ~ age+age2, data=train)

#Elimino los ingresos 0 ya que la var dependiente será transformada
train2<-filter(train, ingtot>0)
test2<-filter(test, ingtot>0)
m3<-lm(log(ingtot) ~ sex, data=train2)
m4<-lm(log(ingtot)  ~sex + age +totalHoursWorked + estrato1 + cuentaPropia + maxEducLevel, data=train2)
m5<-lm(log(ingtot)  ~sex + age + age2 + totalHoursWorked + estrato1 + cuentaPropia + maxEducLevel, data=train2)
m6<-lm(log(ingtot)  ~sex + age + age2 + totalHoursWorked + estrato1 + cuentaPropia + cuentaPropia*sex + maxEducLevel, data=train2)
m7<-lm(log(ingtot)  ~sex + age + age2 + totalHoursWorked + estrato1 + cuentaPropia + cuentaPropia*sex +  maxEducLevel+ maxEducLevel*sex, data=train2)
m8<-lm(log(ingtot)  ~sex + age + age2 + totalHoursWorked + estrato1 + estrato1*sex + cuentaPropia + cuentaPropia*sex +  maxEducLevel+ maxEducLevel*sex, data=train2)
m9<-lm(log(ingtot)  ~sex + age + age2 + totalHoursWorked + totalHoursWorked^2 + estrato1 + estrato1*sex + cuentaPropia + cuentaPropia*sex +  maxEducLevel+ maxEducLevel*sex, data=train2)
m10<-lm(log(ingtot) ~sex + age + age2 + totalHoursWorked + totalHoursWorked^2 + estrato1 + estrato1*sex + estrato1*cuentaPropia + cuentaPropia + cuentaPropia*sex +  maxEducLevel+ maxEducLevel*sex, data=train2)
m11<-lm(log(ingtot) ~sex + age + age2 + totalHoursWorked + totalHoursWorked^2 + estrato1 + estrato1*sex + estrato1*cuentaPropia + cuentaPropia + cuentaPropia*sex +  maxEducLevel+ maxEducLevel*sex + maxEducLevel*estrato1, data=train2)

#Estimo los valores de y mis modelos en la muestra de  -> Error dentro de muestra
train$Modelo1<-predict(m1,newdat= train)
train$Modelo2<-predict(m2,newdat= train)
train$Modelo3<-predict(m3,newdat= train)
train$Modelo4<-predict(m4,newdat= train)
train$Modelo5<-predict(m5,newdat= train)
train$Modelo6<-predict(m6,newdat= train)
train$Modelo7<-predict(m7,newdat= train)
train$Modelo8<-predict(m8,newdat= train)
train$Modelo9<-predict(m9,newdat= train)
train$Modelo10<-predict(m10,newdat= train)
train$Modelo11<-predict(m11,newdat= train)

#en la muestra de test -> Error fuera de muestra

test$Modelo1<-predict(m1,newdat= test)
test$Modelo2<-predict(m2,newdat= test)
test$Modelo3<-predict(m3,newdat= test)
test$Modelo4<-predict(m4,newdat= test)
test$Modelo5<-predict(m5,newdat= test)
test$Modelo6<-predict(m6,newdat= test)
test$Modelo7<-predict(m7,newdat= test)
test$Modelo8<-predict(m8,newdat= test)
test$Modelo9<-predict(m9,newdat= test)
test$Modelo10<-predict(m10,newdat= test)
test$Modelo11<-predict(m11,newdat= test)

#Error de predicción promedio fuera de muestra 

mse1<-with(test, mean((ingtot-Modelo1)^2))
mse2<-with(test, mean((ingtot-Modelo2)^2))
mse3<-with(test, mean((ingtot-exp(Modelo3))^2))
mse4<-with(test, mean((ingtot-exp(Modelo4))^2))
mse5<-with(test, mean((ingtot-exp(Modelo5))^2))
mse6<-with(test, mean((ingtot-exp(Modelo6))^2))
mse7<-with(test, mean((ingtot-exp(Modelo7))^2))
mse8<-with(test, mean((ingtot-exp(Modelo8))^2))
mse9<-with(test, mean((ingtot-exp(Modelo9))^2))
mse10<-with(test, mean((ingtot-exp(Modelo10))^2))
mse11<-with(test, mean((ingtot-exp(Modelo11))^2))

#Error de predicciÃ³n promedio dentro de muestra 

mse1d<-with(train, mean((ingtot-Modelo1)^2))
mse2d<-with(train, mean((ingtot-Modelo2)^2))
mse3d<-with(train, mean((ingtot-exp(Modelo3))^2))
mse4d<-with(train, mean((ingtot-exp(Modelo4))^2))
mse5d<-with(train, mean((ingtot-exp(Modelo5))^2))
mse6d<-with(train, mean((ingtot-exp(Modelo6))^2))
mse7d<-with(train, mean((ingtot-exp(Modelo7))^2))
mse8d<-with(train, mean((ingtot-exp(Modelo8))^2))
mse9d<-with(train, mean((ingtot-exp(Modelo9))^2))
mse10d<-with(test, mean((ingtot-exp(Modelo10))^2))
mse11d<-with(test, mean((ingtot-exp(Modelo11))^2))

#Lo junto todo en una tabla
mse=data.frame()

mse<-rbind(mse, mse1)
mse<-rbind(mse, mse2)
mse<-rbind(mse, mse3)
mse<-rbind(mse, mse4)
mse<-rbind(mse, mse5)
mse<-rbind(mse, mse6)
mse<-rbind(mse, mse7)
mse<-rbind(mse, mse8)
mse<-rbind(mse, mse9)
mse<-rbind(mse, mse10)

mse_d=data.frame()

mse_d<-rbind(mse_d, mse1d)
mse_d<-rbind(mse_d, mse2d)
mse_d<-rbind(mse_d, mse3d)
mse_d<-rbind(mse_d, mse4d)
mse_d<-rbind(mse_d, mse5d)
mse_d<-rbind(mse_d, mse6d)
mse_d<-rbind(mse_d, mse7d)
mse_d<-rbind(mse_d, mse8d)
mse_d<-rbind(mse_d, mse9d)
mse_d<-rbind(mse_d, mse10d)

MSE=cbind(mse, mse_d)
colnames(MSE)=c("MSE_FM", "MSE_DM")

MSE$MSE_FM_MSE_DM= (MSE$MSE_FM/MSE$MSE_D)
rownames(MSE)=c("Modelo 1", "Modelo 2", "Modelo 3" , "Modelo 4" , "Modelo 5", "Modelo 6", "Modelo 7", "Modelo 8", "Modelo 9", "Modelo 10", "Modelo11")                    

#Exporto
write.csv(MSE,"MSE.csv")

summary(m10)
              
##Ana¡lisis ouliers
#calculo el modelo con la muestra total

cook<-plot(m10, which=4, main = "Distancia de Cook")
save.image("cook.jpeg")
                    
with(train2, plot(ingtot, cooks.distance(m10),  main = "Distancia de Cook vs ingesos totales", xlab="Ingresos totales", ylab="Distancia de Cook"))
save.image("cook2.jpeg")                   
#es influente si es más alto de 1

#leverege value por cada observación
lev=hatvalues(m10)
c=32/11402  #Es alto si > 2*(K+1)/n             
c     
 # Regla del pulgar
id_lev<-id.lev<-which(lev>2*c)
lev[id_lev]
c_prop= 973/nrow(train2)   
train2$lev=lev
train2$hat=c
#Gráfico el leverage
ggplot(train2, aes(loging, lev)) +
  ggtitle("Leverage por observación - Modelo 10") +
  geom_point(col="blueviolet", size= 2, shape=5) +
  geom_hline(yintercept=2*c, linetype="dashed", color="red", lwd=1.2) +
  theme_bw()+
  labs(y="Leverage", x="Log(Ingresos Totales")+
  theme(legend.position = 12)
#3221 tienen alto lev


#Validación cruzada k pfolds-------------------------------------------------------------
set.seed(10101)
id_traink <- createDataPartition(df_sinmis$ingtot, p=0.7, list=F)
traink <- df_sinmis[id_traink,]
testk<-df_sinmis[-id_traink,]

traink2<-filter(traink, ingtot>0)
testk2<-filter(testk, ingtot>0)
ctrl<-trainControl(method= "cv", number=10) #para poner en todos los modelos



md2<-train(ingtot~age+age2,
           data =traink,
           trControl=ctrl,
           preProcess=c("center", "scale"),
           method="lm")
md3<-train(loging~sex,
           data =traink2,
           trControl=ctrl,
           preProcess=c("center", "scale"),
           method="lm")
md4<-train(loging~sex + age +totalHoursWorked + estrato1 + cuentaPropia + maxEducLevel,
           data =traink2,
           trControl=ctrl,
           preProcess=c("center", "scale"),
           method="lm")
md5<-train(loging~sex + age + age2 + totalHoursWorked + estrato1 + cuentaPropia + maxEducLevel,
           data =traink2,
           trControl=ctrl,
           preProcess=c("center", "scale"),
           method="lm")
md6<-train(loging~sex + age + age2 + totalHoursWorked + estrato1 + cuentaPropia + cuentaPropia*sex+ maxEducLevel,
           data =traink2,
           trControl=ctrl,
           method="lm")
md7<-train(loging~sex + age + age2 + totalHoursWorked + estrato1 + cuentaPropia + cuentaPropia*sex+ maxEducLevel+maxEducLevel*sex,
           data =traink2,
           trControl=ctrl,
           preProcess=c("center", "scale"),
           method="lm")
md8<-train(loging~sex + age + age2 + totalHoursWorked + estrato1+ estrato1*sex + cuentaPropia + cuentaPropia*sex+ maxEducLevel+maxEducLevel*sex,
           data =traink2,
           trControl=ctrl,
           method="lm")
md9<-train(loging~sex + age + age2 + totalHoursWorked + totalHoursWorked^2 + estrato1 + estrato1*sex + cuentaPropia + cuentaPropia*sex +  maxEducLevel+ maxEducLevel*sex,
           data =traink2,
           trControl=ctrl,
           preProcess=c("center", "scale"),
           method="lm")
md10<-train(loging~sex + age + age2 + totalHoursWorked + totalHoursWorked^2 + estrato1 + estrato1*sex + estrato1*cuentaPropia + cuentaPropia + cuentaPropia*sex +  maxEducLevel+ maxEducLevel*sex,
           data =traink2,
           trControl=ctrl,
           preProcess=c("center", "scale"),
           method="lm")
md11<-train(loging~sex + age + age2 + totalHoursWorked + totalHoursWorked^2 + estrato1 + estrato1*sex + estrato1*cuentaPropia + cuentaPropia + cuentaPropia*sex +  maxEducLevel+ maxEducLevel*sex + maxEducLevel*estrato1,
            data =traink2,
            trControl=ctrl,
            preProcess=c("center", "scale"),
            method="lm")
rms2<-mean(as.data.frame(md2$resample)$RMSE)
rms3<-mean(as.data.frame(md3$resample)$RMSE)
rms4<-mean(as.data.frame(md4$resample)$RMSE)
rms5<-mean(as.data.frame(md5$resample)$RMSE)
rms6<-mean(as.data.frame(md6$resample)$RMSE)
rms7<-mean(as.data.frame(md7$resample)$RMSE)
rms8<-mean(as.data.frame(md8$resample)$RMSE)
rms9<-mean(as.data.frame(md9$resample)$RMSE)
rms10<-mean(as.data.frame(md10$resample)$RMSE)
rms11<-mean(as.data.frame(md11$resample)$RMSE)

RMSE=data.frame()
RMSE<-rbind(RMSE, rms3)
RMSE<-rbind(RMSE, rms4)
RMSE<-rbind(RMSE, rms5)
RMSE<-rbind(RMSE, rms6)
RMSE<-rbind(RMSE, rms7)
RMSE<-rbind(RMSE, rms8)
RMSE<-rbind(RMSE, rms9)
RMSE<-rbind(RMSE, rms10)
RMSE<-rbind(RMSE, rms11)
                 
RMSE$x=rownames(RMSE)
RMSE$x<-as.numeric(RMSE$x)
RMSE$x=RMSE$x+2
colnames(RMSE)=c("RMSE","Modelo")
#Exporto

write.csv2(RMSE, "RMSE.csv")
cv<-plot(RMSE$Modelo, RMSE$RMSE, main = "RMSE - Cros Validtion k=10", xlab="Modelo", ylab="RMSE promedio")

#LOOV---------------------------------------------------------------------------
err_m6<-numeric()
for (i in 1:n) {
  y<-df_sinmis$loging[-i]
  x1<-df_sinmis$sex[-i]
  x2<-df_sinmis$age[-i]
  x3<-df_sinmis$age2[-i]
  x4<-df_sinmis$totalHoursWorked[-i]
  x5<-df_sinmis$estrato1[-i]
  x6<-df_sinmis$cuentaPropia[-i]
  x8<-df_sinmis$maxEducLevel

  m6<-lm(y~x1 + x2 + x3 + x4 + x5 + x6 + x6*x1 + x8)
  yhat<-m6$coef[1]+m6$coef[2]*x1 + m6$coef[3]*x2 + m6$coef[4]*x3 + m6$coef[5]*x4 + m6$coef[6]*x5 + m6$coef[7]*x6 + m6$cof[8]*x6*x1+ m6$coef[9]*x8
  err_m6[i]<-df_sinmis$loging[i]-yhat
}
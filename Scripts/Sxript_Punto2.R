#Problem set 1 Big Data---------------------------------------------------------
setwd("C:/Users/Camila/OneDrive - Universidad de los Andes/1.Big Data")
#Paquetes

install.packages("pacman")
install.packages("tidyverse")
install.packages("rvest")
install.packages("xml2")
install.packages("dplyr")
install.github("vandomed/tab")
install.packages("caret")
install.packages("glmnet")

library(ggplot2)
library(pacman)
library(dplyr)
library(xml2)
library(rvest)
require(pacman)
p_load(rio,
       tidyverse,
       skimr,
       caret)

#Adquisición datos

url<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html"
url2<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html"
url3<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html"
url4<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html"
url5<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html"
url6<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_6.html"
url7<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7.html"
url8<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_8.html"
url9<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9.html"
url10<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html"


htlm1<-as.data.frame(htlm1)
htlm2<-as.data.frame(htlm2)
htlm3<-as.data.frame(htlm3)
htlm4<-as.data.frame(htlm4)
htlm5<-as.data.frame(htlm5)
htlm6<-as.data.frame(htlm6)
htlm7<-as.data.frame(htlm7)
htlm8<-as.data.frame(htlm8)
htlm9<-as.data.frame(htlm9)
htlm10<-as.data.frame(htlm10)

geih<-rbind(htlm1,htlm2)
geih<-rbind(geih,htlm3)
geih<-rbind(geih,htlm4)
geih<-rbind(geih,htlm5)
geih<-rbind(geih,htlm6)
geih<-rbind(geih,htlm7)
geih<-rbind(geih,htlm8) 
geih<-rbind(geih,htlm9)
geih<-rbind(geih,htlm10)

#Creo identificador individual /Id.Hogar+ orden (#persona dentro del hogar)
geih$id<-


htlm <- read_html(url) %>% 
  +   html_table()
htlm2 <- read_html(url2) %>% 
  html_table()
htlm3 <- read_html(url3) %>%
  html_table()
htlm4 <- read_html(url4) %>% 
  html_table()
htlm5 <- read_html(url5) %>% 
  html_table()
htlm6 <- read_html(url6) %>% 
  html_table()
htlm7 <- read_html(url7) %>% 
  html_table()
htlm8 <- read_html(url8) %>%
  html_table()
htlm9 <- read_html(url9) %>% 
  html_table()
htlm10 <- read_html(url10) %>% 
  html_table()
#Pego las bases
htlm1<-htlm
class(htlm)

geih <- data.frame()

geih<-rbind(geih,htlm1)
geih<-rbind(geih,htlm2)
geih<-rbind(geih,htlm3)
geih<-rbind(geih,htlm4)
geih<-rbind(geih,htlm5)
geih<-rbind(geih,htlm6)
geih<-rbind(geih,htlm7)
geih<-rbind(geih,htlm8)
geih<-rbind(geih,htlm9)
geih<-rbind(geih,htlm10)

save(geih, file="geih.Rda")
save(geih, file="geih.csv")

#Limpieza de datos---------------------------------------------------------------
  
#Filto por Bogotá y mayores de 18 años

geih<-filter(geih, dominio=="BOGOTA")

colnames(geih)

sum(is.na(geih$ingtotob))

geih_filtrada<-subset(geih, select=-c(ingtotob, ingtotes, p7140s1, p7150))
geih_filtrada<-subset(geih, select=-c(p7160,p7310,p7350,wap, pet,p7110,p7120,p7090))

geih_filtrada %>%
  filter(age >=18)
geih_fil<-filter(geih_filtrada, age >=18)
geih_fil<-filter(geih_fil, dominio=="BOGOTA")

#Ya no necesito esta var, solo estamos en bogota
geih_fil<-subset(geih_fil, select=-c(dominio))
geih_fil<-subset(geih_fil, select=-c(sizeFirm, microEmpresa))
geih_fil<-subset(geih_fil, select=-c(p6210))
geih_fil<-subset(geih_fil, select=-c(p6620, p6620s1, p6630s1,p6630s1a1, p6630s2,p6630s2a1,p6630s4,p6630s4a1, p6870))
geih_fil<-subset(geih_fil, select=-c(cclasnr11,cclasnr2,cclasnr3,cclasnr4, cclasnr5, cclasnr6, cclasnr7, cclasnr8)) 
 #Renombro mis variables de interes

geih_des<- subset(geih_fil, select=c(ingtot, age,sex, clase, college, cotPension, cuentaPropia, depto, dsi, ocu, inac, estrato1, maxEducLevel, relab, totalHoursWorked))

save(geih, file="geih.Rda")
rename
colnames(geih_fil)

#Despues del análisis de missings, creo una sin missings
#Miro en que formato está cada variable
geih_des<- subset(geih_fil, select=c(ingtot, age,sex, clase, college, informal, cotPension, cuentaPropia, depto, dsi, ocu, inac, estrato1, maxEducLevel, relab, totalHoursWorked))

#Vuelvo las categoricas categóricas, ya que todas son int
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

#MISSING_______________________________________________________________________

missings<-as.data.frame(map(geih_des, ~sum(is.na(.))))

miss<-transpose(missings)
miss$prop <- miss$V1/nrow(geih_des)*100

write.csv2(miss, "miss.csv")
#Relaci�n con las variable dependiente


##Por sexo

sex<-boxplot(loging~sex, data=df_sinmis, main="Log(Ingreso) por sexo",  xlab="Hombre=1, Mujer=0", ylab="Log(Ingreso)")
##Por cuenta propia
cuntap<-boxplot(loging~cuentaPropia, data=df_sinmis, main="Log(Ingreso) si trabajadores cuenta propia",  xlab="Trabajador cuenta propia=1", ylab="Log(Ingreso)")
##Hnivel educ
edu<-boxplot(loging~maxEducLevel, data=df_sinmis, main="Log(Ingresos)-Nivel de educaci�n m�ximo",  xlab="Nivel educ", ylab="Log(Ingreso)")
#Estrato
est<-boxplot(loging~estrato1, data=df_sinmis , main="Log(Ingresos)- Estrato",  xlab="Estrato", ylab="Log(Ingreso)")

sum(is)
### Punto 4

#Crear variable female=1 a partir de sex
table(geih_fil$sex)
geih_fil$sex <- as.factor(geih_fil$sex)
require(plyr)
geih_fil$female = revalue(geih_fil$sex, c("0"="1", "1"="0"))

#Crear variable log(income)
geih_fil$logincome<- log(geih_fil$ingtot)
geih_fil$logincome<- as.numeric(geih_fil$logincome)
geih_fil[is.na(geih_fil) | geih_fil == "-Inf"] <- NA

#Modelo OLS earnings gap
Modelo2 <- lm(logincome ~ female, data=geih_fil)
stargazer(Modelo2, type="text", title = "Resultados earnings gap", align=TRUE)

#Plot de predicted earnings por género
ggplot(data = Modelo2 , mapping = aes(x = female , y = logincome)) + 
  geom_point() 

#Bootstrap e intervalos de confianza

require("boot")
set.seed(1010)
bootGEIH.fn<-function(data,index){
  coef(lm(logincome ~ female, data=geih_fil, subset = index))
}
boot(geih_fil, bootGEIH.fn, R = 1000)

confint(Modelo2, level=0.95)

#Modelo con controles
Modelo3 <- lm(logincome ~ female+maxEducLevel+cuentaPropia+formal+oficio+
                age+relab, data=geih_fil)
stargazer(Modelo3, type="text", title = "Resultados earnings gap controles", align=TRUE)

#FWL

res1<- lm(logincome ~ maxEducLevel, data=geih_fil)$residuals
res2<- lm(female ~ maxEducLevel, data=geih_fil)$residuals
res3<- lm(cuentaPropia ~ maxEducLevel, data=geih_fil)$residuals
res4<- lm(formal ~ maxEducLevel, data=geih_fil)$residuals
res5<- lm(oficio ~ maxEducLevel, data=geih_fil)$residuals
res6<- lm(age ~ maxEducLevel, data=geih_fil)$residuals
res7<- lm(relab ~ maxEducLevel, data=geih_fil)$residuals

Modelo4<- lm(res1 ~ res2+res3+res4+res5+res6+res7)$residuals 
stargazer(Modelo4, type="text", title = "Resultados earnings gap FWL", align=TRUE)

###Punto 3

#Crear variable age2=age^2
geih_fil$age2=geih_fil$age^2
geih_fil

#Modelo OLS
Modelo1 <- lm(ingtot ~ age + age2, data=geih_fil)
summary(Modelo1)

library(stargazer)
stargazer(Modelo1, type="text", title = "Resultados OLS", align=TRUE)

#Plot de predicted earnings

getwd()
graf1 <- ggplot(data = Modelo1 , mapping = aes(x = age , y = ingtot)) + 
  geom_point() 
graf1
graf1 +                     
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")

png("Plot.png")
dev.off()

#Bootstrap e intervalos de confianza
require("boot")
set.seed(1010)
bootGEIH.fn<-function(data,index){
  coef(lm(ingtot ~ age + age2, data=geih_fil, subset = index))
}
boot(geih_fil, bootGEIH.fn, R = 1000)

confint(Modelo1, level=0.95)
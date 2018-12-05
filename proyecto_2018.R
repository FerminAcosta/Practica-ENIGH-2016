setwd("~/proyectoINEGI2017")

datos<-read.csv("ingresos.csv")
summary(datos)
attach(datos)
names(datos)
class(ing_tri)

summary(ing_tri)
boxplot(ing_tri)

quantile(datos$ing_tri,c(0,.1,.2,.3,.4,.5) , type = 5)


decil1<-subset(datos, datos$ing_tri<=293.47)
sum(decil1$ing_tri)
mean(decil1$ing_tri)
length(decil1$ing_tri)
sum(is.na(decil1$ing_tri))

decil2<-subset(datos, 293.47<datos$ing_tri & datos$ing_tri<=586.95)
mean(decil2$ing_tri)
length(decil2$ing_tri)
sum(is.na(decil2$ing_tri))

decil3<-subset(datos, 586.95<datos$ing_tri & datos$ing_tri<=983.6)
mean(decil3$ing_tri)
length(decil3$ing_tri)
sum(is.na(decil3$ing_tri))

decil4<-subset(datos, 983.6<datos$ing_tri & datos$ing_tri<=1467.39)
mean(decil4$ing_tri)
length(decil4$ing_tri)
sum(is.na(decil4$ing_tri))

decil5<-subset(datos, 1467.39<datos$ing_tri & datos$ing_tri<=2152.17)
mean(decil5$ing_tri)
length(decil5$ing_tri)
sum(is.na(decil5$ing_tri))



#salario minimo 2017
salmin<-80.04

# tres meses de salario minimo
salmin*90

#factor entre percentil 100 y percentil 10
34667119.55/293.47


mean(decil1$ing_tri)
mean(decil2$ing_tri)
mean(decil3$ing_tri)
mean(decil4$ing_tri)
mean(decil5$ing_tri)


sum(decil1$ing_tri)
sum(decil2$ing_tri)
sum(decil3$ing_tri)
sum(decil4$ing_tri)
sum(decil5$ing_tri)


#d1<-1:N
#for (i in 1:N){d1[i]=sample(1:6, 1)}
#d2<-1:N
#for (i in 1:N){d2[i]=sample(1:6, 1)}


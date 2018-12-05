datos<-read.csv("ingresos.csv")
summary(datos)
names(datos)
class(datos$ing_tri)

summary(datos$ing_tri)
hist(datos$ing_tri)
boxplot(datos$ing_tri)

limite<-8950+1.5*IQR(datos$ing_tri)
limite
outliers<-datos$ing_tri[datos$ing_tri>limite]
outliers
datos_sin_outliers<-datos$ing_tri[datos$ing_tri<limite]


summary(datos_sin_outliers)
hist(datos_sin_outliers)
boxplot(datos_sin_outliers)


limite1<-6456.5+1.5*IQR(datos_sin_outliers)
limite1
outliers1<-datos_sin_outliers[datos_sin_outliers>limite1]
outliers1
datos_sin_outliers1<-datos_sin_outliers[datos_sin_outliers<limite1]

summary(datos_sin_outliers1)
hist(datos_sin_outliers1)
boxplot(datos_sin_outliers1)


limite2<-6456.5+1.5*IQR(datos_sin_outliers1)
limite2
outliers2<-datos_sin_outliers1[datos_sin_outliers1>limite2]
outliers2
datos_sin_outliers2<-datos_sin_outliers1[datos_sin_outliers1<limite2]

summary(datos_sin_outliers2)
hist(datos_sin_outliers2)
boxplot(datos_sin_outliers2)

limite3<-4348.4+1.5*IQR(datos_sin_outliers2)
limite3
outliers3<-datos_sin_outliers2[datos_sin_outliers2>limite3]
outliers3
datos_sin_outliers3<-datos_sin_outliers2[datos_sin_outliers2<limite3]

summary(datos_sin_outliers3)
hist(datos_sin_outliers3)
boxplot(datos_sin_outliers3)


limite4<-3196.7+1.5*IQR(datos_sin_outliers3)
limite4
outliers4<-datos_sin_outliers3[datos_sin_outliers3>limite4]
outliers4
datos_sin_outliers4<-datos_sin_outliers3[datos_sin_outliers3<limite4]

summary(datos_sin_outliers4)
hist(datos_sin_outliers4)
boxplot(datos_sin_outliers4)

limite5<-2459+1.5*IQR(datos_sin_outliers4)
limite5
outliers5<-datos_sin_outliers4[datos_sin_outliers4>limite5]
outliers5
datos_sin_outliers5<-datos_sin_outliers4[datos_sin_outliers4<limite5]

summary(datos_sin_outliers5)
hist(datos_sin_outliers5)
boxplot(datos_sin_outliers5)

limite6<-2459+1.5*IQR(datos_sin_outliers5)
limite6
outliers6<-datos_sin_outliers5[datos_sin_outliers5>limite6]
outliers6
datos_sin_outliers6<-datos_sin_outliers5[datos_sin_outliers5<limite6]

summary(datos_sin_outliers6)
hist(datos_sin_outliers6)
boxplot(datos_sin_outliers6)


limite7<-1868.8+1.5*IQR(datos_sin_outliers6)
limite7
outliers7<-datos_sin_outliers6[datos_sin_outliers6>limite7]
outliers7
datos_sin_outliers7<-datos_sin_outliers6[datos_sin_outliers6<limite7]

summary(datos_sin_outliers7)
hist(datos_sin_outliers7)
boxplot(datos_sin_outliers7)


limite8<-1730.4+1.5*IQR(datos_sin_outliers7)
limite8
outliers8<-datos_sin_outliers7[datos_sin_outliers7>limite8]
outliers8
datos_sin_outliers8<-datos_sin_outliers7[datos_sin_outliers7<limite8]

summary(datos_sin_outliers8)
hist(datos_sin_outliers8)
boxplot(datos_sin_outliers8)

limite9<-1711.5+1.5*IQR(datos_sin_outliers8)
limite9
outliers9<-datos_sin_outliers8[datos_sin_outliers8>limite9]
outliers9
datos_sin_outliers9<-datos_sin_outliers8[datos_sin_outliers8<limite9]

summary(datos_sin_outliers9)
hist(datos_sin_outliers9)
boxplot(datos_sin_outliers9)


quantile(datos$ing_tri,c(0,.1,.2,.3,.4,.5) , type = 5)


decil1<-subset(datos, datos$ing_tri<=293.47)
hist(decil1$ing_tri)
sum(decil1$ing_tri)
mean(decil1$ing_tri)
length(decil1$ing_tri)
sum(is.na(decil1$ing_tri))

decil2<-subset(datos, 293.47<datos$ing_tri & datos$ing_tri<=586.95)
hist(decil2$ing_tri)
mean(decil2$ing_tri)
length(decil2$ing_tri)
sum(is.na(decil2$ing_tri))

decil3<-subset(datos, 586.95<datos$ing_tri & datos$ing_tri<=983.6)
hist(decil3$ing_tri)
mean(decil3$ing_tri)
length(decil3$ing_tri)
sum(is.na(decil3$ing_tri))

decil4<-subset(datos, 983.6<datos$ing_tri & datos$ing_tri<=1467.39)
hist(decil4$ing_tri)
mean(decil4$ing_tri)
length(decil4$ing_tri)
sum(is.na(decil4$ing_tri))

decil5<-subset(datos, 1467.39<datos$ing_tri & datos$ing_tri<=2152.17)
hist(decil5$ing_tri)
mean(decil5$ing_tri)
length(decil5$ing_tri)
sum(is.na(decil5$ing_tri))

decil6<-subset(datos, 2152.17<datos$ing_tri & datos$ing_tri<=3717.39)
hist(decil6$ing_tri)
mean(decil6$ing_tri)
length(decil6$ing_tri)
sum(is.na(decil6$ing_tri))

decil7<-subset(datos, 3717.39<datos$ing_tri & datos$ing_tri<=7043.47)
hist(decil7$ing_tri)
mean(decil7$ing_tri)
length(decil7$ing_tri)
sum(is.na(decil7$ing_tri))

decil8<-subset(datos, 7043.47<datos$ing_tri & datos$ing_tri<=11739.13)
hist(decil8$ing_tri)
mean(decil8$ing_tri)
length(decil8$ing_tri)
sum(is.na(decil8$ing_tri))

decil9<-subset(datos, 11739.13<datos$ing_tri & datos$ing_tri<=19024.62)
hist(decil9$ing_tri)
mean(decil9$ing_tri)
length(decil9$ing_tri)
sum(is.na(decil9$ing_tri))

decil10<-subset(datos, 19024.62<datos$ing_tri & datos$ing_tri<=34667119.55)
hist(decil10$ing_tri)
mean(decil10$ing_tri)
length(decil10$ing_tri)
sum(is.na(decil10$ing_tri))



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






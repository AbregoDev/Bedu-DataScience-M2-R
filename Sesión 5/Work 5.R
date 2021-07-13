# Work 5
# Ejemplo 1. Regresión Lineal Múltiple
# -------------------------------
# Leer datos de restaurantes
setwd('C:/Users/abreg/Documents/DS-BEDU/Módulo 2/Sesión 5/Data');
nyc <- read.csv("nyc.csv", header = TRUE);

# Dimensión y atado de columnas
dim(nyc);
attach(nyc);

# Matriz de gráficos de dispersión
pairs(~ Price + Food + Decor + Service, data = nyc, gap = 0.4, cex.labels = 1.5);

# Ajuste de modelo
m1 <- lm(Price ~ Food + Decor + Service + East);
summary(m1);

# Modelo sin considerar service
m2 <- lm(Price ~ Food + Decor + East);
summary(m2);

# Forma alternativa de reemplazar el modelo
m2 <- update(m1, ~.-Service);
summary(m2);

# Análisis de covarianza
mfull <- lm(Price ~ Food + Decor + Service + East + 
              Food:East + Decor:East + Service:East);
summary(mfull);

# Prueba de hipótesis H0:beta3=beta5=beta6=beta7=0
# es decir, Y=beta0+beta1*Food+beta2*Decor+beta4*East+e(reducido)
# contra H1:H0 no es verdad
# es decir Y=beta0+beta1*Food+beta2*Decor+beta3*Service+beta4*East
# +beta5*FoodEast+beta6*DecorEast+beta7*ServiceEast+e (completo)

# La prueba de si el efecto de los predictores depende de la variable
# dummy East puede lograrse usando la siguiente prueba-F parcial
anova(m2, mfull);

# Gráfica de Y, el precio contra los valores ajustados
plot(m2$fitted.values, Price, xlab = "Valores ajustados", ylab = "Price");
abline(lsfit(m2$fitted.values, Price));

# Matriz de gráficos de dispersión
pairs(~ Food + Decor, data = nyc, gap = 0.4, cex.labels = 1.5);

# Gráficas de residuales estandarizados
StanRes2 <- rstandard(m2);
par(mfrow = c(2, 2));
plot(Food, StanRes2, ylab = "Residuales Estandarizados");
plot(Decor, StanRes2, ylab = "Residuales Estandarizados");
plot(East, StanRes2, ylab = "Residuales Estandarizados");

# Evidencia para soportar la suposición de normalidad de los errores
qqnorm(StanRes2)
qqline(StanRes2)

# Ejemplo 2. Máquinas de vectores de soporte
# ---------------------------------
# Clasificar clientes potenciales de una compañía de tarjetas de
# crédito usando máquinas de vectores de soporte.

# Importar paquetes
library(dplyr)
library(e1071)
library(ggplot2)
# install.packages('ISLR')
library(ISLR)

?Default
head(Default)
tail(Default)
dim(Default)
str(Default)

# Gráfico de dispersión con la variable balance en x, income en y,
# además, diferenciando las distintas categorías en la variable default
# usando colour.
ggplot(Default, aes(x = balance, y = income, colour = default)) + 
  geom_point() + facet_wrap('student') + 
  theme_grey() + ggtitle("Datos Default")

# Generar vector de índices llamado train, tomando 5000 números aleatorios
# de los primeros 10,000 números naturales.

set.seed(2020)
train = sample(nrow(Default), 
               round(nrow(Default)/2))
tail(Default[train, ])

ggplot(Default[train, ], 
       aes(x = balance, y = income, colour = default)) + 
  geom_point() + facet_wrap('student') + 
  theme_dark() + ggtitle("Conjunto de entrenamiento")

ggplot(Default[-train, ], 
       aes(x = balance, y = income, colour = default)) + 
  geom_point() + facet_wrap('student') + 
  theme_light() + ggtitle("Conjunto de prueba")

# Ahora utilizamos la función `tune` junto con la función `svm` para 
# seleccionar el mejor modelo de un conjunto de modelos, los modelos 
# considerados son aquellos obtenidos al variar los valores de los 
# parámetros `cost` y `gamma`. Kernel Radial
# Tarda muchol
tune.rad = tune(svm, default~., data = Default[train,], 
                kernel = "radial", 
                ranges = list(
                  cost = c(0.1, 1, 10, 100, 1000), 
                  gamma = seq(0.01, 10, 0.5)
                ) 
)

# Se ha elegido el mejor modelo utilizando *validación cruzada de 10 
# iteraciones*

# summary(tune.rad)

# Aquí un resumen del modelo seleccionado

# summary(tune.rad$best.model)

best <- svm(default~.,  data = Default[train,],
            kernel = "radial",
            cost = 100,
            gamma = 1.51
)

# Matriz de confusión
mc <- table(true = Default[-train, "default"], 
            pred = predict(best, 
                           newdata = Default[-train,]))
mc

# El porcentaje total de aciertos obtenido por el modelo usando el 
# conjunto de prueba es el siguiente

round(sum(diag(mc))/sum(colSums(mc)), 5)

# Ahora observemos las siguientes proporciones

rs <- apply(mc, 1, sum)
r1 <- round(mc[1,]/rs[1], 5)
r2 <- round(mc[2,]/rs[2], 5)
rbind(No=r1, Yes=r2)

# Ajustar mejor el modelo
fit <- svm(default ~ ., data = Default[train,], 
           kernel = "radial", cost = 100, gamma = 1.51,
           decision.values = TRUE)

fitted <- attributes(predict(fit, Default[-train,], 
                             decision.values = TRUE))$decision.values

# Clasificación de las observaciones del conjunto de prueba
# utilizando valores predichos por el modelo
eti <- ifelse(fitted < 0, "Yes", "No")

mc <- table(true = Default[-train, "default"], 
            pred = eti)
mc

round(sum(diag(mc))/sum(colSums(mc)), 5)

rs <- apply(mc, 1, sum)
r1 <- round(mc[1,]/rs[1], 5)
r2 <- round(mc[2,]/rs[2], 5)
rbind(No=r1, Yes=r2)

# Ahora usar un umbral de decisión diferente para reducir la proporción
# del error más grave para la compañía de tarjetas de crédito
eti <- ifelse(fitted < 1.002, "Yes", "No")

mc <- table(true = Default[-train, "default"], 
            pred = eti)
mc

round(sum(diag(mc))/sum(colSums(mc)), 5)

rs <- apply(mc, 1, sum)
r1 <- round(mc[1,]/rs[1], 5)
r2 <- round(mc[2,]/rs[2], 5)
rbind(No=r1, Yes=r2)
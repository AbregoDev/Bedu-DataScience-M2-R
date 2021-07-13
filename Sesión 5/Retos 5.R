# Retos 5
# Reto 1. Regresión lineal múltiple
# ------------------------------
# Supongamos que nuestro trabajo consiste en aconsejar a un cliente
# sobre como mejorar las ventas de un producto particular, y el
# conjunto de datos con el que disponemos son datos de publicidad
# que consisten en las ventas de aquel producto en 200 diferentes
# mercados, junto con presupuestos de publicidad para el producto
# en cada uno de aquellos mercados para tres medios de comunicación
# diferentes: TV, radio, y periódico. No es posible para nuestro
# cliente incrementar directamente las ventas del producto. Por otro
# lado, ellos pueden controlar el gasto en publicidad para cada uno
# de los tres medios de comunicación. Por lo tanto, si determinamos
# que hay una asociación entre publicidad y ventas, entonces podemos
# instruir a nuestro cliente para que ajuste los presupuestos de
# publicidad, y así indirectamente incrementar las ventas. En otras
# palabras, nuestro objetivo es desarrollar un modelo preciso que
# pueda ser usado para predecir las ventas sobre la base de los
# tres presupuestos de medios de comunicación.

# Ajuste modelos de regresión lineal múltiple a los datos
# advertisement.csv y elija el modelo "más adecuado" siguiendo
# los procedimientos vistos en el Ejemplo 1.

# Leer csv
setwd('C:/Users/abreg/documents/ds-bedu/módulo 2/sesión 5/data');
adv <- read.csv('advertising.csv');
attach(adv);

# Matriz de gráficos de dispersión
pairs(~ Sales + TV + Radio + Newspaper,
      data = adv, gap = 0.4, cex.labels = 1.5);
# Se observan relaciones aproximadamente lineales

# Ajuste de un modelo
# Sales = beta0 + beta1*TV + beta2*Radio + beta3*newspaper + e
m1 <- lm(Sales ~ TV + Radio + Newspaper);
summary(m1);

# Se usará otro modelo sin newspaper pues el coeficiente de regresión
# no fue estadísticamente significativo (p-value)
# Sales = beta0 + beta1*TV + beta2*Radio + e
m2 <- update(m1, ~.-Newspaper);
summary(m2);

# Diagnósticos
plot(m2$fitted.values, Sales, xlab = "Valores ajustados", ylab = "Sales");
abline(lsfit(m2$fitted.values, Sales));

# Matriz de gráficos de dispersión de los dos predictores continuos
pairs(~ TV + Radio, data = adv, gap = 0.4, cex.labels = 1.5);

# Gráficas de residuales estandarizados contra predictor
StanRes2 <- rstandard(m2)
par(mfrow = c(2, 2))
plot(TV, StanRes2, ylab = "Residuales Estandarizados")
plot(Radio, StanRes2, ylab = "Residuales Estandarizados")

# Evidencia para soportar la hipótesis de normalidad de los errores
qqnorm(StanRes2)
qqline(StanRes2)

shapiro.test(StanRes2)

# Reto 2. Máquinas de vectores de soporte
# ---------------------------
# En el archivo de datos datosclases.csv, adjunto se encuentran
# observaciones correspondientes a dos clases diferentes indicadas
# por la variable y. Únicamente hay dos variables predictoras
# o características. A continuación realice los siguientes
# requerimientos (Hint: transforme primero la variable de respuesta
# y a variable categórica con las funciones mutate y factor):

# Cargar dataset
setwd('C:/Users/abreg/documents/ds-bedu/módulo 2/sesión 5/data');
datos <- read.csv("datosclases.csv");
  
# 1. Carga los paquetes ggplot2 y e1071; observe algunas
# características del data frame con las funciones tail y dim.
# Obtenga el gráfico de dispersión de los datos diferenciando
# las dos clases.

library(dplyr)
library(ggplot2)
library(e1071)

tail(datos);
dim(datos);
str(datos)
datos <- mutate(datos, y = factor(y))

# Obtenemos el gráfico de dispersión de los datos
# diferenciando las dos clases

ggplot(datos, aes(x = x.1, y = x.2, colour = y)) + 
  geom_point() + 
  theme_dark() + ggtitle("Datos")

# 2. Genera de manera aleatoria un vector de índices para filtrar
# un conjunto de entrenamiento a partir del conjunto de datos dado.
# Con ayuda de las funciones tune y svm ajuste máquinas de vectores
# de soporte con un kernel radial a los datos de entrenamiento,
# para valores del parámetro cost igual a 0.1, 1, 10, 100, 1000
# y valores del parámetro gamma igual a 0.5, 1, 2, 3, 4. Obtenga
# un resumen de los resultados.

train <- sample(300, 150)
tail(as.data.frame(train))

###

# Ajustamos máquinas de vectores de soporte con un kernel radial 
# para diferentes valores de los parámetros `cost` y `gamma`

set.seed(67)
tune.out <- tune(svm, y~., data = datos[train, ], 
                 kernel = "radial", 
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000), 
                               gamma = c(0.5, 1, 2, 3, 4)))

# Resumen de datos
summary(tune.out)

# 3. Con el modelo que tuvo el mejor desempeño en el paso anterior
# realiza clasificación con la función predict y el conjunto de datos
# de prueba. Muestre la matriz de confusión.

table(true = datos[-train, "y"], 
      pred = predict(tune.out$best.model, newdata = datos[-train,]))

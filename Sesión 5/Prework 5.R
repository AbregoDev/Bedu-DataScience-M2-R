# Prework 5

# Leer archivo production.txt
setwd('C:/Users/abreg/Documents/Ds-BEDU/Módulo 2/Sesión 5/Data');
production <- read.table('production.txt', header = TRUE);
# Desatomizar data frame
attach(production);
head(production);
dim(production);

# Graficar
plot(RunSize, RunTime, xlab = "Tamaño de Proceso",
     ylab = "Tiempo de Proceso", pch = 16);

# Generar modelo lineal
m1 <- lm(RunTime ~ RunSize);
summary(m1);

# Graficar
plot(RunSize, RunTime, xlab = "Tamaño de ejecución", 
     ylab = "Tiempo de ejecución", pch = 16)
abline(lsfit(RunSize, RunTime)) # Trazamos la recta de regresión estimada
mtext(expression(paste('Modelo de regresión lineal simple:',
                       ' ',
                       y[i] == beta[0] + beta[1]*x[i] + e[i])),
      side = 3, adj=1, font = 2)

# Recta de regresión poblacional
text(x = 200, y = 240, expression(paste('Recta de regresión:',
                                        ' ',
                                        y[i] == beta[0] + beta[1]*x[i])),
     adj = 1, font = 2)

# Recta de regresión estimada
text(x = 350, y = 180, expression(paste('Recta estimada:',
                                        ' ',
                                        hat(y)[i] == hat(beta)[0] + hat(beta)[1]*x[i])),
     adj = 1, font = 2)

# Recta de regresión estimada

text(x = 350, y = 160, expression(paste('Recta estimada:',
                                        ' ',
                                        hat(y)[i] == 149.74770 + 0.25924*x[i])),
     adj = 1, font = 2)

# Residuales

points(189, 215, pch=16, col = "red") # Punto muestral
149.74770 + 0.25924 * 189 # Valor y sobre la recta estimada
## [1] 198.7441
lines(c(189, 189), c(198.7441, 215), col = "red")

points(173, 166, pch=16, col = "red") # Punto muestral
149.74770 + 0.25924 * 173 # Valor y sobre la recta estimada
## [1] 194.5962
lines(c(173, 173), c(166, 194.5962), col = "red")

# Intervalos de confianza del 95%
round(confint(m1, level = 0.95), 3);

# Algunos posibles valores de RunSize
RunSize0 <- c(50,100,150,200,250,300,350)

(conf <- predict(m1, newdata = 
                   data.frame(RunSize = RunSize0), 
                 interval = "confidence", level = 0.95))

# Podemos visualizar gráficamente estos intervalos de confianza
plot(RunSize, RunTime, xlab = "Tamaño de ejecución", 
     ylab = "Tiempo de ejecución", pch = 16)
abline(lsfit(RunSize, RunTime)) # Trazamos la recta de regresión estimada

lines(RunSize0, conf[, 2], lty = 2, lwd = 2, col = "green") # límites inferiores
lines(RunSize0, conf[, 3], lty = 2, lwd = 2, col = "green") # límites superiores

# Diagnósticos de regresión
par(mfrow = c(2, 2));
plot(m1);

# Máquinas de vectores de soporte
# Instalar paquetería e1071
# install.packages('e1071');
suppressMessages(suppressWarnings(library(dplyr))) # también usaremos dplyr
library(e1071);

# Importar datos
dat <- read.table('dat.txt', header = TRUE);
# Eliminar columna n
dat$n <- NULL;

# Graficar
plot(dat[,-3], col = (3-dat[,3]), pch = 16);

# Convertir la columna y a factor
dat <- dat %>% mutate(y = factor(y));
tail(dat)
str(dat)

# Ajustar clasificador de vectores de soporte con la función svm
svmfit <- svm(y~., data = dat, kernel = "linear", cost = 10, scale = FALSE);
# Mostrar clasificador de vectores de soporte
plot(svmfit, dat);

# Máquinas de vectores de soporte
# Importar csv
dat <- read.csv('Dat.csv');
# Graficar
plot(x = dat$x.1, y = dat$x.2, col = dat$y, pch = 16);

# Generar índices para el conjunto de entrenamiento
train <- sample(200, 100);
tail(as.data.frame(train));

# Ajustar máquina de vectores de soporte con un kernel radial
# y parámetros gamma = 1 y cost = 1
svmfit <- svm(y~., data = dat[train, ], kernel = "radial",
              gamma = 1, cost = 1e5);
plot(svmfit, dat[train, ]);
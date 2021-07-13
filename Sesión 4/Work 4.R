# Work 4
# Ejemplo 1. Distribuciones normal y t de student
# ----------------------
# Importar ggplot2 para grafica
library(ggplot2);
# dnorm = función de densidad de la distribución normal
# pnorm = función de distribución de la distribución normal
# qnorm = función de cuantiles de la distribución normal
# rnorm = muestras aleatorias de distribución normal

# Variable aleatoria X que se distribuye como normal con media 175
# y desviación estándar 6 (mu = 175, sigma = 6)
# Función de densidad
x <- seq(-4, 4, 0.01)*6 + 175;
y <- dnorm(x, mean = 175, sd = 6);
plot(x, y, type = "l");
title(
  main = "Densidad de Probabilidad Normal",
  sub = expression(paste(mu == 175, " y ", sigma == 6)));
abline(v = 175, lwd = 2, lty = 2);

# Función de distribución
# Obtener probabilidad de que X tome un valor menor o igual a 180
# Esto es: P(x<=180)
pnorm(q = 180, mean = 175, sd = 6);
par(mfrow = c(2,2));

# Área correspondiente a dicha probabilidad
plot(x, y, type = "l");
title(main = "Densidad de Probabilidad Normal",
      sub = expression(paste(mu == 175, " y ", sigma == 6)));
polygon(c(min(x), x[x<=180], 180), c(0, y[x<180], 0), col = "red");

# Para obtener P(x<=165)
pnorm(q = 165, mean = 175, sd = 6);
par(mfrow = c(2,2));

# Área correspondiente a dicha probabilidad
plot(x, y, type = "l");
title(main = "Densidad de Probabilidad Normal",
      sub = expression(paste(mu == 175, " y ", sigma == 6)));
polygon(c(min(x), x[x<=165], 165), c(0, y[x<165], 0), col = "yellow");

# Obtener P(165 <= X <= 180)
(pnorm(q = 180, mean = 175, sd = 6) - pnorm(q = 165, mean = 175, sd = 6));
par(mfrow = c(2,2));

# Área correspondiente a dicha probabilidad (intervalo)
plot(x, y, type = "l");
title(main = "Densidad de Probabilidad Normal",
      sub = expression(paste(mu == 175, " y ", sigma == 6)));
polygon(c(165, x[x>=165 & x<=180], 180), c(0, y[x>=165 & x<=180], 0), col = "green");

# Obtener P(X>=180)
pnorm(q = 180, mean = 175, sd = 6, lower.tail = F);

# Área correspondiente a dicha probabilidad (intervalo)
plot(x, y, type = "l");
title(main = "Densidad de Probabilidad Normal",
      sub = expression(paste(mu == 175, " y ", sigma == 6)));
polygon(c(180, x[x>=180], max(x)), c(0, y[x>=180], 0), col = "blue");

# Cuantiles
# Encontrar el número b, tal que P(X<=b) = 0.75, es decir, el cuantil de orden 0.75
(b <- qnorm(p = 0.75, mean = 175, sd = 6));
pnorm(b, 175, 6);

# Graficación de la función de distribución (normal) y el cuartil sobre el eje X
plot(x, y, type = "l");
title(main = "Densidad de probabilidad normal", sub = expression(paste(mu == 175, " y ", sigma == 6)));
axis(side = 1, at = b, font = 2, padj = 1, lwd = 2);

# Muestra aleatoria de tamaño n = 1000 de la v.a. X
set.seed(7563)
muestra <- rnorm(n = 1000, mean = 175, sd = 6);
length(muestra);
mdf <- as.data.frame(muestra);
tail(mdf);
hist(muestra, breaks = seq(150, 200, 2));

# Distribución t de Student
# dt = función de densidad de la distribución t de Student (continua)
# pt = función de distribución de la distribución t de Student (continua)
# qt = función de cuantiles de la distribución t de Student (continua)
# rt = muestras aleatorias de distribución t de Student (continua)

# Variable aleatoria T que se distribuye como t de Student como
# 7 grados de libertad (gl) parámetro gl = 7.
x <- seq(-4, 4, 0.01);
y <- dt(x, df = 7);
plot(x, y, type = "l", main = "Densidad t de Student, gl = 7");
abline(v = 0, lwd = 2, lty = 2);

# Función de distribución
# Encontrar P(T<=1.5)
pt(q = 1.5, df = 7);

# Gráfica correspondiente a esta probabilidad
plot(x, y, type = "l", main = "Densidad t de Student, gl = 7");
polygon(c(min(x), x[x<=1.5], 1.5), c(0, y[x<=1.5], 0), col = "purple");

# Encontrar P(T>=2)
pt(q = 2, df = 7, lower.tail = F);

# Gráfica correspondiente a esta probabilidad
plot(x, y, type = "l", main = "Densidad t de Student, gl = 7");
polygon(c(2, x[x>=2], max(x)), c(0, y[x>=2], 0), col = "orange");

# Cuantiles
# Encontrar el número d tal que P(T <= d) = 0.025, es decir
# el cuantil de orden 0.025
(d <- qt(p = 0.025, df = 7));
pt(q = d, df = 7);

# Graficación de la función de distribución (t de student) y el cuartil sobre el eje X
plot(x, y, type = "l", main = "Densidad t de Student, gl = 7");
axis(side = 1, at = d, font = 2, padj = 1, lwd = 2);

# Muestras aleatorias
# Generar una muestra aleatoria de tamaño n = 1000 de la v.a. T
set.seed(777)
muestra <- rt(n = 1000, df = 7);
length(muestra);
mdf <- as.data.frame(muestra);
tail(mdf);
hist(muestra, breaks = seq(-5, 6, 0.4));
min(muestra);

# Ejemplo 2. Teorema central del límite
# ----------------------------
# Cargar ggplot para graficar
library(ggplot2)

# Variable aleatoria (v.a.) X con distribución exponencial
# y parámetro lambda = 2
x <- seq(0, 5, 0.02)
plot(x, dexp(x, rate = 2), type = "l", lwd = 2, ylab = "")
title(main = "Función de Densidad Exponencial", ylab = "f(x)",
      sub = expression("Parámetro " ~ lambda == 2))
text(x = 3, y = 1.5, labels = expression(f(x)==2*exp(-2*x) ~ " para x "  >= 0))
text(x = 3, y = 1.3, labels = paste("0 en otro caso"))
text(x = 1, y = 1, labels = expression("E(X) = " ~ 1/lambda == 1/2), col = 2)
text(x = 3, y = 0.5, labels = expression("DE(X) = " ~ 1/lambda == 1/2), col = 4)

# Obtener muestra aleatoria de tamaño n = 4, de la distribución exponencial
set.seed(10);
(m1.4 <- rexp(n = 4, rate = 2));

# Media de la muestra generada
mean(m1.4);

# Ahora obtener 5 muestras de tamaño 3
set.seed(64);
(m5.3 <- sapply(X = rep(3, 5), FUN = rexp, 2));

# Media de las 5 muestras
(media5.3 <- apply(m5.3, 2, mean));

# Generar 1000 muestras de tamaño 7 y las 1000 medias correspondientes
set.seed(465);
m1000.7 <- sapply(X = rep(7, 1000), FUN = rexp, 2);
media1000.7 <- apply(m1000.7, 2, mean);
mdf <- as.data.frame(media1000.7);
tail(mdf);

# Histograma aproximado a forma de campana
ggplot(mdf, aes(media1000.7)) + 
  geom_histogram(colour = 'green', 
                 fill = 'orange',
                 alpha = 0.7) + # Intensidad del color fill
  geom_vline(xintercept = mean(media1000.7), linetype="dashed", color = "black") + 
  ggtitle('Histograma para las 1000 medias') + 
  labs(x = 'medias', y = 'Frecuencia')+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 

# 1000 muestras de tamaño 33, y las 1000 medias correspondientes
set.seed(4465);
m1000.33 <- sapply(X = rep(33, 1000), FUN = rexp, 2);
media1000.33 <- apply(m1000.33, 2, mean);
mdf <- as.data.frame(media1000.33);
tail(mdf);

# Histograma aún más parecido a la campana
ggplot(mdf, aes(media1000.33)) + 
  geom_histogram(colour = 'yellow', 
                 fill = 'purple',
                 alpha = 0.7) + # Intensidad del color fill
  geom_vline(xintercept = mean(media1000.33), linetype="dashed", color = "black") + 
  ggtitle('Histograma para las 1000 medias') + 
  labs(x = 'medias', y = 'Frecuencia')+
  theme_get() +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 

# Ejemplo 3. Contraste de hipótesis
# ---------------------------------
# Generara dos muestras de tamaño n1 = 56 y n2 = 63
set.seed(174376);
n1 <- 56;
n2 <- 63;
m1 <- rexp(n = n1, rate = 4.1);
# media real = 1/4.1
m2 <- rexp(n = n2, rate = 3.4);
# media real = 1/3.4
# diferencia de medias real = 1/4.1 - 1/3.4

# Interesa encontrar la hipótesis H0:mu1-mu2=0 vs H1:mu1=mu2!=0
# (Contraste de dos colas)

# Valor observado del estadístico de prueba
(z0 <- (mean(m1) - mean(m2) - 0)/sqrt(var(m1)/n1 + var(m2)/n2));

# Supongamos que estamos interesados en encontrar la región de rechazo
# con un nivel de significancia alpha = 0.05, se debe encontrar el valor
# z_{0.025} que satsiface P(Z > z_{0.025}) = 0.025
(z.025 <- qnorm(p = 0.025, lower.tail = FALSE));

# Puesto que
(z0 <- -z.025) | (z0 > z.025);

# Se falla al rechazar la hipótesis nula

# el p-value se calcula como
(pvalue <- 2 * pnorm(z0, lower.tail = FALSE));

x <- seq(-4, 4, 0.01);
y <- dnorm(x);

plot(x, y, type = "l");
title(main = "Densidad normal estándar", sub = expression(paste(mu == 0, " y ", sigma == 1)));

polygon(c(min(x), x[x<=-z0], -z0), c(0, y[x<=-z0], 0), col = "purple");
axis(side = 1, at = -z0, font = 2, padj = 1, lwd = 2);

polygon(c(z0, x[x>=z0], max(x)), c(0, y[x>=z0], 0), col = "purple");
axis(side = 1, at = z0, font = 2, padj = 1, lwd = 2);

# Contraste de dos colas
# Dadas dos muestras aleatorias de tamaños n1 = 23 y n2 = 20
set.seed(1776);
n1 <- 23;
n2 <- 20
m1 <- rnorm(n = n1, mean = 175, sd = 3);
m2 <- rnorm(n = n2, mean = 160, sd = 3);
# diferencia de medias real
175 - 160;

# Se desea contrastar la hipótesis H0:mu1-mu2=0 vs H1:mu1-mu2!=0

# Valor observado del estadístico de prueba en este caso
(t0 <- (mean(m1)-mean(m2)-0)/(sqrt((22*var(m1)+19*var(m2))/(23+20-2))*sqrt(1/23+1/20)));

# Encontrar la región de rechazo (de dos colas) con un nivel de significancia
# alpha = 0.05, esto es, t_{0.025} tal que P(T > t_{0.025}) = 0.025
(t.025 <- qt(p = 0.025, df = 41, lower.tail = FALSE));

# Puesto que 
(t0 <- -t.025) | (t0 > t.025); # es verdadero
# se rechaza la hipótesis nula

# p-value se puede calcular como
(pvalue <- 2*pt(t0, df = 41, lower.tail = FALSE));

# t.test sirve para realizar el procedimiento de contraste de hipótesis
t.test(x = m1, y = m2, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = TRUE);

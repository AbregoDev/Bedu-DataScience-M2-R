# Retos 4
# Reto 1. Distribución normal
# -------------------------------
# Una compañía que manufactura y embotella jugo de manzana
# usa una máquina que automáticamente llena botellas de 16 onzas.
# Sin embargo, existe variación en las cantidades de líquido
# que se ponen en las botellas que se llenan. Se ha observado
# que la cantidad de líquido está normalmente distribuida en forma
# aproximada con media de 16 onzas y desviación estándar de 1 onza.
# Determine la proporción de botellas que tendrán más de 18 onzas.

# P(X>=18)
pnorm(q = 18, mean = 16, sd = 1, lower.tail = F);

# Valorrs posibles de la v.a.
x <- seq(-4, 4, 0.01) + 16;
y <- dnorm(x, mean = 16, sd = 1);
  
# Área correspondiente a dicha probabilidad (intervalo)
plot(x, y, type = "l");
title(main = "Densidad de Probabilidad de Botellas",
      sub = expression(paste(mu == 16, " y ", sigma == 1)));
polygon(c(18, x[x>=18], max(x)), c(0, y[x>=18], 0), col = "green");

# Reto 2. Teorema central del límite
# Las calificaciones de exámenes para todos los estudiantes
# de último año de preparatoria en cierto estado tienen media
# de 60 y varianza de 64. Una muestra aleatoria de n = 100
# estudiantes de una escuela preparatoria grande tuvo una
# calificación media de 58. ¿Hay evidencia para sugerir que
# el nivel de conocimientos de esta escuela sea inferior?
# (Calcule la probabilidad de que la media de una muestra
# aleatoria sea a lo sumo 58 cuando n = 100.)
pnorm(58, mean = 60, sd= 8/10)

# Reto 3. Contraste de hipótesis
# El vicepresidente de ventas de una gran empresa afirma que los
# vendedores están promediando no más de 15 contactos de venta por
# semana. (Le gustaría aumentar esa cantidad.) Como prueba de su
# afirmación, aleatoriamente se seleccionan n = 20 vendedores y se
# registra el número de contactos hechos por cada uno para una sola
# semana seleccionada al azar.
muestra <- c(V1 = 13, V2 = 17, V3 = 20, V4 = 17, V5 = 20, V6 = 20,
             V7 = 18, V8 = 18, V9 = 16, V10 = 19, V11 = 13,
             V12 = 17, V13 = 15, V14 = 19, V15 = 16, V16 = 19,
             V17 = 22, V18 = 10, V19 = 13, V20 = 21);

# ¿La evidencia contradice lo dicho por el vicepresidente?
# use una prueba con un nivel alpha = 0.05
# 1. Establezca las hipótesis nula y alternativa. ¿Es una prueba de
# cola superior, de cola inferior o de dos colas?

# H0: mu = 15 vs H1: mu > 15 (contraste de cola superior)

# 2. Decida si rechazar o no la hipótesis nula si el nivel de significancia es
# alpha = 0.05

(t0 <- (mean(muestra) - 15)/(sd(muestra)/sqrt(20)));

# 3. Obtenga el p-value de la prueba

(pvalue <- pt(t0, df = 19, lower.tail = FALSE));

# 4. Lleve a cabo la prueba con la función t.test

t.test(x = muestra, alternative = "greater", mu = 15);

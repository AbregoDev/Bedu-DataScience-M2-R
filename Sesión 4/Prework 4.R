# Prework 4

# Distribución normal
x <- seq(-4, 4, 0.01)*6 + 175;
y <- dnorm(x, mean = 175, sd = 6);
plot(x, y, type = "l", xlab = "X", ylab = "Y");
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 175, " y ", sigma == 6)));
abline(v = 175, lwd = 2, lty = 2);

# Distribución t-student
x <- seq(-4, 4, 0.01);
y <- dt(x, df = 7);
plot(x, y, type = "l", main = "Densidad t de Student, gl = 7", xlab="", ylab="");
abline(v = 0, lwd=2, lty=2);
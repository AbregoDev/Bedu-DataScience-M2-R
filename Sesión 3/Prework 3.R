# Prework 3
# -----------------
# Gráfico de dispersión
x <- c(1,2,7,7,8,5,8,3,9);
y <- c(3,4,9,7,10,8,14,5,10);
plot(x,y)

# Correlación
# Un valor cercano a 0 indica una alta dispersión
# Valores cercanos a 1 o -1 indican poca dispersión
cor(x,y);

# Box-plots
# Ilustra la distribución de los datos por cuartiles
boxplot(airquality$Ozone, airquality$Temp);

# Retos 3
# -----------------------
# Reto 1. Histogramas
# Cargar el data set BD_Altura_Alumnos.csv
alturas <- read.csv(
  'C:/Users/abreg/Documents/DS-BEDU/Módulo 2/Sesión 3/Data/alturas.csv',
  sep = ';');

# Histograma con hist
hist(alturas$Altura,
     breaks = (seq(145, 195, 5)),
     main = "Histograma de Alturas",
     xlab = "Alturas",
     ylab = "Frecuencia");

# Histograma con ggplot
alturas %>%
  ggplot() +
  aes(Altura) +
  geom_histogram(binwidth = 5, col = "#8C3420", fill = "#D97904") +
  ggtitle("Histograma de Alturas") +
  ylab("Frecuencia") +
  xlab("Alturas") +
  theme_light();

# Reto 2. Estadísticos de la NBA
# -------------------------------
# Cargar archivo
nba <- read.csv(
  'C:/Users/abreg/Documents/DS-BEDU/Módulo 2/Sesión 3/Data/players.csv');

# Usar ggplot y dplr
library(ggplot2)
library(dplyr)
# Generar un histograma de los minutos totales y agregar una línea
# con la media
nba %>%
  ggplot() +
  aes(MIN) +
  geom_histogram(binwidth = 200, col = "#8C3420", fill = "#D97904") +
  ggtitle("Histograma de Minutos Totales") +
  ylab("Frecuencia") +
  xlab("Minutos") +
  theme_light() +
  geom_vline(xintercept = median(nba$MIN));

# Generar el histograma de edad y agregar una línea con la media
na.omit(nba) %>%
  ggplot() +
  aes(na.omit(Age)) +
  geom_histogram(binwidth = 2, col = "#8C3420", fill = "#D97904") +
  ggtitle("Histograma de Edades") +
  ylab("Frecuencia") +
  xlab("Edades") +
  theme_light() +
  geom_vline(xintercept = median(na.omit(nba)$Age));

clean.nba <- na.omit(nba);
# Graficar un scatterplot de las variables Weight y Height.
# Observar la correlación
(my_scatplot <- ggplot(clean.nba, aes(x = Weight, y = Height)) + geom_point());
cor(clean.nba$Weight, clean.nba$Height);

# Utilizar la función which.max para saber quién es el jugador más alto
# Imprimirlo con una leyenda
tallest.i <- which.max(clean.nba$Height);
paste('El jugador más alto es: ',
      clean.nba$Name[tallest.i],
      ' y su altura es: ',
      clean.nba$Height[tallest.i]);

# Utilizar la función which.min
shortest.i <- which.min(clean.nba$Height);
paste('El jugador más alto es: ',
      clean.nba$Name[shortest.i],
      ' y su altura es: ',
      clean.nba$Height[shortest.i]);

# Imprimir la altura promedio
paste('La altura promedio es: ', round(mean(clean.nba$Height), 2));

# Generar un scatterplot donde se representen las Asistencias Totales (AST.TOV)
# contra los puntos (PTS), realizar un face wrap con la posición.
scatter2 <- ggplot(clean.nba, aes(x = AST.TOV, y = PTS)) + geom_point();
(scatter2 + facet_wrap("Pos"));
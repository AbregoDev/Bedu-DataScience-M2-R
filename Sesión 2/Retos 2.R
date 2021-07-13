# Work 2. RETOS

# Reto 1: Medidas de tendencia central
# ---------------------
# Considere el siguiente vector
set.seed(134);
x <- round(rnorm(1000, 175, 6), 1);

# Calcule la media, mediana y moda
mean(x);
median(x);
library(DescTools);
Mode(x);

# Obtenga los deciles de x
quantile(x, seq(0.1, 0.9, by = 0.1));

# Encuentre el rango intercuartílico, desviación estándar y varianza muestral
IQR(x);
quantile(x, 0.75) - quantile(x, 0.25);

sd(x);
var(x);

view(x);

# Reto 2: Función mediana
# ---------------------
# Crear una función que calcule la mediana sin usar median()
mediana <- function(vector) {
  # Sorted vector
  sorted.vector <- sort(vector);
  # Vector length
  vec.len <- length(vector);
  # Vector index
  i <- vec.len / 2;
  if(vec.len %% 2 == 0) {
    mediana <- (sorted.vector[i] + sorted.vector[i + 1]) / 2;
  } else {
    mediana <- sorted.vector[ceiling(i)];
  }
  
  return(mediana);
}

y <- c(2,4,6,7,3,4,6,7);
sort(y);
mediana(y);

# Reto 3. Lectura de datos y uso de dplyr
# ---------------------
# Descargar los archivos ... e importarlos en R
setwd('C:/Users/abreg/Documents/DS-BEDU/Módulo 2/Sesión 2/bundesliga/')
bundesliga.17.18 <- 'https://www.football-data.co.uk/mmz4281/1718/D1.csv';
bundesliga.18.19 <- 'https://www.football-data.co.uk/mmz4281/1819/D1.csv';
bundesliga.19.20 <- 'https://www.football-data.co.uk/mmz4281/1920/D1.csv';
bundesliga.20.21 <- 'https://www.football-data.co.uk/mmz4281/2021/D1.csv';

download.file(url = bundesliga.17.18, destfile = "bundes1718.csv", mode = "wb");
download.file(url = bundesliga.18.19, destfile = "bundes1819.csv", mode = "wb");
download.file(url = bundesliga.19.20, destfile = "bundes1920.csv", mode = "wb");
download.file(url = bundesliga.20.21, destfile = "bundes2021.csv", mode = "wb");

lista <- lapply(list.files(), read.csv);

# Usando select de dplyr, seleccionar solo las columnas:
# Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR
lista <- lapply(lista, select, Date, HomeTeam:FTR)
head(lista[[1]]); # ver lista

# Combina cada uno de los data frames en un único data frame
# con ayuda de rbind y do.call
data <- do.call(rbind, lista);
head(data);
tail(data);
dim(data);

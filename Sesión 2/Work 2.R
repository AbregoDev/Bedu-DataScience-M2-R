# Work 2

# Ejemplo 1. Medidas de tendencia central, de posición y de dispersión
# ---------------------
# Media
x = c(4000, 9000, 9000, 10000);
mean(x);

# Mediana
median(x);

# Moda con paquete DescTools
library('DescTools');
Mode(x);

# Medidas de posición
x <- c(29, 13, 62, 4, 63, 96, 1, 90, 50, 46);
quantile(x, 0.25); # Cuantil 25%
quantile(x, c(0.25, 0.50, 0.75)); # Cuartiles
quantile(x, seq(0.1,0.9, by=0.1)); # Deciles

# Rango intercuantílico
IQR(x);
quantile(x, 0.75) - quantile(x, 0.25);

# Varianza
var(x);

# Desviación estándar
sd(x);

# Ejemplo 2. Función view y crear función
# ---------------------
# Función View. Muestra una vista más amigable de datos como un df
edad <- c(15, 36, 45, 19, 21);
peso <- c(36, 57, 48, 37, 41);
df <- data.frame('edad' = edad, 'peso' = peso);
View(df);

# Función para la moda
moda <- function(vector) {
  f.abs <- table(vector);
  max.f.abs <- max(f.abs);
  pos.max <- which(f.abs == max.f.abs);
  print('La(s) moda(s) es(son): ' );
  print(names(f.abs[pos.max]));
  paste('Con una frecuencia de: ', unique(f.abs[pos.max]));
}

x <- sample(30:80, 50, replace = T)
table(x);
moda(x);

# Ejemplo 3. Funciones na.omit y complete.cases
# ---------------------
# Considerar el dataset airquality (incluído en el paquete "Base R datasets")
# Primeras filas
head(airquality);

# Tipo de objeto y tipos de las variables
str(airquality);

# Dimensión
dim(airquality);

# Filas sin NA
filas.ok <- complete.cases(airquality);
sum(filas.ok);
airquality[filas.ok,];

# Obtener un rango de columnas
str(data);
apply(data, 2, mean);
apply(data, 2, mean, na.rm = T);

# na.omit devuelve el objeto sin datos na
na.omit(data);
(m1 <- apply(na.omit(data), 2, mean));
(b <- complete.cases(data));
(m2 <- apply(data[b,], 2, mean));
identical(m1, m2); # equiv a m1

# Ejemplo 4. Funciones cbind, rbind
# ---------------------
# cbind toma una sucesión de vectores, matrices o data frames
# y los combina por columnas
cbind(1:10, 11:20, 21:30);
cbind(1:10, matrix(31:50, ncol=2));
cbind(data.frame(x = 1:10, y = 11:20), z = 21:30);

# rbind funciona similar a cbind, pero combina por filas
df1 <- data.frame(x = 1:5, y = 6:10, z = 16:20);
df2 <- data.frame(x = 51:55, y = 101:105, z = 151:160);
rbind(df1, df2);

# Ejemplo 5. Funciones apply, lapply y do.call
# ---------------------
# Le aplica a un vector una función y lo retorna
x <- matrix(1:49, ncol = 7);
apply(x, 1, mean); # aplicar por filas
apply(x, 2, median); # aplicar por columnas

# lapply aplica una función a cada uno de los elementos
# de una lista o un vector
library(DescTools);
lapply(c(2,4,6,3,5),  IsOdd)

# Leer ágilmente un conjunto de csv
setwd('C:/Users/abreg/Documents/DS-BEDU/Módulo 2/Sesión 2/soccer/')
u1011 <- "https://www.football-data.co.uk/mmz4281/1011/SP1.csv"
u1112 <- "https://www.football-data.co.uk/mmz4281/1112/SP1.csv"
u1213 <- "https://www.football-data.co.uk/mmz4281/1213/SP1.csv"
u1314 <- "https://www.football-data.co.uk/mmz4281/1314/SP1.csv"

download.file(url = u1011, destfile = "SP1-1011.csv", mode = "wb");
download.file(url = u1112, destfile = "SP1-1112.csv", mode = "wb");
download.file(url = u1213, destfile = "SP1-1213.csv", mode = "wb");
download.file(url = u1314, destfile = "SP1-1314.csv", mode = "wb");

# Ver archivos descargados
dir();

# leer todos los archivos de dir()
lista <- lapply(dir(), read.csv);

# seleccionar solo algunas columnas
lista <- lapply(lista, select, Date:FTR);
head(lista[[1]]);

# Combinar cada uno de los elementos en lista
data <- do.call(rbind, lista);
head(data);
dim(data);

# Ejemplo 6. Paquete dplyr y aplicaciones
# ---------------------
# importar dplyr
library(dplyr);

# Descargar datos del cobis
url1 <- "https://data.humdata.org/hxlproxy/data/download/time_series_covid19_confirmed_global_narrow.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv"
url2 <- "https://data.humdata.org/hxlproxy/data/download/time_series_covid19_deaths_global_narrow.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv"

download.file(url1, 'covid19.confirmados.csv', mode = 'wb');
download.file(url2, 'covid19.muertes.csv', mode = 'wb');

conf <- read.csv('covid19.confirmados.csv');
dec <- read.csv('covid19.muertes.csv');

# Descartar la primera fila de los archivos
Sconf <- conf[-1,];
Sdec <- dec[-1,];

# Seleccionar únicamente país, fecha y número de casos acumulados
Sconf <- select(Sconf, Country.Region, Date, Value);

# Renombrar encabezados de columnas
Sconf <- rename(Sconf, Country = Country.Region,
                Fecha = Date, Infectados = Value);
# equiv a select(Sconf, Country = Country.Region,
#               Fecha = Date, Infectados = Value);
str(Sconf);

# Cambiar el tipo de las columnas
Sconf <- mutate(Sconf, Fecha = as.Date(Fecha, '%Y-%m-%d'),
                Infectados = as.numeric(Infectados));

# Análogamente con el data frame de decesos
Sdec <- select(Sdec, Country = Country.Region,
               Fecha = Date, Decesos = Value);

Sdec <- mutate(Sdec, Fecha = as.Date(Fecha, '%Y-%m-%d'),
               Decesos = as.numeric(Decesos));
str(Sdec);

# Unir data frames por una columna común
Scm <- merge(Sconf, Sdec);
str(Scm);

# Filtrar para el primer día de infectados en México
mex <- filter(Scm, Country == 'Mexico');
mex <- filter(mex, Infectados != 0);

# Agregar nuevas columnas
mex <- mutate(mex, NI = c(1, diff(Infectados))); # Nuevos infectados
mex <- mutate(mex, ND = c(0, diff(Decesos))); # Nuevos decesos
mex <- mutate(mex, Letalidad = round(Decesos / Infectados * 100, 1)); # Tasa de letalidad
mex <- mutate(mex, IDA = lag(Infectados), DDA = lag(Decesos)); # Valores día anterior
mex <- mutate(mex, FCI = Infectados/IDA, FCD = Decesos/DDA); # Factores de Crecimiento
mex <- mutate(mex, Dia = 1:dim(mex)[1]); # Días de contingencia

# Guardar archivo
write.csv(mex, "C19Mexico.csv", row.names = FALSE);

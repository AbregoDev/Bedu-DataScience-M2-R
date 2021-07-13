# Work 1. RETOS

# Reto 1
# ---------------------
# Abrir el fichero que se encuentra en el respositorio
# y obtener sus características, para finalmente imprimir
# los resultados en un fichero CSV.

# Leer el archivo netflix_titles.csv desde GitHub, almacenarlo en un df llamado netflix.
netflix <- read.csv('https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-01/Data/netflix_titles.csv');

# Obtener la dimensión y el tipo de objeto que se obtiene.
dim(netflix);
class(netflix);

# Obtener los títulos que se estrenaron después del 2015. Almacenar este df
# en una variable llamada net.2015.
releases.after.2015 <- netflix[netflix$release_year > 2015, ];
net.2015 <- releases.after.2015$title;

# Escribir los resultados en un archivo .CSV llamado res.netflix.csv
setwd('C:/Users/abreg/Documents/DS-BEDU/Módulo 2/Sesión 1');
write.csv(net.2015, 'res.netflix.csv');

# Reto 2
# ---------------------
# Utiliza el data frame de ventas de libros por Amazon y realiza las siguientes
# actividades

# Almacenar en un data frame que se llame amazon.best
amazon.best <- read.csv("https://raw.githubusercontent.com/ecoronadoj/Sesion_1/main/Data/bestsellers%20with%20categories.csv")

# Calcular el data frame transpuesto, asignarle el nombre de tAamazon
# y convertirlo en un data frame
tAmazon <- as.data.frame(t(amazon.best));

# Usar el nombre de los libros como el nombre de las columnas
colnames(tAmazon) <- tAmazon[1,];
colnames(tAmazon);

# Libro de menor y mayor precio
which.max(tAmazon["Price",]);
which.min(tAmazon["Price",]);

# Reto 3
# ---------------------
# Agregar las instrucciones generales del reto

# Generar un vector de 44 entradas (aleatorias), llamado ran
ran <- c();

for(i in 1:44) {
  ran[i] <- rnorm(1);
}

# Escribir un loop que eleve al cubo las pimeras 15 entradas
# y les sume 12
aux <- c();
for(k in 1:15) {
  aux[k] <- ran[k]**3 + 12;
}

# Guarda el resultado en un data frame, donde la primera columna
# sea el número aleatorio y la segunda el resultado. Nómbralo df.al
df.al <- data.frame(ran[1:15], aux);

# Escribir el pseudocódigo del loop anterior
# CICLO_PARA(índice desde 1 hasta 15 paso 1)
#   aux[k] asignar ran[k] elevado al cubo más 12
# FIN_CICLO_PARA
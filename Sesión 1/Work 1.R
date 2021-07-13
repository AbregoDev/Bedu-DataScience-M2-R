# Work 1

# Ejemplo 1: Tipos de datos y vectores
# ---------------------
# Declaración de algunas variables
var.hola <- 'Cadena';
var.number <- 4L;
var.double <- 2.69;
var.logical <- T;
vector <- c(1,2,3,4,5);

# Tipología de variables con class
class(var.hola);
class(var.number);
class(var.double);
class(var.logical);
class(vector);

# Class y typeof son distintos
# No entendí cómo funcionan xd
typeof(var.number);
typeof(var.double);
typeof(var.logical);
typeof(vector);

# Longitud de un vector
a <- c(4, 6, 8, 10, 12);
b <- c(3, 5, 7, 9);
length(a);
length(b);

# Acceder por índice (1-indizados)
a[2];
b[4];

# Unión de dos vectores
a.plus.b <- c(a, b);
a.plus.b[8];

# Ordenar vectores
sort(a.plus.b, decreasing = F);
sort(a.plus.b, decreasing = T);

# Generar vectores rápidamente
3:14;
d <- 10:3;
e <- 5:16;

# Generar vectores no sucesivos
f <- seq(from = 0, to = 20, by = 3);
(g <- seq(from = 5, to = 35, by = 5));

# Vector con un número n veces
(h <- rep(7, times = 9));

# Vector con otro vector n veces
vector.repetido <- rep(a, 5);
vector.repetido[16];

# Suma de vectores de igual longitud
c(1, 3, 5) + c(5, 3, 2);

# Suma de vectores de diferente longitud
# Longitud del corto debe dividir a la longitud del largo
c(1, 3, 5) + c(5, 3, 2, 8); # Error
c(1, 3, 5) + c(5, 3, 2, 8, 1, 0); # Ta, bien (R alarga el vector corto)

# Operaciones entre vectores
vec1 <- c(1, 5, 9, 3);
vec2 <- c(7, 6, 3, 4);

vec1 + vec2;
vec1 - vec2;
vec1 * vec2;
vec1 / vec2;
vec1^2;
3*vec2;

# Ejemplo 2: Matrices
# ---------------------
# Crear matrices
(m <- matrix(1:9, nrow = 3, ncol = 3));

# Obtener una posición
m[1,1];

# Obtener toda la columna
m[,1];

# Obtener toda la fila
m[1,];

# Suma de un vector y una matriz
# Al parecer, puesto que c() genera una columna, suma a cada columna de la matriz el contenido de c
# siempre que esta sea de la misma dimensión o múltiplo
(sum.vecmat <- c(1) + m)

# Dimensión matriz
(n <- matrix(2:7, 4, 6));
dim(n);

# Subconjunto de una matriz
# Funciona como un filtro pues devuelve un vector
sub.mat <- n[n > 4];
length(sub.mat);

# Obtener posiciones cuyo elemento verifica la condición
which(n > 4);

# Producto matricial
A <- matrix(2:10, 3, 3);
B <- matrix(seq(3, 35, 4), 3, 3);
A %*% B;

# Producto elemento a elemento
A * B;

# Transpuesta
t(A);

# Determinante
det(B);

# Extraer diagonal
diag(A);

# Resolver sistema Ax=b
A <- matrix(c(1, 5, 7, 2), 2, 2);
b <- c(1, 6);
solve(A, b);

# Inversa
solve(A);

# Eigenvalores y eigenvectores
eigen(A);

# Ejemplo 3: Listas y data frames
# ---------------------
# Listas
(mi.lista <- list(nombre = "Pepe", no.hijos = 3, edad.hijos = c(4, 7, 9)));
str(mi.lista);

# Acceder a elementos de la lista por clave
mi.lista$nombre;
mi.lista$no.hijos;

# Data frames
(x <- 10:21);
(y <- letters[x]);
(mydf <- data.frame(edad = x, grupo = y));
str(mydf);

# Acceder a la info del data frame
# Se puede realizar como si fuera una matriz (índice), o como una lista (clave)
mydf[1];
mydf[,1];
mydf$edad;

# Mensaje
paste("La media de la edad es: ", mean(mydf$edad));

# Detalles del data frame
summary(mydf);
dim(mydf);

# Añadir columna al data frame
mydf$sexo <- c("H", "M", "H", "M", "H", "H", "M", "H","H","M", "M", "H");

# Eliminar columna
mydf$sexo <- NULL
dim(mydf);

# Ejemplo 4: Descarga y lectura de data sets.
# ---------------------
# Directorio de trabajo actual
getwd();

# Establecer el directorio de trabajo
setwd("C:/Users/abreg/Documents/DS-BEDU/Módulo 2/Sesión 1/archive");

# Leer CSV desde comandos
read.csv("colors.csv");

# Guardar el CSV en un objeto
lego.colors <- read.csv("colors.csv");
str(lego.colors);
head(lego.colors);
tail(lego.colors);
class(lego.colors); # Data frame
typeof(lego.colors); # List
dim(lego.colors); # 135 x 4

# Leer CSV desde URL
data.url <- read.csv("https://www.football-data.co.uk/mmz4281/2021/SP1.csv");
str(data.url);
head(data.url);
tail(data.url);
dim(data.url);

# Ejemplo 5: Instalar paquetes y cargarlos
# ---------------------
# Instalación y carga de ggplot2
install.packages('ggplot2');
library('ggplot2');

# Versión de R. útil para revisar compatibilidad con paquetes
version;

# Ejemplo 6: Loops y pseudocódigo
# ---------------------
# Elevar al cuadrado un vector cualquiera mediante for
w <- c(2, 4, 6, 8, 10);
print('Loop que calcula el cuadrado de los elementos en el vector w');
for(i in 1:length(w)) {
  w.sq <- w[i]**2;
  print(w.sq);
}

# Ejemplo while
count <- 0;
while(count < 10) {
  print(count);
  count <- count + 1;
}

# Ejemplo if
(x <- runif(1, 0, 10)); # Número aleatorio entre 0 y 10
if(x > 5) {
  y <- T;
  print(paste(y, ", x=", round(x,2), "> 5"));
} else {
  y <- F;
  print(paste(y, ", x=", round(x,2), "<= 5"));
  
}
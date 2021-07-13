# Prework 1

# Imprimir en pantalla
print('Hello');

# Instalar paquetes
install.packages('dplyr');

# Importar el paquete instalado
library('dplyr');

# Obtener tipo de dato
is.integer(4); # TRUE
is.integer('fdsaf'); # FALSE
is.integer('431'); # FALSE
is.character(23); # FALSE
is.character('hola'); # TRUE
class(4); # Numerica
class('fs'); # Character
typeof(1+2i); # Complex
typeof(4.7); # Double

# Vectores 
c(2, 4, 2, 5, 2);
c(2, 4, 'a'); # Creará un vector de caracteres

# Listas (similares a los vectores, pero soporta varios tipos
# en una sola estructura)
list(24, 'a', 2+1i, c(1,2,3));

# Matrices. Un tipo de dato para toda la matriz
matrix(c(1, 2, 4, 1), 2);
matrix(1:12, byrow = TRUE, 3)
matrix(1:12, byrow = FALSE, 2)

# DataFrames. Arreglos bidimensionales multitipo

# Max. Obtiene el número máximo
max(c(3/3, 1/6, 4, 35, 3, 4896/369));

# Variables.
x <- (1+1/500)^500; # Aprox. de e a 2 decimales

# Función mean para promedio
mean(c(4, 2, 6, 3, 5, 7));

# Definición de una función
miFuncion <- function(a, b, c) {
  paso.1 <- (a+b)*4 + 2*4;
  paso.2 <- paso.1*2 + c/2;
  paso.3 <- paso.2*3;
  return(paso.3);
}
print(miFuncion(1, 4, 9));

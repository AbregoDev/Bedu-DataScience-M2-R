# Work 3

# Ejemplo 1. Ggplot 2
# --------------------
# Importar ggplot2
library(ggplot2);

# Se usará el dataset mtcars
dim(mtcars);
str(mtcars);
names(mtcars);

# Graficar cyl en el eje x y hp en el y
ggplot(mtcars, aes(x = cyl, y = hp, colour = mpg)) +
    geom_point();

# Agregando tema y facewrap
ggplot(mtcars,
       aes(x = cyl, y = hp, colour = mpg)) +
  geom_point() +
  theme_minimal() +
  facet_wrap("cyl");

# Etiquetas en los ejes
ggplot(mtcars,
       aes(x = cyl, y = hp, colour = mpg)) +
  geom_point() +
  theme_light() +
  facet_wrap("cyl") +
  xlab('Núm. Acilindros') +
  ylab('Caballos de Fyerza');


# Ejemplo 2. Histogramas
# --------------------------
# Importar el dataset y bibliotecas necesarias
library(dplyr);
setwd('C:/Users/abreg/Documents/DS-BEDU/Módulo 2/Sesión 3');
getwd();
data <- read.csv('C:/Users/abreg/Documents/DS-BEDU/Módulo 2/Sesión 3/Data/boxp.csv');
head(data);
names(data);

# Modificamos una columna para ocultar datos reales en caso de compartirlos
data <- mutate(data, Mediciones = Mediciones*1.23);
head(data)
data <- mutate(data, Mediciones = Mediciones/1.23);
head(data)

# Histograma con función hist
hist(data$Mediciones,
     breaks = (seq(0, 300, 50)),
     main = "Histograma de Mediciones",
     xlab = "Mediciones",
     ylab = "Frecuencia");

# Histograma con ggplot
library(ggplot2)
# Omitir los NA
data <- na.omit(data);
data %>%
  ggplot() +
  aes(Mediciones) +
  geom_histogram(binwidth = 50);

# Vista personalizada
data %>%
  ggplot() +
  aes(Mediciones) +
  geom_histogram(binwidth = 20, col = "#485311", fill = "#ACC321") +
  ggtitle("Histograma de Mediciones") +
  ylab("Frecuencia") +
  xlab("Mediciones") +
  theme_light();

# Ejemplo 3. Gráficos de dispersión
# -------------------------------
# Scatter plot de las variables wt y mpg del data set mtcars
(my_scatplot <- ggplot(mtcars,
                      aes(x = wt, y = mpg)) + geom_point());

# Agregar línea de tendencia
(my_scatplot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
                geom_point() +
                geom_smooth(method = "lm", se = T));

# Agregar nombre de los ejes
(my_scatplot + xlab('Weight (x 1000lbs)') + ylab('Miles per Galoon'));

# Agregar labels mediante labs()
(my_scatplot <- ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) + geom_point())
my_scatplot + labs(x = 'Weight (x1000lbs)', y = 'Miles per Gallon', colour = 'Number of\n Cylinders')

# Dividir la gráfica por el número de cilindros
(my_scatplot + facet_wrap('cyl'));

# Dividir por transmisión
(my_scatplot + facet_grid(am~cyl));

# Ejemplo 4. Boxplots y outliers
# -------------------------------
data <- read.csv('C:/Users/abreg/Documents/DS-BEDU/Módulo 2/Sesión 3/Data/boxp.csv');
summary(data);

# Eliminar NAs
data <- na.omit(data);

# Convertir Categoria y Grupo en factor
data <- mutate(data, Categoria = factor(Categoria), Grupo = factor(Grupo))
str(data);
head(data);

# Distribución de los datos por grupo y categoría
(ggplot(data, aes(x = Categoria, y = Mediciones, fill = Grupo)) +
  geom_boxplot() +
  scale_fill_discrete(name = "Dos Gps", labels = c("G1", "G2")) +
  ggtitle("Boxplots") +
  xlab("Categorías") +
  ylab("Mediciones"));

# Ejemplo 5. Diversos gráficos de tendencias: COVID-19
# ------------------------------
# Importar bibliotecas necesarias
library(scales);
library(dplyr);
library(ggplot2);

# Leer datos covid
setwd('C:/Users/abreg/Documents/DS-BEDU');
mex <- read.csv('./Módulo 2/Sesión 2/COVID19/C19Mexico.csv');
head(mex);
tail(mex);
str(mex);
mex <- mutate(mex, Fecha = as.Date(Fecha, '%Y-%m-%d'));

# Acumulado de casos confirmados
p <- ggplot(mex, aes(x=Fecha, y=Infectados)) + geom_line( color="blue") + geom_point() + labs(x = "Fecha", y = "Acumulado de casos confirmados", title = paste("Confirmados de COVID-19 en México:", format(Sys.time(), tz="America/Mexico_City", usetz=TRUE))) + theme(plot.title = element_text(size=12)) + theme(axis.text.x = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1), axis.text.y = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1));

# Formato de fechas en el eje x
p <- p + scale_x_date(labels = date_format("%d-%m-%Y"));

# Tema
p <- p + theme(plot.margin=margin(10,10,20,10), plot.caption=element_text(hjust=1.05, size=10)) + annotate("text", x = mex$Fecha[round(dim(mex)[1]*0.4)], y = max(mex$Infectados), colour = "blue", size = 5, label = paste("Última actualización: ", mex$Infectados[dim(mex)[1]]));

# Casos confirmados por día
(p <- ggplot(mex, aes(x=Fecha, y=NI)) + geom_line(stat = "identity") + labs(x = "Fecha", y = "Incidencia (Número de casos nuevos)", title = paste("Casos de Incidencia de COVID-19 en México:", format(Sys.time(), tz="America/Mexico_City", usetz=TRUE))) + theme(plot.title = element_text(size=12)) + theme(axis.text.x = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1), axis.text.y = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1)));

# Formato de fechas en el eje x
(p <- p + scale_x_date(labels = date_format("%d-%m-%Y")));

# Kien sabe k le cambiaron
(p <- p + theme(plot.margin=margin(10,10,20,10), plot.caption=element_text(hjust=1.05, size=10)) + annotate("text", x = mex$Fecha[round(dim(mex)[1]*0.4)], y = max(mex$NI), colour = "blue", size = 5, label = paste("Última actualización: ", mex$NI[length(mex$NI)])));

# Tomar solo los decesos
mexm <- subset(mex, Decesos > 0)

# Muertes por COVID19
(p <- ggplot(mexm, aes(x=Fecha, y=Decesos)) + geom_line( color="red") + geom_point() + labs(x = "Fecha", y = "Muertes acumuladas", title = paste("Muertes por COVID-19 en México:", format(Sys.time(), tz="America/Mexico_City",usetz=TRUE))) + theme(axis.text.x = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1), axis.text.y = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1)));
(p <- p + scale_x_date(labels = date_format("%d-%m-%Y")));
(p <- p + theme(plot.margin=margin(10,10,20,10), plot.caption=element_text(hjust=1.05, size=10)) + annotate("text", x = mexm$Fecha[round(dim(mexm)[1]*0.4)], y = max(mexm$Decesos), colour = "red", size = 5, label = paste("Última actualización: ", mexm$Decesos[dim(mexm)[1]])));

# Muertes por día
(p <- ggplot(mexm, aes(x=Fecha, y=ND)) + geom_line(stat = "identity") + labs(x = "Fecha", y = "Número de nuevos decesos", title = paste("Nuevos decesos por COVID-19 en México:", format(Sys.time(), tz="America/Mexico_City",usetz=TRUE))) + theme(plot.title = element_text(size=12)) + theme(axis.text.x = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1), axis.text.y = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1)));
(p <- p + scale_x_date(labels = date_format("%d-%m-%Y")));
(p <- p + theme(plot.margin=margin(10,10,20,10), plot.caption=element_text(hjust=1.05, size=10)) + annotate("text", x = mexm$Fecha[round(dim(mexm)[1]*0.2)], y = max(mexm$ND), colour = "red", size = 5, label = paste("Última actualización: ", mexm$ND[dim(mexm)[1]])));

# Acumulados de casos confirmados y muertes
(p <- ggplot(mex, aes(x=Fecha, y=Infectados)) + geom_line(color="blue") + labs(x = "Fecha", y = "Acumulado de casos", title = paste("COVID-19 en México:", format(Sys.time(), tz="America/Mexico_City",usetz=TRUE))) + geom_line(aes(y = Decesos), color = "red") + theme(axis.text.x = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1), axis.text.y = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1)));
(p <- p + scale_x_date(labels = date_format("%d-%m-%Y")));
(p <- p + theme(plot.margin=margin(10,10,20,10), plot.caption=element_text(hjust=1.05, size=10)) + annotate("text", x = mex$Fecha[round(dim(mex)[1]*0.4)], y = max(mex$Infectados), colour = "blue", size = 5, label = paste("Última actualización para Infectados:", mex$Infectados[dim(mex)[1]])) + annotate("text", x = mex$Fecha[round(dim(mex)[1]*0.4)], y = max(mex$Infectados)-150000, colour = "red", size = 5, label = paste("Última actualización para Muertes:", mex$Decesos[dim(mex)[1]])));

# Tasa de letalidad
(p <- ggplot(mexm, aes(x=Fecha, y=Letalidad)) + geom_line(color="red") + labs(x = "Fecha", y = "Tasa de letalidad", title = paste("COVID-19 en México:", format(Sys.time(), tz="America/Mexico_City",usetz=TRUE))) + theme(axis.text.x = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1), axis.text.y = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1)) +
  scale_y_discrete(name ="Tasa de letalidad", limits=factor(seq(1, 13.5, 1)), labels=paste(seq(1, 13.5, 1), "%", sep = "")));

# Escala fechas
(p <- p + scale_x_date(labels = date_format("%d-%m-%Y")));

# Label
(p <- p + theme(plot.margin=margin(10,10,20,10), plot.caption=element_text(hjust=1.05, size=10)) + annotate("text", x = mexm$Fecha[round(length(mexm$Fecha)*0.5)], y = max(mexm$Letalidad)-1, colour = "red", size = 4, label = paste("Última actualización: ", mexm$Letalidad[dim(mexm)[1]], "%", sep = "")));

# Factor de crecimiento
# Se toman solo valores de crecimiento
mex <- filter(mex, FCD < Inf);

# Gráfico
(p <- ggplot(mex, aes(x=Fecha, y=FCI)) + geom_line(color="blue") + labs(x = "Fecha", y = "Factor de crecimiento", title = paste("COVID-19 en México:", format(Sys.time(), tz="America/Mexico_City",usetz=TRUE))) + geom_line(aes(y = FCD), color = "red") + theme(plot.title = element_text(size=12)) + theme(axis.text.x = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1), axis.text.y = element_text(face = "bold", color="#993333" , size = 10, angle = 45, hjust = 1)));
# Fecha eje X
(p <- p + scale_x_date(labels = date_format("%d-%m-%Y")));
# Label in plot
(p <- p + annotate("text", x = mex$Fecha[round(length(mex$Fecha)*0.4)], y = max(mex$FCD), colour = "blue", size = 5, label = paste("Última actualización para infectados: ", round(mex$FCI[dim(mex)[1]], 4))) + annotate("text", x = mex$Fecha[round(length(mex$Fecha)*0.4)], y = max(mex$FCD)-0.2, colour = "red", size = 5, label = paste("Última actualización para muertes: ", round(mex$FCD[dim(mex)[1]], 4))));


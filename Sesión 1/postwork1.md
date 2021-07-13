# Postwork 1
## This is a subtitle. Our team is Juan, Israel, Gil, Hugo, Fernando and Bryan.
### A smaller text

This is an interesting way to **document** the code.

## Objetivo
Reforzar la lectura de ficheros CSV y la escritura de variables resultado en un nuevo fichero CSV, de este modo será fácil su lectura en otros lenguajes o plataformas.

## Desarrollo

Importar los datos de soccer de la temporada 2019/2020 de la  1ra división de la liga española

```r
temporada <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv");
```

Extraer las columnas FTHG y FTAG
```r
goles = data.frame(locales = temporada$FTHG, visitantes = temporada$FTAG);
```

¿Cómo funciona table en R?
```r
?table;
```

Tablas de frecuencia
```r
freq.local = table(goles$locales, useNA = 'ifany');
local.df = as.data.frame(freq.local);
freq.visit = table(goles$visitantes, useNA = 'ifany');
visit.df = as.data.frame(freq.visit);
```

Probabilidad marginal de que el equipo local anote x goles
```r
names(local.df) <- c('Goles.L', 'Probabilidad');
local.df$Probabilidad = local.df$Probabilidad / length(goles$locales);
local.df$Probabilidad = round(local.df$Probabilidad * 100, 2);
local.df;
```

Probabilidad marginal de que el equipo visitante anote y goles
```r
names(visit.df) <- c('Goles.V', 'Probabilidad');
visit.df$Probabilidad = visit.df$Probabilidad / length(goles$visitantes);
visit.df$Probabilidad = round(visit.df$Probabilidad * 100, 2);
visit.df;
```

Probabilidad conjunta de que el equipo en casa anote x goles y el equipo que juega como visitante anote y goles
```r
x.size <- length(local.df$Goles.L);
y.size <- length(visit.df$Goles.V);
goles.x <- vector('integer', 0);
goles.y <- vector('integer', 0);
prob.x <- vector('numeric', 0);
prob.y <- vector('numeric', 0);
prob.x.y <- vector('numeric', 0);

i <- 1;
while(i <= x.size) {
  j <- 1;
  while(j <= y.size) {
    goles.x <- append(goles.x, as.integer(local.df$Goles.L[i]) - 1);
    goles.y <- append(goles.y, as.integer(visit.df$Goles.V[j]) - 1);
    prob.x <- append(prob.x, local.df$Probabilidad[i]);
    prob.y <- append(prob.y, visit.df$Probabilidad[j]);
    probConjunta <-
      round((local.df$Probabilidad[i] * visit.df$Probabilidad[j]) / 100, 2);
    prob.x.y <- append(prob.x.y, probConjunta);
    
    j <- j + 1;
  }
  i <- i + 1;
}

(loc.visit.df <- data.frame('Goles.L' = goles.x,
                           'Goles.V' = goles.y,
                           'Prob.L' = prob.x,
                           'Prob.V' = prob.y,
                           'Prob.L.V' = prob.x.y));
```
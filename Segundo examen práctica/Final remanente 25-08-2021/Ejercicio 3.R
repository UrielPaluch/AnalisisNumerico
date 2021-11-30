# Uriel Paluch
# 895700
# Ejercicio 3


# Configuracion del entorno -----------------------------------------------
options(scipen = 999)

# Librerias ---------------------------------------------------------------
library(tidyverse)

# Métodos -----------------------------------------------------------------
Cinco_puntos <- function(x, fx){
  n <- length(x)
  
  fprima <- rep(NA, times = n)
  
  h <- x[2] - x[1]
  
  #Punto medio
  for (i in 3:(n-2)) {
    fprima[i] <- (1/(12*h))*(fx[i-2]-8*fx[i-1]+8*fx[i+1]-fx[i+2])
  }
  
  tabla <- data.frame(x, fx, fprima)
  
  return(tabla)
}

SegundaDerivada <- function (x, fx){
  n <- length(x)
  
  fprima <- rep(NA, times = n)
  
  h <- x[2] - x[1]
  
  #Punto medio
  for (i in 2:(n-1)) {
    fprima[i] <- (1/(h^2))*(fx[i-1]-2*fx[i]+fx[i+1])
  }
  
  tabla <- data.frame(x, fx, fprima)
  
  return(tabla)
  
}

# Ejercicio 3 Derivación Numérica -----------------------------------------

df <- data.frame(r = seq(from = 0, to = 0.15, by = 0.01), P = c(115, 109.7069, 104.7135, 100, 95.5482, 91.341, 87.3629, 83.5992, 80.0364, 76.6621, 73.4645, 70.4328, 67.557, 64.8277, 62.2361, 59.7741))


# 3.1 Estime P'(X) --------------------------------------------------------
Cinco_puntos(x = df$r, fx = df$P)
# Para calcular i necesito i-2, i-1, i+1, i+2.
# Por lo tanto, voy a poder calcular con este método a partir del tercer valor
# Y hasta el valor n-2. Siendo n la cantidad de datos.


# Estime P''(x) -----------------------------------------------------------
SegundaDerivada(x = df$r, fx = df$P)

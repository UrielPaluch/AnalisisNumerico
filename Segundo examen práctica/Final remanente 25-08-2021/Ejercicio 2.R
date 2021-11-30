# Uriel Paluch
# 895700
# Ejercicio 2


# Configuracion del entorno -----------------------------------------------
options(scipen = 999)

# Librerias ---------------------------------------------------------------
library(tidyverse)

# Métodos -----------------------------------------------------------------

Trapecio <- function(limiteInferior, limiteSuperior, funcion, n){
  #browser()
  h <- (limiteSuperior - limiteInferior)/n
  
  fx <- rep(NA, times = (n+1))
  for (i in 1:(n+1)) {
    fx[i] <- eval(funcion, list(x = limiteInferior + (i-1)*h))
  }
  
  # Hay que cambiarlo para que quede solo con un 1
  if (n == 1){
    return((h/2) * (fx[1] + fx[2]))
  }
  
}
# n = 1. Regla del trapecio.
# Poner la funcion con "x" como incognita


Simpson <- function(limiteInferior, limiteSuperior, funcion, n){
  h <- (limiteSuperior - limiteInferior)/n
  
  fx <- rep(NA, times = (n+1))
  for (i in 1:(n+1)) {
    fx[i] <- eval(funcion, list(x = limiteInferior + (i-1)*h))
  }
  
  if (n == 2){
    return((h/3) * (fx[1] + 4*fx[2] + fx[3]))
  }
  
}
# n = 2. Regla de Simpson.
# Poner la funcion con "x" como incognita

Simpson_3_8 <- function(limiteInferior, limiteSuperior, funcion, n){
  h <- (limiteSuperior - limiteInferior)/n
  
  fx <- rep(NA, times = (n+1))
  for (i in 1:(n+1)) {
    fx[i] <- eval(funcion, list(x = limiteInferior + (i-1)*h))
  }
  
  if(n == 3){
    return((3/8)*h*(fx[1] + 3*fx[2] + 3*fx[3] + fx[4]))
  }
  
}
# n = 3. Regla de tres octavos de Simpson.
# Poner la funcion con "x" como incognita

SimpsonCompuesta <- function(limiteInferior, limiteSuperior, funcion, n, cantIntervalos){
  
  #browser()
  if ((n == 2 || n == 0) && cantIntervalos%%2 != 0){
    return("cantIntervalos debe ser un entero par")
  }
  
  cantIntervalos <- cantIntervalos/n
  
  
  crecimientoIntervalo <- (limiteSuperior-limiteInferior)/cantIntervalos
  
  fx <- rep(NA, times = (n+1))
  
  resultado <- 0
  
  for (i in 1:cantIntervalos) {
    limiteSuperior <- limiteInferior + crecimientoIntervalo
    
    if (n != 0){
      h <- (limiteSuperior - limiteInferior)/n
    }
    
    
    for (i in 1:(n+1)) {
      fx[i] <- eval(funcion, list(x = limiteInferior + (i-1)*h))
    }
    
    if(n == 2){
      resultado <- resultado + (h/3) * (fx[1] + 4*fx[2] + fx[3])
    }
    
    limiteInferior <- limiteSuperior
  }
  
  return(resultado)
}
# n = 2. Simpson

TrapecioCompuesta <- function(limiteInferior, limiteSuperior, funcion, n, cantIntervalos){
  
  if ((n == 2 || n == 0) && cantIntervalos%%2 != 0){
    return("cantIntervalos debe ser un entero par")
  }
  
  cantIntervalos <- cantIntervalos/n
  
  
  crecimientoIntervalo <- (limiteSuperior-limiteInferior)/cantIntervalos
  
  fx <- rep(NA, times = (n+1))
  
  resultado <- 0
  
  for (i in 1:cantIntervalos) {
    limiteSuperior <- limiteInferior + crecimientoIntervalo
    
    if (n != 0){
      h <- (limiteSuperior - limiteInferior)/n
    }
    
    
    for (i in 1:(n+1)) {
      fx[i] <- eval(funcion, list(x = limiteInferior + (i-1)*h))
    }
    
    # Trapecio
    if (n == 1){
      resultado <- resultado + (h/2) * (fx[1] + fx[2])
    }
    
    limiteInferior <- limiteSuperior
  }
  
  return(resultado)
}
# n = 1. Trapecio

# Ejercicio 2.1 -----------------------------------------------------------

# Resolución del ejercicio ------------------------------------------------
alfa <- 2.52
theeta <- 1.32

gamma <- SimpsonCompuesta(limiteInferior = 0, limiteSuperior = 100, funcion = expression((x^(alfa-1)) * exp(-x)), cantIntervalos = 1000, n = 2)

# grafico para ver que haya escrito bien la funcion
x <- seq(from = 0.01, to = 15, by = 0.01)
y <- eval(expression(((x/theeta)^(alfa) * exp(-x/theeta)) / (x * gamma)), list(x = x))

ggplot() +
  geom_line(aes(x = x, y = y))

# Trapecio ----------------------------------------------------------------
resultado <- Trapecio(limiteInferior = 0.25, limiteSuperior = 5, funcion = expression(((x/theeta)^(alfa) * exp(-x/theeta)) / (x * gamma)), n = 1)

print(paste("Resultado con el método del trapecio:",resultado))
print("Nodos y0 e y1")

# Simpson -----------------------------------------------------------------
resultado <- Simpson(limiteInferior = 0.25, limiteSuperior = 5, funcion = expression(((x/theeta)^(alfa) * exp(-x/theeta)) / (x * gamma)), n = 2)

print(paste("Resultado con el método de Simpson:",resultado))
print("Nodos y0, y1 e y2")


# Simpson 3/8 -------------------------------------------------------------
resultado <- Simpson_3_8(limiteInferior = 0.25, limiteSuperior = 5, funcion = expression(((x/theeta)^(alfa) * exp(-x/theeta)) / (x * gamma)), n = 3)

print(paste("Resultado con el método de Simpson 3/8:",resultado))
print("Nodos y0, y1, y2 e y3")




# Ejercicio 2.2 -----------------------------------------------------------
resultado <- SimpsonCompuesta(limiteInferior = 0.25, limiteSuperior = 5, funcion = expression(((x/theeta)^(alfa) * exp(-x/theeta)) / (x * gamma)), cantIntervalos = 20, n = 2)

print(paste("Resultado con el método de Simpson Compuesto:",resultado))
print("Nodos y0, y1, ..., y20")


# Cota del error ----------------------------------------------------------

x <- seq(from = 0.25, to = 5, by = 0.01)

fx <- eval(D(D(D(D(expression(((x/theeta)^(alfa) * exp(-x/theeta)) / (x * gamma)), "x"),"x"),"x"),"x"), list(x = x))

ggplot() +
  geom_line(aes(x = x, y = fx))

limiteSuperior <- 5
limiteInferior <- 0.25
n <- 20

h <- (limiteSuperior - limiteInferior)/n

((limiteSuperior - limiteInferior)/180) * (h^4) * max(abs(fx))

# Comparación del punto 2.1 y 2.2 -----------------------------------------
# Se diferencian principalmente en la cantidad de nodos que utilizan.

# 2.3 Esperanza matemática ------------------------------------------------

esperanza <- TrapecioCompuesta(limiteInferior = 10^-10, limiteSuperior = 100, funcion = expression( x*(( (x/theeta)^(alfa) * exp(-x/theeta)) / (x * gamma)) ), cantIntervalos = 1500, n = 1)

x <- seq(10^-10, 100, length.out = 10000)

fx <- eval(D(D(expression( x*(( (x/theeta)^(alfa) * exp(-x/theeta)) / (x * gamma)) ), "x"),"x"), list(x = x))

ggplot() +
  geom_line(aes(x = x, y = fx))

limiteSuperior <- 100
limiteInferior <- 10^-10
n <- 1500

h <- (limiteSuperior - limiteInferior)/n

((limiteSuperior - limiteInferior)/12) * (h^2) * max(abs(fx))


# 2.4 Esperanza Matemática de una función condicional --------------------

z_function <- function(y){
  if (y < 5){
    return(y)
  } else{
    return(5)
  }
}

x <- seq(from = 0, to = 25, by = 0.1)
y <- sapply(x, z_function)
plot(x,y)

SimpsonCompuesta(limiteInferior = 10^-10, limiteSuperior = 25, funcion = expression( z_function(x)*(( (x/theeta)^(alfa) * exp(-x/theeta)) / (x * gamma)) ), cantIntervalos = 1500, n = 2)

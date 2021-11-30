# Uriel Paluch
# 895700


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

Simpson <- function(limiteInferior, limiteSuperior, funcion, n){
  #browser()
  h <- (limiteSuperior - limiteInferior)/n
  
  fx <- rep(NA, times = (n+1))
  for (i in 1:(n+1)) {
    fx[i] <- eval(funcion, list(x = limiteInferior + (i-1)*h))
  }
  
  if (n == 2){
    return((h/3) * (fx[1] + 4*fx[2] + fx[3]))
  }
  
}

Simpson38 <- function(limiteInferior, limiteSuperior, funcion, n){
  h <- (limiteSuperior - limiteInferior)/n
  
  fx <- rep(NA, times = (n+1))
  for (i in 1:(n+1)) {
    fx[i] <- eval(funcion, list(x = limiteInferior + (i-1)*h))
  }
  
  if(n == 3){
    return((3/8)*h*(fx[1] + 3*fx[2] + 3*fx[3] + fx[4]))
  }
  
}

TrapecioCompuesta <- function(limiteInferior, limiteSuperior, funcion, n, cantIntervalos){
  
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
    
    # Trapecio
    if (n == 1){
      resultado <- resultado + (h/2) * (fx[1] + fx[2])
    }
    
    limiteInferior <- limiteSuperior
  }
  
  return(resultado)
}



# 1. Integracion ----------------------------------------------------------
alfa <- 4.36
theeta <- 2.52
funcion <- expression( (alfa^2 * (x/theeta)^alfa)/(x*(1+(x/theeta)^alfa)^(alfa+1)) )

# 1.1 Probabilidades simple -----------------------------------------------
# Trapecio
Trapecio(limiteInferior = 1.15, limiteSuperior = 4.99, funcion = funcion, n = 1)
# Nodos y0, y1

# Simpson
Simpson(limiteInferior = 1.15, limiteSuperior = 4.99, funcion = funcion, n = 2)
# Nodos y0, y1, y2

# Simpson 3/8
Simpson38(limiteInferior = 1.15, limiteSuperior = 4.99, funcion = funcion, n = 3)
# Nodos y0, y1, y2, y3


# 1.2 Probabilidades compuesto --------------------------------------------

TrapecioCompuesta(limiteInferior = 1.15, limiteSuperior = 4.99, funcion = funcion, n = 1, cantIntervalos = 17)
# Nodos y0, y1

# Cota del error
x <- seq(1.15, 4.99, length.out = 10000)

fx <- eval(D(D(funcion, "x"),"x"), list(x = x))


limiteSuperior <- 4.99
limiteInferior <- 1.15
n <- 17

h <- (limiteSuperior - limiteInferior)/n

((limiteSuperior - limiteInferior)/12) * (h^2) * max(abs(fx))

# El resultado es mas exacto porque trapecio compuesto genera cierta cantidad de intervalos, en este caso 17,
# Y genera un trapecio por intervalo, por consecuencia, la cota del error es menor.


# 1.3 Esperanza -----------------------------------------------------------



e1 <- TrapecioCompuesta(limiteInferior = 10^-10, limiteSuperior = 100, funcion = expression( x * ((alfa^2 * (x/theeta)^alfa)/(x*(1+(x/theeta)^alfa)^(alfa+1))) ), n = 1, cantIntervalos = 474)
e1

# Cota del error
x <- seq(10^-10, 100, length.out = 10000)

fx <- eval(D(D(expression( x * ((alfa^2 * (x/theeta)^alfa)/(x*(1+(x/theeta)^alfa)^(alfa+1))) ), "x"),"x"), list(x = x))


limiteSuperior <- 10^-10
limiteInferior <- 100
n <- 474

h <- (limiteSuperior - limiteInferior)/n

((limiteSuperior - limiteInferior)/12) * (h^2) * max(abs(fx))


# 1.4 Varianza ------------------------------------------------------------

# E[Y^2]
e2 <- TrapecioCompuesta(limiteInferior = 10^-10, limiteSuperior = 100, funcion = expression( (x^2) * ((alfa^2 * (x/theeta)^alfa)/(x*(1+(x/theeta)^alfa)^(alfa+1))) ), n = 1, cantIntervalos = 474)

e2 - e1^2

# Cota del error
x <- seq(10^-10, 100, length.out = 10000)

fx <- eval(D(D(expression( (x^2) * ((alfa^2 * (x/theeta)^alfa)/(x*(1+(x/theeta)^alfa)^(alfa+1))) ), "x"),"x"), list(x = x))


limiteSuperior <- 10^-10
limiteInferior <- 100
n <- 474

h <- (limiteSuperior - limiteInferior)/n

((limiteSuperior - limiteInferior)/12) * (h^2) * max(abs(fx))
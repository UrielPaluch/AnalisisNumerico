
# Configuracion -----------------------------------------------------------

options(scipen=999)

# Metodo de Newton --------------------------------------------------------

Broyden <- function(x, TOL, N = 100, ecuaciones){
  # x: aproximación inicial
  # TOL: tolerancia
  # N: cantidad maxima de iteraciones
  
  # Instancio las variables -------------------------------------------------
  # n: número de ecuaciones e incognitas
  n <- length(ecuaciones)
  # funciones valuadas en cero
  f0 <- rep(NA, n)
  jacobiano <- matrix(rep(NA, n*n), nrow = n, ncol = n)
  
  # Recorro las filas
  for (i in 1:n) {
    #Evaluo las ecuaciones 
    f0[i] <- eval(ecuaciones[i], x)
    # Recorro las columnas
    for (j in 1:n){
      # Derivo en cada variable y evaluo
      jacobiano[i,j] <- eval((D(ecuaciones[i], glue::glue("x",j))), x)
    }
  }
      
  # Calcula la inversa de una matriz
  A <- solve(jacobiano)
  v <- f0
  s <- -A %*% v
  
  x <- unlist(x, use.names = FALSE) + s
  
  for (max in 1:N) {
    w <- v
    
    # listo las x
    x0 <- list()
    for (i in 1:n) {
      x0[glue::glue("x",i)] <- x[i]
    }
    x <- x0
    for (i in 1:n) {
      #Evaluo las ecuaciones 
      f0[i] <- eval(ecuaciones[i], x)
    }
    v <- f0 
    
    y <- v - w
    
    z <- -A%*%y
    
    p <- -t(s) %*% z
    
    u <- t(s) %*% A
    u <- t(u)
    
    A <- A + (1/p)[1,1] * (s+z) %*% t(u)
    
    s <- -A%*%v
    x <- unlist(x, use.names = FALSE) + s
    
    norma <- norm(s,type = 'M')
    if (norma < TOL) {
      return(x)
    }
  }
  return("Numero de iteraciones maximo excedido")
}


# Llamo a la funcion ------------------------------------------------------
# IMPORTANTE: declarar las funciones con x1, x2, ..., xn
print(Broyden(x = list(x1 = 0.1, x2 = 0.1), TOL = 0.0000001, 
             ecuaciones = c(
               expression(3*x1^2-x2^2),
               expression(3*x1*x2^2-x1^3-1)
             )
))



# Configuracion -----------------------------------------------------------

options(scipen=999)

# Metodo de Newton --------------------------------------------------------

newton <- function(x, TOL, N = 100, ecuaciones){
  # x: aproximación inicial
  # TOL: tolerancia
  # N: cantidad maxima de iteraciones
  
  # Instancio las variables -------------------------------------------------
  # n: número de ecuaciones e incognitas
  n <- length(ecuaciones)
  # funciones valuadas en cero
  f0 <- rep(NA, n)
  jacobiano = matrix(rep(NA, n*n), nrow = n, ncol = n)
  
  # Comienza el método ------------------------------------------------------
  for (max_reps in 1:N) {
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
    
    #Calcula la inversa de una matriz
    y0 <- solve(jacobiano) %*% (-f0)
    x <- y0 + unlist(x, use.names=FALSE)
    
    norma <- norm(y0,type = 'M')
    if (norma < TOL) {
      return(x)
    }
    
    x0 <- list()
    for (i in 1:n) {
      x0[glue::glue("x",i)] <- x[i]
    }
    x <- x0
    
  }
  
  return("Numero de iteraciones maximo excedido")
}


# Llamo a la funcion ------------------------------------------------------
# IMPORTANTE: declarar las funciones con x1, x2, ..., xn
print(newton(x = list(x1 = 0.1, x2 = 0.1, x3 = -0.1), TOL = 0.0001, 
       ecuaciones = c(
                      expression(3*x1-cos(x2*x3)-0.5),
                      expression(x1^2-81*(x2+0.1)^2+sin(x3)+1.06),
                      expression(exp(-x1*x2)+20*x3+(10*pi-3)/3)
                      )
))


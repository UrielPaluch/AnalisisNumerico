
# Configuración -----------------------------------------------------------
options(scipen = 999)


# Lagrange con funcion --------------------------
PolinomioLagrange2 <- function(x, fx, y){
  
  n <- length(x)
  
  l <- rep("", times = n)
  
  resultado <- 0
  
  for (i in 1:n) {
    l[i] <- glue::glue(eval(fx, list("x" = x[i])))
    for (j in 1:n) {
      if (j != i){
        l[i] <- l[i] + glue::glue("*(x-",x[j],")/(",x[i],"-",x[j],")")
      }
    }
  }
  
  for(i in 1:n){
    resultado <- resultado + eval(parse(text=l[i]), y)
  }
  return(paste("El resultado es: ", resultado))
}


# Lagrange con puntos -----------------------------------------------------

PolinomioLagrange <- function(x, fx, y){
  
  n <- length(x)
  
  l <- rep("", times = n)
  
  resultado <- 0
  
  for (i in 1:n) {
    l[i] <- fx[i]
    for (j in 1:n) {
      if (j != i){
        l[i] <- l[i] + glue::glue("*(x-",x[j],")/(",x[i],"-",x[j],")")
      }
    }
  }
  
  for(i in 1:n){
    resultado <- resultado + eval(parse(text=l[i]), y)
  }
  return(paste("El resultado es: ", resultado))
}

# Llamado a la función ----------------------------------------------------
# x es una lista con todos los valores
# fx es la funcion que hay que aproximar
# y es el valor donde se desea aproximar la función
print(PolinomioLagrange2(x = c(2, 2.75, 4), fx = expression(1/x), y = list(x = 3)))


# x es una lista con todos los valores
# fx es f(x)
# y es el valor donde se desea aproximar la función
print(PolinomioLagrange(x = c(0, 0.25, 0.5, 0.75), fx = c(1, 1.64872, 2.71828, 4.48169), y = list(x = 0.43)))

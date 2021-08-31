#Método de Bisección
Biseccion <- function(a, b, N, Tol){
  
  #Instancio las listas vacias
  lista_a <- c(NULL)
  lista_b <- c(NULL)
  lista_p <- c(NULL)
  
  
  for (i in 1:N) {
    #Calculo P
    p <- (a+b)/2
    
    #Agrego el valor a cada lista
    lista_p[i] <- p
    lista_a[i] <- a
    lista_b[i] <- b
    
    #Evaluo la función en p
    fp <- f(p)
    
    #Si la f(p) es 0, entonces es raiz
    #O si esta dentro del límite tolerado
    if(fp == 0 | abs((b-a)/2) <= Tol)
    {
      #Creo un data frame con las listas
      datos <- data.frame(lista_a, lista_b, lista_p)
      colnames(datos) <- c("A", "B", "P")
      print(datos)
      return(p)
    }
    #Si comparten el mismo signo
    if(fp * f(a) > 0){
      a <- p
    }
    else
    {
      b <- p
    }
  }
  
  #En el caso de que falle el método
  return(paste('El método falla luego de: ', N, ' iteraciones'))
}

f <- function(x){
  #Hay que modificar la función
  return(x^3+x-4)
  
  #Ejercicio 1:
  #Visualizar las siguientes con 0.00001 dígitos de aproximación
  #x-2^(-x) [0;1]
  #exp(x)-x^2+3*x-2 [0;1]
  #2*x*cos(2x)-(x+1)^2 [-2;-3] y [-1;0]
  #x*cos(x)-2*x^2+3*x-1 [0.2;0.3] y [1.2;1.3]
  
  #Ejercicio 2
  #Visualizar la siguiente raiz con 0.001 dígitos de aproximación
  #x^3+x-4 [1;4]
}

print(Biseccion(1,4,100, 0.001))

#Para visualizar mas decimales utilizar
options(digits = 10)
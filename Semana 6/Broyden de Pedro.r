
# Vector de aproximaciones iniciales ----
x_i <- matrix( c(0.1, 0.1, -0.1),
               nrow = 3 ,
               ncol = 1,
               byrow = TRUE)
# Elementos del Vector FX, funciones ----
f1 <- function(x1,x2,x3){
  3*x1 - cos(x2*x3) - 0.5 
}

f2 <- function(x1,x2,x3){
  x1^2 -81*(x2+0.1)^2+ sin(x3) + 1.06
}

f3 <- function(x1,x2,x3){
  exp(-x1*x2)+20*x3+(10*pi-3)/3
}

# Elementos de la matriz Jacobiana J(X), derivadas de las funciones ----
# primera funcion derivada respecto de cada variable
df1 <- function(x1,x2,x3){
  df11 <- 3
  df12 <- sin(x2 * x3) * x3
  df13 <- sin(x2 * x3) * x2
  
  return(matrix(c(df11,df12,df13),
                nrow = 1, ncol = 3))
}
# segunda funcion derivada respecto de cada variable
df2 <- function(x1,x2,x3){
  df21 <- 2 * x1
  df22 <- -(81 * (2 * (x2 + 0.1))) 
  df23 <- cos(x3)
  
  return(matrix(c(df21,df22,df23),
                nrow = 1, ncol = 3))
}
# tercera funcion derivada respecto de cada variable
df3 <- function(x1,x2,x3){
  df31 <- -(exp(-x1 * x2) * x2)
  df32 <- -(exp(-x1 * x2) * x1)
  df33 <- 20
  
  return(matrix(c(df31,df32,df33),
                nrow = 1, ncol = 3))
}


BroydenSnL <- function(x , TOL, N){
  
  # defino el vector FX adentro para que tome los valores de x
  FX <- matrix(c(f1(x[1],x[2],x[3]),
                 f2(x[1],x[2],x[3]),
                 f3(x[1],x[2],x[3])),ncol = 1,nrow = 3,byrow=TRUE)
  
  # defino la matriz Jacobiana J(X) que tiene las derivadas de las funciones de F respecto a cada variable x
  # ordenada por fila
  JX <- matrix(data = c(df1(x[1],x[2],x[3]),
                        df2(x[1],x[2],x[3]),
                        df3(x[1],x[2],x[3])),ncol = 3, nrow = 3,byrow = TRUE)
  #Paso 1 asigno FX a v
  v <- FX
  print(v)
  # Paso 2 invierto la matriz A0 y la llamo A
  A <- solve(JX)
  # Paso 3 creo el vector s resultado de Av y x 
  s <- -A%*%v  
  x <- x + s
  k <- 2
  # Paso 4 empieza el bucle y va iterando y reasignando los vectores
  while(k <= N){
    
    # Paso 5
    w <- v
    v <- matrix(c(f1(x[1],x[2],x[3]),
                  f2(x[1],x[2],x[3]),
                  f3(x[1],x[2],x[3])),ncol = 1,nrow = 3,byrow=TRUE)
    y <- v-w
    
    # Paso 6 
    z <- -A%*%y
    
    # Paso 7 uso vector s traspuesto para p
    p <- -t(s)%*%z
    
    # Paso 8 (u) traspuesta (el resultado ya es traspuesto)
    u <- t(s)%*%A
    
    # paso 9 Reasigno la matriz A, el vect s y el vect x para la nueva vuelta
    A <- A + (s+z)%*%(u/p[1])
    
    # Paso 10
    s <- -A%*%v
    
    # Paso 11
    x <- x + s
    
    # Paso 12 condicion para salir del while
    norma <- norm(s,type = 'F') 
    if (norma <= TOL){
      return(x)
      break
    }
    k <- k + 1
  }
  return(paste('El procedimiento fallo luego de superar el maximo de', N,'iteraciones'))
}
BroydenSnL(x_i,10^-6,100)
f1(5.000000e-01,5.346354e-13,-5.235988e-01)
f2(5.000000e-01,5.346354e-13,-5.235988e-01)
f3(5.000000e-01,5.346354e-13,-5.235988e-01)

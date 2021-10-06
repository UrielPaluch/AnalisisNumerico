rm(list = ls())


# Método de biseccion -----------------------------------------------------

Biseccion <- function(a, b, N = 100, tol) {
  #Tiene por default 100 iteraciones
  
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
    if (fp == 0 | abs((b-a)/2) <= tol) {
      #Creo un data frame con las listas
      datos <- data.frame(lista_a, lista_b, lista_p)
      colnames(datos) <- c("A", "B", "P")
      print(datos)
      return(paste("La raiz es: ", p))
    }
    
    #Si comparten el mismo signo
    if (fp * f(a) > 0) {
      a <- p
    } else {
      b <- p
    }
  }
  
  #En el caso de que falle el método
  return(paste('El método falla luego de: ', N, ' iteraciones'))
}

# Inputs ------------------------------------------------------------------

cupon <- 0.05

# NULL porque las filas no llevan nombre
amortizaciones <- matrix(c(1, 2, 3, 4,
                           25, 25, 25, 25), ncol = 2, dimnames = list(NULL, c("t", "amort")))

# Numero de pagos de intereses por año
m <- 2



# Marcha ------------------------------------------------------------------

# Numero de flujos
n <- max(amortizaciones[,1])*m

# Creo la matriz de marcHa con las siguientes columnas: t, saldo, amort, interes, flujo
marcha <- matrix(rep(NA, (n+1)*5), ncol = 5)
colnames(marcha) = c("t", "saldo", "amort", "int", "C.F")

# Columna "t"
marcha[,"t"] <- seq(from = 0, to = n/m, by = 1/m)

# Columnas "amort" y "saldo"
k <- 1
marcha[1, "saldo"] <- sum(amortizaciones[,"amort"])
for (i in 1:(n+1)) {
  if(marcha[i, "t"] == amortizaciones[k, "t"]){
    marcha[i, "amort"] <- amortizaciones[k, "amort"]
    if(i>1){
      marcha[i, "saldo"] <- marcha[i-1, "saldo"] - amortizaciones[k, "amort"]
    }
    k <- k +1
  } else{
    marcha[i, "amort"] <- 0
    if(i>1){
      marcha[i, "saldo"] <- marcha[i-1, "saldo"]
    }
  }
}

marcha[1, "int"] <- 0
for (i in 2:(n+1)) {
  marcha[i, "int"] <- marcha[i-1, "saldo"] * cupon / m
}

marcha[,"C.F"] <- marcha[,"amort"] + marcha[,"int"]




# Precio ------------------------------------------------------------------
Precio <- function(tasa, t, CF){
  n <- length(CF)
  p <- 0
  for (i in 1:n) {
    p <- p + CF[i]*(1+tasa)^-t[i]
  }
  return(p)
}

Precio(0.05, marcha[,"t"], marcha[,"C.F"])
tasa <- (1+0.05/2)^2-1
Precio(tasa, marcha[,"t"], marcha[,"C.F"])



# Grafico tasas ------------------------------------------------------------
tasas <- seq(from = 0, to = 1, by = 0.001)
p <- Precio(tasas, marcha[,"t"], marcha[,"C.F"])
plot(tasas, p, type = "b")

# Calculo TIR -------------------------------------------------------------
f <- function(r){return(-80+Precio(r, marcha[,"t"], marcha[,"C.F"]))}
plot(tasas, f(tasas), type = "l", col = "red")
abline(h=0) #linea horizontal en cero
Biseccion(a = 0, b = 0.2, tol =0.000001)


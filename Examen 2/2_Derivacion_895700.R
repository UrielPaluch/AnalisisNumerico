# Uriel Paluch
# 895700


# Métodos -----------------------------------------------------------------

TresPuntos_extremos <- function(fx, h) {
  return((1/(2*h))*(-3*fx[1]+4*fx[2]-fx[3]))
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


# Ejercicio 2. Derivación -------------------------------

# Cargo un df con los datos
df <- data.frame(r = seq(from = 0, to = 0.12, by = 0.01),
                 P = c(115.3371, 109.7031, 104.8099, 100.0653,  95.3516,  91.1998,  87.2676,  83.5825,  79.9116,  76.8435,  73.9473,
                       70.0320, 67.7022))


# 2.1 Derivada primera ----------------------------------------------------
# P'(0.03)
TresPuntos_extremos(fx = c(100.0653, 95.3516, 91.1998), h = 0.01)

# P'(0.04)
TresPuntos_extremos(fx = c(95.3516, 91.1998, 87.2676), h = 0.01)


# Derivada segunda --------------------------------------------------------
derivadas <- print(SegundaDerivada(x = df$r, fx = df$P))

print(paste("P''(0.04) =", 5619))
print(paste("P''(0.03) =", 309))

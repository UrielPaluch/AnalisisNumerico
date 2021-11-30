# Uriel Paluch
# 895700


# librerias ---------------------------------------------------------------

library(ggplot2)

# Métodos -----------------------------------------------------------------
MetodoEuler <- function(a, b, N, alfa, funcion){
  
  df <- data.frame(t = rep(NA, times = (N+1)), w = rep(NA, times = (N+1)))
  
  # Paso 1
  h <- (b - a) / N
  df[1,1] <- a
  df[1,2] <- alfa
  
  
  # Paso 2
  for (i in 1:N) {
    
    # Paso 3
    df[i+1,1] <- a + i*h
    df[i+1,2] <- df[i,2] + h * eval(funcion, list(y = df[i,2], t = (a + (i-1)*h) ))
  }
  
  return(df)
}


RungeKutta <- function(a, b, N, alfa, funcion){
  
  df <- data.frame(t = rep(NA, times = (N+1)), w = rep(NA, times = (N+1)))
  
  h <- (b - a)/N
  df[1,1] <- a
  df[1,2] <- alfa
  t <- a
  
  for (i in 1:N) {
    k <- rep(NA, times = 4)
    
    # Paso 3
    k[1] <- h * eval(funcion, list(t = t, y = df[i,2] ))
    k[2] <- h * eval(funcion, list(t = t + h/2, y = df[i,2] + k[1]/2))
    k[3] <- h * eval(funcion, list(t = t + h/2, y = df[i,2] + k[2]/2))
    k[4] <- h * eval(funcion, list(t = t + h, y = df[i,2] + k[3]))
    
    df[i+1,2] <- df[i,2] + (k[1] + 2*k[2] + 2*k[3] + k[4])/6
    
    t <- a + i * h
    df[i+1,1] <- t
  }
  
  return(df)
}

# a -----------------------------------------------------------------------
# Alfa = y(0)
a <- MetodoEuler(a = 4, b = 5, N = 45, alfa = 0.46, funcion = expression((cos(y)/t^(1.69))+t/y^3))
a

# b -----------------------------------------------------------------------
# Alfa = y(0)
b <- RungeKutta(a = 4, b = 5, N = 4, alfa = 0.46, funcion = expression((cos(y)/t^(1.69))+(t/(y^3))))


# c -----------------------------------------------------------------------

ggplot() +
  geom_line(aes(x = a$t, y = a$w), colour = "darkgreen") +
  geom_line(aes(x = b$t, y = b$w), colour = "darkblue")

# d -----------------------------------------------------------------------
# Los métodos de Taylor de orden superior (como Runge-Kutta) tienen error de truncamiento de orden alto, lo que hace que aproximen mejor,
# pero es necesario conocer las derivadas. En cambio, el método de Euler, lo hace mas "económico" porque no es necesario conocer las derivadas
# pero esto lo hace menos exacto.


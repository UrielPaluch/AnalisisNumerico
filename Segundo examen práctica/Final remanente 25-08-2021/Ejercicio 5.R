# Uriel Paluch
# 895700
# Ejercicio 5


# Método ------------------------------------------------------------------

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

DiferenciasDivididas <- function(x, y){
  n <- length(x)
  
  #Hago un vector vacio para llenar el df
  empty_vec <- rep(0, times = n)  
  
  df <- data.frame(x, y)
  
  for (i in 1:(n-1)) {
    
    df[glue::glue("Q",i)] <- empty_vec
    
    for (j in (i+1):n) {
      
      df[j, (i+2)] <- ( df[j,(i+1)] - df[(j-1),(i+1)])/(x[j]-x[j-i])
    }
  }
  
  return(df)
}

PolinomioInterpolanteNewton <- function(x, y){
  df <- DiferenciasDivididas(x = x, y = y)
  
  #Saco la primer columna del df
  df[,1] <- NULL
  
  n <- ncol(df)
  
  polinomio <- df[1,1]
  
  for (i in 2:n) {
    polinomio <- polinomio + glue::glue(" + ", df[i,i])
    for (j in 1:(i-1)) {
      polinomio <- polinomio + glue::glue(" * ( x - ", x[j], " )")
    }
  }
  return(polinomio)
}

# Ejercicio 5.1 -----------------------------------------------------------
# Un trazador cúbico es un polinomio interpolante de grado 3 que se asigna a cada intervalo.
# La diferencia del "natural" con el "sujeto" es que el segundo utiliza la derivada primera de la 
# función en su punto inicial y su punto final. Esto agrega exactitud al polinomio interpolante 
# pero agrega la dificultad de pedir un dato que no en todos los casos es posible obtener.



# Ejercicio 5.2 -----------------------------------------------------------

df <- data.frame(PA = seq(from = 0.1, to = 1, by = 0.1), L = c(9.4812 , 9.5244, 10.0952, 11.1160, 12.8725, 13.4483, 14.1008, 14.4795, 15.2031, 19.6600))

PolinomioLagrange(x = df$L, fx = df$PA, y = list(x = 14))
PolinomioLagrange(x = df$L, fx = df$PA, y = list(x = 18))

polinomio <- PolinomioInterpolanteNewton(x = df$L, y = df$PA)

x <- seq(from = 9, to = 20, by = 0.01)
y <- eval(parse(text = polinomio), list(x = x))

ggplot() +
  geom_line(aes(x = df$L, y = df$PA), colour = "red") +
  geom_line(aes(x = x, y = y), colour = "blue")

# El polinomio de Lagrange aproxima mal los valores extremos porque es un polinomio de grado 9.

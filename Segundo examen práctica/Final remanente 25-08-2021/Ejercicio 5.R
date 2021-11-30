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

# Ejercicio 5.1 -----------------------------------------------------------
# Un trazador cúbico es un polinomio interpolante de grado 3 que se asigna a cada intervalo.
# La diferencia del "natural" con el "sujeto" es que el segundo utiliza la derivada primera de la 
# función en su punto inicial y su punto final. Esto agrega exactitud al polinomio interpolante 
# pero agrega la dificultad de pedir un dato que no en todos los casos es posible obtener.



# Ejercicio 5.2 -----------------------------------------------------------

df <- data.frame(PA = seq(from = 0.1, to = 1, by = 0.1), L = c(9.4812 , 9.5244, 10.0952, 11.1160, 12.8725, 13.4483, 14.1008, 14.4795, 15.2031, 19.6600))

PolinomioLagrange(x = df$L, fx = df$PA, y = list(x = 14))
PolinomioLagrange(x = df$L, fx = df$PA, y = list(x = 18))

ggplot() +
  geom_line(aes(x = df$L, y = df$PA))

# El polinomio de Lagrange aproxima mal los valores extremos porque es un polinomio de grado 9.

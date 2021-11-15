SplineCondicionado <- function(x, y, fpo, fpn){
  #browser()
  n <- length(x)
  
  # Paso 1
  h <- rep(NA, times = (n-1))
  for (i in 1:(n-1)) {
    h[i] <- x[i+1] - x[i]
  }; rm(i)
  
  # Paso 2
  alfa <- rep(NA, times = n)
  alfa[1] <- 3 * (y[2] - y[1])/h[1] - 3 * fpo
  alfa[n] <- 3 * fpn - 3 * (y[n] - y[n-1]) / h[n-1]
  
  # Paso 3
  for (i in 2:(n-1)) {
    alfa[i] <- (3/h[i]) * (y[i+1] - y[i]) - (3/h[i-1]) * (y[i] - y[i-1])
  }; rm(i)
  
  # Paso 4
  mu <- rep(NA, times = n)
  zeta <- rep(NA, times = n)
  l <- rep(NA, times = n)
  
  l[1] <- 2 * h[1]
  mu[1] <- 0.5
  zeta[1] <- alfa[1]/l[1]
  
  
  
  # Paso 5
  for (i in 2:(n-1)) {
    l[i] <- 2 * (x[i+1] - x[i-1]) - h[i-1] * mu[i-1]
    mu[i] <- h[i]/l[i]
    zeta[i] <- (alfa[i] - h[i-1] * zeta[i-1])/l[i]
  }
  
  # Paso 6
  l[n] <- h[n-1] * (2 - mu[n-1])
  zeta[n] <- (alfa[n] - h[n-1] * zeta[n-1]) / l[n]
  c <- rep(NA, times = n)
  c[n] <- zeta[n]
  
  # Paso 7
  b <- rep(NA, times = (n-1))
  d <- rep(NA, times = (n-1))
  for (j in (n-1):1) {
    c[j] <- zeta[j] - mu[j] * c[j+1]
    b[j] <- (y[j+1] - y[j]) / h[j] - h[j] * (c[j+1] + 2 * c[j])/3
    d[j] <- (c[j+1] - c[j]) / (3*h[j])
  }
  
  #Paso 7
  resultados <- matrix(rep(NA, 4*(n-1)), nrow = (n-1), ncol = 4, byrow = F)
  for (k in 1:(n-1)) {
    resultados[k, 1] <- y[k]
    resultados[k, 2] <- b[k]
    resultados[k, 3] <- c[k]
    resultados[k, 4] <- d[k]
  }
  
  print(resultados)
  
  #Construyo el polinomio
  polinomios <- rep(NA, times = nrow(resultados))
  for (i in 1:nrow(resultados)) {
    polinomios[i] <- glue::glue(resultados[i,1]) 
    for(j in 2:ncol(resultados)){
      polinomios[i] <- polinomios[i] + glue::glue(" + ", resultados[i,j], " * (x - ", x[i], ")^", (j-1)) 
    }
  }
  
  return(polinomios)
}
SplineCondicionado(x = c(0, 1, 2, 3), y = c(1, exp(1), exp(2), exp(3)), fpo = 1, fpn = exp(3))

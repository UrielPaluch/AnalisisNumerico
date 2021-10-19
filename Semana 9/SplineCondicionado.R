TrazadorCubicoCondicionado = function(x, y, fpo, fpn){
  n = length(y)
  j = n - 1
  
  a = y
  b = c(rep(NA,n))
  c = c(rep(NA,n))
  d = c(rep(NA,n))
  
  A = c(rep(NA,n))
  h = c(rep(NA,n))
  l = c(rep(NA,n))
  u = c(rep(NA,n))
  z = c(rep(NA,n))
  
  #Paso 1
  for (i in 1:j) { 
    h[i] = x[i + 1] - x[i]
  }
  
  #Paso 2
  A[1] = 3*(a[2] - a[1])/(h[1]) - 3*fpo#Atencion indices y division
  A[n] = 3*fpn - 3*(a[n] - a[n-1])/(h[n-1])
  
  #Paso 3
  for (i in 2:j) {
    A[i] = 3*(a[i+1] - a[i])/(h[i]) - 3*(a[i] - a[i-1])/(h[i-1])
  }
  
  #Paso 4
  l[1] = 2*h[1]
  u[1] = 0.5
  z[1] = A[1]/l[1]
  
  #Paso 5
  for (i in 2:j) {
    l[i] = 2*(x[i+1] - x[i-1]) - h[i-1]*u[i-1]
    u[i] = h[i]/l[i]
    z[i] = (A[i] - h[i-1]*z[i-1])/l[i]
  }
  
  #Paso 6
  l[n] = h[n-1]*(2 - u[n-1])
  z[n] = (A[n] - h[n-1]*z[n-1])/l[n]
  c[n] = z[n]
  
  #Paso 7
  for (i in j:1) {
    c[i] = z[i] - u[i]*c[i+1]
    b[i] = (a[i+1] - a[i])/h[i] - h[i] * (c[i+1] + 2*c[i])/3
    d[i] = (c[i+1] - c[i])/(3*h[i])
  }
  
  #Paso 8
  results = matrix(rep(NA, 4*j), nrow = j, ncol = 4, byrow = F)
  for (k in 1:j) {
    results[k, 1] = a[k]
    results[k, 2] = b[k]
    results[k, 3] = c[k]
    results[k, 4] = d[k]
  }
  
  #Construyo el polinomio
  polinomios <- rep(NA, times = nrow(results))
  for (i in 1:nrow(results)) {
    polinomios[i] <- glue::glue(results[i,1]) 
    for(j in 2:ncol(results)){
      polinomios[i] <- polinomios[i] + glue::glue(" + ", results[i,j], " * (x - ", x[i], ")^", (j-1)) 
    }
  }
  
  return(polinomios)
}

#fpo es la derivada de la función valuada en el primer elemento
#fpn es la derivada de la función valuada en el elemento n
TrazadorCubicoCondicionado(x = c(0, 1, 2, 3), y = c(1, exp(1), exp(2), exp(3)), fpo = 1, fpn = exp(3))

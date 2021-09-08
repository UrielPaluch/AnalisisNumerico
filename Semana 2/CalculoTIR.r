## calculo de TIR ----

rm(list = ls()) # limpia la consola y el environment
## Resolucion numerica biseccion ----
Biseccion <- function(f,a, b, TOL, N){
  #paso 1 creo contador i
  i <- 1
  FA = f(a)
  #paso 2 bucle para la iteracion con i (contador)
  while (i <= N){
    p <- a + (b-a)/2
    FP <- f(p)
    #paso 3 cumplimiento de condicion, devuelve la resolucion correcta
    if (FP == 0 | (b-a)/2 < TOL ){
      return(p)
    }
    i <- i + 1
    if (FA*FP > 0){
      a <- p 
      FA <- FP
    } else{
      b <- p
    }
  }
  #paso 4 cuando i > N
  return(paste('El metodo fallo luego de', N,' iteraciones'))
}

## 1 Inputs ----

tcupon = 0.05 # TNA divido por la cantidad de vecs que cap en un anio (2)
pagoCupon = matrix(c(1, 2, 3, 4,
                       25,25,25,25), ncol = 2, dimnames = list(NULL, c('t', 'amortizaciones')))
m = 2
View(pagoCupon)

## 2 marcha progresiva ----

#agarra el maximo de [ , 1] todas las filas de la columna 1
n = max(pagoCupon[ , 1])*m

#creo la matriz de marcha de 9X5 (fxC)
marcha = matrix(rep(NA, (n+1)*5), ncol = 5)
colnames(marcha) = c('t', 'Saldo', 'Amortizacion', 'Interes', 'Cash flow')

  # columna 't' ----
marcha[ , 't'] = seq(from = 0 ,to = n/m, by = 1/m)

  # columnas 'Amortizacion' y 'Saldo' ----
k = 1
marcha[1,'Saldo'] = sum(pagoCupon[ , 'amortizaciones'])

# completa las filas de la columna amortizacion y saldo por periodo
for (i in 1:(n+1)){
  if (marcha[i, 't'] == pagoCupon[k,'t']){ # esto corre para t = 1,2,3,4 y varia el saldo
    marcha[i, 'Amortizacion'] = pagoCupon[k, 'amortizaciones']
    if (i > 1){marcha[i, 'Saldo'] = marcha[i-1, 'Saldo'] - pagoCupon[k, 'amortizaciones']
    }
    k = k + 1
  }else{ # para los t = 0.5 - 1.5 - 2.5 - 3.5, no varia el Saldo
    marcha[i, 'Amortizacion'] = 0
    if (i > 1){marcha[i, 'Saldo'] = marcha[i-1, 'Saldo']}
  }
}

  # columna 'Interes' ----
marcha[1, 'Interes'] = 0
for (i in 2:(n+1)){
  marcha[i, 'Interes'] = marcha[i-1, 'Saldo']*tcupon/m 
}

  # columna 'Cash flow' ----
marcha[ , 'Cash flow'] = marcha[ , 'Interes'] + marcha[ , 'Amortizacion']

View(marcha)

## 3 funcion de precio de bono ----
precio <- function(tasa, t, CashFlow){
  n = length(CashFlow)
  p = 0
  for (i in 1:n){
    p = p + CashFlow[i]*(1+tasa)^(-t[i])
  }
  return(p)
}

tasaefectiva = (1 + tcupon/2)^2 -1
precio(tasaefectiva, marcha[ , 't'], marcha[ ,'Cash flow'])


  # graficos ----
tasas = seq(from = 0 , to = 1, by = 0.001)
P = precio(tasas, marcha[ , 't'], marcha[ , 'Cash flow'])
plot(tasas, P, type='l') # eje x, eje y, type = 'b' con linea y circulos

## 4 Resolucion de ecuacion ----
PM = 80 #precio que disponga el ejercicio
f <- function(r){-PM + precio(r, marcha[ , 't'], marcha[ , 'Cash flow'])}
plot(tasas, f(tasas), type = 'l', col = 'red')
abline(h=0)
Biseccion(f,0, 0.2, 0.00001, 100)


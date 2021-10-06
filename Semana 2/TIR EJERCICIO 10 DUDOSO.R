#### Punto 10 ----
## 1. Inputs 

cupon = 0.22
Amortizaciones = matrix( c(1 , 2,
                           0, 100 ), ncol = 2, dimnames = list(NULL,c("t","Amort")))
sum(Amortizaciones[,"Amort"])
m = 2 #Pagos por anio
sum(Amortizaciones[,2])

## 2. Marcha progresiva 

n = (max(Amortizaciones[,1]))*m #Traigo el max período de la columna 1, y lo multiplico por m(pagos por anio)
#Creo la matriz de marchas con las siguientes columnas: t, Saldo,Amort, Interes, Flujo
Marcha = matrix( rep(NA, (n+1)*5),ncol=5) #La matriz tendra tantas filas como FF ocurran y 5 columnas
colnames(Marcha) = c("t","Saldo","Amort","Int","C.F.")

#columna "t"
Marcha[,"t"] = seq(from = 0, to = n/m, by = 1/m)
Marcha
# columnas "Amort" y "Saldo"
k=1
Marcha[1,"Saldo"] = sum(Amortizaciones[,"Amort"])
for (i in 1:(n+1)) {  #Hace bucle en la matriz de marcha
  if (Marcha[i,"t"] == Amortizaciones[k,"t"]) {
    Marcha[i,"Amort"] = Amortizaciones [k,"Amort"]
    if(i>1){Marcha[i,"Saldo"] = Marcha[i-1,"Saldo"] - Amortizaciones[k,"Amort"]}
    k=k+1
  } else {
    Marcha[i,"Amort"] = 0
    if(i>1){Marcha[i,"Saldo"] = Marcha[i-1,"Saldo"]
    }
  }
}
Marcha
#Columna "int"
Marcha[1,"Int"] = 0 
for (i in 2:(n+1)) { #Hace bucle en matriz de marcha
  Marcha[i,"Int"] = Marcha[i-1,"Saldo"]*cupon/m  #La tasa siempre es nominal
}
Marcha[,"C.F."] = Marcha[,"Amort"] + Marcha[,"Int"]
Marcha[,"C.F."] = round(Marcha[,"Amort"] + Marcha[,"Int"], digits = 3)
Marcha[,"Saldo"]<-round(Marcha[,"Saldo"],digits = 4) #Lo force porque no me daba exacto el Saldo
Marcha
Nov  <- (seq(as.Date("2020-11-21"), as.Date("2021-11-21"), "years"))
May  <- (seq(as.Date("2021-5-21"), as.Date("2022-5-21"), "years"))
T <- c(rbind(Nov,May))
T <- as.Date(T, origin = "1970-1-1")
T <- c(as.Date("2020/07/17"),T)
Marcha <- transform(Marcha,t = as.Date(as.character(T), "%Y%m%d"))
Marcha[,"t"] <- as.Date(T)
DHP <- rep(NA,5)
for (i in 2:5) {
  DHP[i] <- T[i] - T[1]
}
DHP[1] <- 0 #Dias hasta proximo pago
AHP <- DHP/365
AHP #Años hasta proximo pago, lo saque asi para hacer la t en F de precio
DHP
Marcha <- cbind(0:4,Marcha,DHP,AHP)
colnames(Marcha)<-c("t","Fecha de pago","Saldo","Amort","Int","C.F.","DHP","AHP")

## 3. Armado de funcion de precio 

#Hay que armar una funcion P_T(r)= sum[(CF_t_i)*(1+r)^-(t_i)]
Precio <- function(tasa, t, CF){
  n = length(CF)
  P = 0
  for (i in 1:n){
    P = P + CF[i]*(1+tasa)^-t[i]
  }
  return(P)
}
tasa1 <- (1+cupon/m)^m - 1
tasa1
(PTY22 <- Precio(tasa1,Marcha[,"AHP"],Marcha[,"C.F."]))

tasas = seq(from = 0, to = 1.3, by = 0.001)
P = Precio(tasas,Marcha[,"AHP"],Marcha[,"C.F."]) #Precio del periodo 
plot(tasas,P, type = "l")


#Hasta acá está OK, devuelve VN

## 4. Resolucion de la ecuación con mediante Biseccion 
#Precio Teorico(TIR) = Precio de mercado
#sum(Cashflowst_i)*(1+TIR)^-t_i=PM
#-PM + sum(CFt_i)*(1+tir)^-t_i = 0
#f(r) = -PM + sum(CFt_i)*(1+r)^-t_i
PMTY22 <- 103.5
f<-function(r){return(-PMTY22 + Precio(r,(Marcha[,"AHP"]),Marcha[,"C.F."]))}
fp<-function(r){return(-PMTY22 + Precio(r,(Marcha[,"AHP"]),Marcha[,"C.F."]))}
plot(tasas,f(tasas),type = "l", col =  "red")
abline(h=0)

Biseccion <- function(a, b, TOL, N){
  #paso 1 creo contador i
  i <- 1
  FA = fp(a)
  #paso 2 bucle para la iteracion con i (contador)
  while (i <= N){
    p = a + (b-a)/2
    FP = fp(p)
    #paso 3 cumplimiento de condicion, devuelve la resolucion correcta
    if (FP == 0 | (b-a)/2 < TOL ){
      return(paste('f(',p,') ~', fp(p) ,'y se necesitaron ', i ,'iteraciones para llegar'))
    }
    i <- i + 1
    if (FA*FP > 0){
      a <- p 
      FA <- FP
    } else{
      b <- p
    }
  }
}

Biseccion(0,0.3,10^-6,20)
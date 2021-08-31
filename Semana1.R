raices <- function(a, b, c){
  d <-  b^2-4*a*c
  
  if (d >= 0){
    raiz1 <- (-b+sqrt(b^2-4*a*c))/2*a
    raiz2 <- (-b-sqrt(b^2-4*a*c))/2*a
    resultado <- c(raiz1, raiz2)
    return(resultado)
  }
  else{
    return("No calcula raices imaginarias")
  }
  
}

print(raices(1,2,1))


#Borra todas las variables
rm(list = ls(all=TRUE))

#Funci?n factorial
factorial <- function(a){
  b <- 1
  for (i in 1:a){
    b <- i * b
  }
  return(b)
}

factorial(6)

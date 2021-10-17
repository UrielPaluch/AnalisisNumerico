
# Configuracion -----------------------------------------------------------

options(scipen = 999)


# Metodo de Neville -------------------------------------------------------

Neville <- function(x, y, interpolar){
  #cantidad de iteraciones que voy a hacer
  n <- length(x)-1
  
  #Hago un vector vacio para llenar el df
  empty_vec <- rep(0, times = length(x))  

  df <- data.frame(x, y)
  
  for (i in 1:n) {
    df[glue::glue("Q",i)] <- empty_vec
    
    for (j in (i+1):(n+1)) {
      
      df[j, (i+2)] <- ( (interpolar-x[(j-i)]) * df[j,(i+1)] - (interpolar-x[j]) * df[(j-1),(i+1)] )  / (x[j]-x[j-i])
      
    }
  }
  
  print(df)
}


# Llamado a la funcion ----------------------------------------------------
# x es la preimagen
# y es la imagen
# interpolar es el número que se desea interpolar
Neville(x = c(0, 0.25, 0.5, 0.75), y = c(1, 1.64872, 2.71828, 4.48169), interpolar = 0.43)

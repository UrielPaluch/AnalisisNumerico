
# Configuracion -----------------------------------------------------------
options(scipen = 999)


# Metodo de diferencias divididas -----------------------------------------
DiferenciasDivididas <- function(x, y){
  n <- length(x)
  
  #Hago un vector vacio para llenar el df
  empty_vec <- rep(0, times = n)  
  
  df <- data.frame(x, y)
  
  for (i in 1:(n-1)) {
    
    df[glue::glue("Q",i)] <- empty_vec
    
    for (j in (i+1):n) {
      
      #print(paste("j: ", j, " i+2: ", i+2))
      df[j, (i+2)] <- ( df[j,(i+1)] - df[(j-1),(i+1)])/(x[j]-x[j-i])
    }
  }
  
  return(df)
}


# Llamado a la función ----------------------------------------------------

print(DiferenciasDivididas(x = c(1, 1.3, 1.6, 1.9, 2.2), y = c(0.7651977, 0.6200860, 0.4554022, 0.2818186, 0.1103623)))
